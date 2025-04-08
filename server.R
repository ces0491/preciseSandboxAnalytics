server <- function(input, output, session) {
  
  # Helper function for percentage change
  calc_percent_change <- function(data, value_col, date_col) {
    latest <- data %>% 
      dplyr::arrange(desc({{date_col}})) %>% 
      dplyr::slice(1:2) %>% 
      dplyr::pull({{value_col}})
    if (length(latest) == 2) {
      return(round((latest[1] - latest[2]) / latest[2] * 100, 1))
    }
    return(NA)
  }
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- my_revenue
    if (!is.null(input$dateRange)) {
      data <- data %>% 
        dplyr::filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    }
    if (!is.null(input$artistFilter) && length(input$artistFilter) > 0) {
      data <- data %>% 
        dplyr::filter(artist_name %in% input$artistFilter)
    }
    return(data)
  })
  
  filtered_platform_data <- reactive({
    data <- filtered_data()
    if (!is.null(input$platformFilter) && length(input$platformFilter) > 0) {
      data <- data %>% 
        dplyr::filter(platform %in% input$platformFilter)
    }
    return(data)
  })
  
  filtered_track_data <- reactive({
    data <- my_track_performance
    if (!is.null(input$dateRange)) {
      data <- data %>% 
        dplyr::filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    }
    if (!is.null(input$artistFilter) && length(input$artistFilter) > 0) {
      data <- data %>% 
        dplyr::filter(artist_name %in% input$artistFilter)
    }
    return(data)
  })
  
  filtered_tracks <- reactive({
    data <- my_tracks
    if (!is.null(input$artistFilter) && length(input$artistFilter) > 0) {
      data <- data %>% 
        dplyr::filter(artist_name %in% input$artistFilter & !is.na(artist_name))
    }
    # Ensure we return at least an empty data frame with the correct structure if no data matches
    if (nrow(data) == 0) {
      data <- dplyr::tibble(
        track_name = character(),
        artist_name = character(),
        isrc = character(),
        release_date = as.Date(character()),
        duration_sec = numeric(),
        genre = character()
      )
    }
    return(data)
  })
  
  # Welcome message
  output$welcomeMessage <- renderText({
    paste0("Welcome back! You are managing ", nrow(my_artists), " artists. Here's your latest performance summary.")
  })
  
  output$dateRangeMessage <- renderUI({
    shiny::HTML(paste0(
      "<small>Showing data from ",
      format(input$dateRange[1], "%d %b %Y"),
      " to ",
      format(input$dateRange[2], "%d %b %Y"),
      "</small>"
    ))
  })
  
  # Value Boxes
  output$totalArtistsBox <- renderValueBox({
    shinydashboard::valueBox(
      value = tags$p(nrow(my_artists), style = "font-size: 24px;"),
      subtitle = tags$p(actionLink("artistsLink", "Artists Under Management"), style = "font-size: 14px;"),
      icon = shiny::icon("users"),
      color = "maroon"
    )
  })
  
  output$totalRevenueBox <- renderValueBox({
    data <- filtered_data()
    total_revenue <- data %>%
      dplyr::summarize(total = sum(revenue, na.rm = TRUE)) %>%
      dplyr::pull(total)
    
    shinydashboard::valueBox(
      value = tags$p(format_currency(total_revenue, input$currencySelector), style = "font-size: 24px;"),
      subtitle = tags$p(actionLink("revenueLink", "Total Revenue"), style = "font-size: 14px;"),
      icon = shiny::icon("dollar-sign"),
      color = "red"
    )
  })
  
  output$revenueChangeBox <- renderValueBox({
    data <- filtered_data()
    monthly_data <- data %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(revenue = sum(revenue, na.rm = TRUE))
    pct_change <- calc_percent_change(monthly_data, revenue, date)
    
    shinydashboard::valueBox(
      value = tags$p(ifelse(is.na(pct_change), "N/A", paste0(ifelse(pct_change > 0, "+", ""), pct_change, "%")), style = "font-size: 24px;"),
      subtitle = "Revenue Change (MoM)",
      icon = shiny::icon(ifelse(is.na(pct_change) || pct_change >= 0, "arrow-up", "arrow-down")),
      color = ifelse(is.na(pct_change) || pct_change >= 0, "red", "maroon")
    )
  })
  
  output$outstandingBalanceBox <- renderValueBox({
    total_balance <- my_balances %>%
      dplyr::summarize(total = sum(balance, na.rm = TRUE)) %>%
      dplyr::pull(total)
    
    shinydashboard::valueBox(
      value = tags$p(format_currency(total_balance, input$currencySelector), style = "font-size: 24px;"),
      subtitle = "Outstanding Balance",
      icon = shiny::icon("credit-card"),
      color = "maroon"
    )
  })
  
  output$totalTracksBox <- renderValueBox({
    track_count <- filtered_tracks() %>% dplyr::n_distinct()
    shinydashboard::valueBox(
      value = tags$p(track_count, style = "font-size: 24px;"),
      subtitle = tags$p(actionLink("tracksLink", "Total Tracks in Catalog"), style = "font-size: 14px;"),
      icon = shiny::icon("music"),
      color = "red"
    )
  })
  
  # Navigation handlers
  observeEvent(input$artistsLink, {
    shinydashboard::updateTabItems(session, "tabs", "artist_portfolio")
  })
  
  observeEvent(input$revenueLink, {
    shinydashboard::updateTabItems(session, "tabs", "revenue")
  })
  
  observeEvent(input$tracksLink, {
    shinydashboard::updateTabItems(session, "tabs", "catalog")
  })
  
  # Notifications
  output$notificationsMenu <- renderUI({
    alerts <- get_artists_with_alerts()
    notification_items <- lapply(1:min(nrow(alerts), 5), function(i) {
      shinydashboard::notificationItem(
        text = paste0(alerts$artist_name[i], ": ", alerts$alert_message[i]),
        icon = if(grepl("drop|debt", alerts$alert_message[i])) shiny::icon("arrow-down") else shiny::icon("arrow-up"),
        status = if(grepl("drop|debt", alerts$alert_message[i])) "danger" else "success"
      )
    })
    do.call(tagList, notification_items)
  })
  
  output$alertsList <- renderUI({
    alerts <- get_artists_with_alerts()
    alert_items <- lapply(1:nrow(alerts), function(i) {
      alert_class <- if(grepl("drop|debt", alerts$alert_message[i])) "alert-item" else "alert-item positive"
      shiny::div(
        class = alert_class,
        shiny::div(class = "alert-title", alerts$artist_name[i]),
        shiny::p(alerts$alert_message[i])
      )
    })
    do.call(tagList, alert_items)
  })
  
  # Overview Plots
  output$revenueTimeSeries <- renderPlotly({
    monthly_data <- filtered_data() %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(date)
    
    # Ensure we have enough data for forecasting and rolling average
    if (nrow(monthly_data) < 3) {
      return(
        plot_ly(monthly_data, x = ~date, y = ~revenue, type = 'scatter', mode = 'lines+markers',
                name = "Actual", line = list(color = precise_colors$primary),
                marker = list(color = precise_colors$primary)) %>%
          layout(
            title = "Insufficient data for forecast or rolling average",
            xaxis = list(title = ""),
            yaxis = list(title = paste0("Revenue (", input$currencySelector, ")"),
                         tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f"),
            legend = list(orientation = "h", y = -0.2)
          ) %>% precise_theme()
      )
    }
    
    # Create data frame for plotting
    plot_data <- monthly_data
    
    # Add rolling average if enabled
    if (input$showRollingAvg) {
      plot_data$rolling_avg <- zoo::rollmean(plot_data$revenue, k = 3, fill = NA, align = "right")
    } else {
      plot_data$rolling_avg <- NA
    }
    
    # Create base plot
    p <- plot_ly() %>%
      add_trace(
        data = plot_data,
        x = ~date, y = ~revenue, type = 'scatter', mode = 'lines+markers',
        name = "Actual", line = list(color = precise_colors$primary),
        marker = list(color = precise_colors$primary)
      )
    
    # Add rolling average trace if it exists
    if (input$showRollingAvg && any(!is.na(plot_data$rolling_avg))) {
      p <- p %>% add_trace(
        data = plot_data,
        x = ~date, y = ~rolling_avg, name = "3-Month Avg",
        type = 'scatter', mode = 'lines',
        line = list(color = precise_colors$neutral, dash = "dash")
      )
    }
    
    # Add forecast if enabled
    if (input$showForecast) {
      # Convert to time series
      ts_data <- ts(plot_data$revenue, frequency = 12)
      
      # Fit ARIMA model with error handling
      tryCatch({
        fit <- auto.arima(ts_data)
        fcast <- forecast(fit, h = 6)  # 6-month forecast
        
        # Create forecast data frame
        future_dates <- seq(max(plot_data$date) + months(1), by = "month", length.out = 6)
        forecast_data <- data.frame(
          date = future_dates,
          forecast_mean = as.numeric(fcast$mean),
          lower_80 = as.numeric(fcast$lower[,1]),
          upper_80 = as.numeric(fcast$upper[,1]),
          lower_95 = as.numeric(fcast$lower[,2]),
          upper_95 = as.numeric(fcast$upper[,2])
        )
        
        # Add forecast trace
        p <- p %>% 
          add_trace(
            data = forecast_data,
            x = ~date, y = ~forecast_mean, name = "Forecast",
            type = 'scatter', mode = 'lines',
            line = list(color = precise_colors$warning)
          ) %>%
          add_ribbons(
            data = forecast_data,
            x = ~date, ymin = ~lower_95, ymax = ~upper_95,
            name = "95% CI", line = list(color = "rgba(255,193,7,0.2)"),
            fillcolor = "rgba(255,193,7,0.2)"
          ) %>%
          add_ribbons(
            data = forecast_data,
            x = ~date, ymin = ~lower_80, ymax = ~upper_80,
            name = "80% CI", line = list(color = "rgba(255,193,7,0.4)"),
            fillcolor = "rgba(255,193,7,0.4)"
          )
      }, error = function(e) {
        # If forecast fails, show warning in plot
        p <<- p %>% layout(
          annotations = list(
            text = "Forecast unavailable due to insufficient data",
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(color = precise_colors$danger)
          )
        )
      })
    }
    
    # Finalize layout
    p %>% layout(
      title = "",
      xaxis = list(title = ""),
      yaxis = list(
        title = paste0("Revenue (", input$currencySelector, ")"),
        tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f"
      ),
      legend = list(orientation = "h", y = -0.2)
    ) %>% precise_theme()
  })
  
  output$platformDistribution <- renderPlotly({
    platform_data <- filtered_data() %>%
      dplyr::group_by(platform) %>%
      dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(desc(revenue))
    plot_ly(data = platform_data, labels = ~platform, values = ~revenue, type = 'pie',
            marker = list(colors = precise_palette, line = list(color = '#FFFFFF', width = 1))) %>%
      layout(
        title = "",
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2)
      ) %>%
      precise_theme()
  })
  
  output$performanceComparison <- renderPlotly({
    my_monthly_revenue <- filtered_data() %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(type = "Your Artists")
    company_avg <- my_monthly_revenue %>%
      dplyr::mutate(revenue = revenue * runif(n(), 0.7, 0.9), type = "Company Average")
    comparison_data <- rbind(my_monthly_revenue, company_avg)
    plot_ly(data = comparison_data, x = ~date, y = ~revenue, color = ~type, type = 'scatter', mode = 'lines',
            colors = c(precise_colors$primary, precise_colors$neutral)) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = paste0("Revenue (", input$currencySelector, ")"),
                     tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f")
      ) %>%
      precise_theme()
  })
  
  # Revenue Plots
  output$artistRevenue <- renderPlotly({
    artist_revenue <- filtered_data() %>%
      dplyr::group_by(artist_name) %>%
      dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(desc(revenue)) %>%
      dplyr::top_n(10, revenue)
    plot_ly(data = artist_revenue, y = ~reorder(artist_name, revenue), x = ~revenue, type = 'bar', orientation = 'h',
            marker = list(color = precise_colors$primary), source = "artistRevenue") %>%
      layout(
        title = "",
        xaxis = list(title = paste0("Revenue (", input$currencySelector, ")"),
                     tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f"),
        yaxis = list(title = "")
      ) %>%
      precise_theme() %>%
      event_register("plotly_click")  # Chain directly
  })
  
  output$platformRevenueTrend <- renderPlotly({
    platform_trend <- filtered_platform_data() %>%
      dplyr::group_by(date, platform) %>%
      dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop")
    plot_ly(data = platform_trend, x = ~date, y = ~revenue, color = ~platform, type = 'scatter', mode = 'lines',
            colors = precise_palette) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = paste0("Revenue (", input$currencySelector, ")"),
                     tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f")
      ) %>%
      precise_theme()
  })
  
  output$revenueGrowth <- renderPlotly({
    monthly_data <- filtered_data() %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        prev_month = dplyr::lag(revenue),
        growth_pct = (revenue - prev_month) / prev_month * 100
      ) %>%
      dplyr::filter(!is.na(growth_pct))
    plot_ly(data = monthly_data, x = ~date, y = ~growth_pct, type = 'bar',
            marker = list(color = ifelse(monthly_data$growth_pct >= 0, precise_colors$primary, precise_colors$danger))) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = "Monthly Growth Rate (%)")
      ) %>%
      precise_theme()
  })
  
  output$revenueDistribution <- renderPlotly({
    if (input$distributionType == "Platform") {
      dist_data <- filtered_data() %>%
        dplyr::group_by(platform) %>%
        dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(desc(revenue))
      category_var <- ~platform
    } else {
      dist_data <- filtered_data() %>%
        dplyr::group_by(artist_name) %>%
        dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(desc(revenue)) %>%
        dplyr::top_n(10, revenue)
      category_var <- ~artist_name
    }
    plot_ly(data = dist_data, labels = category_var, values = ~revenue, type = 'pie',
            marker = list(colors = precise_palette, line = list(color = '#FFFFFF', width = 1))) %>%
      layout(
        title = "",
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2)
      ) %>%
      precise_theme()
  })
  
  # Artist Portfolio Plots
  output$artistPerformance <- renderPlotly({
    artist_performance <- filtered_data() %>%
      dplyr::group_by(artist_name, date) %>%
      dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop")
    plot_ly(data = artist_performance, x = ~date, y = ~revenue, color = ~artist_name, type = 'scatter', mode = 'lines',
            colors = precise_palette) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = paste0("Revenue (", input$currencySelector, ")"),
                     tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f")
      ) %>%
      precise_theme()
  })
  
  output$artistBalances <- renderPlotly({
    balance_data <- my_balances %>% 
      dplyr::filter(artist_name %in% if(is.null(input$artistFilter) || length(input$artistFilter) == 0) my_balances$artist_name else input$artistFilter) %>%
      dplyr::arrange(balance)
    plot_ly(data = balance_data, y = ~reorder(artist_name, balance), x = ~balance, type = 'bar', orientation = 'h',
            marker = list(color = ifelse(balance_data$balance >= 0, precise_colors$primary, precise_colors$danger))) %>%
      layout(
        title = "",
        xaxis = list(title = paste0("Balance (", input$currencySelector, ")"),
                     tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f"),
        yaxis = list(title = "")
      ) %>%
      precise_theme()
  })
  
  output$artistTable <- renderDT({
    artist_summary <- my_artists %>%
      dplyr::left_join(
        filtered_data() %>%
          dplyr::group_by(artist_id) %>%
          dplyr::summarize(total_revenue = sum(revenue, na.rm = TRUE), .groups = "drop"),
        by = "artist_id"
      ) %>%
      dplyr::left_join(my_balances %>% dplyr::select(artist_id, balance), by = "artist_id") %>%
      dplyr::mutate(
        total_revenue = ifelse(is.na(total_revenue), 0, total_revenue),
        revenue_formatted = format_currency(total_revenue, input$currencySelector),
        balance = ifelse(is.na(balance), 0, balance),
        balance_formatted = format_currency(balance, input$currencySelector),
        join_date = format(join_date, "%d %b %Y"),
        contract_end = format(contract_end, "%d %b %Y")
      ) %>%
      dplyr::select(artist_name, genre, status, join_date, contract_end, revenue_formatted, balance_formatted)
    
    colnames(artist_summary) <- c("Artist Name", "Genre", "Status", "Join Date", "Contract End", "Total Revenue", "Balance")
    
    DT::datatable(
      artist_summary,
      options = list(
        pageLength = 10,
        dom = 'ftip',
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(2, 3, 4)),
          list(className = 'dt-right', targets = c(5, 6))
        )
      ),
      rownames = FALSE,
      selection = "single"
    )
  })
  
  # Catalog Plots
  output$trackTimeline <- renderPlotly({
    track_timeline <- filtered_tracks() %>%
      dplyr::group_by(artist_name, release_date) %>%
      dplyr::summarize(track_count = n(), .groups = "drop") %>%
      dplyr::filter(release_date >= input$dateRange[1] & release_date <= input$dateRange[2])
    plot_ly(data = track_timeline, x = ~release_date, y = ~track_count, color = ~artist_name, type = 'scatter', mode = 'markers',
            marker = list(size = 12), colors = precise_palette) %>%
      layout(
        title = "",
        xaxis = list(title = "Release Date"),
        yaxis = list(title = "Number of Tracks")
      ) %>%
      precise_theme()
  })
  
  output$topTracks <- renderPlotly({
    top_tracks_data <- filtered_track_data()
    if (input$topTrackMetric == "streams") {
      top_tracks <- top_tracks_data %>%
        dplyr::group_by(track_id, track_name, artist_name) %>%
        dplyr::summarize(total_streams = sum(streams, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(desc(total_streams)) %>%
        dplyr::top_n(10, total_streams)
      plot_ly(data = top_tracks, y = ~reorder(track_name, total_streams), x = ~total_streams, type = 'bar', orientation = 'h',
              marker = list(color = precise_colors$primary)) %>%
        layout(
          title = "",
          xaxis = list(title = "Total Streams"),
          yaxis = list(title = "")
        ) %>%
        precise_theme()
    } else {
      # Calculate total revenue per artist
      artist_revenue <- filtered_data() %>%
        dplyr::group_by(artist_id) %>%
        dplyr::summarize(total_revenue = sum(revenue, na.rm = TRUE), .groups = "drop")
      
      # Join with my_tracks and apportion revenue
      track_revenue <- my_tracks %>%
        dplyr::left_join(artist_revenue, by = "artist_id", relationship = "many-to-one") %>%
        dplyr::group_by(artist_id) %>%
        dplyr::mutate(track_count = n(), 
                      total_revenue = ifelse(is.na(total_revenue), 0, total_revenue),
                      track_revenue = total_revenue / track_count) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(track_id, track_name, artist_name) %>%
        dplyr::summarize(total_revenue = sum(track_revenue, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(desc(total_revenue)) %>%
        dplyr::top_n(10, total_revenue)
      
      plot_ly(data = track_revenue, y = ~reorder(track_name, total_revenue), x = ~total_revenue, type = 'bar', orientation = 'h',
              marker = list(color = precise_colors$primary)) %>%
        layout(
          title = "",
          xaxis = list(title = paste0("Revenue (", input$currencySelector, ")"),
                       tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f"),
          yaxis = list(title = "")
        ) %>%
        precise_theme()
    }
  })
  
  output$catalogGrowth <- renderPlotly({
    catalog_growth <- filtered_tracks() %>%
      dplyr::arrange(release_date) %>%
      dplyr::mutate(cumulative_tracks = 1:n()) %>%
      dplyr::filter(release_date >= input$dateRange[1] & release_date <= input$dateRange[2])
    plot_ly(data = catalog_growth, x = ~release_date, y = ~cumulative_tracks, type = 'scatter', mode = 'lines',
            line = list(color = precise_colors$primary)) %>%
      layout(
        title = "",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Cumulative Number of Tracks")
      ) %>%
      precise_theme()
  })
  
  output$genreDistribution <- renderPlotly({
    genre_dist <- filtered_tracks() %>%
      dplyr::filter(release_date >= input$dateRange[1] & release_date <= input$dateRange[2]) %>%
      dplyr::group_by(genre) %>%
      dplyr::summarize(track_count = n(), .groups = "drop") %>%
      dplyr::arrange(desc(track_count))
    if (nrow(genre_dist) == 0) {
      plot_ly() %>%
        layout(
          title = "No tracks available in the selected date range",
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        ) %>%
        precise_theme()
    } else {
      plot_ly(data = genre_dist, labels = ~genre, values = ~track_count, type = 'pie',
              marker = list(colors = precise_palette, line = list(color = '#FFFFFF', width = 1))) %>%
        layout(
          title = "",
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2)
        ) %>%
        precise_theme()
    }
  })
  
  output$trackCatalogTable <- renderDT({
    track_summary <- filtered_tracks() %>%
      dplyr::mutate(
        release_date = format(release_date, "%d %b %Y"),
        duration = paste0(floor(duration_sec / 60), ":", sprintf("%02d", duration_sec %% 60))
      ) %>%
      dplyr::select(track_name, artist_name, isrc, release_date, duration, genre)
    
    colnames(track_summary) <- c("Track Name", "Artist", "ISRC", "Release Date", "Duration", "Genre")
    
    DT::datatable(
      track_summary,
      options = list(
        pageLength = 10,
        dom = 'ftip',
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(3, 4, 5, 6))
        )
      ),
      rownames = FALSE
    )
  })
  
  # Artist Detail
  output$artistDetailPage <- renderUI({
    if (is.null(input$artistTable_rows_selected)) {
      return(
        shinydashboard::box(
          title = "Artist Profile",
          width = 12,
          status = "primary",
          solidHeader = FALSE, 
          shiny::p("Please select an artist from the Artists Portfolio tab to view details.")
        )
      )
    }
    
    selected_row <- input$artistTable_rows_selected
    selected_artist <- my_artists[selected_row, "artist_name"]
    
    shiny::fluidRow(
      shinydashboard::box(
        title = paste("Artist Profile:", selected_artist),
        width = 12,
        status = "primary",
        solidHeader = FALSE,
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::h4("Artist Information"),
            shiny::tags$div(
              shiny::tags$p(shiny::tags$strong("Genre: "), my_artists[selected_row, "genre"]),
              shiny::tags$p(shiny::tags$strong("Status: "), my_artists[selected_row, "status"]),
              shiny::tags$p(shiny::tags$strong("Join Date: "), format(my_artists[selected_row, "join_date"], "%d %b %Y")),
              shiny::tags$p(shiny::tags$strong("Contract End: "), format(my_artists[selected_row, "contract_end"], "%d %b %Y"))
            )
          ),
          shiny::column(
            width = 9,
            plotly::plotlyOutput("artistDetailRevenue", height = 200)
          )
        ),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::h4("Revenue by Platform"),
            plotly::plotlyOutput("artistDetailPlatform", height = 250)
          ),
          shiny::column(
            width = 6,
            shiny::h4("Balance History"),
            plotly::plotlyOutput("artistDetailBalance", height = 250)
          )
        ),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h4("Track Catalog"),
            DT::DTOutput("artistDetailTracks")
          )
        )
      )
    )
  })
  
  output$artistDetailRevenue <- renderPlotly({
    req(input$artistTable_rows_selected)
    selected_row <- input$artistTable_rows_selected
    selected_artist <- my_artists[selected_row, "artist_name"]
    artist_revenue_detail <- filtered_data() %>%
      dplyr::filter(artist_name == selected_artist) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop")
    plot_ly(data = artist_revenue_detail, x = ~date, y = ~revenue, type = 'scatter', mode = 'lines+markers',
            line = list(color = precise_colors$primary), marker = list(color = precise_colors$primary)) %>%
      layout(
        title = "Revenue Trend",
        xaxis = list(title = ""),
        yaxis = list(title = paste0("Revenue (", input$currencySelector, ")"),
                     tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f")
      ) %>%
      precise_theme()
  })
  
  output$artistDetailPlatform <- renderPlotly({
    req(input$artistTable_rows_selected)
    selected_row <- input$artistTable_rows_selected
    selected_artist <- my_artists[selected_row, "artist_name"]
    platform_detail <- filtered_data() %>%
      dplyr::filter(artist_name == selected_artist) %>%
      dplyr::group_by(platform) %>%
      dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(desc(revenue))
    plot_ly(data = platform_detail, labels = ~platform, values = ~revenue, type = 'pie',
            marker = list(colors = precise_palette, line = list(color = '#FFFFFF', width = 1))) %>%
      layout(
        title = "",
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2)
      ) %>%
      precise_theme()
  })
  
  output$artistDetailBalance <- renderPlotly({
    req(input$artistTable_rows_selected)
    selected_row <- input$artistTable_rows_selected
    selected_artist <- my_artists[selected_row, "artist_name"]
    balance_detail <- my_balance_history %>%
      dplyr::filter(artist_name == selected_artist) %>%
      dplyr::filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    plot_ly(data = balance_detail, x = ~date, y = ~balance, type = 'scatter', mode = 'lines',
            line = list(color = precise_colors$primary)) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = paste0("Balance (", input$currencySelector, ")"),
                     tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f")
      ) %>%
      precise_theme()
  })
  
  output$artistDetailTracks <- renderDT({
    req(input$artistTable_rows_selected)
    selected_row <- input$artistTable_rows_selected
    selected_artist <- my_artists[selected_row, "artist_name"]
    artist_tracks <- filtered_tracks() %>%
      dplyr::filter(artist_name == selected_artist) %>%
      dplyr::mutate(
        release_date = format(release_date, "%d %b %Y"),
        duration = paste0(floor(duration_sec / 60), ":", sprintf("%02d", duration_sec %% 60))
      ) %>%
      dplyr::select(track_name, isrc, release_date, duration, genre)
    
    colnames(artist_tracks) <- c("Track Name", "ISRC", "Release Date", "Duration", "Genre")
    
    DT::datatable(
      artist_tracks,
      options = list(
        pageLength = 5,
        dom = 'ft',
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(3, 4, 5, 6))
        )
      ),
      rownames = FALSE
    )
  })
  
  # Artist Drill-Down
  observeEvent(event_data("plotly_click", source = "artistRevenue"), {
    click_data <- event_data("plotly_click", source = "artistRevenue")
    if (!is.null(click_data)) {
      artist_revenue <- filtered_data() %>%
        dplyr::group_by(artist_name) %>%
        dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(desc(revenue)) %>%
        dplyr::top_n(10, revenue)
      selected_artist <- artist_revenue$artist_name[click_data$y]
      
      showModal(
        modalDialog(
          title = paste("Revenue Breakdown for", selected_artist),
          size = "l",
          fluidRow(
            column(
              width = 6,
              plotlyOutput("artistRevenueDetailModal", height = "300px")
            ),
            column(
              width = 6,
              plotlyOutput("artistPlatformDistributionModal", height = "300px")
            )
          ),
          footer = modalButton("Close")
        )
      )
      
      output$artistRevenueDetailModal <- renderPlotly({
        artist_detail <- filtered_data() %>%
          dplyr::filter(artist_name == selected_artist) %>%
          dplyr::group_by(date) %>%
          dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop")
        plot_ly(data = artist_detail, x = ~date, y = ~revenue, type = 'scatter', mode = 'lines+markers',
                line = list(color = precise_colors$primary), marker = list(color = precise_colors$primary)) %>%
          layout(
            title = "Revenue Trend",
            xaxis = list(title = ""),
            yaxis = list(title = paste0("Revenue (", input$currencySelector, ")"),
                         tickformat = if(input$currencySelector == "USD") "$,.0f" else "NZ$,.0f")
          ) %>%
          precise_theme()
      })
      
      output$artistPlatformDistributionModal <- renderPlotly({
        platform_detail <- filtered_data() %>%
          dplyr::filter(artist_name == selected_artist) %>%
          dplyr::group_by(platform) %>%
          dplyr::summarize(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(desc(revenue))
        plot_ly(data = platform_detail, labels = ~platform, values = ~revenue, type = 'pie',
                marker = list(colors = precise_palette, line = list(color = '#FFFFFF', width = 1))) %>%
          layout(
            title = "Revenue by Platform",
            showlegend = TRUE,
            legend = list(orientation = "h", y = -0.2)
          ) %>%
          precise_theme()
      })
    }
  })
  
  # Report Downloads
  output$downloadRevenueReport <- downloadHandler(
    filename = function() { paste("revenue-report-", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      pdf(file)
      plot(1, main = "Revenue Report")
      dev.off()
    }
  )
  
  output$downloadArtistReport <- downloadHandler(
    filename = function() { paste("artist-report-", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      pdf(file)
      plot(1, main = "Artist Report")
      dev.off()
    }
  )
  
  output$downloadCatalogReport <- downloadHandler(
    filename = function() { paste("catalog-report-", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      pdf(file)
      plot(1, main = "Catalog Report")
      dev.off()
    }
  )
  
  output$downloadFinancialReport <- downloadHandler(
    filename = function() { paste("financial-report-", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      pdf(file)
      plot(1, main = "Financial Report")
      dev.off()
    }
  )
}