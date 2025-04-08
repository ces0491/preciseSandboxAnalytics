ui <- shinydashboard::dashboardPage(
  skin = "black",
  
  # Header
  shinydashboard::dashboardHeader(
    title = shiny::div(
      shiny::img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRXmcq_nUDu2Ch3qSCMYc4Ev9vFlksRjbWGrA&s", height = 30),
      "Precise Digital"
    ),
    shinydashboard::dropdownMenu(
      type = "notifications",
      badgeStatus = "danger",
      shiny::uiOutput("notificationsMenu")
    ),
    shinydashboard::dropdownMenu(
      type = "tasks",
      badgeStatus = "danger",
      shinydashboard::notificationItem(
        text = "Download artist statements",
        icon = shiny::icon("file-export"),
        status = "danger"
      ),
      shinydashboard::notificationItem(
        text = "Review outstanding balances",
        icon = shiny::icon("check"),
        status = "success"
      )
    )
  ),
  
  # Sidebar
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Overview", tabName = "overview", icon = shiny::icon("dashboard")),
      shinydashboard::menuItem("Revenue", tabName = "revenue", icon = shiny::icon("dollar-sign")),
      shinydashboard::menuItem("Artists", tabName = "artists", icon = shiny::icon("users"),
                               shinydashboard::menuSubItem("Portfolio", tabName = "artist_portfolio"),
                               shinydashboard::menuSubItem("Detail View", tabName = "artist_detail")
      ),
      shinydashboard::menuItem("Catalog", tabName = "catalog", icon = shiny::icon("music")),
      shinydashboard::menuItem("Reports", tabName = "reports", icon = shiny::icon("file-pdf"))
    ),
    shiny::br(),
    shiny::div(
      style = "padding: 0 15px; width: 100%;",
      shiny::dateRangeInput(
        "dateRange",
        "Date Range:",
        start = min(dates) + months(12),
        end = max(dates),
        min = min(dates),
        max = max(dates),
        separator = " to ",
        autoclose = TRUE,
        width = "100%"
      )
    ),
    shiny::br(),
    shiny::div(
      style = "padding: 0 15px;",
      shiny::radioButtons(
        "currencySelector",
        "Display Currency:",
        choices = c("NZD", "USD"),
        selected = "NZD",
        inline = TRUE
      )
    ),
    shiny::br(),
    shiny::div(
      style = "padding: 0 15px;",
      shinyWidgets::pickerInput(
        inputId = "artistFilter",
        label = "Filter by Artist:",
        choices = unique(as.character(my_artists$artist_name)),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} artists selected",
          `live-search` = TRUE,
          `none-selected-text` = "All Artists",
          `style` = "btn-default"
        )
      )
    )
  ),
  
  # Body
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        /* Existing styles */
        .skin-black .main-header .logo {
          background-color: #000000;
          color: #ffffff;
        }
        .skin-black .main-header .navbar {
          background-color: #000000;
        }
        .skin-black .main-header .navbar .sidebar-toggle:hover {
          background-color: #333333;
        }
        .skin-black .main-sidebar {
          background-color: #121212;
        }
        .skin-black .sidebar a {
          color: #ffffff;
        }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li:hover > a {
          border-left-color: #ff0000;
        }
        .box.box-primary {
          border-top-color: #ff0000;
          background-color: #ffffff;
          color: #000000;
        }
        .box.box-primary .box-header {
          background-color: #ffffff;
          color: #000000;
        }
        .bg-red {
          background-color: #ff0000 !important;
        }
        .bg-black {
          background-color: #000000 !important;
          color: #ffffff;
        }
        .bg-maroon {
          background-color: #800000 !important;
          color: #ffffff;
        }
        .small-box {height: 120px;}
        .info-box {height: 100px; margin-bottom: 15px;}
        .content-wrapper, .right-side {background-color: #f4f6f9;}
        .box {border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);}
        .alert-item {
          padding: 10px;
          margin-bottom: 10px;
          border-radius: 4px;
          border-left: 4px solid #ff0000;
          background-color: #FFEBEE;
        }
        .alert-item.positive {
          border-left: 4px solid #4CAF50;
          background-color: #E8F5E9;
        }
        .alert-title {
          font-weight: bold;
          margin-back: 5px;
        }
        .date-range-label {
          margin-bottom: 5px;
          color: #757575;
          font-size: 12px;
        }
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter {
          margin-bottom: 15px;
        }
        .dataTables_info {
          margin-top: 10px;
        }
        .btn-primary {
          background-color: #000000;
          border-color: #000000;
          color: #ffffff;
        }
        .btn-primary:hover, .btn-primary:focus, .btn-primary:active {
          background-color: #333333;
          border-color: #333333;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #ff0000;
        }
        .picker-input {
          width: 100%;
        }
        .dropdown-menu {
          color: #000000 !important;
          background-color: #ffffff !important;
        }
        .dropdown-menu > li > a {
          color: #000000 !important;
        }
        .dropdown-menu > li > a:hover {
          background-color: #f0f0f0 !important;
        }

        /* New styles to freeze sidebar */
        .main-sidebar {
          position: fixed;
          top: 50px; /* Height of the header */
          left: 0;
          height: calc(100vh - 50px); /* Full height minus header */
          width: 230px; /* Default sidebar width in shinydashboard */
          overflow-y: auto; /* Allow scrolling within sidebar if content overflows */
          z-index: 1000; /* Ensure sidebar stays above content */
        }
        .content-wrapper {
          margin-left: 230px; /* Match sidebar width to push content right */
          height: calc(100vh - 50px); /* Full height minus header */
          overflow-y: auto; /* Allow independent scrolling of content */
        }
        .main-header {
          position: fixed;
          width: 100%;
          top: 0;
          left: 0;
          z-index: 1100; /* Ensure header stays above sidebar and content */
        }
        body {
          padding-top: 50px; /* Prevent content from being hidden under fixed header */
        }
      "))
    ),
    
    shinydashboard::tabItems(
      # Overview Tab
      shinydashboard::tabItem(
        tabName = "overview",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Welcome [Account Manager Name]",
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            shiny::h4("Accounts Overview"),
            shiny::div(
              shiny::textOutput("welcomeMessage"),
              shiny::htmlOutput("dateRangeMessage")
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::valueBoxOutput("totalArtistsBox", width = 2),
          shinydashboard::valueBoxOutput("totalRevenueBox", width = 2),
          shinydashboard::valueBoxOutput("revenueChangeBox", width = 2),
          shinydashboard::valueBoxOutput("outstandingBalanceBox", width = 2),
          shinydashboard::valueBoxOutput("totalTracksBox", width = 2)
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Alerts & Notifications",
            status = "primary",
            solidHeader = FALSE,
            width = 12,
            shiny::div(
              style = "max-height: 150px; overflow-y: auto;",
              shiny::uiOutput("alertsList")
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Revenue Trend",
            status = "primary",
            solidHeader = FALSE,
            width = 8,
            shiny::div(class = "date-range-label", "Filtered by selected date range"),
            shiny::checkboxInput("showForecast", "Show Forecast", value = FALSE),
            shiny::checkboxInput("showRollingAvg", "Show 3-Month Rolling Average", value = FALSE),
            plotly::plotlyOutput("revenueTimeSeries", height = 300)
          ),
          shinydashboard::box(
            title = "Revenue by Platform",
            status = "primary",
            solidHeader = FALSE,
            width = 4,
            shiny::div(class = "date-range-label", "Filtered by selected date range"),
            plotly::plotlyOutput("platformDistribution", height = 300)
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Your Performance vs. Company Average",
            status = "primary",
            solidHeader = FALSE,
            width = 12,
            shiny::div(class = "date-range-label", "Filtered by selected date range"),
            plotly::plotlyOutput("performanceComparison", height = 250)
          )
        )
      ),
      
      # Revenue Tab
      shinydashboard::tabItem(
        tabName = "revenue",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Revenue Analysis",
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            shiny::h4("Detailed Revenue Breakdown"),
            shiny::div(class = "date-range-label", "Filtered by selected date range")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Revenue by Artist",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            shiny::p("Click on an artist bar to view detailed breakdown"),
            plotly::plotlyOutput("artistRevenue", height = 350)
          ),
          shinydashboard::box(
            title = "Revenue by Platform Over Time",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            shiny::selectizeInput(
              "platformFilter",
              "Filter by Platform:",
              choices = unique(as.character(revenue_data$platform)),
              selected = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Select platforms (leave blank for all)",
                plugins = list("remove_button"),
                maxItems = NULL
              )
            ),
            plotly::plotlyOutput("platformRevenueTrend", height = 300)
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Monthly Growth Rates",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            plotly::plotlyOutput("revenueGrowth", height = 250)
          ),
          shinydashboard::box(
            title = "Revenue Distribution",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            shiny::radioButtons(
              "distributionType",
              "View By:",
              choices = c("Platform", "Artist"),
              selected = "Platform",
              inline = TRUE
            ),
            plotly::plotlyOutput("revenueDistribution", height = 250)
          )
        )
      ),
      
      # Artists Portfolio Tab
      shinydashboard::tabItem(
        tabName = "artist_portfolio",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Artist Management",
            width = 12,
            status = "primary",
            solidHeader = FALSE, 
            shiny::h4("Your Artist Portfolio"),
            shiny::div(class = "date-range-label", "Filtered by selected date range")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Artists Performance Comparison",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            shiny::p("Click on an artist line to view details"),
            plotly::plotlyOutput("artistPerformance", height = 350)
          ),
          shinydashboard::box(
            title = "Outstanding Balances by Artist",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            shiny::p("Positive values are owed to artists, negative are owed by artists"),
            plotly::plotlyOutput("artistBalances", height = 350)
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Artists List",
            status = "primary",
            solidHeader = FALSE,
            width = 12,
            shiny::p("Click on artist name to view detailed profile"),
            DT::DTOutput("artistTable")
          )
        )
      ),
      
      # Artist Detail Tab
      shinydashboard::tabItem(
        tabName = "artist_detail",
        shiny::uiOutput("artistDetailPage")
      ),
      
      # Catalog Tab
      shinydashboard::tabItem(
        tabName = "catalog",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Catalog Overview",
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            shiny::h4("Track Catalog Analysis"),
            shiny::div(class = "date-range-label", "Filtered by selected date range")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Track Release Timeline",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            plotly::plotlyOutput("trackTimeline", height = 350)
          ),
          shinydashboard::box(
            title = "Top Performing Tracks",
            status = "primary",
            solidHeader = FALSE, 
            width = 6,
            shiny::selectizeInput(
              "topTrackMetric",
              "Sort By:",
              choices = c("Revenue" = "revenue", "Streams" = "streams"),
              selected = "revenue"
            ),
            plotly::plotlyOutput("topTracks", height = 300)
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Catalog Growth Over Time",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            plotly::plotlyOutput("catalogGrowth", height = 250)
          ),
          shinydashboard::box(
            title = "Genre Distribution",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            plotly::plotlyOutput("genreDistribution", height = 250)
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Track Catalog",
            status = "primary",
            solidHeader = FALSE, 
            width = 12,
            DT::DTOutput("trackCatalogTable")
          )
        )
      ),
      
      # Reports Tab
      shinydashboard::tabItem(
        tabName = "reports",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Generate Reports",
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            shiny::h4("Download Custom Reports"),
            shiny::div(class = "date-range-label", "Reports will use the selected date range")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Revenue Reports",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            shiny::checkboxGroupInput(
              "revenueReportOptions",
              "Include:",
              choices = c(
                "Summary by Artist" = "artist_summary",
                "Platform Breakdown" = "platform_breakdown",
                "Monthly Trend" = "monthly_trend",
                "Growth Analysis" = "growth_analysis"
              ),
              selected = c("artist_summary", "platform_breakdown")
            ),
            shiny::downloadButton("downloadRevenueReport", "Generate Revenue Report")
          ),
          shinydashboard::box(
            title = "Artist Reports",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            shiny::selectizeInput(
              "artistReportSelector",
              "Select Artists:",
              choices = unique(as.character(my_artists$artist_name)),
              multiple = TRUE,
              selected = unique(as.character(my_artists$artist_name))[1]
            ),
            shiny::checkboxGroupInput(
              "artistReportOptions",
              "Include:",
              choices = c(
                "Revenue Details" = "revenue_details",
                "Track Performance" = "track_performance",
                "Balance History" = "balance_history"
              ),
              selected = c("revenue_details", "track_performance")
            ),
            shiny::downloadButton("downloadArtistReport", "Generate Artist Report")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Catalog Reports",
            status = "primary",
            solidHeader = FALSE, 
            width = 6,
            shiny::checkboxGroupInput(
              "catalogReportOptions",
              "Include:",
              choices = c(
                "Track Listing" = "track_listing",
                "Performance Analysis" = "performance_analysis",
                "Release Timeline" = "release_timeline",
                "Genre Analysis" = "genre_analysis"
              ),
              selected = c("track_listing", "performance_analysis")
            ),
            shiny::downloadButton("downloadCatalogReport", "Generate Catalog Report")
          ),
          shinydashboard::box(
            title = "Financial Reports",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            shiny::checkboxGroupInput(
              "financialReportOptions",
              "Include:",
              choices = c(
                "Outstanding Balances" = "balances",
                "Payment History" = "payment_history",
                "Projected Revenue" = "projected_revenue"
              ),
              selected = c("balances", "payment_history")
            ),
            shiny::downloadButton("downloadFinancialReport", "Generate Financial Report")
          )
        )
      )
    )
  )
)