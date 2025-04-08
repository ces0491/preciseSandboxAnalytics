# Required Libraries
library(shiny)
library(plotly)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(DT)
library(scales)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyr)
library(RColorBrewer)
library(forecast)
library(zoo)

# Color palettes and branding
precise_colors <- list(
  primary = "#E53935",   # Red (Precise primary)
  secondary = "#212121", # Black (or very dark grey)
  accent = "#FFFFFF",    # White
  warning = "#FFC107",   # Yellow
  danger = "#B71C1C",    # Darker red for emphasis
  neutral = "#424242",   # Medium grey
  text = "#FFFFFF",      # White (for text on black/red)
  light_text = "#E0E0E0" # Light grey (for less important text)
)

precise_palette <- c(
  "#E53935", # Red
  "#212121", # Black
  "#FFFFFF", # White
  "#B71C1C", # Darker Red
  "#424242"  # Darker Grey
)

# Custom theme for plots
precise_theme <- function(p) {
  p %>% 
    plotly::layout(
      font = list(family = "Arial, sans-serif", size = 12, color = precise_colors$text),
      paper_bgcolor = precise_colors$secondary,
      plot_bgcolor = precise_colors$secondary,
      xaxis = list(
        gridcolor = precise_colors$neutral,
        zerolinecolor = precise_colors$danger,
        tickfont = list(size = 10, color = precise_colors$light_text)
      ),
      yaxis = list(
        gridcolor = precise_colors$neutral,
        zerolinecolor = precise_colors$danger,
        tickfont = list(size = 10, color = precise_colors$light_text)
      ),
      legend = list(
        font = list(size = 10, color = precise_colors$light_text)
      )
    )
}

# Function to format currency based on selected currency
format_currency <- function(amount, currency = "NZD") {
  if (currency == "USD") {
    conversion_rate <- 0.61 # Static rate; in production, fetch from API
    converted_amount <- amount * conversion_rate
    return(scales::dollar(converted_amount, prefix = "$"))
  } else {
    return(scales::dollar(amount, prefix = "NZ$"))
  }
}

# Load and prepare sample data
load_sample_data <- function() {
  # Current account manager ID (in production, from authentication)
  current_account_manager_id <- 101
  
  # Set seed for reproducibility
  set.seed(456)
  
  # Generate dates for the last 24 months
  dates <- seq(from = Sys.Date() %m-% months(23), to = Sys.Date(), by = "month")
  
  # Artists data
  
  # Word banks
  first_parts <- c("Lil", "DJ", "MC", "The", "Saint", "Young", "Big", "Dr", "King", "Miss", "21")
  second_parts <- c("Echo", "Vibe", "Shadow", "Wolf", "Flame", "Pixel", "Ghost", "Storm", "Muse", "Nova", "District")
  
  # Function to generate a random artist name
  generate_artist_name <- function() {
    paste(sample(first_parts, 1), sample(second_parts, 1))
  }
  
  # Generate 8 unique artist names
  artist_names <- unique(replicate(20, generate_artist_name()))
  artist_names <- sample(artist_names, 15)
  
  artists <- data.frame(
    artist_id = 1:15,
    artist_name = artist_names,
    account_manager_id = c(rep(current_account_manager_id, 8), rep(102, 4), rep(103, 3)),
    join_date = sample(seq(from = Sys.Date() %m-% years(3), to = Sys.Date(), length.out = 15)),
    genre = sample(c("Pop", "Rock", "Hip-Hop", "Electronic", "R&B"), 15, replace = TRUE),
    status = sample(c("Active", "Inactive", "On Hold"), 15, replace = TRUE, prob = c(0.8, 0.1, 0.1)),
    contract_end = sample(seq(from = Sys.Date(), to = Sys.Date() %m+% years(2), length.out = 15)),
    stringsAsFactors = FALSE
  )
  
  # Revenue data
  revenue_data <- expand.grid(
    date = dates,
    artist_id = artists$artist_id,
    platform = c("Spotify", "Apple Music", "YouTube", "Deezer", "Amazon Music")
  )
  
  revenue_data$revenue <- runif(nrow(revenue_data), 100, 5000)
  revenue_data$revenue <- ifelse(
    revenue_data$artist_id %in% artists$artist_id[artists$account_manager_id == current_account_manager_id],
    revenue_data$revenue * 1.2,
    revenue_data$revenue
  )
  
  platform_multipliers <- c("Spotify" = 1.5, "Apple Music" = 1.3, "YouTube" = 2.0, "Deezer" = 0.8, "Amazon Music" = 1.0)
  revenue_data$revenue <- revenue_data$revenue * platform_multipliers[revenue_data$platform]
  
  for (i in 1:length(dates)) {
    season_effect <- 1 + 0.2 * sin(2 * pi * i / 12)
    growth_effect <- 1 + 0.03 * i
    revenue_data$revenue[revenue_data$date == dates[i]] <- 
      revenue_data$revenue[revenue_data$date == dates[i]] * season_effect * growth_effect
  }
  
  revenue_data$currency <- "NZD"
  
  # Track data
  
  # Word banks
  adjectives <- c("Broken", "Silent", "Endless", "Golden", "Lonely", "Fading", "Wild", "Crimson", "Electric", "Frozen")
  nouns <- c("Dream", "Night", "Heart", "Sky", "Fire", "Shadow", "Rain", "Storm", "Memory", "Light")
  connectors <- c("", " of the", " in the", " with the", " under the", " beyond the")
  
  # Function to generate a random song title
  generate_title <- function() {
    adj <- sample(adjectives, 1)
    noun1 <- sample(nouns, 1)
    connector <- sample(connectors, 1)
    noun2 <- ifelse(connector != "", sample(nouns, 1), "")
    paste(adj, noun1, connector, noun2)
  }
  
  # Generate 150 unique titles
  song_titles <- unique(replicate(300, generate_title()))
  song_titles <- sample(song_titles, 150)
  
  tracks <- data.frame(
    track_id = 1:150,
    isrc = paste0("US-S1Z-", sample(10:99, 150, replace = TRUE), "-", sample(1000:9999, 150, replace = TRUE)),
    track_name = song_titles,
    artist_id = sample(artists$artist_id, 150, replace = TRUE),
    release_date = sample(seq(from = Sys.Date() %m-% years(3), to = Sys.Date(), length.out = 150)),
    duration_sec = sample(120:420, 150, replace = TRUE),
    genre = sample(c("Pop", "Rock", "Hip-Hop", "Electronic", "R&B"), 150, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Track performance data
  track_performance <- data.frame(
    track_id = rep(tracks$track_id, each = length(dates)),
    date = rep(dates, length(tracks$track_id)),
    streams = runif(length(tracks$track_id) * length(dates), 0, 10000)
  )
  
  for (i in 1:nrow(tracks)) {
    days_since_release <- as.numeric(track_performance$date[track_performance$track_id == tracks$track_id[i]] - 
                                       tracks$release_date[i])
    track_performance$streams[track_performance$track_id == tracks$track_id[i]] <- 
      ifelse(days_since_release < 0, 0,
             track_performance$streams[track_performance$track_id == tracks$track_id[i]] * 
               exp(-0.01 * days_since_release))
  }
  
  # Outstanding balances
  balances <- data.frame(
    artist_id = artists$artist_id,
    balance = runif(nrow(artists), -2000, 5000),
    last_payment_date = sample(seq(from = Sys.Date() %m-% months(6), to = Sys.Date(), length.out = nrow(artists)))
  )
  
  # Balance history
  balance_history <- data.frame()
  for (i in 1:length(dates)) {
    month_balances <- data.frame(
      artist_id = artists$artist_id,
      date = dates[i],
      balance = runif(nrow(artists), -3000, 6000)
    )
    balance_history <- rbind(balance_history, month_balances)
  }
  
  for (a in unique(balance_history$artist_id)) {
    artist_balances <- balance_history[balance_history$artist_id == a, ]
    artist_balances <- artist_balances[order(artist_balances$date), ]
    for (i in 2:nrow(artist_balances)) {
      artist_balances$balance[i] <- 0.8 * artist_balances$balance[i-1] + artist_balances$balance[i] * 0.2
    }
    balance_history[balance_history$artist_id == a, "balance"] <- artist_balances$balance
  }
  
  for (a in unique(balances$artist_id)) {
    balance_history[balance_history$artist_id == a & balance_history$date == max(balance_history$date), "balance"] <- 
      balances[balances$artist_id == a, "balance"]
  }
  
  return(list(
    artists = artists,
    revenue_data = revenue_data,
    tracks = tracks,
    track_performance = track_performance,
    balances = balances,
    balance_history = balance_history,
    current_account_manager_id = current_account_manager_id,
    dates = dates
  ))
}

# Load data
data_list <- load_sample_data()

# Extract datasets
artists <- data_list$artists
revenue_data <- data_list$revenue_data
tracks <- data_list$tracks
track_performance <- data_list$track_performance
balances <- data_list$balances
balance_history <- data_list$balance_history
current_account_manager_id <- data_list$current_account_manager_id
dates <- data_list$dates

# Prepare data for visualizations
my_artists <- artists %>% dplyr::filter(account_manager_id == current_account_manager_id)
my_revenue <- revenue_data %>% 
  dplyr::filter(artist_id %in% my_artists$artist_id) %>%
  dplyr::left_join(artists, by = "artist_id")
my_tracks <- tracks %>% 
  dplyr::filter(artist_id %in% my_artists$artist_id) %>%
  dplyr::left_join(
    dplyr::select(artists, artist_id, artist_name, account_manager_id, join_date, status, contract_end), 
    by = "artist_id",
    relationship = "many-to-one"
  )
my_track_performance <- track_performance %>%
  dplyr::filter(track_id %in% my_tracks$track_id) %>%
  dplyr::left_join(my_tracks, by = "track_id", relationship = "many-to-one")
my_balances <- balances %>% 
  dplyr::filter(artist_id %in% my_artists$artist_id) %>%
  dplyr::left_join(artists, by = "artist_id")
my_balance_history <- balance_history %>% 
  dplyr::filter(artist_id %in% my_artists$artist_id) %>%
  dplyr::left_join(artists, by = "artist_id")

# Helper function to identify artists with alerts
get_artists_with_alerts <- function() {
  monthly_artist_revenue <- my_revenue %>%
    dplyr::group_by(artist_id, artist_name, date) %>%
    dplyr::summarize(monthly_revenue = sum(revenue, na.rm = TRUE), .groups = "drop")
  
  artist_alerts <- monthly_artist_revenue %>%
    dplyr::group_by(artist_id, artist_name) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      prev_3m_avg = dplyr::lag(zoo::rollmean(monthly_revenue, k = 3, fill = NA, align = "right"), 1),
      current = monthly_revenue,
      pct_change = (current - prev_3m_avg) / prev_3m_avg * 100
    ) %>%
    dplyr::filter(!is.na(pct_change)) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(alert_type = case_when(
      pct_change < -20 ~ "Significant Drop",
      pct_change > 20 ~ "Significant Increase",
      TRUE ~ "Normal"
    ))
  
  balance_alerts <- my_balances %>%
    dplyr::mutate(balance_status = case_when(
      balance < -1000 ~ "High Debt",
      balance > 3000 ~ "High Credit",
      TRUE ~ "Normal"
    )) %>%
    dplyr::select(artist_id, artist_name, balance, balance_status)
  
  combined_alerts <- artist_alerts %>%
    dplyr::left_join(balance_alerts, by = c("artist_id", "artist_name")) %>%
    dplyr::mutate(
      has_alert = alert_type != "Normal" | balance_status != "Normal",
      alert_message = case_when(
        alert_type == "Significant Drop" & balance_status == "High Debt" ~ "Revenue drop & high debt",
        alert_type == "Significant Drop" ~ "Revenue drop",
        balance_status == "High Debt" ~ "High debt",
        balance_status == "High Credit" ~ "Large payment due",
        alert_type == "Significant Increase" ~ "Strong growth",
        TRUE ~ ""
      )
    ) %>%
    dplyr::filter(has_alert)
  
  return(combined_alerts)
}