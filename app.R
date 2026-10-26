# ============================================================================
#   Jamaica Bay Wildlife Refuge - Wind Data Dashboard
#   Shiny Application (shinylive-compatible)
#   Data source: RM-0002 environmental sensor, West Pond, NY
#   PIs: P. Staniczenko & C. Zarnoch, CUNY
# ============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# ============================================================================
# DATA LOADING
# ============================================================================

# Read pre-processed clean CSV (built by update_dashboard.R from all raw downloads)
all_data <- read.csv("jamaica_bay_clean.csv", stringsAsFactors = FALSE)

# Parse datetime - try both formats (with and without seconds)
all_data$datetime <- mdy_hms(all_data$local_time, quiet = TRUE)
missing <- is.na(all_data$datetime)
if (any(missing)) {
  all_data$datetime[missing] <- mdy_hm(all_data$local_time[missing], quiet = TRUE)
}
all_data <- all_data[!is.na(all_data$datetime), ]

# Sort and add derived columns
all_data <- all_data %>%
  arrange(datetime) %>%
  mutate(
    date       = as.Date(datetime),
    year       = year(datetime),
    hour       = hour(datetime)
  )

# Month labels include the year ("Oct 2025") so the same month in different
# years is never pooled. Levels are in chronological order (data is sorted).
month_year_labels <- format(all_data$datetime, "%b %Y")
all_data$month_abbr <- factor(month_year_labels, levels = unique(month_year_labels))

data_years <- sort(unique(all_data$year))

# NA-safe summaries: months where a probe was offline show "-" instead of Inf/NaN
safe <- function(f) function(x) if (all(is.na(x))) NA_real_ else f(x, na.rm = TRUE)
s_mean <- safe(mean); s_sd <- safe(sd); s_min <- safe(min); s_max <- safe(max)

# One fill colour per month box (a fixed palette runs out after 9 months)
month_fills <- function(n, low, high) colorRampPalette(c(low, high))(n)

# ============================================================================
# MAINTENANCE LOG  (edit by hand - shown on the Sensor Issues tab)
# One row per issue. Status: "Open", "Decision needed", "Monitoring", "Resolved".
# ============================================================================
maintenance_log <- data.frame(
  Date = c("2026-02-16", "2026-07", "2026-03-16, 2026-05-20/21", "2025-08-05 to 2025-09-18"),
  Component = c("Temperature & pressure sensor",
                "Humidity sensor",
                "Temperature & pressure sensor",
                "Whole station"),
  Issue = c("Both stopped giving valid readings at 05:47 on Feb 16, 2026 (temperature stuck at -144.15 C, pressure 560-825 mb). Simultaneous failure suggests a single shared sensor.",
            "About 60% of humidity readings are exactly 100% since Jul 2026 (about 10% in spring). May be failing along with the temperature/pressure sensor.",
            "114 physically impossible spikes (e.g. 179.8 C, 1270.8 mb).",
            "Readings before installation (Sep 18, 2025 12:00) are bench testing; humidity fixed at 100%."),
  Action = c("Inspect / replace at next site visit",
             "Check against the station spec sheet; inspect at next site visit",
             "None - removed automatically by update_dashboard.R cutoffs",
             "Decide whether to exclude (DEPLOY_START in update_dashboard.R)"),
  Status = c("Open", "Open", "Resolved", "Decision needed"),
  stringsAsFactors = FALSE
)

# ============================================================================
# AUTOMATIC HEALTH CHECKS  (Sensor Issues tab)
# Based on the most recent HEALTH_DAYS of data, independent of the filters.
# ============================================================================
HEALTH_DAYS   <- 7
STUCK_HOURS   <- 12    # a wind reading unchanged this long = possibly stuck
SATURATED_PCT <- 40    # % of humidity readings at 100% that warrants a check...
SATURATED_DAYS <- 30   # ...measured over this many days (daily share swings 0-100%)
BATT_CHECK_V  <- 3.7   # battery below this = check charging / solar panel
BATT_FAULT_V  <- 3.5   # battery below this = sensor about to shut down
STALE_DAYS    <- 45    # days since the last reading before a download reminder
GAP_HOURS     <- 3     # transmission gap this long = check radio / gateway

compute_health <- function(d) {
  end_t <- max(d$datetime)
  w <- d[d$datetime > end_t - days(HEALTH_DAYS), ]
  pct_valid <- function(x) 100 * mean(!is.na(x))
  last_valid <- function(v) {
    i <- which(!is.na(d[[v]]))
    if (!length(i)) "never" else format(d$datetime[max(i)], "%b %d, %Y %H:%M")
  }
  longest_flat_h <- function(x, t) {
    ok <- !is.na(x); x <- x[ok]; t <- t[ok]
    if (length(x) < 2) return(0)
    r <- rle(x); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
    max(as.numeric(difftime(t[ends], t[starts], units = "hours")))
  }
  by_valid <- function(p) if (p < 50) "Fault" else if (p < 95) "Check" else "OK"
  row <- function(component, status, valid, last, detail)
    data.frame(Component = component, Status = status, Valid = valid,
               Last = last, Detail = detail, stringsAsFactors = FALSE)
  fmt_pct <- function(p) sprintf("%.0f%%", p)
  out <- list()

  for (v in list(c("temperature", "Temperature sensor"), c("pressure", "Pressure sensor"))) {
    p <- pct_valid(w[[v[1]]]); s <- by_valid(p)
    out[[length(out) + 1]] <- row(v[2], s, fmt_pct(p), last_valid(v[1]),
      if (s == "OK") "Readings normal"
      else paste("No valid readings since", last_valid(v[1]), "- probe needs repair"))
  }

  p <- pct_valid(w$humidity); s <- by_valid(p)
  w_sat <- d[d$datetime > end_t - days(SATURATED_DAYS), ]
  sat <- 100 * mean(w_sat$humidity >= 99.9, na.rm = TRUE)
  det <- "Readings normal"
  if (s != "OK") det <- "Many readings missing"
  else if (sat > SATURATED_PCT) {
    s <- "Check"
    det <- sprintf("%.0f%% of readings in the last %d days stuck at 100%% - possibly saturated or failing",
                   sat, SATURATED_DAYS)
  }
  out[[length(out) + 1]] <- row("Humidity sensor", s, fmt_pct(p), last_valid("humidity"), det)

  for (v in list(c("speed", "Wind speed (anemometer)"), c("dir", "Wind direction (vane)"))) {
    p <- pct_valid(w[[v[1]]]); s <- by_valid(p)
    flat <- longest_flat_h(w[[v[1]]], w$datetime)
    det <- if (s != "OK") "Many readings missing" else "Readings normal"
    if (s == "OK" && flat >= STUCK_HOURS) {
      s <- "Check"
      det <- sprintf("Reading unchanged for %.0f hours - possibly stuck", flat)
    }
    out[[length(out) + 1]] <- row(v[2], s, fmt_pct(p), last_valid(v[1]), det)
  }

  if ("battery" %in% names(d) && any(!is.na(w$battery))) {
    b_last <- tail(w$battery[!is.na(w$battery)], 1)
    b_min  <- min(w$battery, na.rm = TRUE)
    s <- if (b_min < BATT_FAULT_V) "Fault" else if (b_min < BATT_CHECK_V) "Check" else "OK"
    out[[length(out) + 1]] <- row("Battery", s, fmt_pct(pct_valid(w$battery)), last_valid("battery"),
      sprintf("Latest %.2f V, %d-day low %.2f V%s", b_last, HEALTH_DAYS, b_min,
              if (s == "OK") "" else " - check charging / solar panel"))
  }

  gaps <- as.numeric(diff(w$datetime), units = "hours")
  max_gap <- if (length(gaps)) max(gaps) else 0
  age <- as.numeric(Sys.Date() - as.Date(end_t))
  s <- "OK"; det <- sprintf("No transmission gaps over %d hours", GAP_HOURS)
  if (max_gap >= GAP_HOURS) {
    s <- "Check"; det <- sprintf("Longest gap %.1f hours - check radio / gateway", max_gap)
  }
  if (age > STALE_DAYS) {
    s <- "Check"; det <- paste0(det, sprintf("; last reading %d days ago - download new data", age))
  }
  out[[length(out) + 1]] <- row("Data feed", s, "-", format(end_t, "%b %d, %Y %H:%M"), det)

  list(table = do.call(rbind, out), start = end_t - days(HEALTH_DAYS), end = end_t)
}

health_badge <- function(s) {
  col <- c(OK = "#27ae60", Check = "#e67e22", Fault = "#c0392b", Open = "#c0392b",
           `Decision needed` = "#e67e22", Monitoring = "#2980b9", Resolved = "#7f8c8d")[s]
  if (is.na(col)) col <- "#7f8c8d"
  sprintf('<span style="background:%s;color:white;padding:2px 9px;border-radius:10px;font-size:12px;font-weight:600;white-space:nowrap;">%s</span>',
          col, htmltools::htmlEscape(s))
}

html_table <- function(df, badge_col) {
  esc <- function(x) htmltools::htmlEscape(as.character(x))
  head <- paste0("<tr>", paste0("<th>", esc(names(df)), "</th>", collapse = ""), "</tr>")
  body <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(names(df), function(n)
      if (n == badge_col) health_badge(df[[n]][i]) else esc(df[[n]][i]), "")
    paste0("<tr>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
  }, "")
  HTML(paste0('<table class="table table-striped table-bordered" style="width:100%;font-size:13px;">',
              head, paste(body, collapse = ""), "</table>"))
}

# Helper: downsample for faster plotting
downsample <- function(df, max_points = 15000) {
  n <- nrow(df)
  if (n <= max_points) return(df)
  idx <- seq(1, n, by = ceiling(n / max_points))
  df[idx, ]
}

# Direction binning helper (8 sectors centered on cardinal/intercardinal)
bin_direction <- function(dirs) {
  breaks <- c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)
  labels_9 <- c("N","NE","E","SE","S","SW","W","NW","N")
  sector <- cut(dirs, breaks = breaks, labels = labels_9,
                include.lowest = TRUE, right = FALSE)
  # Merge the two N bins
  levels(sector) <- c("N","NE","E","SE","S","SW","W","NW","N")
  sector
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(

  tags$head(tags$style(HTML("
    body { background-color: #ecf0f1; font-family: 'Segoe UI', Tahoma, sans-serif; }
    .title-panel {
      background: linear-gradient(135deg, #2c3e50, #3498db);
      color: white; padding: 20px 30px; margin: -15px -15px 20px -15px;
    }
    .title-panel h2 { margin: 0 0 5px 0; font-weight: 700; }
    .title-panel p  { margin: 0; opacity: 0.85; font-size: 14px; }
    .stat-box {
      background: white; border-left: 4px solid #3498db;
      padding: 15px; margin-bottom: 15px; border-radius: 4px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    .stat-box .stat-value { font-size: 26px; font-weight: 700; color: #2c3e50; }
    .stat-box .stat-label { font-size: 11px; color: #7f8c8d; text-transform: uppercase; }
    .stat-box.wind  { border-left-color: #3498db; }
    .stat-box.temp  { border-left-color: #e74c3c; }
    .stat-box.dir   { border-left-color: #2ecc71; }
    .stat-box.obs   { border-left-color: #f39c12; }
    .well { background: white; border: 1px solid #ddd; }
  "))),

  div(class = "title-panel",
    h2("Jamaica Bay Wildlife Refuge"),
    p("Wind & Temperature Dashboard | RM-0002 Environmental Sensor | West Pond, NY")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      selectInput("year", "Year:",
                  choices = c("All years" = "all",
                              setNames(as.character(data_years), data_years))),
      dateRangeInput("date_range", "Date Range:",
                     start = min(all_data$date),
                     end   = max(all_data$date),
                     min   = min(all_data$date),
                     max   = max(all_data$date)),
      hr(),
      h4("Time Series Options"),
      selectInput("ts_variable", "Variable:",
                  choices = c("Wind Speed (m/s)"      = "speed",
                              "Wind Direction (deg)"   = "dir",
                              "Temperature (C)"        = "temperature")),
      checkboxInput("show_smooth", "Show trend line", value = TRUE),
      hr(),
      h4("Wind Rose Options"),
      selectInput("rose_type", "Wind rose type:",
                  choices = c("Overall"      = "overall",
                              "By Year"      = "yearly",
                              "By Month"     = "monthly",
                              "Day vs Night" = "daynight")),
      hr(),
      div(style = "font-size: 11px; color: #95a5a6;",
        p("PIs: P. Staniczenko & C. Zarnoch, CUNY"),
        p(textOutput("data_summary_text"))
      )
    ),

    mainPanel(
      width = 9,

      fluidRow(
        column(3, div(class = "stat-box wind",
          div(class = "stat-label", "Mean Wind Speed"),
          div(class = "stat-value", textOutput("mean_speed", inline = TRUE)),
          div(style = "font-size: 12px; color: #95a5a6;", "m/s")
        )),
        column(3, div(class = "stat-box dir",
          div(class = "stat-label", "Dominant Direction"),
          div(class = "stat-value", textOutput("dom_dir", inline = TRUE)),
          div(style = "font-size: 12px; color: #95a5a6;", textOutput("dom_dir_pct", inline = TRUE))
        )),
        column(3, div(class = "stat-box temp",
          div(class = "stat-label", "Mean Temperature"),
          div(class = "stat-value", textOutput("mean_temp", inline = TRUE)),
          div(style = "font-size: 12px; color: #95a5a6;", "C")
        )),
        column(3, div(class = "stat-box obs",
          div(class = "stat-label", "Observations"),
          div(class = "stat-value", textOutput("n_obs", inline = TRUE)),
          div(style = "font-size: 12px; color: #95a5a6;", textOutput("n_days", inline = TRUE))
        ))
      ),

      tabsetPanel(
        id = "main_tabs", type = "tabs",

        tabPanel("Time Series",
          br(),
          plotOutput("ts_plot", height = "400px"),
          hr(),
          h4("All Variables Overview"),
          plotOutput("ts_facet_plot", height = "450px")
        ),

        tabPanel("Wind Rose",
          br(),
          plotOutput("wind_rose_plot", height = "500px"),
          hr(),
          h4("Wind Direction Frequency"),
          plotOutput("dir_bar_plot", height = "300px")
        ),

        tabPanel("Monthly Boxplots",
          br(),
          fluidRow(
            column(6, plotOutput("box_speed", height = "400px")),
            column(6, plotOutput("box_temp",  height = "400px"))
          ),
          hr(),
          plotOutput("box_dir", height = "350px")
        ),

        tabPanel("Summary Table",
          br(),
          h4("Annual Summary Statistics"),
          tableOutput("annual_table"),
          hr(),
          h4("Monthly Summary Statistics"),
          tableOutput("monthly_table"),
          hr(),
          h4("Wind Direction Statistics"),
          tableOutput("direction_table")
        ),

        tabPanel("Sensor Issues",
          br(),
          uiOutput("health_banner"),
          h4("Current Status"),
          uiOutput("health_window"),
          uiOutput("health_table"),
          hr(),
          h4("Data Quality by Month"),
          p(style = "font-size: 12px; color: #7f8c8d;",
            "Share of readings missing, removed as impossible, or (humidity) stuck at 100%.
             Covers all data, regardless of the filters."),
          plotOutput("health_heatmap", height = "300px"),
          hr(),
          h4("Battery Voltage"),
          plotOutput("battery_plot", height = "260px"),
          hr(),
          h4("Maintenance Log"),
          p(style = "font-size: 12px; color: #7f8c8d;",
            "Known issues and actions. Maintained by hand at the top of app.R."),
          uiOutput("maintenance_table")
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # Picking a year sets the date range to that year (clipped to the data);
  # the range can still be narrowed by hand afterwards.
  observeEvent(input$year, {
    lo <- min(all_data$date); hi <- max(all_data$date)
    if (input$year != "all") {
      y  <- as.integer(input$year)
      lo <- max(lo, as.Date(sprintf("%d-01-01", y)))
      hi <- min(hi, as.Date(sprintf("%d-12-31", y)))
    }
    updateDateRangeInput(session, "date_range", start = lo, end = hi)
  }, ignoreInit = TRUE)

  filtered <- reactive({
    req(input$date_range)
    d <- all_data %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
    validate(need(nrow(d) > 0, "No data in selected date range."))
    d$month_abbr <- droplevels(d$month_abbr)
    d
  })

  # Shared summary columns for the annual and monthly tables
  summarise_block <- function(g) {
    g %>% summarise(
      N            = n(),
      `Speed Mean` = round(s_mean(speed), 2),
      `Speed SD`   = round(s_sd(speed), 2),
      `Speed Max`  = round(s_max(speed), 1),
      `Temp Mean`  = round(s_mean(temperature), 2),
      `Temp SD`    = round(s_sd(temperature), 2),
      `Temp Min`   = round(s_min(temperature), 1),
      `Temp Max`   = round(s_max(temperature), 1),
      `Dir Mean`   = round(s_mean(dir), 0),
      .groups = "drop"
    )
  }

  # ---- Summary stat boxes ----
  output$mean_speed <- renderText({
    round(mean(filtered()$speed, na.rm = TRUE), 1)
  })

  output$dom_dir <- renderText({
    d <- filtered()
    d$sector <- bin_direction(d$dir)
    tbl <- sort(table(d$sector), decreasing = TRUE)
    names(tbl)[1]
  })

  output$dom_dir_pct <- renderText({
    d <- filtered()
    d$sector <- bin_direction(d$dir)
    tbl <- sort(table(d$sector), decreasing = TRUE)
    paste0(round(100 * tbl[1] / sum(tbl), 1), "% of obs")
  })

  output$mean_temp <- renderText({
    v <- s_mean(filtered()$temperature)
    if (is.na(v)) "-" else round(v, 1)
  })

  output$n_obs <- renderText({
    format(nrow(filtered()), big.mark = ",")
  })

  output$n_days <- renderText({
    paste(length(unique(filtered()$date)), "days")
  })

  output$data_summary_text <- renderText({
    paste0(format(nrow(all_data), big.mark = ","), " total observations")
  })

  # ---- Time Series: single variable ----
  output$ts_plot <- renderPlot({
    d <- filtered()
    var <- input$ts_variable
    var_labels <- c(speed = "Wind Speed (m/s)",
                    dir   = "Wind Direction (degrees)",
                    temperature = "Temperature (C)")
    var_colors <- c(speed = "#3498db", dir = "#2ecc71", temperature = "#e74c3c")

    d_plot <- downsample(d, 15000)

    p <- ggplot(d_plot, aes(x = datetime, y = .data[[var]])) +
      geom_line(alpha = 0.35, color = var_colors[var], linewidth = 0.3) +
      labs(title = paste(var_labels[var], "- Time Series"),
           subtitle = paste(format(input$date_range[1], "%b %d, %Y"), "to",
                            format(input$date_range[2], "%b %d, %Y")),
           x = NULL, y = var_labels[var]) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))

    if (input$show_smooth && nrow(d_plot) > 10) {
      p <- p + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                           color = var_colors[var], se = TRUE, alpha = 0.2,
                           linewidth = 1.2)
    }
    p
  })

  # ---- Time Series: all variables faceted ----
  output$ts_facet_plot <- renderPlot({
    d_plot <- downsample(filtered(), 10000)

    d_long <- d_plot %>%
      select(datetime, speed, dir, temperature) %>%
      pivot_longer(cols = c(speed, dir, temperature),
                   names_to = "variable", values_to = "value") %>%
      mutate(variable = factor(variable,
               levels = c("speed", "temperature", "dir"),
               labels = c("Wind Speed (m/s)", "Temperature (C)",
                           "Wind Direction (deg)")))

    ggplot(d_long, aes(x = datetime, y = value)) +
      geom_line(alpha = 0.3, color = "#34495e", linewidth = 0.2) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                  color = "#e74c3c", se = FALSE, linewidth = 1) +
      facet_wrap(~ variable, ncol = 1, scales = "free_y") +
      labs(title = "All Variables Overview", x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold", size = 12))
  })

  # ---- Wind Rose (pure ggplot2 with coord_polar) ----
  output$wind_rose_plot <- renderPlot({
    d <- filtered()

    speed_breaks <- c(0, 2, 4, 6, 8, 10, 15, Inf)
    speed_labels <- c("0-2", "2-4", "4-6", "6-8", "8-10", "10-15", ">15")
    dir_labels_16 <- c("N","NNE","NE","ENE","E","ESE","SE","SSE",
                        "S","SSW","SW","WSW","W","WNW","NW","NNW")
    dir_centers <- seq(0, 337.5, by = 22.5)
    dir_half <- 11.25

    rose <- d %>%
      mutate(
        speed_bin = cut(speed, breaks = speed_breaks, labels = speed_labels,
                        include.lowest = TRUE, right = FALSE),
        dir_idx = ((round((dir %% 360) / 22.5) %% 16) + 1),
        dir_label = factor(dir_labels_16[dir_idx], levels = dir_labels_16),
        dir_angle = dir_centers[dir_idx]
      )

    if (input$rose_type == "yearly") {
      rose$facet_var <- as.character(d$year)
    } else if (input$rose_type == "monthly") {
      rose$facet_var <- d$month_abbr
    } else if (input$rose_type == "daynight") {
      rose$facet_var <- ifelse(d$hour >= 6 & d$hour < 18,
                               "Day (6AM-6PM)", "Night (6PM-6AM)")
    } else {
      rose$facet_var <- "All Data"
    }

    rose_summary <- rose %>%
      filter(!is.na(speed_bin), !is.na(dir_label)) %>%
      group_by(facet_var, dir_label, dir_angle, speed_bin) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(facet_var) %>%
      mutate(pct = 100 * count / sum(count)) %>%
      ungroup()

    rose_colors <- c("#4575b4", "#74add1", "#abd9e9",
                     "#fee090", "#fdae61", "#f46d43", "#d73027")

    title_text <- switch(input$rose_type,
      "overall"  = "Wind Rose - Jamaica Bay Wildlife Refuge",
      "yearly"   = "Wind Rose by Year",
      "monthly"  = "Wind Rose by Month",
      "daynight" = "Wind Rose: Day vs Night"
    )

    p <- ggplot(rose_summary, aes(x = dir_angle, y = pct, fill = speed_bin)) +
      geom_bar(stat = "identity", width = 20, color = "white", linewidth = 0.15) +
      coord_polar(start = -pi/16) +
      scale_x_continuous(breaks = seq(0, 315, by = 45),
                         labels = c("N","NE","E","SE","S","SW","W","NW"),
                         limits = c(-11.25, 348.75)) +
      scale_fill_manual(values = rose_colors, name = "Speed (m/s)") +
      labs(title = title_text, x = NULL, y = "Frequency (%)") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            axis.text.y = element_text(size = 8),
            legend.position = "right")

    if (input$rose_type != "overall") {
      p <- p + facet_wrap(~ facet_var)
    }
    p
  })

  # ---- Direction bar chart ----
  output$dir_bar_plot <- renderPlot({
    d <- filtered()
    dir_labels <- c("N","NE","E","SE","S","SW","W","NW")
    d$sector <- bin_direction(d$dir)

    dir_stats <- d %>%
      filter(!is.na(sector)) %>%
      group_by(sector) %>%
      summarise(
        freq = n(),
        pct = round(100 * n() / nrow(d), 1),
        mean_speed = round(mean(speed, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      mutate(sector = factor(sector, levels = dir_labels))

    ggplot(dir_stats, aes(x = sector, y = pct, fill = mean_speed)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0(pct, "%\n", mean_speed, " m/s")),
                vjust = -0.3, size = 3.5, fontface = "bold") +
      scale_fill_gradient(low = "#74add1", high = "#d73027",
                          name = "Mean Speed\n(m/s)") +
      labs(title = "Wind Direction Frequency & Mean Speed",
           x = "Direction", y = "Frequency (%)") +
      ylim(0, max(dir_stats$pct) * 1.25) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })

  # ---- Monthly Boxplots ----
  output$box_speed <- renderPlot({
    d_plot <- downsample(filtered(), 30000)
    ggplot(d_plot, aes(x = month_abbr, y = speed, fill = month_abbr)) +
      geom_boxplot(outlier.alpha = 0.05, outlier.size = 0.3) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#e74c3c") +
      scale_fill_manual(values = month_fills(nlevels(d_plot$month_abbr), "#deebf7", "#2171b5")) +
      labs(title = "Wind Speed by Month", x = NULL, y = "Wind Speed (m/s)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$box_temp <- renderPlot({
    d_plot <- downsample(filtered(), 30000)
    d_plot <- d_plot[!is.na(d_plot$temperature), ]
    validate(need(nrow(d_plot) > 0,
                  "No valid temperature readings in this range (probe offline since March 2026)."))
    d_plot$month_abbr <- droplevels(d_plot$month_abbr)
    ggplot(d_plot, aes(x = month_abbr, y = temperature, fill = month_abbr)) +
      geom_boxplot(outlier.alpha = 0.05, outlier.size = 0.3) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#3498db") +
      scale_fill_manual(values = month_fills(nlevels(d_plot$month_abbr), "#fee0d2", "#cb181d")) +
      labs(title = "Temperature by Month", x = NULL, y = "Temperature (C)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$box_dir <- renderPlot({
    d_plot <- downsample(filtered(), 30000)
    ggplot(d_plot, aes(x = month_abbr, y = dir, fill = month_abbr)) +
      geom_boxplot(outlier.alpha = 0.05, outlier.size = 0.3) +
      scale_fill_manual(values = month_fills(nlevels(d_plot$month_abbr), "#e5f5e0", "#238b45")) +
      labs(title = "Wind Direction by Month", x = NULL, y = "Direction (degrees)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # ---- Summary Tables (using base Shiny renderTable) ----
  output$annual_table <- renderTable({
    filtered() %>%
      mutate(Year = as.character(year)) %>%
      group_by(Year) %>%
      summarise_block()
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", na = "-")

  output$monthly_table <- renderTable({
    filtered() %>%
      group_by(Month = month_abbr) %>%
      summarise_block()
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", na = "-")

  # ---- Sensor Issues ----
  health <- compute_health(all_data)

  output$health_banner <- renderUI({
    st <- health$table$Status
    n_fault <- sum(st == "Fault"); n_check <- sum(st == "Check")
    n_open  <- sum(maintenance_log$Status %in% c("Open", "Decision needed"))
    if (n_fault + n_check == 0) {
      col <- "#27ae60"; msg <- "All sensors are reporting normally."
    } else {
      col <- if (n_fault > 0) "#c0392b" else "#e67e22"
      msg <- paste0(
        if (n_fault > 0) sprintf("%d component%s need%s repair", n_fault,
                                 if (n_fault > 1) "s" else "", if (n_fault > 1) "" else "s"),
        if (n_fault > 0 && n_check > 0) "; " else "",
        if (n_check > 0) sprintf("%d need%s checking", n_check, if (n_check > 1) "" else "s"),
        ".")
    }
    div(style = sprintf("background:%s;color:white;padding:12px 18px;border-radius:4px;margin-bottom:15px;font-size:15px;", col),
        strong(msg),
        if (n_open > 0) span(style = "opacity:0.9;",
                             sprintf(" %d open item%s in the maintenance log.", n_open,
                                     if (n_open > 1) "s" else "")))
  })

  output$health_window <- renderUI({
    p(style = "font-size: 12px; color: #7f8c8d;",
      sprintf("Automatic checks on the latest %d days of data (%s to %s), regardless of the filters.",
              HEALTH_DAYS, format(health$start, "%b %d, %Y"), format(health$end, "%b %d, %Y")))
  })

  output$health_table <- renderUI({
    t <- health$table
    names(t) <- c("Component", "Status", sprintf("Valid (%dd)", HEALTH_DAYS),
                  "Last valid reading", "Details")
    html_table(t, "Status")
  })

  output$maintenance_table <- renderUI({
    ord <- order(match(maintenance_log$Status,
                       c("Open", "Decision needed", "Monitoring", "Resolved")))
    html_table(maintenance_log[ord, ], "Status")
  })

  output$health_heatmap <- renderPlot({
    d <- all_data
    q <- d %>%
      group_by(month_abbr) %>%
      summarise(
        `Temperature`         = 100 * mean(is.na(temperature)),
        `Pressure`            = 100 * mean(is.na(pressure)),
        `Humidity (missing)`  = 100 * mean(is.na(humidity)),
        `Humidity (at 100%)`  = 100 * mean(humidity >= 99.9, na.rm = TRUE),
        `Wind speed`          = 100 * mean(is.na(speed)),
        `Wind direction`      = 100 * mean(is.na(dir)),
        .groups = "drop") %>%
      pivot_longer(-month_abbr, names_to = "check", values_to = "pct") %>%
      mutate(check = factor(check, levels = rev(c("Temperature", "Pressure", "Humidity (missing)",
                                                  "Humidity (at 100%)", "Wind speed", "Wind direction"))))
    ggplot(q, aes(x = month_abbr, y = check, fill = pct)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = ifelse(pct < 0.5, "", sprintf("%.0f", pct)),
                    color = pct > 55), size = 3.4, show.legend = FALSE) +
      scale_color_manual(values = c(`FALSE` = "#2c3e50", `TRUE` = "white")) +
      scale_fill_gradient(low = "#f7f7f7", high = "#c0392b", limits = c(0, 100),
                          name = "% flagged") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
  })

  output$battery_plot <- renderPlot({
    validate(need("battery" %in% names(all_data) && any(!is.na(all_data$battery)),
                  "Battery voltage not in this data file (re-run update_dashboard.R)."))
    b <- all_data %>%
      filter(!is.na(battery)) %>%
      group_by(date) %>%
      summarise(low = min(battery), high = max(battery), .groups = "drop")
    ggplot(b, aes(x = date)) +
      geom_ribbon(aes(ymin = low, ymax = high), fill = "#3498db", alpha = 0.25) +
      geom_line(aes(y = low), color = "#2c3e50", linewidth = 0.5) +
      geom_hline(yintercept = BATT_CHECK_V, linetype = "dashed", color = "#e67e22") +
      geom_hline(yintercept = BATT_FAULT_V, linetype = "dashed", color = "#c0392b") +
      annotate("text", x = min(b$date), y = c(BATT_CHECK_V, BATT_FAULT_V) + 0.02,
               label = c("Check", "Fault"), hjust = 0, size = 3.5,
               color = c("#e67e22", "#c0392b")) +
      scale_y_continuous(limits = c(min(3.4, min(b$low)), max(4.3, max(b$high)))) +
      labs(x = NULL, y = "Volts", subtitle = "Daily range (band) and daily minimum (line)") +
      theme_minimal(base_size = 12)
  })

  output$direction_table <- renderTable({
    d <- filtered()
    d$sector <- bin_direction(d$dir)
    d %>%
      filter(!is.na(sector)) %>%
      group_by(Direction = sector) %>%
      summarise(
        N           = n(),
        `Freq (%)`  = round(100 * n() / nrow(d), 1),
        `Speed Mean` = round(s_mean(speed), 2),
        `Speed SD`   = round(s_sd(speed), 2),
        `Speed Max`  = round(s_max(speed), 1),
        `Temp Mean`  = round(s_mean(temperature), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(`Freq (%)`))
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", na = "-")
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
