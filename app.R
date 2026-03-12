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

# Read pre-processed clean CSV (all 7 collection periods combined)
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
    month      = month(datetime, label = TRUE, abbr = FALSE),
    month_abbr = month(datetime, label = TRUE),
    hour       = hour(datetime)
  )

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
          h4("Monthly Summary Statistics"),
          tableOutput("monthly_table"),
          hr(),
          h4("Wind Direction Statistics"),
          tableOutput("direction_table")
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  filtered <- reactive({
    req(input$date_range)
    d <- all_data %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
    validate(need(nrow(d) > 0, "No data in selected date range."))
    d
  })

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
    round(mean(filtered()$temperature, na.rm = TRUE), 1)
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

    if (input$rose_type == "monthly") {
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
      scale_fill_brewer(palette = "Blues") +
      labs(title = "Wind Speed by Month", x = NULL, y = "Wind Speed (m/s)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), legend.position = "none")
  })

  output$box_temp <- renderPlot({
    d_plot <- downsample(filtered(), 30000)
    ggplot(d_plot, aes(x = month_abbr, y = temperature, fill = month_abbr)) +
      geom_boxplot(outlier.alpha = 0.05, outlier.size = 0.3) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#3498db") +
      scale_fill_brewer(palette = "Reds") +
      labs(title = "Temperature by Month", x = NULL, y = "Temperature (C)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), legend.position = "none")
  })

  output$box_dir <- renderPlot({
    d_plot <- downsample(filtered(), 30000)
    ggplot(d_plot, aes(x = month_abbr, y = dir, fill = month_abbr)) +
      geom_boxplot(outlier.alpha = 0.05, outlier.size = 0.3) +
      scale_fill_brewer(palette = "Greens") +
      labs(title = "Wind Direction by Month", x = NULL, y = "Direction (degrees)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), legend.position = "none")
  })

  # ---- Summary Tables (using base Shiny renderTable) ----
  output$monthly_table <- renderTable({
    d <- filtered()
    d %>%
      group_by(Month = month_abbr) %>%
      summarise(
        N            = n(),
        `Speed Mean`  = round(mean(speed, na.rm = TRUE), 2),
        `Speed SD`    = round(sd(speed, na.rm = TRUE), 2),
        `Speed Max`   = round(max(speed, na.rm = TRUE), 1),
        `Temp Mean`   = round(mean(temperature, na.rm = TRUE), 2),
        `Temp SD`     = round(sd(temperature, na.rm = TRUE), 2),
        `Temp Min`    = round(min(temperature, na.rm = TRUE), 1),
        `Temp Max`    = round(max(temperature, na.rm = TRUE), 1),
        `Dir Mean`    = round(mean(dir, na.rm = TRUE), 0),
        .groups = "drop"
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  output$direction_table <- renderTable({
    d <- filtered()
    d$sector <- bin_direction(d$dir)
    d %>%
      filter(!is.na(sector)) %>%
      group_by(Direction = sector) %>%
      summarise(
        N           = n(),
        `Freq (%)`  = round(100 * n() / nrow(d), 1),
        `Speed Mean` = round(mean(speed, na.rm = TRUE), 2),
        `Speed SD`   = round(sd(speed, na.rm = TRUE), 2),
        `Speed Max`  = round(max(speed, na.rm = TRUE), 1),
        `Temp Mean`  = round(mean(temperature, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(`Freq (%)`))
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
