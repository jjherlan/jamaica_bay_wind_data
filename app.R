# ============================================================================
#   Jamaica Bay Wildlife Refuge - Wind Data Dashboard
#   Shiny Application
#   Data source: RM-0002 environmental sensor
# ============================================================================

# Load required packages (install if needed)
required_packages <- c("shiny", "ggplot2", "dplyr", "lubridate", "tidyr", "DT")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(DT)

# ============================================================================
# DATA LOADING
# ============================================================================

# Function to clean a single raw CSV file
clean_jamaica_bay_file <- function(filepath) {
  cat("  Processing:", basename(filepath), "... ")

  raw_lines <- readLines(filepath, warn = FALSE)

  clean_numeric <- function(x) {
    cleaned <- gsub("[^0-9.\\-]", "", x)
    cleaned <- gsub("(\\..*)\\..*", "\\1", cleaned)
    as.numeric(cleaned)
  }

  # Extract rm-0002 data lines
  data_lines <- grep("^rm-0002", raw_lines, value = TRUE)

  if (length(data_lines) == 0) {
    cat("no rm-0002 lines found, skipping.\n")
    return(NULL)
  }

  result <- do.call(rbind, lapply(data_lines, function(line) {
    parts <- strsplit(line, ",")[[1]]
    if (length(parts) >= 11) {
      data.frame(
        local_time  = trimws(parts[4]),
        dir         = clean_numeric(parts[7]),
        humidity    = clean_numeric(parts[8]),
        pressure    = clean_numeric(parts[9]),
        speed       = clean_numeric(parts[10]),
        temperature = clean_numeric(parts[11]),
        stringsAsFactors = FALSE
      )
    }
  }))

  if (is.null(result) || nrow(result) == 0) {
    cat("no valid rows, skipping.\n")
    return(NULL)
  }

  # Try mdy_hms first (M/D/YYYY H:MM:SS), then mdy_hm (M/D/YYYY H:MM)
  result$datetime <- mdy_hms(result$local_time, quiet = TRUE)
  missing <- is.na(result$datetime)
  if (any(missing)) {
    result$datetime[missing] <- mdy_hm(result$local_time[missing], quiet = TRUE)
  }
  result <- result[!is.na(result$datetime), ]

  cat(nrow(result), "rows\n")
  result
}

# Load and combine all CSV files from the csv_claude folder
# Try multiple paths in order of preference
possible_paths <- c(
  "C:/Users/boros/csv_claude",
  file.path(dirname(getwd()), "csv_claude"),
  file.path(getwd(), "..", "csv_claude"),
  getwd()
)

csv_dir <- NULL
for (p in possible_paths) {
  if (dir.exists(p) && length(list.files(p, pattern = "jamaica_bay_\\d+\\.csv$")) > 0) {
    csv_dir <- p
    break
  }
}

if (is.null(csv_dir)) {
  stop("Could not find CSV directory. Please set csv_dir manually.")
}

csv_files <- list.files(csv_dir, pattern = "jamaica_bay_\\d+\\.csv$",
                        full.names = TRUE)

cat("Loading", length(csv_files), "CSV files from:", csv_dir, "\n")

all_data <- do.call(rbind, lapply(csv_files, clean_jamaica_bay_file))

# Deduplicate and sort
all_data <- all_data %>%
  distinct(datetime, .keep_all = TRUE) %>%
  arrange(datetime)

# Add derived columns
all_data <- all_data %>%
  mutate(
    date       = as.Date(datetime),
    month      = month(datetime, label = TRUE, abbr = FALSE),
    month_abbr = month(datetime, label = TRUE),
    hour       = hour(datetime),
    ws = speed,
    wd = dir
  )

cat("Loaded", nrow(all_data), "observations from",
    format(min(all_data$datetime), "%Y-%m-%d"),
    "to", format(max(all_data$datetime), "%Y-%m-%d"), "\n")

# Helper: downsample data for plotting (keep every nth row)
downsample <- function(df, max_points = 20000) {
  n <- nrow(df)
  if (n <= max_points) return(df)
  idx <- seq(1, n, by = ceiling(n / max_points))
  df[idx, ]
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(

  # Custom CSS
  tags$head(tags$style(HTML("
    body { background-color: #ecf0f1; font-family: 'Segoe UI', Tahoma, Geneva, sans-serif; }
    .title-panel {
      background: linear-gradient(135deg, #2c3e50, #3498db);
      color: white;
      padding: 20px 30px;
      margin: -15px -15px 20px -15px;
    }
    .title-panel h2 { margin: 0 0 5px 0; font-weight: 700; }
    .title-panel p  { margin: 0; opacity: 0.85; font-size: 14px; }
    .stat-box {
      background: white;
      border-left: 4px solid #3498db;
      padding: 15px;
      margin-bottom: 15px;
      border-radius: 4px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    .stat-box .stat-value { font-size: 26px; font-weight: 700; color: #2c3e50; }
    .stat-box .stat-label { font-size: 11px; color: #7f8c8d; text-transform: uppercase; letter-spacing: 0.5px; }
    .stat-box.wind  { border-left-color: #3498db; }
    .stat-box.temp  { border-left-color: #e74c3c; }
    .stat-box.dir   { border-left-color: #2ecc71; }
    .stat-box.obs   { border-left-color: #f39c12; }
    .well { background: white; border: 1px solid #ddd; }
    .nav-tabs > li.active > a { font-weight: 600; }
  "))),

  # Title
  div(class = "title-panel",
    h2("Jamaica Bay Wildlife Refuge"),
    p("Wind & Temperature Dashboard | RM-0002 Environmental Sensor | West Pond, NY")
  ),

  # Sidebar
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
                  choices = c("Wind Speed (m/s)"   = "speed",
                              "Wind Direction (deg)" = "dir",
                              "Temperature (C)"    = "temperature")),
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

    # Main panel
    mainPanel(
      width = 9,

      # Summary stat boxes
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

      # Tabs
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
          DTOutput("monthly_table"),
          hr(),
          h4("Wind Direction Statistics"),
          DTOutput("direction_table")
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # Filtered data reactive
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
    d <- filtered()
    round(mean(d$speed, na.rm = TRUE), 1)
  })

  output$dom_dir <- renderText({
    d <- filtered()
    dir_labels <- c("N","NE","E","SE","S","SW","W","NW")
    breaks <- c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)
    d$sector <- cut(d$dir, breaks = breaks,
                    labels = c("N","NE","E","SE","S","SW","W","NW","N"),
                    include.lowest = TRUE, right = FALSE)
    # Combine the two N bins
    levels(d$sector) <- c("N","NE","E","SE","S","SW","W","NW","N")
    tbl <- sort(table(d$sector), decreasing = TRUE)
    names(tbl)[1]
  })

  output$dom_dir_pct <- renderText({
    d <- filtered()
    breaks <- c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)
    d$sector <- cut(d$dir, breaks = breaks,
                    labels = c("N","NE","E","SE","S","SW","W","NW","N"),
                    include.lowest = TRUE, right = FALSE)
    levels(d$sector) <- c("N","NE","E","SE","S","SW","W","NW","N")
    tbl <- sort(table(d$sector), decreasing = TRUE)
    paste0(round(100 * tbl[1] / sum(tbl), 1), "% of obs")
  })

  output$mean_temp <- renderText({
    d <- filtered()
    round(mean(d$temperature, na.rm = TRUE), 1)
  })

  output$n_obs <- renderText({
    format(nrow(filtered()), big.mark = ",")
  })

  output$n_days <- renderText({
    d <- filtered()
    paste(length(unique(d$date)), "days")
  })

  output$data_summary_text <- renderText({
    paste0(format(nrow(all_data), big.mark = ","), " total observations loaded")
  })

  # ---- Time Series: single variable ----
  output$ts_plot <- renderPlot({
    d <- filtered()

    var <- input$ts_variable
    var_labels <- c(speed = "Wind Speed (m/s)",
                    dir   = "Wind Direction (degrees)",
                    temperature = "Temperature (C)")
    var_colors <- c(speed = "#3498db", dir = "#2ecc71", temperature = "#e74c3c")

    # Downsample for faster plotting
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
      # Use gam for large datasets (much faster than loess)
      p <- p + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                           color = var_colors[var], se = TRUE, alpha = 0.2,
                           linewidth = 1.2)
    }
    p
  })

  # ---- Time Series: all variables faceted ----
  output$ts_facet_plot <- renderPlot({
    d <- filtered()
    d_plot <- downsample(d, 10000)

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
      theme(
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 12)
      )
  })

  # ---- Wind Rose (ggplot2-based, no openair dependency) ----
  output$wind_rose_plot <- renderPlot({
    d <- filtered()

    # Create wind speed bins and direction bins
    speed_breaks <- c(0, 2, 4, 6, 8, 10, 15, Inf)
    speed_labels <- c("0-2", "2-4", "4-6", "6-8", "8-10", "10-15", ">15")
    dir_breaks <- seq(0, 360, by = 22.5)
    dir_labels_22 <- c("N","NNE","NE","ENE","E","ESE","SE","SSE",
                        "S","SSW","SW","WSW","W","WNW","NW","NNW")

    rose <- d %>%
      mutate(
        speed_bin = cut(speed, breaks = speed_breaks, labels = speed_labels,
                        include.lowest = TRUE, right = FALSE),
        dir_bin = cut(dir %% 360,
                      breaks = c(0, 11.25, 33.75, 56.25, 78.75, 101.25,
                                 123.75, 146.25, 168.75, 191.25, 213.75,
                                 236.25, 258.75, 281.25, 303.75, 326.25,
                                 348.75, 360),
                      labels = c("N", dir_labels_22[2:16], "N"),
                      include.lowest = TRUE, right = FALSE)
      )

    # Combine the two N bins
    levels(rose$dir_bin) <- c("N", dir_labels_22[2:16], "N")

    # Facet by type if requested
    if (input$rose_type == "monthly") {
      rose$facet_var <- d$month_abbr
    } else if (input$rose_type == "daynight") {
      rose$facet_var <- ifelse(d$hour >= 6 & d$hour < 18,
                               "Day (6AM-6PM)", "Night (6PM-6AM)")
    } else {
      rose$facet_var <- "All Data"
    }

    rose_summary <- rose %>%
      filter(!is.na(speed_bin), !is.na(dir_bin)) %>%
      group_by(facet_var, dir_bin, speed_bin) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(facet_var) %>%
      mutate(pct = 100 * count / sum(count)) %>%
      ungroup()

    # Map direction labels to angles
    dir_angles <- setNames(seq(0, 337.5, by = 22.5), dir_labels_22)
    rose_summary$angle <- dir_angles[as.character(rose_summary$dir_bin)]

    rose_colors <- c("#4575b4", "#74add1", "#abd9e9",
                     "#fee090", "#fdae61", "#f46d43", "#d73027")

    title_text <- switch(input$rose_type,
      "overall"  = "Wind Rose - Jamaica Bay Wildlife Refuge",
      "monthly"  = "Wind Rose by Month",
      "daynight" = "Wind Rose: Day vs Night"
    )

    p <- ggplot(rose_summary, aes(x = angle, y = pct, fill = speed_bin)) +
      geom_bar(stat = "identity", width = 20, color = "white", linewidth = 0.1) +
      coord_polar(start = -pi/16) +
      scale_x_continuous(breaks = seq(0, 337.5, by = 45),
                         labels = c("N","NE","E","SE","S","SW","W","NW"),
                         limits = c(0, 360)) +
      scale_fill_manual(values = rose_colors, name = "Speed (m/s)") +
      labs(title = title_text, x = NULL, y = "Frequency (%)") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 8),
        legend.position = "right"
      )

    if (input$rose_type != "overall") {
      p <- p + facet_wrap(~ facet_var)
    }

    p
  })

  # ---- Direction bar chart ----
  output$dir_bar_plot <- renderPlot({
    d <- filtered()

    dir_labels <- c("N","NE","E","SE","S","SW","W","NW")
    breaks <- c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)
    d$sector <- cut(d$dir, breaks = breaks,
                    labels = c("N","NE","E","SE","S","SW","W","NW","N"),
                    include.lowest = TRUE, right = FALSE)
    levels(d$sector) <- c("N","NE","E","SE","S","SW","W","NW","N")

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
    d <- filtered()
    d_plot <- downsample(d, 30000)

    ggplot(d_plot, aes(x = month_abbr, y = speed, fill = month_abbr)) +
      geom_boxplot(outlier.alpha = 0.05, outlier.size = 0.3) +
      stat_summary(fun = mean, geom = "point", shape = 18,
                   size = 3, color = "#e74c3c") +
      scale_fill_brewer(palette = "Blues") +
      labs(title = "Wind Speed by Month",
           x = NULL, y = "Wind Speed (m/s)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
  })

  output$box_temp <- renderPlot({
    d <- filtered()
    d_plot <- downsample(d, 30000)

    ggplot(d_plot, aes(x = month_abbr, y = temperature, fill = month_abbr)) +
      geom_boxplot(outlier.alpha = 0.05, outlier.size = 0.3) +
      stat_summary(fun = mean, geom = "point", shape = 18,
                   size = 3, color = "#3498db") +
      scale_fill_brewer(palette = "Reds") +
      labs(title = "Temperature by Month",
           x = NULL, y = "Temperature (C)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
  })

  output$box_dir <- renderPlot({
    d <- filtered()
    d_plot <- downsample(d, 30000)

    ggplot(d_plot, aes(x = month_abbr, y = dir, fill = month_abbr)) +
      geom_boxplot(outlier.alpha = 0.05, outlier.size = 0.3) +
      scale_fill_brewer(palette = "Greens") +
      labs(title = "Wind Direction by Month",
           x = NULL, y = "Direction (degrees)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none")
  })

  # ---- Monthly Summary Table ----
  output$monthly_table <- renderDT({
    d <- filtered()

    monthly <- d %>%
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

    datatable(monthly, options = list(pageLength = 12, dom = "t"),
              rownames = FALSE)
  })

  # ---- Direction Stats Table ----
  output$direction_table <- renderDT({
    d <- filtered()

    dir_labels <- c("N","NE","E","SE","S","SW","W","NW")
    breaks <- c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)
    d$sector <- cut(d$dir, breaks = breaks,
                    labels = c("N","NE","E","SE","S","SW","W","NW","N"),
                    include.lowest = TRUE, right = FALSE)
    levels(d$sector) <- c("N","NE","E","SE","S","SW","W","NW","N")

    dir_tbl <- d %>%
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

    datatable(dir_tbl, options = list(pageLength = 8, dom = "t"),
              rownames = FALSE)
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
