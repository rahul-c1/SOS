options(shiny.maxRequestSize=1000*1024^2)
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(lubridate)

# UI
ui <- fluidPage(
  titlePanel("SOS (Start of Swing) Trading Strategy Scanner"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("datafile", "Upload CSV (symbol, date, open, high, low, close, volume)",
                accept = c(".csv")),
      
      hr(),
      h4("Market Conditions"),
      selectInput("market_condition", "Market Condition:",
                  choices = c("Bullish" = "bullish",
                              "Choppy" = "choppy",
                              "Bearish" = "bearish"),
                  selected = "bullish"),
      
      numericInput("t2108", "T2108 Value (for Bottom Bounce):",
                   value = 50, min = 0, max = 100),
      
      hr(),
      h4("Scan Settings"),
      dateInput("scan_date", "Scan Date:", value = Sys.Date()),
      
      checkboxGroupInput("setup_types", "Setup Types:",
                         choices = c("Plain SOS" = "plain",
                                   "Continuation SOS" = "continuation",
                                   "Consolidation SOS" = "consolidation",
                                   "Bottom Bounce SOS" = "bottom_bounce"),
                         selected = c("plain", "continuation")),
      
      hr(),
      h4("Filters"),
      numericInput("min_price", "Minimum Close Price ($):",
                   value = 10, min = 0, step = 1),
      
      hr(),
      h4("Position Sizing"),
      numericInput("position_size", "Default Position Size (shares):",
                   value = 2000, min = 100, max = 10000, step = 100),
      
      actionButton("scan_btn", "Run SOS Scan", class = "btn-primary btn-block"),
      
      hr(),
      helpText("SOS Strategy: Find momentum bursts via range expansion. Expected: 8-20% over 3-5 days.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Qualified Setups",
                 br(),
                 h4("SOS Qualified Candidates"),
                 DTOutput("qualified_table"),
                 br(),
                 downloadButton("download_setups", "Download Results")
        ),
        
        tabPanel("All Breakouts",
                 br(),
                 h4("Raw Range Expansions (Pre-Qualification)"),
                 DTOutput("raw_scans")
        ),
        
        tabPanel("Stock Detail",
                 br(),
                 textInput("detail_symbol", "Enter Symbol:", value = ""),
                 actionButton("show_detail", "Show Chart & Analysis"),
                 br(), br(),
                 plotOutput("stock_chart", height = "400px"),
                 br(),
                 verbatimTextOutput("stock_analysis")
        ),
        
        tabPanel("Strategy Guide",
                 br(),
                 h3("SOS Strategy Summary"),
                 tags$ul(
                   tags$li(tags$b("Core Concept:"), "Momentum bursts via range expansion"),
                   tags$li(tags$b("Expected Return:"), "8-20% over 3-5 days"),
                   tags$li(tags$b("Entry:"), "Buy breakout day only, preferably 9:30-10:00am"),
                   tags$li(tags$b("Stop Loss:"), "Half-day range (or low of day for early entry)"),
                   tags$li(tags$b("Exit:"), "3-5 day hold, peel at +8-10%, exit if no follow-through")
                 ),
                 hr(),
                 h4("Plain SOS Sectors (REQUIRED):"),
                 tags$ul(
                   tags$li("Healthcare / Biotech"),
                   tags$li("Technology"),
                   tags$li("Consumer Discretionary")
                 ),
                 hr(),
                 h4("Setup Types:"),
                 tags$ul(
                   tags$li(tags$b("Plain SOS:"), "Range expansion from consolidation"),
                   tags$li(tags$b("Continuation:"), "First leg 15%+, 3-10 day pause, new expansion"),
                   tags$li(tags$b("Consolidation:"), "First leg 50-150%+, multi-week base, volume surge"),
                   tags$li(tags$b("Bottom Bounce:"), "Only when T2108 < 20, institutional stocks")
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data storage
  stock_data <- reactiveVal(NULL)
  scan_results <- reactiveVal(NULL)
  qualified_results <- reactiveVal(NULL)
  
  # Load data
  observeEvent(input$datafile, {
    req(input$datafile)
    
    tryCatch({
      df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
      
      # Validate columns
      required_cols <- c("symbol", "date", "open", "high", "low", "close", "volume")
      if (!all(required_cols %in% tolower(names(df)))) {
        showNotification("CSV must have columns: symbol, date, open, high, low, close, volume",
                        type = "error", duration = 5)
        return(NULL)
      }
      
      # Standardize column names
      names(df) <- tolower(names(df))
      df$date <- as.Date(df$date)
      
      stock_data(df)
      showNotification("Data loaded successfully!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 5)
    })
  })
  
  # Calculate technical indicators
  calculate_indicators <- function(df) {
    df <- df %>%
      arrange(symbol, date) %>%
      group_by(symbol) %>%
      mutate(
        # Range calculations
        range = high - low,
        avg_range_10 = zoo::rollmean(range, k = 10, fill = NA, align = "right"),
        range_expansion = range / lag(avg_range_10, 1),
        
        # Price change
        pct_change = (close - lag(close, 1)) / lag(close, 1) * 100,
        
        # Volume
        avg_volume_10 = zoo::rollmean(volume, k = 10, fill = NA, align = "right"),
        volume_surge = volume / avg_volume_10,
        
        # Running metrics
        days_up = ifelse(close > lag(close, 1), 1, 0),
        consecutive_up = NA,
        
        # For continuation setup
        high_20 = zoo::rollapply(high, width = 20, FUN = max, fill = NA, align = "right", partial = TRUE),
        low_20 = zoo::rollapply(low, width = 20, FUN = min, fill = NA, align = "right", partial = TRUE),
        
        # Price levels
        gain_from_20d_low = (close - low_20) / low_20 * 100
      ) %>%
      ungroup()
    
    # Calculate consecutive up days
    df <- df %>%
      group_by(symbol) %>%
      mutate(
        consecutive_up = ave(days_up, cumsum(days_up == 0), FUN = cumsum)
      ) %>%
      ungroup()
    
    return(df)
  }
  
  # Scan for range expansions
  scan_range_expansions <- function(df, scan_date, min_price) {
    df_indicators <- calculate_indicators(df)
    
    # Filter to scan date and minimum price
    scan_data <- df_indicators %>%
      filter(date == scan_date, close >= min_price) %>%
      filter(!is.na(range_expansion), !is.na(avg_volume_10))
    
    # Bullish scan: range expansion + price up
    bullish <- scan_data %>%
      filter(
        range_expansion >= 1.5,  # Range is 50%+ larger than average
        pct_change > 2,           # Price up at least 2%
        volume > avg_volume_10 * 0.8  # Volume decent
      ) %>%
      mutate(scan_type = "Bullish")
    
    # Bearish scan: range expansion + price down
    bearish <- scan_data %>%
      filter(
        range_expansion >= 1.5,
        pct_change < -2,
        volume > avg_volume_10 * 0.8
      ) %>%
      mutate(scan_type = "Bearish")
    
    results <- bind_rows(bullish, bearish) %>%
      arrange(desc(volume))
    
    return(results)
  }
  
  # Qualify setups based on SOS rules
  qualify_setups <- function(raw_scans, setup_types, market_condition, t2108_value) {
    # Initialize with proper structure
    qualified <- data.frame(
      symbol = character(),
      date = as.Date(character()),
      setup_type = character(),
      close = numeric(),
      range_expansion = numeric(),
      pct_change = numeric(),
      volume_surge = numeric(),
      consecutive_up = numeric(),
      stop_loss = numeric(),
      risk_per_share = numeric(),
      notes = character(),
      stringsAsFactors = FALSE
    )
    
    # Check if raw_scans is empty
    if (is.null(raw_scans) || nrow(raw_scans) == 0) {
      return(qualified)
    }
    
    for (i in 1:nrow(raw_scans)) {
      row <- raw_scans[i, ]
      symbol <- as.character(row$symbol)
      scan_date <- as.Date(row$date)
      
      # Skip if symbol is empty or NA
      if (is.na(symbol) || symbol == "" || length(symbol) == 0) next
      
      # Get historical data for this symbol
      # First check if symbol has enough data (at least 61 days)
      symbol_data <- stock_data() %>%
        filter(symbol == !!symbol, date <= scan_date) %>%
        arrange(date)
      
      if (nrow(symbol_data) < 61) next  # Skip if insufficient data
      
      hist <- symbol_data %>%
        tail(60)  # Last 60 days
      
      if (nrow(hist) < 20) next
      
      hist <- calculate_indicators(hist)
      current <- tail(hist, 1)
      
      # Check setup types
      setup_type <- NA
      sector <- "Unknown"  # You would need sector data in your CSV
      qualified_setup <- FALSE
      notes <- ""
      
      # Plain SOS
      if ("plain" %in% setup_types) {
        if (current$range_expansion >= 1.5 && current$consecutive_up <= 2) {
          setup_type <- "Plain SOS"
          # Note: Sector check would go here - you need sector data
          notes <- "Check sector: Must be Healthcare, Technology, or Consumer Discretionary"
          qualified_setup <- TRUE
        }
      }
      
      # Continuation SOS
      if ("continuation" %in% setup_types && is.na(setup_type)) {
        # Look for first leg (15%+ move in <10 days, then 3-10 day pause)
        hist_20 <- tail(hist, 20)
        max_gain <- max((hist_20$high - hist_20$low_20) / hist_20$low_20 * 100, na.rm = TRUE)
        
        if (max_gain >= 15 && current$consecutive_up <= 2) {
          setup_type <- "Continuation SOS"
          notes <- "Verify: First leg 15%+, orderly 3-10 day pause before this breakout"
          qualified_setup <- TRUE
        }
      }
      
      # Consolidation SOS
      if ("consolidation" %in% setup_types && is.na(setup_type)) {
        if (current$gain_from_20d_low >= 50 && current$volume_surge >= 1.5) {
          setup_type <- "Consolidation SOS"
          notes <- "Verify: First leg 50-150%+, multi-week consolidation, catalyst present"
          qualified_setup <- TRUE
        }
      }
      
      # Bottom Bounce SOS
      if ("bottom_bounce" %in% setup_types && is.na(setup_type)) {
        if (t2108_value < 20 && current$consecutive_up >= 1) {
          setup_type <- "Bottom Bounce SOS"
          notes <- "T2108 < 20. Verify: Institutional quality, takes out 3 days weakness"
          qualified_setup <- TRUE
        }
      }
      
      if (qualified_setup) {
        # Calculate stop loss (half-day range)
        stop_loss <- current$close - (current$range / 2)
        risk_per_share <- current$close - stop_loss
        
        qualified <- bind_rows(qualified, data.frame(
          symbol = symbol,
          date = scan_date,
          setup_type = setup_type,
          close = current$close,
          range_expansion = round(current$range_expansion, 2),
          pct_change = round(current$pct_change, 2),
          volume_surge = round(current$volume_surge, 2),
          consecutive_up = current$consecutive_up,
          stop_loss = round(stop_loss, 2),
          risk_per_share = round(risk_per_share, 2),
          notes = notes,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(qualified)
  }
  
  # Run scan
  observeEvent(input$scan_btn, {
    req(stock_data())
    
    withProgress(message = 'Scanning for SOS setups...', value = 0, {
      
      incProgress(0.3, detail = "Finding range expansions...")
      raw <- scan_range_expansions(stock_data(), input$scan_date, input$min_price)
      scan_results(raw)
      
      incProgress(0.4, detail = "Qualifying setups...")
      qualified <- qualify_setups(raw, input$setup_types, input$market_condition, input$t2108)
      qualified_results(qualified)
      
      incProgress(0.3, detail = "Complete!")
    })
    
    if (is.null(qualified_results()) || nrow(qualified_results()) == 0) {
      showNotification("No qualified setups found for selected criteria.",
                      type = "warning", duration = 5)
    } else {
      showNotification(paste("Found", nrow(qualified_results()), "qualified SOS setups!"),
                      type = "message", duration = 3)
    }
  })
  
  # Display qualified setups
  output$qualified_table <- renderDT({
    req(qualified_results())
    
    df <- qualified_results()
    
    # Check if required columns exist before processing
    if (!"risk_per_share" %in% names(df)) {
      return(datatable(data.frame(Message = "No qualified setups found or data error"),
                      options = list(dom = 't')))
    }
    
    df <- df %>%
      mutate(
        position_size = input$position_size,
        total_risk = round(risk_per_share * position_size, 2)
      ) %>%
      select(symbol, setup_type, close, pct_change, range_expansion, 
             volume_surge, consecutive_up, stop_loss, risk_per_share, 
             position_size, total_risk, notes)
    
    datatable(df, 
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE) %>%
      formatCurrency(c("close", "stop_loss", "risk_per_share", "total_risk"), "$") %>%
      formatRound(c("pct_change", "range_expansion", "volume_surge"), 2)
  })
  
  # Display raw scans
  output$raw_scans <- renderDT({
    req(scan_results())
    
    df <- scan_results() %>%
      select(symbol, scan_type, close, pct_change, range_expansion, 
             volume_surge, consecutive_up, volume)
    
    datatable(df,
              options = list(pageLength = 50, scrollX = TRUE),
              rownames = FALSE) %>%
      formatCurrency("close", "$") %>%
      formatRound(c("pct_change", "range_expansion", "volume_surge"), 2)
  })
  
  # Stock detail chart
  observeEvent(input$show_detail, {
    req(input$detail_symbol, stock_data())
    
    symbol <- toupper(trimws(input$detail_symbol))
    
    df <- stock_data() %>%
      filter(symbol == !!symbol) %>%
      arrange(date) %>%
      tail(60)
    
    if (nrow(df) == 0) {
      showNotification("Symbol not found in data", type = "error")
      return(NULL)
    }
    
    output$stock_chart <- renderPlot({
      ggplot(df, aes(x = date)) +
        geom_segment(aes(xend = date, y = low, yend = high), color = "gray50") +
        geom_segment(aes(xend = date, y = open, yend = close, color = close >= open), size = 3) +
        scale_color_manual(values = c("red", "green")) +
        geom_line(aes(y = zoo::rollmean(close, 10, fill = NA)), color = "blue", size = 1) +
        labs(title = paste(symbol, "- Last 60 Days"),
             y = "Price", x = "Date") +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    output$stock_analysis <- renderPrint({
      df_calc <- calculate_indicators(df)
      latest <- tail(df_calc, 1)
      
      cat("=== Stock Analysis for", symbol, "===\n\n")
      cat("Latest Close:", round(latest$close, 2), "\n")
      cat("Today's % Change:", round(latest$pct_change, 2), "%\n")
      cat("Range Expansion:", round(latest$range_expansion, 2), "x\n")
      cat("Volume vs Avg:", round(latest$volume_surge, 2), "x\n")
      cat("Consecutive Up Days:", latest$consecutive_up, "\n")
      cat("20-day Low:", round(latest$low_20, 2), "\n")
      cat("Gain from 20d Low:", round(latest$gain_from_20d_low, 2), "%\n\n")
      
      cat("--- Stop Loss Calculation ---\n")
      cat("Today's Range:", round(latest$range, 2), "\n")
      cat("Suggested Stop (Half-Range):", round(latest$close - latest$range/2, 2), "\n")
    })
  })
  
  # Download results
  output$download_setups <- downloadHandler(
    filename = function() {
      paste0("SOS_setups_", input$scan_date, ".csv")
    },
    content = function(file) {
      write.csv(qualified_results(), file, row.names = FALSE)
    }
  )
}

# Run app
shinyApp(ui = ui, server = server)