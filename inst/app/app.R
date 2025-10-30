
library(shiny)

# ---------------------------
# Helper functions (shared)
# ---------------------------
next_pentad <- function(y, m, p) {
  if (p < 6) return(list(year = y, month = m, pentad = p + 1))
  if (m < 12) return(list(year = y, month = m + 1, pentad = 1))
  list(year = y + 1, month = 1, pentad = 1)
}
next_dekad <- function(y, m, d) {
  if (d < 3) return(list(year = y, month = m, dekad = d + 1))
  if (m < 12) return(list(year = y, month = m + 1, dekad = 1))
  list(year = y + 1, month = 1, dekad = 1)
}
next_month <- function(y, m) {
  if (m < 12) return(list(year = y, month = m + 1))
  list(year = y + 1, month = 1)
}
next_day <- function(y, m, d) {
  dt <- as.Date(sprintf("%04d-%02d-%02d", y, m, d))
  ndt <- dt + 1
  list(year = as.integer(format(ndt, "%Y")),
       month = as.integer(format(ndt, "%m")),
       day = as.integer(format(ndt, "%d")))
}
is_before_or_equal <- function(y1, m1, d1, y2, m2, d2) {
  dt1 <- as.Date(sprintf("%04d-%02d-%02d", y1, m1, d1))
  dt2 <- as.Date(sprintf("%04d-%02d-%02d", y2, m2, d2))
  dt1 <= dt2
}

# ---------------------------
# --- Added: safe_download helper (retry logic)
# This helper will attempt to download a file multiple times, waiting between attempts.
# Replaces direct download.file(...) calls across tabs.
# ---------------------------
safe_download <- function(url, destfile, mode = "wb", quiet = TRUE, retries = 5, wait = 5, log_fun = NULL) {
  # retries: number of attempts
  # wait: seconds to wait between attempts
  # log_fun: optional function(txt) to log messages (e.g., appendLog_pent)
  attempt <- 1
  while (attempt <= retries) {
    ok <- tryCatch({
      # Use download.file; quiet parameter forwarded for consistency
      download.file(url, destfile, mode = mode, quiet = quiet)
      TRUE
    }, error = function(e) {
      if (!is.null(log_fun)) log_fun(paste0("Attempt ", attempt, " failed for ", basename(destfile), " - ", conditionMessage(e)))
      FALSE
    })
    # Check file existence and non-zero size
    if (isTRUE(ok) && file.exists(destfile) && file.info(destfile)$size > 0) {
      if (!is.null(log_fun) && attempt > 1) log_fun(paste0("Succeeded on attempt ", attempt, ": ", basename(destfile)))
      return(TRUE)
    }
    # If file is empty or missing after download, remove it before retrying
    if (file.exists(destfile) && file.info(destfile)$size == 0) {
      try({ file.remove(destfile) }, silent = TRUE)
      if (!is.null(log_fun)) log_fun(paste0("Attempt ", attempt, " produced empty file for ", basename(destfile)))
    }
    if (attempt < retries) {
      if (!is.null(log_fun)) log_fun(paste0("Retrying in ", wait, "s... (", attempt, "/", retries, ")"))
      Sys.sleep(wait)
    }
    attempt <- attempt + 1
  }
  if (!is.null(log_fun)) log_fun(paste0("All ", retries, " attempts failed for ", basename(destfile)))
  return(FALSE)
}
# ---------------------------
# End safe_download helper
# ---------------------------

# ---------------------------
# UI - Tabs for the five apps
# ---------------------------
ui <- fluidPage(
  titlePanel("CHIRPS, CHIRP & CHIRTS TIF Downloaders"),
  tabsetPanel(
    # Tab 1: CHIRPS Pentadal / Dekadal / Monthly
    tabPanel(
      title = "CHIRPS v3.0 Pentadal / Dekadal / Monthly Downloader",
      sidebarLayout(
        sidebarPanel(
          h4("Open Dataset Websites"),
          actionButton("open_url_pent", "Open Pentadal Dataset Website"),
          actionButton("open_url_dek", "Open Dekadal Dataset Website"),
          actionButton("open_url_month", "Open Monthly Dataset Website"),
          br(), br(),
          selectInput("freq_pent", "Select Frequency:", choices = c("Pentadal", "Dekadal", "Monthly")),
          numericInput("start_year_pent", "Start Year:", value = 2025, min = 1981, max = 2100),
          numericInput("start_month_pent", "Start Month:", value = 1, min = 1, max = 12),
          conditionalPanel(
            condition = "input.freq_pent == 'Pentadal'",
            numericInput("start_pentad_pent", "Start Pentad (1-6):", value = 1, min = 1, max = 6)
          ),
          conditionalPanel(
            condition = "input.freq_pent == 'Dekadal'",
            numericInput("start_dekad_pent", "Start Dekad (1-3):", value = 1, min = 1, max = 3)
          ),
          numericInput("end_year_pent", "End Year:", value = 2025, min = 1981, max = 2100),
          numericInput("end_month_pent", "End Month:", value = 1, min = 1, max = 12),
          conditionalPanel(
            condition = "input.freq_pent == 'Pentadal'",
            numericInput("end_pentad_pent", "End Pentad (1-6):", value = 1, min = 1, max = 6)
          ),
          conditionalPanel(
            condition = "input.freq_pent == 'Dekadal'",
            numericInput("end_dekad_pent", "End Dekad (1-3):", value = 1, min = 1, max = 3)
          ),
          uiOutput("validation_error_ui_pent"),
          textInput("outdir_pent", "Output Directory:",
                    value = "C:/Users/gaded/Documents/fews_tools_WS/ProgramSettings/Data/Climate/"),
          radioButtons("overwrite_pent", "If file exists:", choices = c("Skip", "Overwrite"), selected = "Overwrite"),
          actionButton("download_pent", "Start Download")
        ),
        mainPanel(
          verbatimTextOutput("log_pent")
        )
      )
    ),
    
    # Tab 2: CHIRPS Daily
    tabPanel(
      title = "CHIRPS v3.0 Daily Downloader",
      sidebarLayout(
        sidebarPanel(
          h4("Open Dataset Websites"),
          actionButton("open_url_daily", "Open CHIRPS V3.0 Daily Dataset", style="margin-bottom:12px;"),
          numericInput("start_year_daily", "Start Year:", value = 2025, min = 1998, max = 2050),
          numericInput("start_month_daily", "Start Month:", value = 1, min = 1, max = 12),
          numericInput("start_day_daily", "Start Day:", value = 1, min = 1, max = 31),
          numericInput("end_year_daily", "End Year:", value = 2025, min = 1998, max = 2050),
          numericInput("end_month_daily", "End Month:", value = 1, min = 1, max = 12),
          numericInput("end_day_daily", "End Day:", value = 1, min = 1, max = 31),
          uiOutput("validation_error_ui_daily"),
          textInput("outdir_daily", "Output Directory:",
                    value = "C:/Users/gaded/Documents/fews_tools_WS/ProgramSettings/Data/Climate/"),
          radioButtons("overwrite_daily", "If file exists:", choices = c("Skip", "Overwrite"), selected = "Overwrite"),
          actionButton("download_daily", "Start Download")
        ),
        mainPanel(
          verbatimTextOutput("log_daily")
        )
      )
    ),
    
    # Tab 3: CHIRTS-ERA5 Tmax/Tmin (monthly/pentadal) - existing
    tabPanel(
      title = "CHIRTS-ERA5 Tmax/Tmin Downloader",
      sidebarLayout(
        sidebarPanel(
          h4("Open Dataset Websites"),
          actionButton("open_tmax_pent", "Open Tmax Pentadal Dataset Website"),
          actionButton("open_tmax_mon", "Open Tmax Monthly Dataset Website"),
          actionButton("open_tmin_pent", "Open Tmin Pentadal Dataset Website"),
          actionButton("open_tmin_mon", "Open Tmin Monthly Dataset Website"),
          hr(),
          selectInput("chirts_var", "Variable:", choices = c("Tmax", "Tmin")),
          selectInput("chirts_freq", "Frequency:", choices = c("Pentadal", "Monthly")),
          numericInput("chirts_start_year", "Start Year:", value = 2025, min = 1980, max = 2100),
          numericInput("chirts_start_month", "Start Month:", value = 1, min = 1, max = 12),
          conditionalPanel(
            condition = "input.chirts_freq == 'Pentadal'",
            numericInput("chirts_start_pentad", "Start Pentad (1-6):", value = 1, min = 1, max = 6)
          ),
          numericInput("chirts_end_year", "End Year:", value = 2025, min = 1980, max = 2100),
          numericInput("chirts_end_month", "End Month:", value = 1, min = 1, max = 12),
          conditionalPanel(
            condition = "input.chirts_freq == 'Pentadal'",
            numericInput("chirts_end_pentad", "End Pentad (1-6):", value = 6, min = 1, max = 6)
          ),
          uiOutput("validation_error_ui_chirts"),
          textInput("chirts_outdir", "Output Directory:",
                    value = "C:/Users/gaded/Documents/fews_tools_WS/ProgramSettings/Data/Climate/"),
          radioButtons("chirts_overwrite", "If file exists:", choices = c("Skip", "Overwrite"), selected = "Overwrite"),
          actionButton("chirts_download", "Start Download")
        ),
        mainPanel(
          verbatimTextOutput("log_chirts")
        )
      )
    ),
    
    # Tab 4: CHIRTS-ERA5 Daily Tmin/Tmax Downloader (new tab, keep UI & messages same)
    tabPanel(
      title = "CHIRTS-ERA5 Daily Tmin/Tmax Downloader",
      sidebarLayout(
        sidebarPanel(
          h4("Open Dataset Websites"),
          actionButton("tab4_open_url_tmax", "Open CHIRTS-ERA5 Tmax Daily Dataset", style="margin-bottom:12px;"),
          actionButton("tab4_open_url_tmin", "Open CHIRTS-ERA5 Tmin Daily Dataset", style="margin-bottom:12px;"),
          selectInput("tab4_var", "Variable:", choices = c("Tmax", "Tmin")),
          numericInput("tab4_start_year", "Start Year:", value = 2025, min = 1980, max = 2050),
          numericInput("tab4_start_month", "Start Month:", value = 1, min = 1, max = 12),
          numericInput("tab4_start_day", "Start Day:", value = 1, min = 1, max = 31),
          numericInput("tab4_end_year", "End Year:", value = 2025, min = 1980, max = 2050),
          numericInput("tab4_end_month", "End Month:", value = 1, min = 1, max = 12),
          numericInput("tab4_end_day", "End Day:", value = 1, min = 1, max = 31),
          uiOutput("tab4_validation_error_ui"),
          textInput("tab4_outdir", "Output Directory:",
                    value = "C:/Users/gaded/Documents/fews_tools_WS/ProgramSettings/Data/Climate/"),
          radioButtons("tab4_overwrite", "If file exists:", choices = c("Skip", "Overwrite"), selected = "Overwrite"),
          actionButton("tab4_download", "Start Download")
        ),
        mainPanel(
          verbatimTextOutput("tab4_log")
        )
      )
    ),
    
    # Tab 5: CHIRP v3 TIF Downloader (new tab, keep UI & messages same)
    tabPanel(
      title = "CHIRP v3.0 Downloader",
      sidebarLayout(
        sidebarPanel(
          h4("Open Dataset Websites"),
          actionButton("tab5_open_url_pent", "Open Pentadal Dataset Website"),
          actionButton("tab5_open_url_dek", "Open Dekadal Dataset Website"),
          actionButton("tab5_open_url_month", "Open Monthly Dataset Website"),
          br(), br(),
          selectInput("tab5_freq", "Select Frequency:", choices = c("Pentadal", "Dekadal", "Monthly")),
          numericInput("tab5_start_year", "Start Year:", value = 2025, min = 1981, max = 2100),
          numericInput("tab5_start_month", "Start Month:", value = 1, min = 1, max = 12),
          conditionalPanel(
            condition = "input.tab5_freq == 'Pentadal'",
            numericInput("tab5_start_pentad", "Start Pentad (1-6):", value = 1, min = 1, max = 6)
          ),
          conditionalPanel(
            condition = "input.tab5_freq == 'Dekadal'",
            numericInput("tab5_start_dekad", "Start Dekad (1-3):", value = 1, min = 1, max = 3)
          ),
          numericInput("tab5_end_year", "End Year:", value = 2025, min = 1981, max = 2100),
          numericInput("tab5_end_month", "End Month:", value = 1, min = 1, max = 12),
          conditionalPanel(
            condition = "input.tab5_freq == 'Pentadal'",
            numericInput("tab5_end_pentad", "End Pentad (1-6):", value = 1, min = 1, max = 6)
          ),
          conditionalPanel(
            condition = "input.tab5_freq == 'Dekadal'",
            numericInput("tab5_end_dekad", "End Dekad (1-3):", value = 1, min = 1, max = 3)
          ),
          uiOutput("tab5_validation_error_ui"),
          textInput("tab5_outdir", "Output Directory:",
                    value = "C:/Users/gaded/Documents/fews_tools_WS/ProgramSettings/Data/Climate/"),
          radioButtons("tab5_overwrite", "If file exists:", choices = c("Skip", "Overwrite"), selected = "Overwrite"),
          actionButton("tab5_download", "Start Download")
        ),
        mainPanel(
          verbatimTextOutput("tab5_log")
        )
      )
    )
    
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # ---------------------------
  # Tab 1 logic: CHIRPS V3 TIF Downloader (Pentadal/Dekadal/Monthly)
  # ---------------------------
  logtext_pent <- reactiveVal("")
  appendLog_pent <- function(txt) {
    logtext_pent(paste0(logtext_pent(), if (nzchar(logtext_pent())) "\n" else "", txt))
    flush.console()
    Sys.sleep(0.01)
  }
  output$log_pent <- renderText({ logtext_pent() })
  validation_msg_pent <- reactiveVal(NULL)
  output$validation_error_ui_pent <- renderUI({
    msg <- validation_msg_pent()
    if (is.null(msg)) return(NULL)
    tags$div(
      style = "color: white; background-color: #c0392b; padding: 8px; border-radius: 4px; margin-bottom: 8px;",
      tags$strong("Error: "), tags$span(msg)
    )
  })
  observeEvent(input$open_url_pent, { tryCatch({ browseURL("https://data.chc.ucsb.edu/products/CHIRPS/v3.0/pentads/africa/tifs/") }, error = function(e) { showNotification("Cannot open browser from Shiny server.", type = "error") }) })
  observeEvent(input$open_url_dek, { tryCatch({ browseURL("https://data.chc.ucsb.edu/products/CHIRPS/v3.0/dekads/africa/tifs/") }, error = function(e) { showNotification("Cannot open browser from Shiny server.", type = "error") }) })
  observeEvent(input$open_url_month, { tryCatch({ browseURL("https://data.chc.ucsb.edu/products/CHIRPS/v3.0/monthly/africa/tifs/") }, error = function(e) { showNotification("Cannot open browser from Shiny server.", type = "error") }) })
  
  validate_inputs_pent <- function() {
    cur_year <- as.integer(format(Sys.Date(), "%Y"))
    req_fields <- list(
      start_year = input$start_year_pent,
      start_month = input$start_month_pent,
      end_year = input$end_year_pent,
      end_month = input$end_month_pent
    )
    if (input$freq_pent == "Pentadal") {
      req_fields$start_pentad <- input$start_pentad_pent
      req_fields$end_pentad <- input$end_pentad_pent
    }
    if (input$freq_pent == "Dekadal") {
      req_fields$start_dekad <- input$start_dekad_pent
      req_fields$end_dekad <- input$end_dekad_pent
    }
    for (nm in names(req_fields)) {
      val <- req_fields[[nm]]
      if (is.null(val) || is.na(val) || identical(val, "")) return("All date fields are required.")
    }
    if (input$start_year_pent < 1981 || input$end_year_pent < 1981) return("Year must be >= 1981.")
    if (input$start_year_pent > cur_year || input$end_year_pent > cur_year) return("Years cannot be in the future.")
    if (!(input$start_month_pent %in% 1:12)) return("Start month must be 1-12.")
    if (!(input$end_month_pent %in% 1:12)) return("End month must be 1-12.")
    if (input$freq_pent == "Pentadal" && (!(input$start_pentad_pent %in% 1:6) || !(input$end_pentad_pent %in% 1:6))) return("Pentads must be 1-6.")
    if (input$freq_pent == "Dekadal" && (!(input$start_dekad_pent %in% 1:3) || !(input$end_dekad_pent %in% 1:3))) return("Dekads must be 1-3.")
    
    start_tuple <- c(input$start_year_pent, input$start_month_pent,
                     ifelse(input$freq_pent=="Pentadal", input$start_pentad_pent,
                            ifelse(input$freq_pent=="Dekadal", input$start_dekad_pent, 0)))
    end_tuple <- c(input$end_year_pent, input$end_month_pent,
                   ifelse(input$freq_pent=="Pentadal", input$end_pentad_pent,
                          ifelse(input$freq_pent=="Dekadal", input$end_dekad_pent, 0)))
    
    if ((start_tuple[1] > end_tuple[1]) ||
        (start_tuple[1]==end_tuple[1] && start_tuple[2]>end_tuple[2]) ||
        (start_tuple[1]==end_tuple[1] && start_tuple[2]==end_tuple[2] && start_tuple[3]>end_tuple[3])) {
      return("Start date must be earlier than or equal to end date.")
    }
    
    if (is.null(input$outdir_pent) || input$outdir_pent == "" || !dir.exists(input$outdir_pent)) {
      return("Output directory does not exist. Please create it before downloading.")
    }
    
    return(NULL)
  }
  
  observeEvent(input$download_pent, {
    msg <- validate_inputs_pent()
    if (!is.null(msg)) { validation_msg_pent(msg); return() } else validation_msg_pent(NULL)
    outdir <- input$outdir_pent
    if (is.null(outdir) || outdir == "" || !dir.exists(outdir)) {
      validation_msg_pent("Output directory does not exist. Please specify a valid folder before downloading.")
      return()
    } else {
      validation_msg_pent(NULL)
    }
    freq <- input$freq_pent
    files <- list()
    if (freq == "Pentadal") {
      cur <- list(year=input$start_year_pent, month=input$start_month_pent, pentad=input$start_pentad_pent)
      end <- list(year=input$end_year_pent, month=input$end_month_pent, pentad=input$end_pentad_pent)
      base_url <- "https://data.chc.ucsb.edu/products/CHIRPS/v3.0/pentads/africa/tifs/"
      while ((cur$year < end$year) ||
             (cur$year == end$year & cur$month < end$month) ||
             (cur$year == end$year & cur$month == end$month & cur$pentad <= end$pentad)) {
        fname <- sprintf("chirps-v3.0.%d.%02d.%d.tif", cur$year, cur$month, cur$pentad)
        files[[fname]] <- paste0(base_url, fname)
        cur <- next_pentad(cur$year, cur$month, cur$pentad)
      }
    } else if (freq == "Dekadal") {
      cur <- list(year=input$start_year_pent, month=input$start_month_pent, dekad=input$start_dekad_pent)
      end <- list(year=input$end_year_pent, month=input$end_month_pent, dekad=input$end_dekad_pent)
      base_url <- "https://data.chc.ucsb.edu/products/CHIRPS/v3.0/dekads/africa/tifs/"
      while ((cur$year < end$year) ||
             (cur$year == end$year & cur$month < end$month) ||
             (cur$year == end$year & cur$month == end$month & cur$dekad <= end$dekad)) {
        fname <- sprintf("chirps-v3.0.%d.%02d.%d.tif", cur$year, cur$month, cur$dekad)
        files[[fname]] <- paste0(base_url, fname)
        cur <- next_dekad(cur$year, cur$month, cur$dekad)
      }
    } else {
      cur <- list(year=input$start_year_pent, month=input$start_month_pent)
      end <- list(year=input$end_year_pent, month=input$end_month_pent)
      base_url <- "https://data.chc.ucsb.edu/products/CHIRPS/v3.0/monthly/africa/tifs/"
      while ((cur$year < end$year) || (cur$year == end$year & cur$month <= end$month)) {
        fname <- sprintf("chirps-v3.0.%d.%02d.tif", cur$year, cur$month)
        files[[fname]] <- paste0(base_url, fname)
        cur <- next_month(cur$year, cur$month)
      }
    }
    downloaded <- 0; skipped <- 0; failed <- 0
    n <- length(files)
    withProgress(message="Downloading CHIRPS data...", value=0, {
      if(n==0) { appendLog_pent("No files to download."); return() }
      for(i in seq_along(files)) {
        fname <- names(files)[i]
        destfile <- file.path(outdir, fname)
        action <- NULL
        if(file.exists(destfile)) {
          if(input$overwrite_pent=="Skip") action <- "skipped" else action <- "download"
        } else action <- "download"
        if (action=="download") {
          # --- Modified: use safe_download instead of download.file ---
          success <- tryCatch({
            safe_download(files[[fname]], destfile, mode="wb", quiet=TRUE, retries = 8, wait = 10, log_fun = appendLog_pent)
            TRUE
          }, error=function(e){
            appendLog_pent(paste("Failed:", fname, "-", conditionMessage(e)))
            FALSE
          })
          # verify file exists and non-zero (safe_download already checks, but keep parity)
          if (isTRUE(success) && (!file.exists(destfile) || file.info(destfile)$size==0)) {
            success <- FALSE
            appendLog_pent(paste("Failed (empty/missing):",fname))
          }
          if (success) {
            appendLog_pent(paste("Downloaded:", fname))
            downloaded <- downloaded + 1
          } else {
            failed <- failed + 1
          }
        } else if (action=="skipped") {
          appendLog_pent(paste("Skipped:", fname))
          skipped <- skipped + 1
        }
        incProgress(1/max(1,n), detail = paste(action, ":", fname))
        Sys.sleep(0.01)
      }
    })
    appendLog_pent(paste0("Job summary: ", downloaded, " files downloaded, ", skipped, " files skipped, ", failed, " files failed."))
  })
  
  # ---------------------------
  # Tab 2 logic: CHIRPS Daily Downloader (ERA5)
  # ---------------------------
  base_url_daily <- "https://data.chc.ucsb.edu/products/CHIRPS/v3.0/daily/final/sat/"
  logtext_daily <- reactiveVal("")
  appendLog_daily <- function(txt) {
    logtext_daily(paste0(logtext_daily(), if (nzchar(logtext_daily())) "\n" else "", txt))
    flush.console()
    Sys.sleep(0.01)
  }
  output$log_daily <- renderText({ logtext_daily() })
  validation_msg_daily <- reactiveVal(NULL)
  output$validation_error_ui_daily <- renderUI({
    msg <- validation_msg_daily()
    if (is.null(msg)) return(NULL)
    tags$div(style="color:white;background-color:#c0392b;padding:8px;border-radius:4px;margin-bottom:8px;",
             tags$strong("Error: "), tags$span(msg))
  })
  observeEvent(input$open_url_daily, { tryCatch({ browseURL(base_url_daily) }, error=function(e){ showNotification("Cannot open browser from Shiny server.", type="error") }) })
  validate_inputs_daily <- function() {
    req_fields <- list(
      start_year=input$start_year_daily, start_month=input$start_month_daily, start_day=input$start_day_daily,
      end_year=input$end_year_daily, end_month=input$end_month_daily, end_day=input$end_day_daily
    )
    for (nm in names(req_fields)) {
      val <- req_fields[[nm]]
      if (is.null(val) || is.na(val) || identical(val,"") ) return("All date fields are required.")
    }
    start_date <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", input$start_year_daily,input$start_month_daily,input$start_day_daily)), error=function(e) NA)
    end_date <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", input$end_year_daily,input$end_month_daily,input$end_day_daily)), error=function(e) NA)
    if (is.na(start_date)) return("Start date is invalid.")
    if (is.na(end_date)) return("End date is invalid.")
    if (start_date < as.Date("1998-01-01")) return("Start date cannot be earlier than 1998-01-01.")
    if (start_date > end_date) return("Start date must be earlier than or equal to end date.")
    if (end_date > Sys.Date()) return("Years cannot be in the future.")
    if (!dir.exists(input$outdir_daily)) return("Output folder does not exist. Please create it before downloading.")
    return(NULL)
  }
  observeEvent(input$download_daily, {
    msg <- validate_inputs_daily()
    if (!is.null(msg)) { validation_msg_daily(msg); return() } else validation_msg_daily(NULL)
    outdir <- input$outdir_daily
    overwrite_mode <- input$overwrite_daily
    files <- list()
    cur <- list(year=input$start_year_daily, month=input$start_month_daily, day=input$start_day_daily)
    end <- list(year=input$end_year_daily, month=input$end_month_daily, day=input$end_day_daily)
    while (is_before_or_equal(cur$year, cur$month, cur$day, end$year, end$month, end$day)) {
      fname <- sprintf("chirps-v3.0.sat.%04d.%02d.%02d.tif", cur$year, cur$month, cur$day)
      url <- paste0(base_url_daily, cur$year, "/", fname)
      files[[fname]] <- url
      cur <- next_day(cur$year, cur$month, cur$day)
    }
    downloaded <- 0; skipped <- 0; failed <- 0; n <- length(files)
    withProgress(message="Downloading CHIRPS daily files...", value=0, {
      if (n==0) { appendLog_daily("No files to download."); return() }
      for (i in seq_along(files)) {
        fname <- names(files)[i]; url <- files[[fname]]; destfile <- file.path(outdir,fname)
        action <- if(file.exists(destfile) && overwrite_mode=="Skip") "skipped" else "download"
        if (action=="download") {
          # --- Modified: use safe_download instead of download.file ---
          success <- tryCatch({
            safe_download(url, destfile, mode="wb", quiet=TRUE, retries = 8, wait = 10, log_fun = appendLog_daily)
            TRUE
          }, error=function(e){ appendLog_daily(paste("Failed:",fname,"-",conditionMessage(e))); FALSE })
          if (isTRUE(success) && (!file.exists(destfile) || file.info(destfile)$size==0)) {
            success <- FALSE
            appendLog_daily(paste("Failed (empty/missing):",fname))
          }
          if (success) { appendLog_daily(paste("Downloaded:",fname)); downloaded <- downloaded+1 }
          else failed <- failed+1
        } else { appendLog_daily(paste("Skipped:",fname)); skipped <- skipped+1 }
        incProgress(1/max(1,n), detail=paste(action,":",fname)); Sys.sleep(0.01)
      }
    })
    appendLog_daily(paste0("Job summary: ",downloaded," files downloaded, ",skipped," files skipped, ",failed," files failed."))
  })
  
  # ---------------------------
  # Tab 3 logic: CHIRTS-ERA5 Tmax/Tmin Downloader (monthly/pentadal)
  # ---------------------------
  observeEvent(input$open_tmax_pent, { browseURL("https://data.chc.ucsb.edu/experimental/CHIRTS-ERA5/tmax/tifs/pentads/") })
  observeEvent(input$open_tmax_mon,  { browseURL("https://data.chc.ucsb.edu/experimental/CHIRTS-ERA5/tmax/tifs/monthly/") })
  observeEvent(input$open_tmin_pent, { browseURL("https://data.chc.ucsb.edu/experimental/CHIRTS-ERA5/tmin/tifs/pentads/") })
  observeEvent(input$open_tmin_mon,  { browseURL("https://data.chc.ucsb.edu/experimental/CHIRTS-ERA5/tmin/tifs/monthly/") })
  
  logtext_chirts <- reactiveVal("")
  appendLog_chirts <- function(txt) {
    logtext_chirts(paste0(logtext_chirts(), if (nzchar(logtext_chirts())) "\n" else "", txt))
    flush.console()
    Sys.sleep(0.01)
  }
  output$log_chirts <- renderText({ logtext_chirts() })
  
  validation_msg_chirts <- reactiveVal(NULL)
  output$validation_error_ui_chirts <- renderUI({
    msg <- validation_msg_chirts()
    if (is.null(msg)) return(NULL)
    tags$div(style = "color:white;background-color:#c0392b;padding:8px;border-radius:4px;margin-bottom:8px;",
             tags$strong("Error: "), tags$span(msg))
  })
  
  validate_inputs_chirts <- function() {
    cur_year <- as.integer(format(Sys.Date(), "%Y"))
    req_fields <- list(
      start_year = input$chirts_start_year,
      start_month = input$chirts_start_month,
      end_year = input$chirts_end_year,
      end_month = input$chirts_end_month
    )
    if (input$chirts_freq == "Pentadal") {
      req_fields$start_pentad <- input$chirts_start_pentad
      req_fields$end_pentad <- input$chirts_end_pentad
    }
    for (nm in names(req_fields)) {
      val <- req_fields[[nm]]
      if (is.null(val) || is.na(val) || identical(val, "")) return("All date fields are required.")
    }
    if (input$chirts_start_year < 1980 || input$chirts_end_year < 1980) return("Year must be >= 1980.")
    if (input$chirts_start_year > cur_year || input$chirts_end_year > cur_year) return("Years cannot be in the future.")
    if (!(input$chirts_start_month %in% 1:12)) return("Start month must be 1-12.")
    if (!(input$chirts_end_month %in% 1:12)) return("End month must be 1-12.")
    if (input$chirts_freq == "Pentadal") {
      if (!(input$chirts_start_pentad %in% 1:6)) return("Start pentad must be 1-6.")
      if (!(input$chirts_end_pentad %in% 1:6)) return("End pentad must be 1-6.")
    }
    start_tuple <- c(input$chirts_start_year, input$chirts_start_month, ifelse(input$chirts_freq=="Pentadal", input$chirts_start_pentad, 0))
    end_tuple <- c(input$chirts_end_year, input$chirts_end_month, ifelse(input$chirts_freq=="Pentadal", input$chirts_end_pentad, 0))
    if ((start_tuple[1] > end_tuple[1]) ||
        (start_tuple[1]==end_tuple[1] && start_tuple[2]>end_tuple[2]) ||
        (start_tuple[1]==end_tuple[1] && start_tuple[2]==end_tuple[2] && start_tuple[3]>end_tuple[3])) {
      return("Start date must be earlier than or equal to end date.")
    }
    return(NULL)
  }
  
  observeEvent(input$chirts_download, {
    msg <- validate_inputs_chirts()
    if (!is.null(msg)) { validation_msg_chirts(msg); return() } else validation_msg_chirts(NULL)
    
    outdir <- input$chirts_outdir
    if (is.null(outdir) || outdir == "" || !dir.exists(outdir)) {
      validation_msg_chirts("Output directory does not exist. Please specify a valid folder before downloading.")
      return()
    } else {
      validation_msg_chirts(NULL)
    }
    
    freq <- input$chirts_freq
    var <- input$chirts_var
    overwrite_mode <- input$chirts_overwrite
    files <- list()
    
    if (freq == "Pentadal") {
      cur <- list(year=input$chirts_start_year, month=input$chirts_start_month, pentad=input$chirts_start_pentad)
      end <- list(year=input$chirts_end_year, month=input$chirts_end_month, pentad=input$chirts_end_pentad)
      base_url <- paste0("https://data.chc.ucsb.edu/experimental/CHIRTS-ERA5/", tolower(var), "/tifs/pentads/")
      while ((cur$year < end$year) ||
             (cur$year==end$year && cur$month<end$month) ||
             (cur$year==end$year && cur$month==end$month && cur$pentad <= end$pentad)) {
        fname <- sprintf("CHIRTS-ERA5.pentads_%s.%d.%02d.%d.tif", var, cur$year, cur$month, cur$pentad)
        files[[fname]] <- paste0(base_url, fname)
        cur <- next_pentad(cur$year, cur$month, cur$pentad)
      }
    } else {
      cur <- list(year=input$chirts_start_year, month=input$chirts_start_month)
      end <- list(year=input$chirts_end_year, month=input$chirts_end_month)
      base_url <- paste0("https://data.chc.ucsb.edu/experimental/CHIRTS-ERA5/", tolower(var), "/tifs/monthly/")
      while ((cur$year < end$year) || (cur$year==end$year && cur$month <= end$month)) {
        fname <- sprintf("CHIRTS-ERA5.monthly_%s.%d.%02d.tif", var, cur$year, cur$month)
        files[[fname]] <- paste0(base_url, fname)
        cur <- next_month(cur$year, cur$month)
      }
    }
    downloaded <- 0; skipped <- 0; failed <- 0
    n <- length(files)
    withProgress(message = paste("Downloading CHIRTS-ERA5", var, "..."), value = 0, {
      if (n==0) { appendLog_chirts("No files to download."); return() }
      for (i in seq_along(files)) {
        fname <- names(files)[i]
        destfile <- file.path(outdir, fname)
        action <- if (file.exists(destfile)) {
          if (overwrite_mode=="Skip") "skipped" else "download"
        } else "download"
        if (action=="download") {
          # --- Modified: use safe_download instead of download.file ---
          success <- tryCatch({
            safe_download(files[[fname]], destfile, mode="wb", quiet=TRUE, retries = 8, wait = 10, log_fun = appendLog_chirts)
            TRUE
          }, error=function(e){ appendLog_chirts(paste("Failed:", fname, "-", conditionMessage(e))); FALSE })
          if (success) { appendLog_chirts(paste("Downloaded:", fname)); downloaded <- downloaded + 1 } else { failed <- failed + 1 }
        } else { appendLog_chirts(paste("Skipped:", fname)); skipped <- skipped + 1 }
        incProgress(1/max(1,n), detail = paste(action, ":", fname))
        Sys.sleep(0.01)
      }
    })
    appendLog_chirts(paste0("Job summary: ", downloaded, " files downloaded, ", skipped, " files skipped, ", failed, " files failed."))
  })
  
  # ---------------------------
  # Tab 4 logic: CHIRTS-ERA5 Daily Tmin/Tmax Downloader (new tab)
  # ---------------------------
  tab4_base_url_tmax <- "https://data.chc.ucsb.edu/experimental/CHIRTS-ERA5/tmax/tifs/daily/"
  tab4_base_url_tmin <- "https://data.chc.ucsb.edu/experimental/CHIRTS-ERA5/tmin/tifs/daily/"
  
  tab4_logtext <- reactiveVal("")
  appendLog_tab4 <- function(txt) {
    tab4_logtext(paste0(tab4_logtext(), if (nzchar(tab4_logtext())) "\n" else "", txt))
    flush.console()
    Sys.sleep(0.01)
  }
  output$tab4_log <- renderText({ tab4_logtext() })
  
  tab4_validation_msg <- reactiveVal(NULL)
  output$tab4_validation_error_ui <- renderUI({
    msg <- tab4_validation_msg()
    if (is.null(msg)) return(NULL)
    tags$div(style="color:white;background-color:#c0392b;padding:8px;border-radius:4px;margin-bottom:8px;",
             tags$strong("Error: "), tags$span(msg))
  })
  
  observeEvent(input$tab4_open_url_tmax, {
    tryCatch({ browseURL(tab4_base_url_tmax) }, error=function(e){ showNotification("Cannot open browser from Shiny server.", type="error") })
  })
  observeEvent(input$tab4_open_url_tmin, {
    tryCatch({ browseURL(tab4_base_url_tmin) }, error=function(e){ showNotification("Cannot open browser from Shiny server.", type="error") })
  })
  
  validate_inputs_tab4 <- function() {
    req_fields <- list(
      start_year=input$tab4_start_year, start_month=input$tab4_start_month, start_day=input$tab4_start_day,
      end_year=input$tab4_end_year, end_month=input$tab4_end_month, end_day=input$tab4_end_day
    )
    for (nm in names(req_fields)) {
      val <- req_fields[[nm]]
      if (is.null(val) || is.na(val) || identical(val,"")) return("All date fields are required.")
    }
    start_date <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", input$tab4_start_year,input$tab4_start_month,input$tab4_start_day)), error=function(e) NA)
    end_date <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", input$tab4_end_year,input$tab4_end_month,input$tab4_end_day)), error=function(e) NA)
    if (is.na(start_date)) return("Start date is invalid.")
    if (is.na(end_date)) return("End date is invalid.")
    if (start_date < as.Date("1980-01-01")) return("Start date cannot be earlier than 1980-01-01.")
    if (start_date > end_date) return("Start date must be earlier than or equal to end date.")
    if (end_date > Sys.Date()) return("Years cannot be in the future.")
    if (!dir.exists(input$tab4_outdir)) return("Output folder does not exist. Please create it before downloading.")
    return(NULL)
  }
  
  observeEvent(input$tab4_download, {
    msg <- validate_inputs_tab4()
    if (!is.null(msg)) { tab4_validation_msg(msg); return() } else tab4_validation_msg(NULL)
    
    outdir <- input$tab4_outdir
    overwrite_mode <- input$tab4_overwrite
    var <- input$tab4_var
    base_url <- if (var == "Tmax") tab4_base_url_tmax else tab4_base_url_tmin
    
    files <- list()
    cur <- list(year=input$tab4_start_year, month=input$tab4_start_month, day=input$tab4_start_day)
    end <- list(year=input$tab4_end_year, month=input$tab4_end_month, day=input$tab4_end_day)
    
    while (is_before_or_equal(cur$year, cur$month, cur$day, end$year, end$month, end$day)) {
      fname <- sprintf("CHIRTS-ERA5.daily_%s.%04d.%02d.%02d.tif", var, cur$year, cur$month, cur$day)
      url <- paste0(base_url, cur$year, "/", fname)
      files[[fname]] <- url
      cur <- next_day(cur$year, cur$month, cur$day)
    }
    
    downloaded <- 0; skipped <- 0; failed <- 0; n <- length(files)
    withProgress(message=paste("Downloading CHIRTS-ERA5", var, "files..."), value=0, {
      if (n==0) { appendLog_tab4("No files to download."); return() }
      for (i in seq_along(files)) {
        fname <- names(files)[i]; url <- files[[fname]]; destfile <- file.path(outdir,fname)
        action <- if(file.exists(destfile) && overwrite_mode=="Skip") "skipped" else "download"
        if (action=="download") {
          # --- Modified: use safe_download instead of download.file ---
          success <- tryCatch({
            safe_download(url, destfile, mode="wb", quiet=TRUE, retries = 8, wait = 10, log_fun = appendLog_tab4)
            TRUE
          }, error=function(e){ appendLog_tab4(paste("Failed:",fname,"-",conditionMessage(e))); FALSE })
          if (isTRUE(success) && (!file.exists(destfile) || file.info(destfile)$size==0)) {
            success <- FALSE
            appendLog_tab4(paste("Failed (empty/missing):",fname))
          }
          if (success) { appendLog_tab4(paste("Downloaded:",fname)); downloaded <- downloaded+1 } else failed <- failed+1
        } else { appendLog_tab4(paste("Skipped:",fname)); skipped <- skipped+1 }
        incProgress(1/max(1,n), detail=paste(action,":",fname)); Sys.sleep(0.01)
      }
    })
    appendLog_tab4(paste0("Job summary: ",downloaded," files downloaded, ",skipped," files skipped, ",failed," files failed."))
  })
  
  # ---------------------------
  # Tab 5 logic: CHIRP v3 TIF Downloader (new tab)
  # ---------------------------
  tab5_logtext <- reactiveVal("")
  appendLog_tab5 <- function(txt) {
    tab5_logtext(paste0(tab5_logtext(), if (nzchar(tab5_logtext())) "\n" else "", txt))
    flush.console()
    Sys.sleep(0.01)
  }
  output$tab5_log <- renderText({ tab5_logtext() })
  
  tab5_validation_msg <- reactiveVal(NULL)
  output$tab5_validation_error_ui <- renderUI({
    msg <- tab5_validation_msg()
    if (is.null(msg)) return(NULL)
    tags$div(
      style = "color: white; background-color: #c0392b; padding: 8px; border-radius: 4px; margin-bottom: 8px;",
      tags$strong("Error: "), tags$span(msg)
    )
  })
  
  observeEvent(input$tab5_open_url_pent, {
    browseURL("https://data.chc.ucsb.edu/products/CHIRP-v3.0/pentads/global/tifs/")
  })
  observeEvent(input$tab5_open_url_dek, {
    browseURL("https://data.chc.ucsb.edu/products/CHIRP-v3.0/dekads/global/tifs/")
  })
  observeEvent(input$tab5_open_url_month, {
    browseURL("https://data.chc.ucsb.edu/products/CHIRP-v3.0/monthly/global/tifs/")
  })
  
  validate_inputs_tab5 <- function() {
    cur_year <- as.integer(format(Sys.Date(), "%Y"))
    req_fields <- list(
      start_year = input$tab5_start_year,
      start_month = input$tab5_start_month,
      end_year = input$tab5_end_year,
      end_month = input$tab5_end_month
    )
    if (input$tab5_freq == "Pentadal") {
      req_fields$start_pentad <- input$tab5_start_pentad
      req_fields$end_pentad <- input$tab5_end_pentad
    }
    if (input$tab5_freq == "Dekadal") {
      req_fields$start_dekad <- input$tab5_start_dekad
      req_fields$end_dekad <- input$tab5_end_dekad
    }
    for (nm in names(req_fields)) {
      val <- req_fields[[nm]]
      if (is.null(val) || is.na(val) || identical(val, "")) return("All date fields are required.")
    }
    if (input$tab5_start_year < 1981 || input$tab5_end_year < 1981) return("Year must be >= 1981.")
    if (input$tab5_start_year > cur_year || input$tab5_end_year > cur_year) return("Years cannot be in the future.")
    if (!(input$tab5_start_month %in% 1:12)) return("Start month must be 1-12.")
    if (!(input$tab5_end_month %in% 1:12)) return("End month must be 1-12.")
    if (input$tab5_freq == "Pentadal" && (!(input$tab5_start_pentad %in% 1:6) || !(input$tab5_end_pentad %in% 1:6))) return("Pentads must be 1-6.")
    if (input$tab5_freq == "Dekadal" && (!(input$tab5_start_dekad %in% 1:3) || !(input$tab5_end_dekad %in% 1:3))) return("Dekads must be 1-3.")
    
    start_tuple <- c(input$tab5_start_year, input$tab5_start_month,
                     ifelse(input$tab5_freq=="Pentadal", input$tab5_start_pentad,
                            ifelse(input$tab5_freq=="Dekadal", input$tab5_start_dekad, 0)))
    end_tuple <- c(input$tab5_end_year, input$tab5_end_month,
                   ifelse(input$tab5_freq=="Pentadal", input$tab5_end_pentad,
                          ifelse(input$tab5_freq=="Dekadal", input$tab5_end_dekad, 0)))
    
    if ((start_tuple[1] > end_tuple[1]) ||
        (start_tuple[1]==end_tuple[1] && start_tuple[2]>end_tuple[2]) ||
        (start_tuple[1]==end_tuple[1] && start_tuple[2]==end_tuple[2] && start_tuple[3]>end_tuple[3])) {
      return("Start date must be earlier than or equal to end date.")
    }
    return(NULL)
  }
  
  observeEvent(input$tab5_download, {
    msg <- validate_inputs_tab5()
    if (!is.null(msg)) { tab5_validation_msg(msg); return() } else tab5_validation_msg(NULL)
    
    outdir <- input$tab5_outdir
    if (is.null(outdir) || outdir == "" || !dir.exists(outdir)) {
      tab5_validation_msg("Output directory does not exist. Please specify a valid folder before downloading.")
      return()
    } else {
      tab5_validation_msg(NULL)
    }
    
    freq <- input$tab5_freq
    files <- list()
    
    if (freq == "Pentadal") {
      cur <- list(year=input$tab5_start_year, month=input$tab5_start_month, pentad=input$tab5_start_pentad)
      end <- list(year=input$tab5_end_year, month=input$tab5_end_month, pentad=input$tab5_end_pentad)
      base_url <- "https://data.chc.ucsb.edu/products/CHIRP-v3.0/pentads/global/tifs/"
      while ((cur$year < end$year) ||
             (cur$year == end$year & cur$month < end$month) ||
             (cur$year == end$year & cur$month == end$month & cur$pentad <= end$pentad)) {
        fname <- sprintf("chirp-v3.0.%d.%02d.%d.tif", cur$year, cur$month, cur$pentad)
        files[[fname]] <- paste0(base_url, fname)
        cur <- next_pentad(cur$year, cur$month, cur$pentad)
      }
    } else if (freq == "Dekadal") {
      cur <- list(year=input$tab5_start_year, month=input$tab5_start_month, dekad=input$tab5_start_dekad)
      end <- list(year=input$tab5_end_year, month=input$tab5_end_month, dekad=input$tab5_end_dekad)
      base_url <- "https://data.chc.ucsb.edu/products/CHIRP-v3.0/dekads/global/tifs/"
      while ((cur$year < end$year) ||
             (cur$year == end$year & cur$month < end$month) ||
             (cur$year == end$year & cur$month == end$month & cur$dekad <= end$dekad)) {
        fname <- sprintf("chirps-v3.0.%d.%02d.%d.tif", cur$year, cur$month, cur$dekad)
        files[[fname]] <- paste0(base_url, fname)
        cur <- next_dekad(cur$year, cur$month, cur$dekad)
      }
    } else {
      cur <- list(year=input$tab5_start_year, month=input$tab5_start_month)
      end <- list(year=input$tab5_end_year, month=input$tab5_end_month)
      base_url <- "https://data.chc.ucsb.edu/products/CHIRP-v3.0/monthly/global/tifs/"
      while ((cur$year < end$year) || (cur$year == end$year & cur$month <= end$month)) {
        fname <- sprintf("chirps-v3.0.%d.%02d.tif", cur$year, cur$month)
        files[[fname]] <- paste0(base_url, fname)
        cur <- next_month(cur$year, cur$month)
      }
    }
    
    downloaded <- 0; skipped <- 0; failed <- 0
    n <- length(files)
    
    withProgress(message="Downloading CHIRP data...", value=0, {
      if(n==0) { appendLog_tab5("No files to download."); return() }
      for(i in seq_along(files)) {
        fname <- names(files)[i]
        destfile <- file.path(outdir, fname)
        action <- NULL
        if(file.exists(destfile)) {
          if(input$tab5_overwrite=="Skip") action <- "skipped" else action <- "download"
        } else action <- "download"
        if (action=="download") {
          # --- Modified: use safe_download instead of download.file ---
          success <- tryCatch({
            safe_download(files[[fname]], destfile, mode="wb", quiet=TRUE, retries = 8, wait = 10, log_fun = appendLog_tab5)
            TRUE
          }, error=function(e){
            appendLog_tab5(paste("Failed:", fname, "-", conditionMessage(e)))
            FALSE
          })
          if (success) {
            appendLog_tab5(paste("Downloaded:", fname))
            downloaded <- downloaded + 1
          } else {
            failed <- failed + 1
          }
        } else if (action=="skipped") {
          appendLog_tab5(paste("Skipped:", fname))
          skipped <- skipped + 1
        }
        incProgress(1/n, detail = paste(action, ":", fname))
        Sys.sleep(0.01)
      }
    })
    appendLog_tab5(paste0("Job summary: ", downloaded, " files downloaded, ", skipped, " files skipped, ", failed, " files failed."))
  })
  
}

# Run the integrated app
shinyApp(ui, server)
