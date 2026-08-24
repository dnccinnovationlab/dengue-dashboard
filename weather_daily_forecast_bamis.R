# ============================================================
# BAMIS WRF 1-7 DAY FORECAST SCRAPER
# ============================================================
#
# Source:
# https://www.bamis.gov.bd/en/bmd/wrf/table/all/1
# ...
# https://www.bamis.gov.bd/en/bmd/wrf/table/all/7
#
# Purpose:
#   - Download BAMIS WRF forecasts for horizons 1-7
#   - Extract Dhaka only
#   - Calculate individual daily forecast values
#   - Rainfall = daily total
#   - Temperature/Humidity/Wind speed = daily values
#   - Append forecast history to Google Sheets
#
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "rvest",
  "dplyr",
  "stringr",
  "lubridate",
  "googlesheets4",
  "httr"
)


# Install missing packages when running locally/GitHub
installed <- rownames(
  installed.packages()
)

for (pkg in required_packages) {

  if (!(pkg %in% installed)) {

    install.packages(
      pkg,
      repos = "https://cloud.r-project.org"
    )

  }

}


# Load packages

library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(googlesheets4)
library(httr)


# ============================================================
# 2. SETTINGS
# ============================================================

# ------------------------------------------------------------
# Google Spreadsheet
# ------------------------------------------------------------

sheet_id <-
  "1YKiQPMtUzd-AwU2cBYVozwfgKVRsHhrTBbc4KFFBQOM"


# ------------------------------------------------------------
# Google Sheet TAB name
#
# IMPORTANT:
# Change this if your tab has another name.
# ------------------------------------------------------------

sheet_name <-
  "BAMIS_WRF_Forecast"


# ------------------------------------------------------------
# BAMIS WRF URL
# ------------------------------------------------------------

base_url <-
  "https://www.bamis.gov.bd/en/bmd/wrf/table/all/"


# ------------------------------------------------------------
# Target district
# ------------------------------------------------------------

target_district <-
  "Dhaka"


# ------------------------------------------------------------
# Browser-like User-Agent
#
# BAMIS (and many .gov.bd sites) sit behind a WAF that
# silently rejects requests carrying the default libcurl /
# rvest user agent. A normal browser UA avoids that.
# ------------------------------------------------------------

browser_user_agent <-
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"


# ------------------------------------------------------------
# Max download attempts per horizon
# ------------------------------------------------------------

max_download_attempts <- 4


# ============================================================
# 3. FUNCTION: CLEAN NUMBERS
# ============================================================

clean_number <- function(x) {

  x <- as.character(x)


  # ----------------------------------------------------------
  # Convert Bangla digits to English digits
  # ----------------------------------------------------------

  x <- str_replace_all(x, "০", "0")
  x <- str_replace_all(x, "১", "1")
  x <- str_replace_all(x, "২", "2")
  x <- str_replace_all(x, "৩", "3")
  x <- str_replace_all(x, "৪", "4")
  x <- str_replace_all(x, "৫", "5")
  x <- str_replace_all(x, "৬", "6")
  x <- str_replace_all(x, "৭", "7")
  x <- str_replace_all(x, "৮", "8")
  x <- str_replace_all(x, "৯", "9")


  # ----------------------------------------------------------
  # Commas
  # ----------------------------------------------------------

  x <- str_replace_all(
    x,
    ",",
    ""
  )


  # ----------------------------------------------------------
  # Unicode minus
  # ----------------------------------------------------------

  x <- str_replace_all(
    x,
    "−",
    "-"
  )


  # ----------------------------------------------------------
  # Remove non-numeric characters
  # ----------------------------------------------------------

  x <- str_replace_all(
    x,
    "[^0-9.\\-]",
    ""
  )


  # ----------------------------------------------------------
  # Convert to numeric
  # ----------------------------------------------------------

  suppressWarnings(
    as.numeric(x)
  )

}


# ============================================================
# 4. FUNCTION: REPAIR COLUMN NAMES
# ============================================================

repair_column_names <- function(df) {

  old_names <- names(df)


  # ----------------------------------------------------------
  # Replace NA / blank names
  # ----------------------------------------------------------

  old_names[
    is.na(old_names) |
      old_names == ""
  ] <-
    "unknown"


  # ----------------------------------------------------------
  # Remove line breaks
  # ----------------------------------------------------------

  new_names <-
    str_replace_all(
      old_names,
      "[\r\n]+",
      " "
    )


  # ----------------------------------------------------------
  # Remove excessive spaces
  # ----------------------------------------------------------

  new_names <-
    str_squish(
      new_names
    )


  # ----------------------------------------------------------
  # Make names unique
  # ----------------------------------------------------------

  new_names <-
    make.unique(
      new_names,
      sep = "__"
    )


  names(df) <-
    new_names


  df

}


# ============================================================
# 5. FUNCTION: FIND COLUMN
# ============================================================

find_column <- function(
    df,
    pattern,
    required = TRUE) {

  nm <- names(df)


  matches <- nm[
    str_detect(
      str_to_lower(nm),
      pattern
    )
  ]


  if (
    length(matches) == 0
  ) {

    if (required) {

      stop(
        "\n\nCOLUMN NOT FOUND\n",
        "Pattern: ",
        pattern,
        "\n\nAvailable columns:\n",
        paste(
          nm,
          collapse = "\n"
        )
      )

    } else {

      return(NULL)

    }

  }


  matches[1]

}


# ============================================================
# 6. FUNCTION: FIND ALL COLUMNS
# ============================================================

find_columns <- function(
    df,
    pattern) {

  nm <- names(df)


  nm[
    str_detect(
      str_to_lower(nm),
      pattern
    )
  ]

}


# ============================================================
# 7. FUNCTION:
#    CHOOSE BEST NUMERIC COLUMN
# ============================================================

choose_numeric_column <- function(
    df,
    candidates) {


  if (
    length(candidates) == 0
  ) {

    stop(
      "No candidate columns found."
    )

  }


  # ----------------------------------------------------------
  # Calculate number of numeric values in each candidate
  # ----------------------------------------------------------

  scores <- sapply(

    candidates,

    function(col) {

      values <-
        clean_number(
          df[[col]]
        )

      sum(
        !is.na(values)
      )

    }

  )


  # ----------------------------------------------------------
  # Print scores
  # ----------------------------------------------------------

  message(
    "Numeric values detected in candidate columns:"
  )

  print(
    data.frame(
      column = candidates,
      numeric_values = scores
    )
  )


  # ----------------------------------------------------------
  # Select column with most numeric values
  # ----------------------------------------------------------

  candidates[
    which.max(scores)
  ]

}


# ============================================================
# 8. FUNCTION:
#    DOWNLOAD A URL WITH BROWSER HEADERS + RETRY
# ============================================================
#
# This replaces a bare read_html(url) call, which is the
# step that was failing with:
#   "Unable to download BAMIS page. cannot open the connection"
#
# read_html() alone sends a plain libcurl User-Agent with no
# retry. If the request is dropped once (WAF filtering a
# non-browser UA, or a transient network hiccup on the GitHub
# Actions runner), the whole job dies immediately.
#
# fetch_html_with_retry() instead:
#   - sends a normal desktop-browser User-Agent + Accept headers
#   - retries with backoff on connection errors or non-200s
#   - only calls read_html() once we actually have HTML back
# ============================================================

fetch_html_with_retry <- function(
    url,
    max_attempts = max_download_attempts) {

  last_error <- NULL


  for (attempt in seq_len(max_attempts)) {

    message(
      "Fetch attempt ",
      attempt,
      " of ",
      max_attempts,
      " -> ",
      url
    )


    response <- tryCatch(

      httr::GET(

        url,

        httr::user_agent(
          browser_user_agent
        ),

        httr::add_headers(
          "Accept" =
            "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "Accept-Language" =
            "en-US,en;q=0.9"
        ),

        httr::timeout(60)

      ),

      error = function(e) e

    )


    # --------------------------------------------------------
    # Connection-level failure (DNS, TLS, timeout, refused...)
    # --------------------------------------------------------

    if (
      inherits(
        response,
        "error"
      )
    ) {

      last_error <-
        response$message

      message(
        "  Connection error: ",
        last_error
      )

      if (attempt < max_attempts) {

        Sys.sleep(
          5 * attempt
        )

      }

      next

    }


    # --------------------------------------------------------
    # HTTP-level failure (403, 429, 5xx, etc.)
    # --------------------------------------------------------

    status <-
      httr::status_code(
        response
      )


    if (
      status != 200
    ) {

      last_error <-
        paste0(
          "HTTP status ",
          status
        )

      message(
        "  ",
        last_error
      )

      if (attempt < max_attempts) {

        Sys.sleep(
          5 * attempt
        )

      }

      next

    }


    # --------------------------------------------------------
    # Success — parse and return
    # --------------------------------------------------------

    html_text <-
      httr::content(
        response,
        as = "text",
        encoding = "UTF-8"
      )

    return(
      read_html(
        html_text
      )
    )

  }


  # ----------------------------------------------------------
  # All attempts failed
  # ----------------------------------------------------------

  stop(
    "Unable to download page after ",
    max_attempts,
    " attempts.\n",
    "URL: ",
    url,
    "\n",
    "Last error: ",
    last_error
  )

}


# ============================================================
# 9. FUNCTION:
#    DOWNLOAD ONE BAMIS WRF HORIZON
# ============================================================

read_wrf_horizon <- function(day) {


  # ----------------------------------------------------------
  # URL
  # ----------------------------------------------------------

  url <-
    paste0(
      base_url,
      day
    )


  message("")
  message(
    "================================================"
  )

  message(
    "Downloading BAMIS WRF horizon: ",
    day
  )

  message(
    "URL: ",
    url
  )

  message(
    "================================================"
  )


  # ----------------------------------------------------------
  # Download page (browser headers + retry)
  # ----------------------------------------------------------

  page <-
    fetch_html_with_retry(
      url
    )


  # ----------------------------------------------------------
  # Extract HTML tables
  #
  # IMPORTANT:
  # Do NOT use name_repair here.
  # Your version of rvest does not support it.
  # ----------------------------------------------------------

  tables <- page |>
    html_elements("table") |>
    html_table(
      fill = TRUE
    )


  # ----------------------------------------------------------
  # Check tables
  # ----------------------------------------------------------

  if (
    length(tables) == 0
  ) {

    stop(
      "No table found on BAMIS page:\n",
      url
    )

  }


  message(
    "Number of tables found: ",
    length(tables)
  )


  # ----------------------------------------------------------
  # First table
  # ----------------------------------------------------------

  df <-
    tables[[1]]


  # ----------------------------------------------------------
  # Repair duplicate column names
  # ----------------------------------------------------------

  df <-
    repair_column_names(
      df
    )


  message(
    "Rows: ",
    nrow(df),
    " | Columns: ",
    ncol(df)
  )


  message(
    "Column names after repair:"
  )

  print(
    names(df)
  )


  # ==========================================================
  # FIND DISTRICT COLUMN
  # ==========================================================

  district_col <-
    find_column(
      df,

      "district|জেলা"
    )


  message(
    "District column: ",
    district_col
  )


  # ==========================================================
  # SHOW DISTRICTS
  # ==========================================================

  message(
    "District values:"
  )

  print(
    unique(
      df[[district_col]]
    )
  )


  # ==========================================================
  # FILTER DHAKA
  # ==========================================================

  district_values <-
    str_to_lower(
      str_squish(
        as.character(
          df[[district_col]]
        )
      )
    )


  dhaka_rows <-
    str_detect(
      district_values,
      "^dhaka$|^ঢাকা$"
    )


  df <-
    df[
      dhaka_rows,
      ,
      drop = FALSE
    ]


  # ==========================================================
  # CHECK DHAKA
  # ==========================================================

  if (
    nrow(df) == 0
  ) {

    stop(
      "Dhaka was not found in BAMIS WRF horizon ",
      day
    )

  }


  # ----------------------------------------------------------
  # If more than one Dhaka row, use first
  # ----------------------------------------------------------

  if (
    nrow(df) > 1
  ) {

    message(
      "More than one Dhaka row found."
    )

    message(
      "Using first Dhaka row."
    )

    df <-
      df[
        1,
        ,
        drop = FALSE
      ]

  }


  df

}


# ============================================================
# 10. DOWNLOAD HORIZONS 1 TO 7
# ============================================================

message("")
message(
  "################################################"
)

message(
  "# DOWNLOADING BAMIS WRF HORIZONS 1 TO 7"
)

message(
  "################################################"
)


wrf_tables <-
  lapply(
    1:7,
    read_wrf_horizon
  )


# ============================================================
# 11. USE DAY-7 TABLE TO IDENTIFY COLUMNS
# ============================================================

df7 <-
  wrf_tables[[7]]


message("")
message(
  "################################################"
)

message(
  "# IDENTIFYING WEATHER VARIABLES"
)

message(
  "################################################"
)


# ============================================================
# DISTRICT
# ============================================================

district_col <-
  find_column(
    df7,
    "district|জেলা"
  )


# ============================================================
# TEMPERATURE
# ============================================================

temp_min_col <-
  find_column(
    df7,

    "temperature.*min|temp.*min|সর্বনিম্ন.*তাপ"
  )


temp_avg_col <-
  find_column(
    df7,

    "temperature.*avg|temperature.*average|temp.*avg|temp.*average|গড়.*তাপ"
  )


temp_max_col <-
  find_column(
    df7,

    "temperature.*max|temp.*max|সর্বোচ্চ.*তাপ"
  )


# ============================================================
# HUMIDITY
# ============================================================

humidity_min_col <-
  find_column(
    df7,

    "humidity.*min|আর্দ্রতা.*min|আর্দ্রতা.*সর্বনিম্ন"
  )


humidity_avg_col <-
  find_column(
    df7,

    "humidity.*avg|humidity.*average|আর্দ্রতা.*গড়"
  )


humidity_max_col <-
  find_column(
    df7,

    "humidity.*max|আর্দ্রতা.*max|আর্দ্রতা.*সর্বোচ্চ"
  )


# ============================================================
# RAINFALL
# ============================================================

rainfall_candidates <-
  find_columns(
    df7,

    "rainfall|rain fall|বৃষ্টিপাত"
  )


message("")
message(
  "Rainfall columns found:"
)

print(
  rainfall_candidates
)


if (
  length(rainfall_candidates) == 0
) {

  stop(
    "No rainfall column was found."
  )

}


# ------------------------------------------------------------
# Print rainfall columns
# ------------------------------------------------------------

message("")
message(
  "Rainfall candidate values:"
)


for (
  col in rainfall_candidates
) {

  message(
    "--------------------------------------------"
  )

  message(
    "Column: ",
    col
  )

  print(
    df7[[col]]
  )

}


# ------------------------------------------------------------
# Select rainfall column
# ------------------------------------------------------------

rainfall_col <-
  choose_numeric_column(
    df7,
    rainfall_candidates
  )


message("")
message(
  "Selected rainfall column: ",
  rainfall_col
)


# ============================================================
# WIND SPEED
# ============================================================

wind_min_col <-
  find_column(
    df7,

    "wind.*speed.*min|wind.*min|বাতাসের.*গতি.*সর্বনিম্ন"
  )


wind_avg_col <-
  find_column(
    df7,

    "wind.*speed.*avg|wind.*speed.*average|wind.*avg|wind.*average|বাতাসের.*গতি.*গড়"
  )


wind_max_col <-
  find_column(
    df7,

    "wind.*speed.*max|wind.*max|বাতাসের.*গতি.*সর্বোচ্চ"
  )


# ============================================================
# WIND DIRECTION
# ============================================================

wind_direction_col <-
  find_column(
    df7,

    "wind.*direction|বাতাসের.*দিক"
  )


# ============================================================
# DISPLAY SELECTED COLUMNS
# ============================================================

message("")
message(
  "################################################"
)

message(
  "# SELECTED COLUMNS"
)

message(
  "################################################"
)


print(
  c(

    district =
      district_col,

    temp_min =
      temp_min_col,

    temp_avg =
      temp_avg_col,

    temp_max =
      temp_max_col,

    humidity_min =
      humidity_min_col,

    humidity_avg =
      humidity_avg_col,

    humidity_max =
      humidity_max_col,

    rainfall =
      rainfall_col,

    wind_min =
      wind_min_col,

    wind_avg =
      wind_avg_col,

    wind_max =
      wind_max_col,

    wind_direction =
      wind_direction_col

  )
)


# ============================================================
# 12. EXTRACT ALL HORIZONS
# ============================================================

wrf_cumulative <-
  bind_rows(

    lapply(

      1:7,

      function(day) {


        df <-
          wrf_tables[[day]]


        tibble(

          horizon =
            day,


          district =
            as.character(
              df[[district_col]]
            ),


          temp_min =
            clean_number(
              df[[temp_min_col]]
            ),


          temp_avg =
            clean_number(
              df[[temp_avg_col]]
            ),


          temp_max =
            clean_number(
              df[[temp_max_col]]
            ),


          humidity_min =
            clean_number(
              df[[humidity_min_col]]
            ),


          humidity_avg =
            clean_number(
              df[[humidity_avg_col]]
            ),


          humidity_max =
            clean_number(
              df[[humidity_max_col]]
            ),


          rainfall =
            clean_number(
              df[[rainfall_col]]
            ),


          wind_min =
            clean_number(
              df[[wind_min_col]]
            ),


          wind_avg =
            clean_number(
              df[[wind_avg_col]]
            ),


          wind_max =
            clean_number(
              df[[wind_max_col]]
            ),


          wind_direction_avg =
            clean_number(
              df[[wind_direction_col]]
            )

        )

      }

    )

  )


# ============================================================
# 13. PRINT RAW HORIZON DATA
# ============================================================

message("")
message(
  "################################################"
)

message(
  "# RAW BAMIS HORIZON VALUES"
)

message(
  "################################################"
)

print(
  wrf_cumulative
)


# ============================================================
# 14. FUNCTION:
# CUMULATIVE AVERAGE -> DAILY VALUE
# ============================================================

cumulative_average_to_daily <-
  function(x) {


    n <-
      length(x)


    daily <-
      rep(
        NA_real_,
        n
      )


    for (
      i in seq_len(n)
    ) {


      # ------------------------------------------------------
      # Day 1
      # ------------------------------------------------------

      if (
        i == 1
      ) {

        daily[i] <-
          x[i]

      }


      # ------------------------------------------------------
      # Day 2-7
      # ------------------------------------------------------

      else {


        previous_values <-
          daily[
            1:(i - 1)
          ]


        if (
          is.na(x[i]) ||
          any(
            is.na(
              previous_values
            )
          )
        ) {

          daily[i] <-
            NA_real_

        } else {


          previous_total <-
            sum(
              previous_values
            )


          daily[i] <-
            (
              i * x[i]
            ) -
            previous_total

        }

      }

    }


    daily

  }


# ============================================================
# 15. CONVERT AVERAGE VARIABLES
# ============================================================

average_variables <- c(

  "temp_min",
  "temp_avg",
  "temp_max",

  "humidity_min",
  "humidity_avg",
  "humidity_max",

  "wind_min",
  "wind_avg",
  "wind_max"

)


for (
  variable in average_variables
) {

  wrf_cumulative[[variable]] <-
    cumulative_average_to_daily(
      wrf_cumulative[[variable]]
    )

}


# ============================================================
# 16. RAINFALL:
# CUMULATIVE TOTAL -> DAILY TOTAL
# ============================================================

rainfall_cumulative <-
  wrf_cumulative$rainfall


wrf_cumulative$rainfall <-
  c(

    rainfall_cumulative[1],

    diff(
      rainfall_cumulative
    )

  )


# ============================================================
# 17. DATES
# ============================================================

today <-
  Sys.Date()


wrf_daily <-
  wrf_cumulative |>
  mutate(

    forecast_run_date =
      today,

    forecast_date =
      today +
      days(horizon),

    day_ahead =
      horizon

  )


# ============================================================
# 18. REMOVE SMALL NEGATIVE ROUNDING ERRORS
# ============================================================

numeric_variables <- c(

  "temp_min",
  "temp_avg",
  "temp_max",

  "humidity_min",
  "humidity_avg",
  "humidity_max",

  "rainfall",

  "wind_min",
  "wind_avg",
  "wind_max"

)


wrf_daily <-
  wrf_daily |>

  mutate(

    across(

      all_of(
        numeric_variables
      ),

      ~ ifelse(

        !is.na(.x) &
          .x < 0 &
          .x > -0.1,

        0,

        .x

      )

    )

  )


# ============================================================
# 19. ROUND VALUES
# ============================================================

wrf_daily <-
  wrf_daily |>

  mutate(

    across(

      all_of(
        numeric_variables
      ),

      ~ round(
        .x,
        1
      )

    ),

    wind_direction_avg =
      round(
        wind_direction_avg,
        1
      )

  )


# ============================================================
# 20. FINAL DATASET
# ============================================================

daily_forecast <-
  wrf_daily |>

  select(

    forecast_run_date,

    forecast_date,

    day_ahead,

    district,

    temp_min,
    temp_avg,
    temp_max,

    humidity_min,
    humidity_avg,
    humidity_max,

    rainfall,

    wind_min,
    wind_avg,
    wind_max,

    wind_direction_avg

  ) |>

  arrange(
    day_ahead
  )


# ============================================================
# 21. PRINT FINAL FORECAST
# ============================================================

message("")
message(
  "################################################"
)

message(
  "# FINAL DHAKA DAILY FORECAST"
)

message(
  "################################################"
)

print(
  daily_forecast
)


# ============================================================
# 22. CHECK 7 DAYS
# ============================================================

if (
  nrow(daily_forecast) != 7
) {

  stop(

    "Expected 7 forecast rows but received ",

    nrow(
      daily_forecast
    )

  )

}


# ============================================================
# 23. GOOGLE AUTHENTICATION
# ============================================================

message("")
message(
  "Authenticating Google Sheets..."
)


google_credentials <-
  Sys.getenv(
    "GSHEET_JSON"
  )


if (
  google_credentials == ""
) {

  stop(
    "GSHEET_JSON secret is missing."
  )

}


# ------------------------------------------------------------
# Create temporary credentials file
# ------------------------------------------------------------

credential_file <-
  tempfile(
    fileext = ".json"
  )


writeLines(
  google_credentials,
  credential_file
)


# ------------------------------------------------------------
# Authenticate
# ------------------------------------------------------------

gs4_auth(
  path =
    credential_file
)


# ============================================================
# 24. READ EXISTING GOOGLE SHEET
# ============================================================

message(
  "Reading existing Google Sheet..."
)


existing <-
  tryCatch(

    read_sheet(

      ss =
        sheet_id,

      sheet =
        sheet_name

    ),

    error =
      function(e) {

        message(
          "Could not read sheet."
        )

        message(
          "Error: ",
          e$message
        )

        NULL

      }

  )


# ============================================================
# 25. FIRST RUN
# ============================================================

if (

  is.null(
    existing
  ) ||

  nrow(
    existing
  ) == 0

) {


  message(
    "Sheet is empty."
  )


  message(
    "Writing 7-day forecast..."
  )


  sheet_write(

    data =
      daily_forecast,

    ss =
      sheet_id,

    sheet =
      sheet_name

  )


} else {


  # ==========================================================
  # CONVERT EXISTING DATES
  # ==========================================================

  if (
    "forecast_run_date"
    %in%
    names(existing)
  ) {

    existing$forecast_run_date <-
      as.Date(
        existing$forecast_run_date
      )

  }


  if (
    "forecast_date"
    %in%
    names(existing)
  ) {

    existing$forecast_date <-
      as.Date(
        existing$forecast_date
      )

  }


  # ==========================================================
  # FIND NEW ROWS
  # ==========================================================

  new_rows <-
    daily_forecast |>

    anti_join(

      existing |>

        select(

          forecast_run_date,

          forecast_date,

          day_ahead

        ),

      by = c(

        "forecast_run_date",

        "forecast_date",

        "day_ahead"

      )

    )


  # ==========================================================
  # APPEND
  # ==========================================================

  if (
    nrow(
      new_rows
    ) > 0
  ) {


    message("")
    message(
      "Appending ",
      nrow(
        new_rows
      ),
      " new rows..."
    )


    sheet_append(

      ss =
        sheet_id,

      sheet =
        sheet_name,

      data =
        new_rows

    )


    message(
      "Successfully appended."
    )


  } else {


    message("")
    message(
      "Today's forecast already exists."
    )


    message(
      "Nothing to append."
    )

  }

}


# ============================================================
# 26. DELETE TEMPORARY CREDENTIAL
# ============================================================

unlink(
  credential_file
)


# ============================================================
# 27. FINISH
# ============================================================

message("")
message(
  "################################################"
)

message(
  "# BAMIS WRF UPDATE COMPLETED"
)

message(
  "################################################"
)

print(
  daily_forecast
)
