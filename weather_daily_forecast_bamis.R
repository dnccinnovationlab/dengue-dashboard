# ============================================================
# BAMIS WRF 1-7 DAY FORECAST SCRAPER
# ============================================================
#
# Source:
# https://www.bamis.gov.bd/bmd/wrf/table/all/1
# https://www.bamis.gov.bd/bmd/wrf/table/all/2
# ...
# https://www.bamis.gov.bd/bmd/wrf/table/all/7
#
# Target:
# Dhaka
#
# Output:
# Google Sheets
#
# ============================================================


# ============================================================
# 1. INSTALL / LOAD PACKAGES
# ============================================================

required_packages <- c(
  "rvest",
  "httr2",
  "dplyr",
  "stringr",
  "lubridate",
  "googlesheets4"
)

installed_packages <- rownames(
  installed.packages()
)

for (pkg in required_packages) {

  if (!(pkg %in% installed_packages)) {

    install.packages(
      pkg,
      repos = "https://cloud.r-project.org"
    )

  }

}


library(rvest)
library(httr2)
library(dplyr)
library(stringr)
library(lubridate)
library(googlesheets4)


# ============================================================
# 2. SETTINGS
# ============================================================

# ------------------------------------------------------------
# Google Spreadsheet ID
# ------------------------------------------------------------

sheet_id <-
  "1YKiQPMtUzd-AwU2cBYVozwfgKVRsHhrTBbc4KFFBQOM"


# ------------------------------------------------------------
# Google Sheet tab
#
# CHANGE THIS IF YOUR TAB HAS A DIFFERENT NAME
# ------------------------------------------------------------

sheet_name <-
  "BAMIS_WRF_Forecast"


# ------------------------------------------------------------
# BAMIS WRF BASE URL
#
# IMPORTANT:
# This is the original URL structure you provided.
# ------------------------------------------------------------

base_url <-
  "https://www.bamis.gov.bd/bmd/wrf/table/all/"


# ------------------------------------------------------------
# Target district
# ------------------------------------------------------------

target_district <-
  "Dhaka"


# ============================================================
# 3. FUNCTION: CONVERT BANGLA DIGITS AND CLEAN NUMBERS
# ============================================================

clean_number <- function(x) {

  x <- as.character(x)

  # Bangla digits -> English digits

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

  # Remove commas

  x <- str_replace_all(
    x,
    ",",
    ""
  )

  # Unicode minus

  x <- str_replace_all(
    x,
    "−",
    "-"
  )

  # Remove other characters

  x <- str_replace_all(
    x,
    "[^0-9.\\-]",
    ""
  )

  suppressWarnings(
    as.numeric(x)
  )

}


# ============================================================
# 4. FUNCTION: REPAIR COLUMN NAMES
# ============================================================

repair_column_names <- function(df) {

  nm <- names(df)

  # Blank names

  nm[
    is.na(nm) |
      nm == ""
  ] <- "unknown"

  # Remove line breaks

  nm <- str_replace_all(
    nm,
    "[\r\n]+",
    " "
  )

  # Remove excessive spaces

  nm <- str_squish(nm)

  # Make duplicate names unique

  nm <- make.unique(
    nm,
    sep = "__"
  )

  names(df) <- nm

  df

}


# ============================================================
# 5. FUNCTION: FIND ONE COLUMN
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
        "\nCOLUMN NOT FOUND\n",
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
# 6. FUNCTION: FIND ALL MATCHING COLUMNS
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
# SELECT THE RAINFALL COLUMN WITH ACTUAL NUMERIC DATA
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

  message("")
  message(
    "Numeric values in rainfall candidates:"
  )

  print(
    data.frame(
      column = candidates,
      numeric_values = scores
    )
  )

  candidates[
    which.max(scores)
  ]

}


# ============================================================
# 8. FUNCTION:
# DOWNLOAD ONE BAMIS WRF TABLE
# ============================================================

read_wrf_horizon <- function(day) {

  url <- paste0(
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
  # DOWNLOAD USING HTTR2
  # ----------------------------------------------------------

  response <- tryCatch(

    request(url) |>

      req_user_agent(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/151.0 Safari/537.36"
      ) |>

      req_header(
        "Accept" =
          "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
      ) |>

      req_timeout(
        60
      ) |>

      req_perform(),

    error = function(e) {

      stop(
        "\nUnable to download BAMIS page.\n\n",
        "URL: ",
        url,
        "\n\n",
        "Error:\n",
        e$message
      )

    }

  )


  # ----------------------------------------------------------
  # HTTP STATUS
  # ----------------------------------------------------------

  status <-
    resp_status(
      response
    )

  message(
    "HTTP status: ",
    status
  )


  if (
    status != 200
  ) {

    stop(
      "\nBAMIS returned HTTP status ",
      status,
      "\nURL: ",
      url
    )

  }


  # ----------------------------------------------------------
  # READ HTML
  # ----------------------------------------------------------

  html_text <-
    resp_body_string(
      response
    )


  page <-
    read_html(
      html_text
    )


  # ----------------------------------------------------------
  # FIND TABLES
  # ----------------------------------------------------------

  tables <-
    page |>
    html_elements("table") |>
    html_table(
      fill = TRUE
    )


  if (
    length(tables) == 0
  ) {

    stop(
      "\nNo HTML table found.\n",
      "URL: ",
      url
    )

  }


  message(
    "Number of tables found: ",
    length(tables)
  )


  # ----------------------------------------------------------
  # USE FIRST TABLE
  # ----------------------------------------------------------

  df <-
    tables[[1]]


  # ----------------------------------------------------------
  # REPAIR DUPLICATE COLUMN NAMES
  # ----------------------------------------------------------

  df <-
    repair_column_names(
      df
    )


  message(
    "Rows: ",
    nrow(df)
  )

  message(
    "Columns: ",
    ncol(df)
  )


  message(
    "Column names:"
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


  if (
    nrow(df) == 0
  ) {

    stop(
      "\nDhaka was not found in WRF horizon ",
      day
    )

  }


  # ----------------------------------------------------------
  # If multiple Dhaka rows
  # ----------------------------------------------------------

  if (
    nrow(df) > 1
  ) {

    message(
      "Multiple Dhaka rows found."
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
# 9. DOWNLOAD HORIZONS 1-7
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
# 10. USE HORIZON 7 TO IDENTIFY VARIABLES
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
    "No rainfall column found."
  )

}


# ------------------------------------------------------------
# Display rainfall candidate values
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
# PRINT SELECTED COLUMNS
# ============================================================

message("")
message(
  "################################################"
)

message(
  "# SELECTED VARIABLES"
)

message(
  "################################################"
)


print(
  c(

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
# 11. EXTRACT RAW HORIZON VALUES
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
              df[[find_column(
                df,
                "district|জেলা"
              )]]
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
# 12. SHOW RAW VALUES
# ============================================================

message("")
message(
  "################################################"
)

message(
  "# RAW BAMIS HORIZON DATA"
)

message(
  "################################################"
)

print(
  wrf_cumulative
)


# ============================================================
# 13. FUNCTION:
# CUMULATIVE AVERAGE -> INDIVIDUAL DAILY VALUE
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

        previous_daily <-
          daily[
            1:(i - 1)
          ]


        if (
          is.na(x[i]) ||
          any(
            is.na(
              previous_daily
            )
          )
        ) {

          daily[i] <-
            NA_real_

        } else {

          previous_total <-
            sum(
              previous_daily
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
# 14. CONVERT CUMULATIVE AVERAGES
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
# 15. CONVERT CUMULATIVE RAINFALL
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
# 16. CREATE FORECAST DATES
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
# 17. REMOVE TINY NEGATIVE VALUES
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
# 18. ROUND NUMERIC VALUES
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
# 19. FINAL OUTPUT
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
# 20. DISPLAY FINAL FORECAST
# ============================================================

message("")
message(
  "################################################"
)

message(
  "# FINAL DHAKA 7-DAY FORECAST"
)

message(
  "################################################"
)

print(
  daily_forecast
)


# ============================================================
# 21. VALIDATE
# ============================================================

if (
  nrow(
    daily_forecast
  ) != 7
) {

  stop(
    "Expected 7 rows but found ",
    nrow(
      daily_forecast
    )
  )

}


# ============================================================
# 22. GOOGLE SHEETS AUTHENTICATION
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
    "GOOGLE_SERVICE_ACCOUNT GitHub Secret is missing."
  )

}


# ------------------------------------------------------------
# Temporary JSON file
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
# 23. READ GOOGLE SHEET
# ============================================================

message(
  "Reading Google Sheet..."
)


existing <-
  tryCatch(

    read_sheet(

      ss =
        sheet_id,

      sheet =
        sheet_name

    ),

    error = function(e) {

      message(
        "Google Sheet could not be read."
      )

      message(
        e$message
      )

      NULL

    }

  )


# ============================================================
# 24. FIRST RUN
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
    "Google Sheet is empty."
  )

  message(
    "Writing initial 7-day forecast..."
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
  # IDENTIFY NEW ROWS
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
      "Successfully appended new forecast."
    )


  } else {

    message("")
    message(
      "Today's forecast already exists."
    )

    message(
      "No new rows appended."
    )

  }

}


# ============================================================
# 25. REMOVE TEMPORARY CREDENTIAL
# ============================================================

unlink(
  credential_file
)


# ============================================================
# 26. FINISHED
# ============================================================

message("")
message(
  "################################################"
)

message(
  "# BAMIS WRF UPDATE COMPLETED SUCCESSFULLY"
)

message(
  "################################################"
)

print(
  daily_forecast
)
