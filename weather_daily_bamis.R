library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(googlesheets4)
library(jsonlite)

# ============================================================
# SETTINGS
# ============================================================

bamis_url <- "https://www.bamis.gov.bd/bmd/observed/table/all/7/41923/"

sheet_id <- "1YKiQPMtUzd-AwU2cBYVozwfgKVRsHhrTBbc4KFFBQOM"

# IMPORTANT:
# Change this to your actual worksheet/tab name
sheet_name <- "weather_bamis"

# ============================================================
# BANGLA DIGITS TO ENGLISH
# ============================================================

bangla_to_english <- function(x) {

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

  return(x)
}

# ============================================================
# BANGLA MONTH TO ENGLISH
# ============================================================

bangla_month_to_english <- function(x) {

  x <- str_replace_all(x, "জানুয়ারি", "January")
  x <- str_replace_all(x, "ফেব্রুয়ারি", "February")
  x <- str_replace_all(x, "মার্চ", "March")
  x <- str_replace_all(x, "এপ্রিল", "April")
  x <- str_replace_all(x, "মে", "May")
  x <- str_replace_all(x, "জুন", "June")
  x <- str_replace_all(x, "জুলাই", "July")
  x <- str_replace_all(x, "অগাস্ট", "August")
  x <- str_replace_all(x, "আগস্ট", "August")
  x <- str_replace_all(x, "সেপ্টেম্বর", "September")
  x <- str_replace_all(x, "অক্টোবর", "October")
  x <- str_replace_all(x, "নভেম্বর", "November")
  x <- str_replace_all(x, "ডিসেম্বর", "December")

  return(x)
}

# ============================================================
# DOWNLOAD BAMIS DATA
# ============================================================

message("Downloading BAMIS weather data...")

page <- read_html(bamis_url)

tables <- page |>
  html_elements("table") |>
  html_table(fill = TRUE)

weather <- tables[[1]]

# ============================================================
# RENAME COLUMNS
# ============================================================

names(weather) <- c(
  "station",
  "location",
  "date",
  "temp_min",
  "temp_avg",
  "temp_max",
  "humidity_min",
  "humidity_avg",
  "humidity_max",
  "rainfall",
  "wind_min",
  "wind_avg",
  "wind_max",
  "wind_direction_avg",
  "cloud_amount_avg"
)

# ============================================================
# CLEAN DATE
# ============================================================

weather <- weather |>
  mutate(
    date = bangla_to_english(date),
    date = bangla_month_to_english(date),
    date = dmy(date)
  )

# ============================================================
# CLEAN NUMERIC VARIABLES
# ============================================================

numeric_cols <- c(
  "temp_min",
  "temp_avg",
  "temp_max",
  "humidity_min",
  "humidity_avg",
  "humidity_max",
  "rainfall",
  "wind_min",
  "wind_avg",
  "wind_max",
  "wind_direction_avg",
  "cloud_amount_avg"
)

weather <- weather |>
  mutate(
    across(
      all_of(numeric_cols),
      ~ as.numeric(
        bangla_to_english(
          as.character(.x)
        )
      )
    )
  )

# ============================================================
# ADD STATION CODE
# ============================================================

weather <- weather |>
  mutate(
    station_code = "41923"
  )

# ============================================================
# KEEP ONLY THE LATEST DATE
# ============================================================

latest_weather <- weather |>
  filter(
    date == max(date, na.rm = TRUE)
  ) |>
  select(
    date,
    station,
    station_code,
    location,
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
    wind_direction_avg,
    cloud_amount_avg
  )

message("Latest BAMIS observation:")

print(latest_weather)

# ============================================================
# GOOGLE SHEETS AUTHENTICATION
# ============================================================

# Service account JSON is stored as GitHub Secret
service_account_json <- Sys.getenv("GSHEET_JSON")

# Write temporary credential file
writeLines(
  service_account_json,
  "google-service-account.json"
)

gs4_auth(
  path = "google-service-account.json"
)

# ============================================================
# READ EXISTING GOOGLE SHEET
# ============================================================

existing_data <- read_sheet(
  ss = sheet_id,
  sheet = sheet_name
)

# Ensure date is Date format
if ("date" %in% names(existing_data)) {

  existing_data <- existing_data |>
    mutate(
      date = as.Date(date)
    )

}

# ============================================================
# CHECK IF DATE ALREADY EXISTS
# ============================================================

latest_date <- latest_weather$date[1]

date_exists <- latest_date %in% existing_data$date

if (date_exists) {

  message(
    "Date already exists: ",
    latest_date
  )

  message("No new row added.")

} else {

  message(
    "Appending new date: ",
    latest_date
  )

  sheet_append(
    ss = sheet_id,
    data = latest_weather,
    sheet = sheet_name
  )

  message("Successfully appended to Google Sheet.")

}

# ============================================================
# CLEAN UP
# ============================================================

unlink("google-service-account.json")
