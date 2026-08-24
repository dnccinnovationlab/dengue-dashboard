# ============================================================
# OPENWEATHER HOURLY OBSERVED WEATHER
# DHAKA, BANGLADESH
# ============================================================

# Packages
packages <- c(
  "httr2",
  "jsonlite",
  "dplyr",
  "lubridate",
  "googlesheets4"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(
      p,
      repos = "https://cloud.r-project.org"
    )
  }
}

library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)
library(googlesheets4)


# ============================================================
# SETTINGS
# ============================================================

latitude <- 23.8103
longitude <- 90.4125

location <- "Dhaka"

timezone <- "Asia/Dhaka"

sheet_id <-
  "1YKiQPMtUzd-AwU2cBYVozwfgKVRsHhrTBbc4KFFBQOM"

sheet_name <- "OpenWeather_Observed"

api_url <-
  "https://api.openweathermap.org/data/3.0/onecall/timemachine"


# ============================================================
# API KEY
# ============================================================

api_key <- Sys.getenv("OPENWEATHER_API_KEY")

if (api_key == "") {
  stop(
    "OPENWEATHER_API_KEY GitHub Secret is missing."
  )
}


# ============================================================
# DETERMINE LAST COMPLETED HOUR
# ============================================================

now_dhaka <- now(
  tz = timezone
)

current_hour <- floor_date(
  now_dhaka,
  unit = "hour"
)

observation_time <- current_hour - hours(1)


message("")
message("================================================")
message("OPENWEATHER OBSERVED WEATHER")
message("================================================")

message(
  "Current Dhaka time: ",
  format(now_dhaka, "%Y-%m-%d %H:%M:%S")
)

message(
  "Observation time: ",
  format(observation_time, "%Y-%m-%d %H:%M:%S")
)


# ============================================================
# CONVERT TO UNIX TIMESTAMP
# ============================================================

observation_utc <- with_tz(
  observation_time,
  "UTC"
)

unix_timestamp <- as.numeric(
  observation_utc
)

message(
  "UTC time: ",
  format(
    observation_utc,
    "%Y-%m-%d %H:%M:%S"
  )
)

message(
  "Unix timestamp: ",
  unix_timestamp
)


# ============================================================
# CALL OPENWEATHER
# ============================================================

message("")
message("Requesting OpenWeather...")


response <- tryCatch(

  request(api_url) |>

    req_url_query(
      lat = latitude,
      lon = longitude,
      dt = unix_timestamp,
      appid = api_key,
      units = "metric"
    ) |>

    req_user_agent(
      "Dhaka Hourly Weather Data Collection"
    ) |>

    req_timeout(60) |>

    req_perform(),

  error = function(e) {

    stop(
      "OpenWeather API request failed:\n",
      e$message
    )

  }
)


# ============================================================
# CHECK HTTP STATUS
# ============================================================

status <- resp_status(response)

message(
  "HTTP status: ",
  status
)

if (status != 200) {

  error_text <- resp_body_string(
    response
  )

  stop(
    "\nOpenWeather returned HTTP ",
    status,
    "\n\nResponse:\n",
    error_text
  )

}


# ============================================================
# PARSE JSON
# ============================================================

json_text <- resp_body_string(
  response
)

weather_data <- fromJSON(
  json_text,
  flatten = TRUE
)


# ============================================================
# CHECK DATA
# ============================================================

if (is.null(weather_data$data)) {

  stop(
    "OpenWeather did not return observation data."
  )

}


obs <- as.data.frame(
  weather_data$data
)

message(
  "Observations returned: ",
  nrow(obs)
)


# ============================================================
# HELPER FUNCTION
# ============================================================

get_value <- function(
    df,
    column,
    default = NA_real_) {

  if (column %in% names(df)) {

    return(
      df[[column]][1]
    )

  }

  return(default)
}


get_character <- function(
    df,
    column,
    default = NA_character_) {

  if (column %in% names(df)) {

    return(
      as.character(
        df[[column]][1]
      )
    )

  }

  return(default)
}


# ============================================================
# OBSERVED DATETIME
# ============================================================

observed_unix <- get_value(
  obs,
  "dt"
)

observed_datetime_utc <- as.POSIXct(
  observed_unix,
  origin = "1970-01-01",
  tz = "UTC"
)

observed_datetime <- with_tz(
  observed_datetime_utc,
  timezone
)


# ============================================================
# WEATHER INFORMATION
# ============================================================

weather_condition <- get_character(
  obs,
  "weather.1.main"
)

weather_description <- get_character(
  obs,
  "weather.1.description"
)

weather_icon <- get_character(
  obs,
  "weather.1.icon"
)

weather_id <- get_value(
  obs,
  "weather.1.id",
  NA_integer_
)


# ============================================================
# RAINFALL
# ============================================================

rain_1h <- get_value(
  obs,
  "rain.1h",
  0
)

snow_1h <- get_value(
  obs,
  "snow.1h",
  0
)


# ============================================================
# CREATE FINAL DATA
# ============================================================

observed <- tibble(

  location = location,

  latitude = latitude,

  longitude = longitude,

  datetime_utc =
    observed_datetime_utc,

  datetime =
    observed_datetime,

  date =
    as.Date(observed_datetime),

  hour =
    hour(observed_datetime),

  temperature =
    get_value(obs, "temp"),

  feels_like =
    get_value(obs, "feels_like"),

  pressure =
    get_value(obs, "pressure"),

  humidity =
    get_value(obs, "humidity"),

  dew_point =
    get_value(obs, "dew_point"),

  clouds =
    get_value(obs, "clouds"),

  wind_speed =
    get_value(obs, "wind_speed"),

  wind_direction =
    get_value(obs, "wind_deg"),

  wind_gust =
    get_value(obs, "wind_gust"),

  visibility =
    get_value(obs, "visibility"),

  rain_1h =
    rain_1h,

  snow_1h =
    snow_1h,

  weather_condition =
    weather_condition,

  weather_description =
    weather_description,

  weather_id =
    weather_id,

  weather_icon =
    weather_icon

)


# ============================================================
# ROUND NUMERIC VARIABLES
# ============================================================

numeric_columns <- c(

  "temperature",
  "feels_like",
  "pressure",
  "humidity",
  "dew_point",
  "clouds",
  "wind_speed",
  "wind_direction",
  "wind_gust",
  "visibility",
  "rain_1h",
  "snow_1h"

)

observed <- observed |>

  mutate(

    across(
      all_of(numeric_columns),
      ~ round(
        as.numeric(.x),
        2
      )
    )

  )


# ============================================================
# DISPLAY
# ============================================================

message("")
message("================================================")
message("DHAKA OBSERVED WEATHER")
message("================================================")

print(observed)


# ============================================================
# GOOGLE SERVICE ACCOUNT
# ============================================================

google_credentials <-
  Sys.getenv(
    "GOOGLE_SERVICE_ACCOUNT"
  )

if (google_credentials == "") {

  stop(
    "GOOGLE_SERVICE_ACCOUNT GitHub Secret is missing."
  )

}


credential_file <- tempfile(
  fileext = ".json"
)

writeLines(
  google_credentials,
  credential_file
)


# ============================================================
# GOOGLE AUTHENTICATION
# ============================================================

gs4_auth(
  path = credential_file
)


# ============================================================
# READ EXISTING SHEET
# ============================================================

message("")
message(
  "Reading Google Sheet..."
)


existing <- tryCatch(

  read_sheet(
    ss = sheet_id,
    sheet = sheet_name
  ),

  error = function(e) {

    message(
      "Sheet does not exist or is empty."
    )

    NULL

  }

)


# ============================================================
# FIRST RUN
# ============================================================

if (

  is.null(existing) ||

  nrow(existing) == 0

) {

  message(
    "Creating initial sheet..."
  )

  sheet_write(
    data = observed,
    ss = sheet_id,
    sheet = sheet_name
  )


} else {


  # ==========================================================
  # EXISTING DATETIME
  # ==========================================================

  if (
    "datetime_utc" %in%
    names(existing)
  ) {

    existing_datetime <- as.character(
      existing$datetime_utc
    )

  } else {

    existing_datetime <- character(0)

  }


  # ==========================================================
  # REMOVE DUPLICATES
  # ==========================================================

  new_record <- observed |>

    filter(

      !(
        as.character(datetime_utc)
        %in%
        existing_datetime
      )

    )


  # ==========================================================
  # APPEND
  # ==========================================================

  if (
    nrow(new_record) > 0
  ) {

    message(
      "Appending ",
      nrow(new_record),
      " new observation(s)..."
    )

    sheet_append(
      ss = sheet_id,
      sheet = sheet_name,
      data = new_record
    )

    message(
      "Successfully appended."
    )

  } else {

    message(
      "Observation already exists."
    )

  }

}


# ============================================================
# CLEAN UP
# ============================================================

unlink(
  credential_file
)


message("")
message("================================================")
message("UPDATE COMPLETED")
message("================================================")
