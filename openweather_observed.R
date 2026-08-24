# ============================================================
# OPENWEATHER HOURLY OBSERVED WEATHER
# ============================================================
#
# LOCATION:
# Dhaka, Bangladesh
#
# PURPOSE:
# Collect observed/historical weather for the most recent
# completed hour and append it to Google Sheets.
#
# API:
# OpenWeather One Call 3.0 Historical / Timemachine
#
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "httr2",
  "jsonlite",
  "dplyr",
  "lubridate",
  "googlesheets4"
)


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


library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)
library(googlesheets4)


# ============================================================
# 2. SETTINGS
# ============================================================

# ------------------------------------------------------------
# Dhaka coordinates
# ------------------------------------------------------------

latitude <- 23.8103

longitude <- 90.4125


# ------------------------------------------------------------
# Location
# ------------------------------------------------------------

location <- "Dhaka"


# ------------------------------------------------------------
# Bangladesh timezone
# ------------------------------------------------------------

timezone <- "Asia/Dhaka"


# ------------------------------------------------------------
# Google Sheet
# ------------------------------------------------------------

sheet_id <-
  "1YKiQPMtUzd-AwU2cBYVozwfgKVRsHhrTBbc4KFFBQOM"


sheet_name <-
  "OpenWeather_Observed"


# ------------------------------------------------------------
# OpenWeather historical endpoint
# ------------------------------------------------------------

api_url <-
  "https://api.openweathermap.org/data/3.0/onecall/timemachine"


# ============================================================
# 3. GET OPENWEATHER API KEY
# ============================================================

api_key <-
  Sys.getenv(
    "OPENWEATHER_API_KEY"
  )


if (
  api_key == ""
) {

  stop(
    "OPENWEATHER_API_KEY GitHub Secret is missing."
  )

}


# ============================================================
# 4. DETERMINE MOST RECENT COMPLETED HOUR
# ============================================================

now_dhaka <-
  now(
    tz = timezone
  )


# ------------------------------------------------------------
# Round down to the beginning of the current hour
# ------------------------------------------------------------

current_hour <-
  floor_date(
    now_dhaka,
    unit = "hour"
  )


# ------------------------------------------------------------
# We want the previous completed hour
# ------------------------------------------------------------

observation_time <-
  current_hour -
  hours(1)


message("")
message(
  "================================================"
)

message(
  "OPENWEATHER OBSERVED WEATHER"
)

message(
  "================================================"
)

message(
  "Current Dhaka time: ",
  format(
    now_dhaka,
    "%Y-%m-%d %H:%M:%S"
  )
)

message(
  "Observation hour: ",
  format(
    observation_time,
    "%Y-%m-%d %H:%M:%S"
  )
)


# ============================================================
# 5. CONVERT TO UNIX UTC TIMESTAMP
# ============================================================

observation_utc <-
  with_tz(
    observation_time,
    "UTC"
  )


unix_timestamp <-
  as.numeric(
    observation_utc
  )


message(
  "UTC observation time: ",
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
# 6. CALL OPENWEATHER
# ============================================================

message("")
message(
  "Requesting OpenWeather historical observation..."
)


response <- tryCatch(

  request(api_url) |>

    req_url_query(

      lat =
        latitude,

      lon =
        longitude,

      dt =
        unix_timestamp,

      appid =
        api_key,

      units =
        "metric"

    ) |>

    req_user_agent(
      "Dhaka Hourly Weather Data Collection"
    ) |>

    req_timeout(
      60
    ) |>

    req_perform(),

  error = function(e) {

    stop(
      "\nOpenWeather API request failed:\n",
      e$message
    )

  }

)


# ============================================================
# 7. CHECK RESPONSE
# ============================================================

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

  response_text <-
    resp_body_string(
      response
    )

  stop(

    "\nOpenWeather returned HTTP status: ",

    status,

    "\n\nResponse:\n",

    response_text

  )

}


# ============================================================
# 8. PARSE JSON
# ============================================================

json_text <-
  resp_body_string(
    response
  )


weather_data <-
  fromJSON(
    json_text,
    flatten = TRUE
  )


# ============================================================
# 9. CHECK DATA
# ============================================================

if (
  is.null(
    weather_data$data
  )
) {

  stop(
    "No historical observation was returned by OpenWeather."
  )

}


# ============================================================
# 10. EXTRACT OBSERVATION
# ============================================================

obs <-
  as.data.frame(
    weather_data$data
  )


message("")
message(
  "Observations returned: ",
  nrow(obs)
)


# Usually there should be one observation for the
# requested timestamp.


# ============================================================
# 11. HELPER FUNCTION
# ============================================================

get_value <- function(
    df,
    column,
    default = NA_real_) {

  if (
    column %in% names(df)
  ) {

    return(
      df[[column]][1]
    )

  }

  default

}


# ============================================================
# 12. OBSERVATION UNIX TIME
# ============================================================

observed_unix <-
  get_value(
    obs,
    "dt"
  )


observed_datetime_utc <-
  as.POSIXct(
    observed_unix,
    origin = "1970-01-01",
    tz = "UTC"
  )


observed_datetime <-
  with_tz(
    observed_datetime_utc,
    timezone
  )


# ============================================================
# 13. WEATHER CONDITION
# ============================================================

weather_condition <-
  NA_character_


weather_description <-
  NA_character_


weather_id <-
  NA_integer_


weather_icon <-
  NA_character_


if (
  "weather.1.main"
  %in%
  names(obs)
) {

  weather_condition <-
    obs[
      ["weather.1.main"]
    ][1]

}


if (
  "weather.1.description"
  %in%
  names(obs)
) {

  weather_description <-
    obs[
      ["weather.1.description"]
    ][1]

}


if (
  "weather.1.id"
  %in%
  names(obs)
) {

  weather_id <-
    obs[
      ["weather.1.id"]
    ][1]

}


if (
  "weather.1.icon"
  %in%
  names(obs)
) {

  weather_icon <-
    obs[
      ["weather.1.icon"]
    ][1]

}


# ============================================================
# 14. CREATE FINAL RECORD
# ============================================================

observed <- tibble(

  location =
    location,

  latitude =
    latitude,

  longitude =
    longitude,

  datetime_utc =
    observed_datetime_utc,

  datetime =
    observed_datetime,

  date =
    as.Date(
      observed_datetime
    ),

  hour =
    hour(
      observed_datetime
    ),

  temperature =
    get_value(
      obs,
      "temp"
    ),

  feels_like =
    get_value(
      obs,
      "feels_like"
    ),

  pressure =
    get_value(
      obs,
      "pressure"
    ),

  humidity =
    get_value(
      obs,
      "humidity"
    ),

  dew_point =
    get_value(
      obs,
      "dew_point"
    ),

  clouds =
    get_value(
      obs,
      "clouds"
    ),

  wind_speed =
    get_value(
      obs,
      "wind_speed"
    ),

  wind_direction =
    get_value(
      obs,
      "wind_deg"
    ),

  wind_gust =
    get_value(
      obs,
      "wind_gust"
    ),

  visibility =
    get_value(
      obs,
      "visibility"
    ),

  rain_1h =
    get_value(
      obs,
      "rain.1h",
      0
    ),

  snow_1h =
    get_value(
      obs,
      "snow.1h",
      0
    ),

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
# 15. ROUND VALUES
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


observed <-
  observed |>

  mutate(

    across(

      all_of(
        numeric_columns
      ),

      ~ round(
        as.numeric(.x),
        2
      )

    )

  )


# ============================================================
# 16. DISPLAY OBSERVATION
# ============================================================

message("")
message(
  "================================================"
)

message(
  "OBSERVED DHAKA WEATHER"
)

message(
  "================================================"
)

print(
  observed
)


# ============================================================
# 17. GOOGLE SHEETS AUTHENTICATION
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
    "GSHEET_JSON GitHub Secret is missing."
  )

}


credential_file <-
  tempfile(
    fileext = ".json"
  )


writeLines(
  google_credentials,
  credential_file
)


gs4_auth(
  path =
    credential_file
)


# ============================================================
# 18. READ EXISTING SHEET
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
        "Sheet is empty or does not exist."
      )

      NULL

    }

  )


# ============================================================
# 19. FIRST RUN
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
    "Writing first observation..."
  )


  sheet_write(

    data =
      observed,

    ss =
      sheet_id,

    sheet =
      sheet_name

  )


} else {


  # ==========================================================
  # 20. PREVENT DUPLICATES
  # ==========================================================

  existing_datetime <-
    as.character(
      existing$datetime_utc
    )


  new_record <-
    observed |>

    filter(

      !(
        as.character(
          datetime_utc
        )
        %in%
        existing_datetime
      )

    )


  # ==========================================================
  # 21. APPEND
  # ==========================================================

  if (
    nrow(
      new_record
    ) > 0
  ) {

    message(
      "Appending new observation..."
    )


    sheet_append(

      ss =
        sheet_id,

      sheet =
        sheet_name,

      data =
        new_record

    )


    message(
      "Observation appended successfully."
    )

  } else {

    message(
      "This observation already exists."
    )

  }

}


# ============================================================
# 22. DELETE TEMPORARY CREDENTIAL
# ============================================================

unlink(
  credential_file
)


# ============================================================
# 23. FINISHED
# ============================================================

message("")
message(
  "================================================"
)

message(
  "OPENWEATHER OBSERVED DATA UPDATE COMPLETED"
)

message(
  "================================================"
)
