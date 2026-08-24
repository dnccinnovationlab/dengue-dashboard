# ============================================================
# OPENWEATHER HOURLY OBSERVED WEATHER
# DHAKA, BANGLADESH
# ============================================================
#
# Purpose:
#   Download the most recent completed hourly observation
#   from OpenWeather Historical / Timemachine API
#
# Time zone:
#   Asia/Dhaka (UTC+6)
#
# Output:
#   Google Sheets
#
# ============================================================


# ============================================================
# 1. INSTALL / LOAD PACKAGES
# ============================================================

packages <- c(
  "httr2",
  "jsonlite",
  "dplyr",
  "lubridate",
  "googlesheets4"
)

for (pkg in packages) {

  if (!requireNamespace(pkg, quietly = TRUE)) {

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
# Google Spreadsheet
# ------------------------------------------------------------

sheet_id <-
  "1YKiQPMtUzd-AwU2cBYVozwfgKVRsHhrTBbc4KFFBQOM"


# ------------------------------------------------------------
# Google Sheet tab
# ------------------------------------------------------------

sheet_name <- "OpenWeather_Observed"


# ------------------------------------------------------------
# OpenWeather historical API
# ------------------------------------------------------------

api_url <-
  "https://api.openweathermap.org/data/3.0/onecall/timemachine"


# ============================================================
# 3. GET OPENWEATHER API KEY
# ============================================================

api_key <-
  Sys.getenv("OPENWEATHER_API_KEY")


if (api_key == "") {

  stop(
    "ERROR: OPENWEATHER_API_KEY GitHub Secret is missing."
  )

}


# ============================================================
# 4. CURRENT DHAKA TIME
# ============================================================

now_dhaka <-
  now(
    tz = timezone
  )


message("")
message("================================================")
message("OPENWEATHER HOURLY OBSERVED WEATHER")
message("================================================")

message(
  "Current Dhaka time: ",
  format(
    now_dhaka,
    "%Y-%m-%d %H:%M:%S"
  )
)


# ============================================================
# 5. FIND LAST COMPLETED HOUR
# ============================================================

# Example:
#
# Current time = 15:25
# Current hour = 15:00
# Last completed hour = 14:00
#
# Therefore the API will request the 14:00 observation.


current_hour <-
  floor_date(
    now_dhaka,
    unit = "hour"
  )


observation_time <-
  current_hour -
  hours(1)


message(
  "Requested Dhaka observation time: ",
  format(
    observation_time,
    "%Y-%m-%d %H:%M:%S"
  )
)


# ============================================================
# 6. CONVERT LOCAL TIME TO UTC
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
  "Requested UTC time: ",
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
# 7. CALL OPENWEATHER API
# ============================================================

message("")
message(
  "Requesting OpenWeather historical data..."
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
      "OpenWeather API request failed:\n",
      e$message
    )

  }

)


# ============================================================
# 8. CHECK HTTP STATUS
# ============================================================

status <-
  resp_status(
    response
  )


message(
  "HTTP status: ",
  status
)


if (status != 200) {

  error_text <-
    resp_body_string(
      response
    )

  stop(

    "OpenWeather returned HTTP status ",
    status,

    "\n\nResponse:\n",
    error_text

  )

}


# ============================================================
# 9. READ JSON
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
# 10. CHECK FOR OBSERVATION DATA
# ============================================================

if (
  is.null(
    weather_data$data
  )
) {

  stop(
    "OpenWeather returned no historical observation data."
  )

}


obs <-
  as.data.frame(
    weather_data$data
  )


message(
  "Number of observations returned: ",
  nrow(obs)
)


# ============================================================
# 11. HELPER FUNCTIONS
# ============================================================

get_numeric <- function(
    df,
    column,
    default = NA_real_) {

  if (
    column %in% names(df)
  ) {

    value <-
      df[[column]][1]

    if (
      length(value) == 0 ||
      is.null(value)
    ) {

      return(default)

    }

    return(
      as.numeric(value)
    )

  }

  return(default)

}


get_character <- function(
    df,
    column,
    default = NA_character_) {

  if (
    column %in% names(df)
  ) {

    value <-
      df[[column]][1]

    if (
      length(value) == 0 ||
      is.null(value)
    ) {

      return(default)

    }

    return(
      as.character(value)
    )

  }

  return(default)

}


# ============================================================
# 12. GET OBSERVED UNIX TIME
# ============================================================

observed_unix <-
  get_numeric(
    obs,
    "dt"
  )


if (
  is.na(observed_unix)
) {

  stop(
    "The API response does not contain a valid dt timestamp."
  )

}


# ============================================================
# 13. CONVERT OBSERVED TIME
# ============================================================

observed_datetime_utc <-
  as.POSIXct(
    observed_unix,
    origin = "1970-01-01",
    tz = "UTC"
  )


observed_datetime_dhaka <-
  with_tz(
    observed_datetime_utc,
    timezone
  )


# ============================================================
# 14. WEATHER CONDITION
# ============================================================
#
# OpenWeather's weather field can appear in different
# structures depending on how jsonlite flattens the response.
#
# We therefore check several possible column names.
#
# ============================================================


weather_condition <-
  NA_character_


weather_description <-
  NA_character_


weather_id <-
  NA_integer_


weather_icon <-
  NA_character_


# ------------------------------------------------------------
# Main flattened names
# ------------------------------------------------------------

if (
  "weather.1.main"
  %in%
  names(obs)
) {

  weather_condition <-
    get_character(
      obs,
      "weather.1.main"
    )

}


if (
  "weather.1.description"
  %in%
  names(obs)
) {

  weather_description <-
    get_character(
      obs,
      "weather.1.description"
    )

}


if (
  "weather.1.id"
  %in%
  names(obs)
) {

  weather_id <-
    get_numeric(
      obs,
      "weather.1.id"
    )

}


if (
  "weather.1.icon"
  %in%
  names(obs)
) {

  weather_icon <-
    get_character(
      obs,
      "weather.1.icon"
    )

}


# ============================================================
# 15. RAINFALL
# ============================================================

rain_1h <-
  get_numeric(
    obs,
    "rain.1h",
    0
  )


snow_1h <-
  get_numeric(
    obs,
    "snow.1h",
    0
  )


# ============================================================
# 16. CREATE OBSERVATION RECORD
# ============================================================

observed <-
  tibble(

    location =
      location,

    latitude =
      latitude,

    longitude =
      longitude,


    # --------------------------------------------------------
    # UTC datetime
    # --------------------------------------------------------

    datetime_utc =
      format(
        observed_datetime_utc,
        "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),


    # --------------------------------------------------------
    # Bangladesh local datetime
    # --------------------------------------------------------

    datetime =
      format(
        observed_datetime_dhaka,
        "%Y-%m-%d %H:%M:%S",
        tz = timezone
      ),


    # --------------------------------------------------------
    # Bangladesh local date
    # --------------------------------------------------------

    date =
      format(
        observed_datetime_dhaka,
        "%Y-%m-%d",
        tz = timezone
      ),


    # --------------------------------------------------------
    # Bangladesh local hour
    # --------------------------------------------------------

    hour =
      hour(
        observed_datetime_dhaka
      ),


    # --------------------------------------------------------
    # Temperature
    # --------------------------------------------------------

    temperature =
      get_numeric(
        obs,
        "temp"
      ),


    feels_like =
      get_numeric(
        obs,
        "feels_like"
      ),


    # --------------------------------------------------------
    # Atmospheric variables
    # --------------------------------------------------------

    pressure =
      get_numeric(
        obs,
        "pressure"
      ),

    humidity =
      get_numeric(
        obs,
        "humidity"
      ),

    dew_point =
      get_numeric(
        obs,
        "dew_point"
      ),


    # --------------------------------------------------------
    # Clouds
    # --------------------------------------------------------

    clouds =
      get_numeric(
        obs,
        "clouds"
      ),


    # --------------------------------------------------------
    # Wind
    # --------------------------------------------------------

    wind_speed =
      get_numeric(
        obs,
        "wind_speed"
      ),

    wind_direction =
      get_numeric(
        obs,
        "wind_deg"
      ),

    wind_gust =
      get_numeric(
        obs,
        "wind_gust"
      ),


    # --------------------------------------------------------
    # Visibility
    # --------------------------------------------------------

    visibility =
      get_numeric(
        obs,
        "visibility"
      ),


    # --------------------------------------------------------
    # Rain
    # --------------------------------------------------------

    rain_1h =
      rain_1h,


    # --------------------------------------------------------
    # Snow
    # --------------------------------------------------------

    snow_1h =
      snow_1h,


    # --------------------------------------------------------
    # Weather
    # --------------------------------------------------------

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
# 17. ROUND NUMERIC VARIABLES
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
# 18. DISPLAY FINAL RECORD
# ============================================================

message("")
message("================================================")
message("OBSERVATION TO BE STORED")
message("================================================")

print(
  observed
)


# ============================================================
# 19. GOOGLE SERVICE ACCOUNT
# ============================================================

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


# ============================================================
# 20. GOOGLE SHEETS AUTHENTICATION
# ============================================================

message("")
message(
  "Authenticating Google Sheets..."
)


gs4_auth(
  path =
    credential_file
)


# ============================================================
# 21. READ EXISTING GOOGLE SHEET
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

    error = function(e) {

      message(
        "The sheet is empty or does not exist."
      )

      NULL

    }

  )


# ============================================================
# 22. FIRST RUN
# ============================================================

if (

  is.null(existing) ||

  nrow(existing) == 0

) {

  message(
    "No existing data found."
  )

  message(
    "Creating the sheet..."
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
  # 23. GET EXISTING UTC DATETIMES
  # ==========================================================

  if (
    "datetime_utc"
    %in%
    names(existing)
  ) {

    existing_datetime <-
      as.character(
        existing$datetime_utc
      )

  } else {

    existing_datetime <-
      character(0)

  }


  # ==========================================================
  # 24. CHECK WHETHER OBSERVATION ALREADY EXISTS
  # ==========================================================

  new_record <-
    observed |>

    filter(

      !(
        datetime_utc
        %in%
        existing_datetime
      )

    )


  # ==========================================================
  # 25. APPEND NEW OBSERVATION
  # ==========================================================

  if (
    nrow(new_record) > 0
  ) {

    message(
      "New observation found."
    )

    message(
      "Appending ",
      nrow(new_record),
      " record(s)..."
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
      "Observation successfully appended."
    )

  } else {

    message(
      "Observation already exists."
    )

    message(
      "No duplicate record added."
    )

  }

}


# ============================================================
# 26. REMOVE TEMPORARY GOOGLE CREDENTIAL
# ============================================================

unlink(
  credential_file
)


# ============================================================
# 27. FINAL MESSAGE
# ============================================================

message("")
message("================================================")
message("OPENWEATHER UPDATE COMPLETED")
message("================================================")

message(
  "Bangladesh observation time: ",
  format(
    observed_datetime_dhaka,
    "%Y-%m-%d %H:%M:%S"
  )
)

message(
  "UTC observation time: ",
  format(
    observed_datetime_utc,
    "%Y-%m-%d %H:%M:%S"
  )
)

message("================================================")
