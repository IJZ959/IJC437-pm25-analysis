install.packages(c(
  "httr2",
  "jsonlite",
  "dplyr",
  "lubridate",
  "ggplot2",
  "readr",
  "tidyr"
))
library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(tidyr)

## Get Sheffield locations
key <- Sys.getenv("OPENAQ_API_KEY")

loc_req <- request("https://api.openaq.org/v3/locations") |>
  req_headers(`X-API-Key` = key) |>
  req_url_query(
    city = "Sheffield",
    limit = 100,
    page = 1
  )

loc_json <- loc_req |> req_perform() |> resp_body_string()
loc_parsed <- fromJSON(loc_json, flatten = TRUE)

loc_tbl <- as_tibble(loc_parsed$results) |>
  select(id, name)

nrow(loc_tbl)
head(loc_tbl)

## Get PM2.5 sensors for Sheffield

sensor_list <- list()

for (i in seq_len(nrow(loc_tbl))) {
  
  Sys.sleep(1)
  
  s_req <- request(
    paste0("https://api.openaq.org/v3/locations/", loc_tbl$id[i], "/sensors")
  ) |>
    req_headers(`X-API-Key` = key) |>
    req_url_query(limit = 1000)
  
  s_json <- s_req |> req_perform() |> resp_body_string()
  s_parsed <- fromJSON(s_json, flatten = TRUE)
  
  if (length(s_parsed$results) > 0) {
    tmp <- as_tibble(s_parsed$results)
    tmp$location_id <- loc_tbl$id[i]
    sensor_list[[length(sensor_list) + 1]] <- tmp
  }
}

sensors_tbl <- bind_rows(sensor_list)

# Keep PM2.5 sensors only
pm25_sensors <- sensors_tbl |>
  filter(`parameter.id` == 2)

nrow(pm25_sensors)
head(pm25_sensors)

## Select a PM2.5 sensor with sufficient recent data

pm25_sensor_id <- NA
pm25_found <- NA

for (i in seq_len(nrow(pm25_sensors))) {
  
  Sys.sleep(1)
  
  req <- request(
    paste0(
      "https://api.openaq.org/v3/sensors/",
      pm25_sensors$id[i],
      "/measurements"
    )
  ) |>
    req_headers(`X-API-Key` = key) |>
    req_url_query(
      datetime_from = "2024-01-01T00:00:00Z",
      limit = 1
    )
  
  json <- tryCatch(
    req |> req_perform() |> resp_body_string(),
    error = function(e) NULL
  )
  
  if (is.null(json)) next
  
  found <- fromJSON(json, flatten = TRUE)$meta$found
  
  cat("Sensor", pm25_sensors$id[i], "found =", found, "\n")
  
  if (!is.na(found) && found >= 5000) {
    pm25_sensor_id <- pm25_sensors$id[i]
    pm25_found <- found
    break
  }
}

pm25_sensor_id
pm25_found

## Deep probe: check if data exists around the 6000th record

probe_req <- request(
  paste0("https://api.openaq.org/v3/sensors/", pm25_sensor_id, "/measurements")
) |>
  req_headers(`X-API-Key` = key) |>
  req_url_query(
    datetime_from = "2024-01-01T00:00:00Z",
    limit = 1000,
    page = 6
  )

probe_json <- tryCatch(
  probe_req |> req_perform() |> resp_body_string(),
  error = function(e) NULL
)

if (!is.null(probe_json)) {
  
  probe_res <- fromJSON(probe_json, flatten = TRUE)
  
  if (length(probe_res$results) > 0) {
    message("Great: sensor has sufficient 2024 PM2.5 data (>= ~6000).")
  } else {
    message("Warning: limited recent data, consider another sensor.")
  }
  
} else {
  message("Request failed.")
}

## Download PM2.5 measurements (2024 onwards)

pm25_sensor_id <- 12234787

meas_list <- list()

for (p in 1:6) {
  
  Sys.sleep(1)
  
  req <- request(
    paste0(
      "https://api.openaq.org/v3/sensors/",
      pm25_sensor_id,
      "/measurements"
    )
  ) |>
    req_headers(`X-API-Key` = key) |>
    req_url_query(
      datetime_from = "2024-01-01T00:00:00Z",
      limit = 1000,
      page = p
    )
  
  json <- req |> req_perform() |> resp_body_string()
  parsed <- fromJSON(json, flatten = TRUE)
  
  meas_list[[p]] <- as_tibble(parsed$results)
}

m_raw <- bind_rows(meas_list)

nrow(m_raw)
head(m_raw)

## Check time coverage
range(as.POSIXct(m_raw$period.datetimeFrom.utc, tz = "UTC"))
## Keep core fields only
df_meas <- m_raw |>
  transmute(
    datetime = as.POSIXct(period.datetimeFrom.utc, tz = "UTC"),
    pm25 = value
  )
## Check PM2.5 values
summary(df_meas$pm25)
## Remove invalid PM2.5 values
df_meas <- df_meas |>
  filter(
    pm25 >= 0,
    pm25 <= 500
  )
## Aggregate to daily mean PM2.5 with quality control

df_day <- df_meas |>
  mutate(date = as.Date(datetime)) |>
  group_by(date) |>
  summarise(
    pm25_daily = mean(pm25),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 18) |>
  arrange(date)

nrow(df_day)
head(df_day)