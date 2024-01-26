no_functions()

rm(list = ls())

source("1-code/100-tools.R")

###heart rate
file_name <- dir("2-raw_data/heart_rate/")

hr_data <-
  file_name %>%
  purrr::map(function(i) {
    temp <- readr::read_csv(file.path("2-raw_data/heart_rate/", i))
    temp <-
      temp %>%
      tidyr::pivot_longer(
        cols = -times,
        names_to = c("date"),
        values_to = "value"
      ) %>%
      dplyr::mutate(subject_id = i) %>%
      dplyr::rename(time = times) %>%
      dplyr::select(subject_id, date, time, value)
    
    temp$accurate_time <-
      paste(temp$date, temp$time) %>%
      lubridate::as_datetime(tz = "UTC")
    
    temp$date <-
      as.POSIXct(temp$date, "UTC")
    temp
  }) %>%
  do.call(rbind, .) %>%
  as.data.frame()

hr_data$subject_id <-
  hr_data$subject_id %>%
  stringr::str_replace("_heart_rate\\.csv", "") %>%
  stringr::str_to_upper()

head(hr_data)

save(hr_data, file = "3-data_analysis/1-data_preparation/hr_data")
