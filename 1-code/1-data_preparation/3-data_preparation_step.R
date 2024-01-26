no_functions()

rm(list = ls())

source("1-code/100-tools.R")

###step
step_data <-
  readxl::read_xlsx("2-raw_data/step/CAMP Garmin Data_Zaharieva_11.08.22.xlsx")

colnames(step_data) <-
  c(
    "subject_id",
    "subject_id2",
    "date",
    "accurate_time",
    "mets",
    "value",
    "active_kilocalories",
    "intensity"
  )

time <-
  format(step_data$accurate_time, format = "%H:%M:%S") %>%
  hms::as_hms()

date <-
  step_data$date

accurate_time <-
  paste(date, time) %>%
  lubridate::as_datetime(tz = "UTC")

step_data$date <- date
step_data$time <- time
step_data$accurate_time <-
  accurate_time

step_data$subject_id <-
  step_data$subject_id %>%
  stringr::str_to_upper()

head(step_data)

save(step_data, file = "3-data_analysis/1-data_preparation/step_data")

step_data %>%
  dplyr::filter(subject_id == "DCAMPAAA") %>%
  ggplot(aes(time, value)) +
  geom_line() +
  facet_wrap(facets = vars(date))

