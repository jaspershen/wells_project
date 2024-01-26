no_functions()

rm(list = ls())

source("1-code/100-tools.R")

####CGM
library(tidymass)
library(wearabledataset)
library(tidyverse)

cgm_data <-
  readxl::read_xlsx("2-raw_data/CGM/⁯Kids Camp_CGM_Compiled.xlsx")

colnames(cgm_data)

colnames(cgm_data) <-
  c("subject_id", "date", "time", "value", "transmitter_id")

cgm_data$date
cgm_data$time

library(lubridate)
library(hms)

time <-
  format(cgm_data$time, format = "%H:%M:%S") %>%
  hms::as_hms()

date <-
  cgm_data$date

accurate_time <-
  paste(date, time) %>%
  lubridate::as_datetime(tz = "UTC")

cgm_data$date <- date
cgm_data$time <- time
cgm_data$accurate_time <-
  accurate_time

save(cgm_data, file = "3-data_analysis/1-data_preparation/cgm_data")

cgm_data %>%
  dplyr::filter(subject_id == "DCAMPAAA") %>%
  ggplot(aes(time, value)) +
  geom_line() +
  facet_wrap(facets = vars(date))

