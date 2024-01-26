no_functions()

rm(list = ls())

setwd(r4projects::get_project_wd())

source("1-code/100-tools.R")

####CGM
library(tidymass)
library(tidyverse)
library(wearabledataset)
library(laggedcor)

load("3-data_analysis/1-data_preparation/cgm_data")

dir.create("3-data_analysis/2-data_overview/CGM", recursive = TRUE)
setwd("3-data_analysis/2-data_overview/CGM/")

temp_data <-
  cgm_data %>%
  dplyr::filter(subject_id == "DCAMPAAA")

time_plot(
  x = temp_data$value,
  time = temp_data$accurate_time,
  facet = TRUE,
  color = wearable_color["CGM"]
) +
  facet_grid(rows = vars(day))

time_plot(
  x = temp_data$value,
  time = temp_data$accurate_time,
  facet = FALSE,
  color = wearable_color["CGM"]
)

dir.create("plot")

# for each person
# for (i in unique(cgm_data$subject_id)) {
#   cat(i, "")
#   temp_data <-
#     cgm_data %>%
#     dplyr::filter(subject_id == i)
#
#   plot1 <-
#     time_plot(
#       x = temp_data$value,
#       time = temp_data$accurate_time,
#       facet = TRUE,
#       color = wearable_color["CGM"],
#       y_axis_name = "Glucose (mg/dL)"
#     ) +
#     facet_grid(rows = vars(day))
#
#   plot2 <-
#     time_plot(
#       x = temp_data$value,
#       time = temp_data$accurate_time,
#       facet = FALSE,
#       color = wearable_color["CGM"],
#       y_axis_name = "Glucose (mg/dL)"
#     )
#
#   ggsave(
#     plot1,
#     file = file.path("plot", paste0(i, "_plot1.pdf")),
#     width = 7,
#     height = 15
#   )
#
#   ggsave(
#     plot2,
#     file = file.path("plot", paste0(i, "_plot2.pdf")),
#     width = 14,
#     height = 3
#   )
#
#
# }

temp_data <-
unique(cgm_data$subject_id) %>%
  purrr::map(function(temp_subject_id) {
    temp_time <-
      cgm_data %>%
      dplyr::filter(subject_id == temp_subject_id) %>%
      dplyr::arrange(accurate_time) %>%
      pull(accurate_time)
    diff_time <-
      diff(temp_time) %>%
      as.numeric()
    
    data.frame(subject_id = temp_subject_id,
               diff_time = diff_time)
    
  }) %>%
  do.call(rbind, .) %>%
  as.data.frame()

plot(temp_data$diff_time)
