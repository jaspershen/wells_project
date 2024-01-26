####Step
no_functions()

rm(list = ls())

setwd(r4projects::get_project_wd())

source("1-code/100-tools.R")

####step
library(tidymass)
library(tidyverse)
library(laggedcor)

load("3-data_analysis/1-data_preparation/step_data")

dir.create("3-data_analysis/2-data_overview/Step", recursive = TRUE)
setwd("3-data_analysis/2-data_overview/Step/")

temp_data <-
  step_data %>%
  dplyr::filter(subject_id == "DCAMPAAA")

time_plot(
  x = temp_data$value,
  time = temp_data$accurate_time,
  facet = TRUE,
  color = wearable_color["Step"]
) +
  facet_grid(rows = vars(day))

time_plot(
  x = temp_data$value,
  time = temp_data$accurate_time,
  facet = FALSE,
  color = wearable_color["Step"]
)

dir.create("plot")

# for each person
for (i in unique(step_data$subject_id)) {
  cat(i, "")
  temp_data <-
    step_data %>%
    dplyr::filter(subject_id == i)
  
  plot1 <-
    time_plot(
      x = temp_data$value,
      time = temp_data$accurate_time,
      facet = TRUE,
      color = wearable_color["Step"],
      y_axis_name = "Step (count)"
    ) +
    facet_grid(rows = vars(day))
  
  plot2 <-
    time_plot(
      x = temp_data$value,
      time = temp_data$accurate_time,
      facet = FALSE,
      color = wearable_color["Step"],
      y_axis_name = "Step (count)"
    )
  
  ggsave(
    plot1,
    file = file.path("plot", paste0(i, "_plot1.pdf")),
    width = 7,
    height = 15
  )
  
  ggsave(
    plot2,
    file = file.path("plot", paste0(i, "_plot2.pdf")),
    width = 14,
    height = 3
  )
  
  
}




