####food
no_functions()

rm(list = ls())

setwd(r4projects::get_project_wd())

source("1-code/100-tools.R")

library(tidymass)
library(tidyverse)
library(laggedcor)

load("3-data_analysis/1-data_preparation/food_data")

dir.create("3-data_analysis/2-data_overview/food", recursive = TRUE)
setwd("3-data_analysis/2-data_overview/food/")

food_data <-
  food_data %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(!is.na(meaning))

dir.create("plot")

# for each person
for (i in unique(food_data@sample_info$subject_id)) {
  cat(i, "")
  temp_data <-
    food_data %>%
    activate_mass_dataset(what = "sample_info") %>%
    dplyr::filter(subject_id == i)
  
  temp_data <-
    temp_data@expression_data %>%
    tibble::rownames_to_column(var = "variable_id") %>%
    dplyr::left_join(temp_data@variable_info, by = "variable_id")
  
  colnames(temp_data)[2] <- "value"
  
  plot <-
    temp_data %>%
    ggplot(aes(
      x = variable_id,
      xend = variable_id,
      y = 0,
      yend = value
    )) +
    geom_segment() +
    geom_point(data = temp_data, aes(variable_id, value)) +
    ggforce::facet_row(vars(class),
                       scales = 'free', space = 'free') +
    labs(x = "", y = "Value") +
    theme_bw() +
    theme(axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ))
  
  ggsave(
    plot,
    file = file.path("plot", paste0(i, "_plot.pdf")),
    width = 14,
    height = 3
  )
}
