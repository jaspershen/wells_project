no_functions()

library(tidyverse)
library(tidymass)

rm(list = ls())

setwd(r4projects::get_project_wd())

source("1-code/100-tools.R")

phenotype_info <-
  readxl::read_xlsx("2-raw_data/demongraphics_data/2022CAMPDemographicData.xlsx")

bmi <- readxl::read_xlsx("2-raw_data/demongraphics_data/bmi.xlsx")


load("3-data_analysis/1-data_preparation/cgm_data")
load("3-data_analysis/1-data_preparation/hr_data")
load("3-data_analysis/1-data_preparation/step_data")

subject_id <-
  unique(c(
    cgm_data$subject_id,
    hr_data$subject_id,
    step_data$subject_id
  ))

subject_id2 <-
  subject_id %>%
  stringr::str_sub(-3)

sort(subject_id2)
sort(phenotype_info$`Study ID`)

match(phenotype_info$`Study ID`,
      subject_id2)

phenotype_info <-
  data.frame(subject_id = subject_id,
             subject_id2 = subject_id2) %>%
  dplyr::left_join(phenotype_info,
                   by = c("subject_id2" = "Study ID")) %>%
  dplyr::filter(!is.na(Age))

bmi$subject_id <-
  stringr::str_to_upper(bmi$subject_id)


phenotype_info <-
  phenotype_info %>%
  dplyr::left_join(bmi, by = "subject_id")

save(phenotype_info,
     file = "3-data_analysis/1-data_preparation/phenotype_info")
