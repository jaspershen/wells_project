no_functions()

rm(list = ls())

source("1-code/100-tools.R")

####food
food_data <-
  readr::read_csv("2-raw_data/food/camp_averages.csv")

colnames(food_data)
sample_info <-
  food_data[, "id"] %>%
  as.data.frame()

sample_info$id <-
  stringr::str_to_upper(sample_info$id)

colnames(sample_info) <-
  "sample_id"

sample_info$class <- "Subject"

sample_info$subject_id <-
  sample_info$sample_id

expression_data <-
  food_data[, -1] %>%
  as.data.frame()

rownames(expression_data) <-
  sample_info$id

variable_info <-
  data.frame(variable_id = colnames(expression_data))

library(docxtractr)

doc <- read_docx("2-raw_data/food/Diet_variables.docx")
tbls <- docx_extract_all_tbls(doc)

tbls[[2]] <-
  tbls[[2]][, -1]

colnames(tbls[[1]]) <-
  colnames(tbls[[2]]) <-
  c("meaning", "variable_id")

tbls <-
  rbind(
    data.frame(tbls[[1]], class = "By_Diet"),
    data.frame(tbls[[2]], class = "Healthy_Eating_Index")
  )

setdiff(tbls$variable_id, variable_info$variable_id)
setdiff(variable_info$variable_id, tbls$variable_id)

variable_info <-
  variable_info %>%
  dplyr::left_join(tbls, by = "variable_id")

rownames(expression_data) <- sample_info$sample_id

expression_data <-
  t(expression_data) %>%
  as.data.frame()

food_data <-
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )

save(food_data, file = "3-data_analysis/1-data_preparation/food_data")
