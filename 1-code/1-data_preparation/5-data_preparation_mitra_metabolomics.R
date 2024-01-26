no_functions()

library(tidyverse)
library(tidymass)

rm(list = ls())

setwd(r4projects::get_project_wd())

source("1-code/100-tools.R")

load("2-raw_data/metabolomics_data/mitra/RPLC/pos/Result/object_rplc_pos")
load("2-raw_data/metabolomics_data/mitra/RPLC/neg/Result/object_rplc_neg")

load("2-raw_data/metabolomics_data/mitra/HILIC/pos/Result/object_hilic_pos")
load("2-raw_data/metabolomics_data/mitra/HILIC/neg/Result/object_hilic_neg")

dir.create("3-data_analysis/1-data_preparation/metabolomics/peaks", recursive = TRUE)
dir.create("3-data_analysis/1-data_preparation/metabolomics/metabolites", recursive = TRUE)

setwd("3-data_analysis/1-data_preparation/metabolomics")

##remove redundant samples
###RPLC positive
colnames(object_rplc_pos)

# ##KC_SP_RP_pos_062723_19
plot(
  log(object_rplc_pos$KC_SP_RP_pos_062723_19, 2),
  log(object_rplc_pos$KC_SP_RP_pos_062723_19_2, 2)
)

# ##KC_QC_RP_pos_062723_02
plot(
  log(object_rplc_pos$KC_QC_RP_pos_062723_02, 2),
  log(object_rplc_pos$KC_QC_RP_pos_062723_02_2, 2)
)

object_rplc_pos <-
  object_rplc_pos %>%
  activate_mass_dataset(what = "expression_data") %>%
  dplyr::select(-c(KC_SP_RP_pos_062723_19_2,
                   KC_QC_RP_pos_062723_02_2))

object_rplc_pos <-
  object_rplc_pos %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::mutate(mode = "RPLC_positive")

###RPLC neg
sort(colnames(object_rplc_neg))

object_rplc_neg <-
  object_rplc_neg %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::mutate(mode = "RPLC_negative")

###cbind RPLC pos and neg
# colnames(object_rplc_pos) <-
#   colnames(object_rplc_pos) %>%
#   stringr::str_replace("RP_pos_", "")
object_rplc_pos <-
  object_rplc_pos %>%
  activate_mass_dataset(what = "expression_data") %>%
  rename_with( ~ str_replace(., "RP_pos_", "")) %>% 
  rename_with( ~ str_replace(., "_[0-9]{5,7}", ""))

object_rplc_neg <-
  object_rplc_neg %>%
  activate_mass_dataset(what = "expression_data") %>%
  rename_with( ~ str_replace(., "RP_neg_", "")) %>% 
  rename_with( ~ str_replace(., "_[0-9]{5,7}", ""))

sort(colnames(object_rplc_pos))
sort(colnames(object_rplc_neg))

setdiff(colnames(object_rplc_pos),
        colnames(object_rplc_neg))

setdiff(colnames(object_rplc_neg),
        colnames(object_rplc_pos))

sum(colnames(object_rplc_pos) == colnames(object_rplc_neg))
sum(sort(colnames(object_rplc_pos)) == sort(colnames(object_rplc_neg)))

object_rplc_pos@variable_info$variable_id <-
  rownames(object_rplc_pos@expression_data) <-
  paste(rownames(object_rplc_pos),
        object_rplc_pos@variable_info$mode,
        sep = "_")

object_rplc_pos@annotation_table$variable_id <-
  paste(object_rplc_pos@annotation_table$variable_id,
        object_rplc_pos@variable_info$mode[1],
        sep = "_")

object_rplc_neg@variable_info$variable_id <-
  rownames(object_rplc_neg@expression_data) <-
  paste(rownames(object_rplc_neg),
        object_rplc_neg@variable_info$mode,
        sep = "_")

object_rplc_neg@annotation_table$variable_id <-
  paste(object_rplc_neg@annotation_table$variable_id,
        object_rplc_neg@variable_info$mode[1],
        sep = "_")

object_rplc <-
  rbind(object_rplc_pos,
        object_rplc_neg)

##remove redundant samples
###HILIC positive
colnames(object_hilic_pos)

object_hilic_pos <-
  object_hilic_pos %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::mutate(mode = "HILIC_positive")

###HILIC neg
sort(colnames(object_hilic_neg))

object_hilic_neg <-
  object_hilic_neg %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::mutate(mode = "HILIC_negative")

###cbind HILIC pos and neg
# colnames(object_hilic_pos) <-
#   colnames(object_hilic_pos) %>%
#   stringr::str_replace("RP_pos_", "")
object_hilic_pos <-
  object_hilic_pos %>%
  activate_mass_dataset(what = "expression_data") %>%
  rename_with( ~ str_replace(., "HILIC_pos_", "")) %>% 
  rename_with( ~ str_replace(., "_pH9_[0-9]{5,7}", ""))

object_hilic_neg <-
  object_hilic_neg %>%
  activate_mass_dataset(what = "expression_data") %>%
  rename_with( ~ str_replace(., "HILIC_neg_", "")) %>% 
  rename_with( ~ str_replace(., "_pH9_[0-9]{5,7}", "")) %>% 
  rename_with( ~ str_replace(., "sBL", "SBL"))

sort(colnames(object_hilic_pos))
sort(colnames(object_hilic_neg))

setdiff(colnames(object_hilic_pos),
        colnames(object_hilic_neg))

setdiff(colnames(object_hilic_neg),
        colnames(object_hilic_pos))

object_hilic_pos <-
  object_hilic_pos %>% 
  activate_mass_dataset(what = "expression_data") %>% 
  dplyr::select(-KC_QC_15)

object_hilic_pos <-
object_hilic_pos %>% 
  activate_mass_dataset(what = "expression_data") %>% 
  dplyr::rename(KC_QC_01 = KC_QC_08,
                KC_QC_02 = KC_QC_09,
                KC_QC_03 = KC_QC_10,
                KC_QC_04 = KC_QC_11,
                KC_QC_05 = KC_QC_12,
                KC_QC_06 = KC_QC_13,
                KC_QC_07 = KC_QC_14)

sum(colnames(object_hilic_pos) == colnames(object_hilic_neg))
sum(sort(colnames(object_hilic_pos)) == sort(colnames(object_hilic_neg)))

object_hilic_pos@variable_info$variable_id <-
  rownames(object_hilic_pos@expression_data) <-
  paste(rownames(object_hilic_pos),
        object_hilic_pos@variable_info$mode,
        sep = "_")

object_hilic_pos@annotation_table$variable_id <-
  paste(object_hilic_pos@annotation_table$variable_id,
        object_hilic_pos@variable_info$mode[1],
        sep = "_")

object_hilic_neg@variable_info$variable_id <-
  rownames(object_hilic_neg@expression_data) <-
  paste(rownames(object_hilic_neg),
        object_hilic_neg@variable_info$mode,
        sep = "_")

object_hilic_neg@annotation_table$variable_id <-
  paste(object_hilic_neg@annotation_table$variable_id,
        object_hilic_neg@variable_info$mode[1],
        sep = "_")

object_hilic <-
  rbind(object_hilic_pos,
        object_hilic_neg)

object_rplc <-
  object_rplc %>%
  activate_mass_dataset(what = "expression_data") %>%
  rename_with( ~ str_replace(., "sBL", "SBL"))

colnames(object_rplc)
colnames(object_hilic)

sum(colnames(object_rplc) == 
colnames(object_hilic))

####peaks
object <-
  rbind(object_rplc,
        object_hilic)

colnames(object@sample_info)

colnames(object)

save(object, file = "peaks/object")

# export_mass_dataset(object, file_type = "xlsx", path = "peaks/")

###metabolites

object <-
  object %>%
  activate_mass_dataset(what = "annotation_table") %>%
  dplyr::filter(!is.na(Level))

dim(object)

###remove redundant metabolites
variable_info <-
  extract_variable_info(object)

library(plyr)

variable_info <-
  variable_info %>%
  plyr::dlply(.variables = .(Compound.name))

variable_info <-
  variable_info %>%
  purrr::map(function(x) {
    if (nrow(x) == 1) {
      return(x)
    } else{
      x %>%
        dplyr::filter(Level == min(Level)) %>%
        dplyr::filter(SS == max(SS)) %>%
        dplyr::filter(Total.score == max(Total.score)) %>%
        head(1)
    }
  }) %>%
  dplyr::bind_rows() %>%
  as.data.frame()

object <-
  object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(variable_id %in% variable_info$variable_id)

library(plyr)

variable_info <-
  extract_variable_info(object) %>%
  plyr::dlply(.variables = .(Compound.name)) %>%
  purrr::map(function(x) {
    if (nrow(x) == 1) {
      return(x)
    } else{
      x %>%
        dplyr::filter(Level == min(Level)) %>%
        dplyr::filter(SS == max(SS, na.rm = TRUE)) %>%
        dplyr::filter(Total.score == max(Total.score, na.rm = TRUE)) %>%
        head(1)
    }
  }) %>%
  dplyr::bind_rows()

object <-
  object %>%
  activate_mass_dataset(what = "variable_info") %>%
  dplyr::filter(variable_id %in% variable_info$variable_id)

save(object, file = "metabolites/object")

# export_mass_dataset(object = object,
#                     file_type = "xlsx",
#                     path = "metabolite/")


dim(object)
