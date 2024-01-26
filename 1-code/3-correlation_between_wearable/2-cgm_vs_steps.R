no_functions()

library(tidyverse)
library(tidymass)
library(laggedcor)

rm(list = ls())

setwd(r4projects::get_project_wd())

source("1-code/100-tools.R")

###load data
load("3-data_analysis/1-data_preparation/cgm_data")
load("3-data_analysis/1-data_preparation/step_data")

dir.create("3-data_analysis/3-correlation_between_wearable/cgm_vs_steps_vs_hr",
           recursive = TRUE)
setwd("3-data_analysis/3-correlation_between_wearable/cgm_vs_steps_vs_hr")


#####correlation
#####cgm vs step
####lagged correlation
dir.create("step_vs_cgm")
unique(cgm_data$subject_id)
unique(step_data$subject_id)

median(diff(cgm_data$accurate_time))
median(diff(step_data$accurate_time))

###should remove the steps that are zero

# for (temp_subject_id in unique(step_data$subject_id)) {
#   cat(temp_subject_id, " ")
#   dir.create(file.path("step_vs_cgm/", temp_subject_id))
#
#   temp_step_data <-
#     step_data %>%
#     dplyr::filter(subject_id == temp_subject_id) %>%
#     dplyr::filter(value > 0)
#
#   temp_cgm_data <-
#     cgm_data %>%
#     dplyr::filter(subject_id == temp_subject_id)
#
#   x <-
#     temp_step_data$value
#
#   y <-
#     temp_cgm_data$value
#
#   time1 <-
#     temp_step_data$accurate_time
#
#   time2 <-
#     temp_cgm_data$accurate_time
#
#   lagged_cor_result <-
#     calculate_lagged_correlation(
#       x = x,
#       y = y,
#       time1 = time1,
#       time2 = time2,
#       time_tol = 2,
#       step = 5 / 60,
#       min_matched_sample = 10,
#       progressbar = TRUE,
#       threads = 10,
#       cor_method = "spearman"
#     )
#
#   save(
#     lagged_cor_result,
#     file = file.path("step_vs_cgm", temp_subject_id, "lagged_cor_result"),
#     compress = "xz"
#   )
#
#   evalutation_result <-
#     evaluate_lagged_cor(lagged_cor_result,
#                         plot = TRUE)
#
#   save(
#     evalutation_result,
#     file = file.path("step_vs_cgm", temp_subject_id, "evalutation_result"),
#     compress = "xz"
#   )
#
#   ggsave(
#     evalutation_result[[2]],
#     filename = file.path("step_vs_cgm", temp_subject_id, "evalutation.pdf"),
#     width = 7,
#     height = 5
#   )
#
#   plot1 <-
#     lagged_alignment_plot(
#       object = lagged_cor_result,
#       x_color = unname(wearable_color["Step"]),
#       y_color = unname(wearable_color["CGM"]),
#       x_name = "Step",
#       y_name = "CGM",
#       which = "global",
#       x_limit = c(1, length(y)),
#       integrated = FALSE,
#       time_gap = 12
#     )
#
#   ggsave(
#     plot1,
#     filename = file.path("step_vs_cgm", temp_subject_id, "globale_plot.pdf"),
#     width = 14,
#     height = 6
#   )
#
#   plot2 <-
#     lagged_alignment_plot(
#       object = lagged_cor_result,
#       x_color = unname(wearable_color["Step"]),
#       y_color = unname(wearable_color["CGM"]),
#       x_name = "Step",
#       y_name = "CGM",
#       which = "max",
#       x_limit = c(1, length(y)),
#       integrated = TRUE,
#       time_gap = 12
#     )
#
#   ggsave(
#     plot2,
#     filename = file.path("step_vs_cgm", temp_subject_id, "max_plot.pdf"),
#     width = 14,
#     height = 6
#   )
#
#   plot3 <-
#     lagged_scatter_plot(
#       object = lagged_cor_result,
#       x_name = "Step",
#       y_name = "CGM",
#       which = "max",
#       hex = FALSE
#     )
#
#   ggsave(
#     plot3,
#     filename = file.path("step_vs_cgm", temp_subject_id, "scatter_plot.pdf"),
#     width = 7,
#     height = 7
#   )
# }

# step_cgm_result <-
#   unique(step_data$subject_id) %>%
#   purrr::map(function(temp_subject_id) {
#     cat(temp_subject_id, " ")
#     load(file.path("step_vs_cgm", temp_subject_id, "lagged_cor_result"))
#     if (is.null(lagged_cor_result)) {
#       return(NULL)
#     }
#     cor <-
#       laggedcor::extract_all_cor(lagged_cor_result)
#     p <-
#       laggedcor::extract_all_cor_p(lagged_cor_result)
#
#     data.frame(subject_id = temp_subject_id)
#     data1 <-
#       as.data.frame(matrix(cor, nrow = 1))
#     colnames(data1) <-
#       paste("cor", names(cor), sep = "_")
#     data2 <-
#       as.data.frame(matrix(p, nrow = 1))
#     colnames(data2) <-
#       paste("p", names(p), sep = "_")
#     data <-
#       cbind(data1, data2)
#     data$subject_id <-
#       temp_subject_id
#     data$max_shift_time <-
#       names(laggedcor::extract_max_cor(lagged_cor_result))
#     data$max_cor <-
#       unname(laggedcor::extract_max_cor(lagged_cor_result))
#     data
#   })
#
# step_cgm_result <-
#   step_cgm_result %>%
#   do.call(rbind, .) %>%
#   as.data.frame() %>%
#   dplyr::select(subject_id, max_cor, max_shift_time, everything())
#
# library(openxlsx)
# openxlsx::write.xlsx(
#   step_cgm_result,
#   "step_vs_cgm/step_cgm_result.xlsx",
#   asTable = TRUE,
#   overwrite = TRUE
# )
# save(step_cgm_result, file = "step_vs_cgm/step_cgm_result")

load("step_vs_cgm/step_cgm_result")

cor <-
  step_cgm_result %>%
  dplyr::select(subject_id, max_shift_time, dplyr::starts_with("cor_")) %>%
  dplyr::rename_with(~ gsub("cor_", "", .x)) %>%
  tidyr::pivot_longer(
    cols = -c(subject_id, max_shift_time),
    names_to = "shift_time",
    values_to = "cor"
  )

p <-
  step_cgm_result %>%
  dplyr::select(subject_id, max_shift_time, dplyr::starts_with("p_")) %>%
  dplyr::rename_with(~ gsub("p_", "", .x)) %>%
  tidyr::pivot_longer(
    cols = -c(subject_id, max_shift_time),
    names_to = "shift_time",
    values_to = "p"
  )

temp_data <-
  cor %>%
  dplyr::left_join(p, by = c("subject_id", "max_shift_time", "shift_time"))


temp_data$shift_time <-
  stringr::str_split(temp_data$shift_time, pattern = ",") %>%
  purrr::map(function(x) {
    mean(as.numeric(stringr::str_replace(x, "\\(|\\]", "")))
  }) %>%
  unlist()

temp_data$max_shift_time <-
  stringr::str_split(temp_data$max_shift_time, pattern = ",") %>%
  purrr::map(function(x) {
    mean(as.numeric(stringr::str_replace(x, "\\(|\\]", "")))
  }) %>%
  unlist()

plot <-
  temp_data %>%
  dplyr::mutate(class = case_when(
    max_shift_time >= 0 ~ "shift_pos",
    max_shift_time < 0 ~ "shift_neg"
  )) %>% 
  ggplot(aes(shift_time, cor)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(
    aes(
      group = subject_id,
      color = subject_id,
      size = -log(p, 10)
    ),
    show.legend = TRUE,
    alpha = 0.5
  ) +
  geom_smooth(aes(color = subject_id),
              se = FALSE, show.legend = FALSE) +
  geom_vline(
    aes(xintercept = max_shift_time,
        color = subject_id),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = subject_color) +
  scale_size_continuous(range = c(0.1, 4)) +
  labs(x = "Shift time (Steps - CGM, mins)",
       y = "Spearman correlation") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  facet_grid(rows = vars(class))
plot

ggsave(plot,
       filename = "step_vs_cgm/shift_time_vs_cor.pdf",
       width = 7,
       height = 10)

ggsave(plot,
       filename = "step_vs_cgm/shift_time_vs_cor.png",
       width = 7,
       height = 10)

temp_data %>%
  dplyr::distinct(subject_id, .keep_all = TRUE) %>%
  pull(max_shift_time) %>% 
  `<`(0) %>% 
  sum()

temp_data %>%
  dplyr::distinct(subject_id, .keep_all = TRUE) %>%
  pull(max_shift_time) %>% 
  `>=`(0) %>% 
  sum()
