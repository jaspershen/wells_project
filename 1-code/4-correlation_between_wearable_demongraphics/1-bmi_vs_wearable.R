no_functions()

library(tidyverse)
library(tidymass)
library(laggedcor)

rm(list = ls())

setwd(r4projects::get_project_wd())

source("1-code/100-tools.R")

###load data
load("3-data_analysis/1-data_preparation/step_data")
load("3-data_analysis/1-data_preparation/hr_data")
load("3-data_analysis/1-data_preparation/cgm_data")
load("3-data_analysis/1-data_preparation/phenotype_info")


dir.create(
  "3-data_analysis/4-correlation_between_demongraphics_wearable/bmi_vs_hr",
  recursive = TRUE
)
setwd("3-data_analysis/4-correlation_between_demongraphics_wearable/bmi_vs_hr")


#####correlation
#####bmi vs hr
temp_data <-
  hr_data %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::group_by(subject_id) %>%
  dplyr::summarize(median = median(value),
                   mean = mean(value))

test <-
  cor.test(temp_data$median,
           temp_data$mean)
correlation <- round(test$estimate, digits = 2)
p_value <- round(test$p.value, digits = 4)

hr_mean_median_plot <-
  temp_data %>%
  ggplot(aes(x = mean, y = median)) +
  geom_point(aes(color = subject_id),
             show.legend = FALSE,
             size = 5) +
  scale_color_manual(values = subject_color) +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  labs(x = "Mean HR",
       y = "Median HR") +
  annotate(
    "text",
    x = max(temp_data$mean),
    y = max(temp_data$median),
    label = paste("R = ", correlation, "\np-value = ", p_value),
    hjust = 1
  )

hr_mean_median_plot

ggsave(
  hr_mean_median_plot,
  filename = "hr_mean_median_plot.pdf",
  width = 7,
  height = 7
)

ggsave(
  hr_mean_median_plot,
  filename = "hr_mean_median_plot.png",
  width = 7,
  height = 7
)

temp_data <-
  temp_data %>%
  dplyr::left_join(phenotype_info, by = "subject_id") %>%
  dplyr::filter(!is.na(BMI))

test <-
  cor.test(temp_data$mean,
           temp_data$BMI)
correlation <- round(test$estimate, digits = 2)
p_value <- round(test$p.value, digits = 4)

bmi_vs_mean_hr_plot <-
  temp_data %>%
  ggplot(aes(BMI, mean)) +
  geom_point(aes(color = subject_id),
             show.legend = FALSE,
             size = 5) +
  scale_color_manual(values = subject_color) +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  labs(x = "BMI",
       y = "Mean HR") +
  annotate(
    "text",
    x = max(temp_data$BMI),
    y = max(temp_data$mean),
    label = paste("R = ", correlation, "\np-value = ", p_value),
    hjust = 1
  )
bmi_vs_mean_hr_plot


ggsave(
  bmi_vs_mean_hr_plot,
  filename = "bmi_vs_mean_hr_plot.pdf",
  width = 7,
  height = 7
)

ggsave(
  bmi_vs_mean_hr_plot,
  filename = "bmi_vs_mean_hr_plot.png",
  width = 7,
  height = 7
)

# library(gghalves)
# library(ggsignif)
# temp_data %>%
#   dplyr::filter(Gender %in% c("Male", "Female")) %>%
#   ggplot(aes(Gender, mean)) +
#   geom_half_boxplot(side = "l") +
#   geom_half_violin(side = "r") +
#   geom_half_dotplot(side = "r",
#                     aes(fill = subject_id),
#                     show.legend = FALSE) +
#   scale_fill_manual(values = subject_color) +
#   theme_bw() +
#   labs(x = "", y = "Mean HR") +
#   geom_signif(
#     comparisons = list(c("Male", "Female")),
#     map_signif_level = TRUE,
#     test = "wilcox.test"
#   )






#####bmi vs step
setwd(r4projects::get_project_wd())
dir.create(
  "3-data_analysis/4-correlation_between_demongraphics_wearable/bmi_vs_step",
  recursive = TRUE
)
setwd("3-data_analysis/4-correlation_between_demongraphics_wearable/bmi_vs_step")

temp_data <-
  step_data %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::filter(value > 0) %>%
  dplyr::group_by(subject_id) %>%
  dplyr::summarize(median = median(value),
                   mean = mean(value))

test <-
  cor.test(temp_data$median,
           temp_data$mean)
correlation <- round(test$estimate, digits = 2)
p_value <- round(test$p.value, digits = 4)

step_mean_median_plot <-
  temp_data %>%
  ggplot(aes(x = mean, y = median)) +
  geom_point(aes(color = subject_id),
             show.legend = FALSE,
             size = 5) +
  scale_color_manual(values = subject_color) +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  labs(x = "Mean Step",
       y = "Median Step") +
  annotate(
    "text",
    x = max(temp_data$mean),
    y = max(temp_data$median),
    label = paste("R = ", correlation, "\np-value = ", p_value),
    hjust = 1
  )

step_mean_median_plot

ggsave(
  step_mean_median_plot,
  filename = "step_mean_median_plot.pdf",
  width = 7,
  height = 7
)

ggsave(
  step_mean_median_plot,
  filename = "step_mean_median_plot.png",
  width = 7,
  height = 7
)

temp_data <-
  temp_data %>%
  dplyr::left_join(phenotype_info, by = "subject_id") %>%
  dplyr::filter(!is.na(BMI))

test <-
  cor.test(temp_data$mean,
           temp_data$BMI)
correlation <- round(test$estimate, digits = 2)
p_value <- round(test$p.value, digits = 4)

bmi_vs_mean_step_plot <-
  temp_data %>%
  ggplot(aes(BMI, mean)) +
  geom_point(aes(color = subject_id),
             show.legend = FALSE,
             size = 5) +
  scale_color_manual(values = subject_color) +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  labs(x = "BMI",
       y = "Mean Step") +
  annotate(
    "text",
    x = max(temp_data$BMI),
    y = max(temp_data$mean),
    label = paste("R = ", correlation, "\np-value = ", p_value),
    hjust = 1
  )
bmi_vs_mean_step_plot


ggsave(
  bmi_vs_mean_step_plot,
  filename = "bmi_vs_mean_step_plot.pdf",
  width = 7,
  height = 7
)

ggsave(
  bmi_vs_mean_step_plot,
  filename = "bmi_vs_mean_step_plot.png",
  width = 7,
  height = 7
)



#####bmi vs cgm
setwd(r4projects::get_project_wd())
dir.create(
  "3-data_analysis/4-correlation_between_demongraphics_wearable/bmi_vs_cgm",
  recursive = TRUE
)
setwd("3-data_analysis/4-correlation_between_demongraphics_wearable/bmi_vs_cgm")

temp_data <-
  cgm_data %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::filter(value > 0) %>%
  dplyr::group_by(subject_id) %>%
  dplyr::summarize(median = median(value),
                   mean = mean(value))

test <-
  cor.test(temp_data$median,
           temp_data$mean)
correlation <- round(test$estimate, digits = 2)
p_value <- round(test$p.value, digits = 4)

cgm_mean_median_plot <-
  temp_data %>%
  ggplot(aes(x = mean, y = median)) +
  geom_point(aes(color = subject_id),
             show.legend = FALSE,
             size = 5) +
  scale_color_manual(values = subject_color) +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  labs(x = "Mean CGM",
       y = "Median CGM") +
  annotate(
    "text",
    x = max(temp_data$mean),
    y = max(temp_data$median),
    label = paste("R = ", correlation, "\np-value = ", p_value),
    hjust = 1
  )

cgm_mean_median_plot

ggsave(
  cgm_mean_median_plot,
  filename = "cgm_mean_median_plot.pdf",
  width = 7,
  height = 7
)

ggsave(
  cgm_mean_median_plot,
  filename = "cgm_mean_median_plot.png",
  width = 7,
  height = 7
)

temp_data <-
  temp_data %>%
  dplyr::left_join(phenotype_info, by = "subject_id") %>%
  dplyr::filter(!is.na(BMI))

test <-
  cor.test(temp_data$mean,
           temp_data$BMI)
correlation <- round(test$estimate, digits = 2)
p_value <- round(test$p.value, digits = 4)

bmi_vs_mean_cgm_plot <-
  temp_data %>%
  ggplot(aes(BMI, mean)) +
  geom_point(aes(color = subject_id),
             show.legend = FALSE,
             size = 5) +
  scale_color_manual(values = subject_color) +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  labs(x = "BMI",
       y = "Mean CGM") +
  annotate(
    "text",
    x = max(temp_data$BMI),
    y = max(temp_data$mean),
    label = paste("R = ", correlation, "\np-value = ", p_value),
    hjust = 1
  )
bmi_vs_mean_cgm_plot


ggsave(
  bmi_vs_mean_cgm_plot,
  filename = "bmi_vs_mean_cgm_plot.pdf",
  width = 7,
  height = 7
)

ggsave(
  bmi_vs_mean_cgm_plot,
  filename = "bmi_vs_mean_cgm_plot.png",
  width = 7,
  height = 7
)
