if(!require(r4projects)) {
  remotes::install_github("jaspershen/r4projects")
}

if (!require(tidyverse)) {
  install.packages(tidyverse)
}

if (!require(ggplot2)) {
  install.packages(ggplot2)
}

wearable_color <-
  c(
    "CGM" = ggsci::pal_jama()(n = 7)[1],
    "Step" = ggsci::pal_jama()(n = 7)[2],
    "HR" = ggsci::pal_jama()(n = 7)[3]
  )

match_sample <-
  function(time1,
           time2,
           time_window = 1 / 60,
           ##unit is hour
           slice_length = 10000) {
    if (length(time2) > slice_length) {
      starts <-
        seq(1, length(time2), by = slice_length)
      ends <-
        pmin(starts + slice_length - 1, length(time2))
      slices <-
        data.frame(starts,
                   ends)
    } else{
      slices <-
        data.frame(starts = 1,
                   ends = length(time2))
    }
    
    result <-
      purrr::map(seq_len(nrow(slices)),
                 function(idx) {
                   result <-
                     outer(
                       time1,
                       time2[slices$starts[idx]:slices$ends[idx]],
                       FUN = function(x, y) {
                         as.numeric(difftime(x, y, units = "hours"))
                       }
                     )
                   
                   result <-
                     which(abs(result) < time_window,
                           arr.ind = TRUE) %>%
                     as.data.frame()
                   
                   colnames(result) <- c("index1",
                                         "index2")
                   
                   result$index2 <-
                     result$index2 + (idx - 1) * slice_length
                   
                   result$time1 <-
                     time1[result$index1]
                   
                   result$time2 <-
                     time2[result$index2]
                   
                   result$diff_time <-
                     difftime(result$time1,
                              result$time2, units = "hours") %>%
                     as.numeric()
                   
                   result
                   
                 })
    
    result <-
      result %>%
      do.call(rbind, .) %>%
      as.data.frame()
    result
  }





alignment_plot =
  function(object,
           time1,
           time2,
           x,
           y,
           x_color = "#631879FF",
           y_color = "#E377C2FF",
           x_name = "x",
           y_name = "y",
           x_limit = c(1, 1000),
           non_matched_point_size = 0.1,
           x_point_size = 3,
           y_point_size = 1,
           integrated = FALSE,
           add_connect_line = FALSE,
           add_point = FALSE,
           time_gap = 4) {
    # which = match.arg(which)
    
    # if (which == "global") {
    #   idx = object@global_idx
    #   shift = object@shift_time[object@which_global_idx]
    #   correlation = round(object@global_cor, 3)
    # } else{
    #   idx = object@max_idx
    #   shift = object@shift_time[object@which_max_idx]
    #   correlation = round(object@max_cor, 3)
    # }
    
    # time1 = object@time1
    # time2 = object@time2
    # x = object@x
    # y = object@y
    
    # if (x_limit[2] > length(x)) {
    #   x_limit[2] = length(x)
    # }
    
    # if (max(x) > min(y)) {
    #   x = x - (max(x) - min(y))
    # }
    
    #######non integrated
    if (integrated) {
      time2 =
        purrr::map(idx, function(x) {
          mean(time2[c(head(x, 1), tail(x, 1))])
        }) %>%
        unlist() %>%
        lubridate::as_datetime(tz = "America/Los_Angeles")
      
      y =
        purrr::map(idx, function(x) {
          mean(y[x])
        }) %>%
        unlist()
      
      if (max(x, na.rm = TRUE) > min(y, na.rm = TRUE)) {
        x = x - (max(x, na.rm = TRUE) - min(y, na.rm = TRUE))
      }
      
      x2 = data.frame(time = time1,
                      value = x,
                      class = x_name)
      y2 = data.frame(time = time2,
                      value = y,
                      class = y_name)
      
      x2$matched = 'NO'
      x2$matched[which(unlist(lapply(idx, length)) > 0)] =  "YES"
      
      y2$matched = 'YES'
      
      y2 =
        y2 %>%
        dplyr::filter(!is.na(time))
      
      value = rbind(x2, y2)
      
      value =
        value %>%
        dplyr::mutate(matched =
                        case_when(matched == "YES" ~ class,
                                  matched == "NO" ~ "NO"))
    } else{
      x2 = data.frame(time = time1,
                      value = x,
                      class = x_name)
      y2 = data.frame(time = time2,
                      value = y,
                      class = y_name)
      
      x2$matched = 'NO'
      # x2$matched[which(unlist(lapply(idx, length)) > 0)] =  "YES"
      
      y2$matched = 'NO'
      # y2$matched[unique(unlist(idx))] = "YES"
      
      value = rbind(x2, y2)
      
      value =
        value %>%
        dplyr::mutate(matched =
                        case_when(matched == "YES" ~ class,
                                  matched == "NO" ~ "NO"))
    }
    
    if (x_limit[2] > nrow(value)) {
      x_limit[2] <-
        nrow(value)
    }
    
    time1 = sort(unique(value$time))[x_limit[1]]
    time2 = sort(unique(value$time))[x_limit[2]]
    
    value =
      value %>%
      dplyr::filter(time >= time1 & time < time2)
    
    sun_rise =
      lubridate::ymd_hms(paste(unique(lubridate::date(value$time)), c("6:00:00")),
                         tz = lubridate::tz(value$time))
    sun_set =
      lubridate::ymd_hms(paste(unique(lubridate::date(value$time)), c("18:00:00")),
                         tz = lubridate::tz(value$time))
    
    day_night_df =
      data.frame(start = sun_rise,
                 end = sun_set) %>%
      dplyr::filter()
    
    plot =
      ggplot() +
      geom_rect(
        mapping = aes(
          xmin = start,
          xmax = end,
          ymin = -Inf,
          ymax = Inf
        ),
        fill = "lightyellow",
        data = day_night_df,
        show.legend = FALSE
      ) +
      geom_line(
        data = value,
        aes(
          x = time,
          y = value,
          group = class,
          color = class
        ),
        show.legend = TRUE
      ) +
      facet_grid(rows = vars(class), scales = "free_y")
    
    color_list <-
      c(unname(x_color),
        unname(y_color),
        'NO' = "grey")
    
    names(color_list) <-
      c(x_name, y_name, 'NO')
    
    size_list <-
      c(x_point_size,
        y_point_size,
        'NO' = non_matched_point_size)
    
    names(size_list) <-
      c(x_name, y_name, 'NO')
    
    plot =
      plot +
      scale_size_manual(values = size_list) +
      scale_x_datetime(
        breaks = scales::date_breaks(paste(time_gap, "hour")),
        date_labels = "%a %H:%M",
        timezone = "America/Los_Angeles",
        limits = c(min(time1[!is.na(time1)][x_limit[1]],
                       time2[!is.na(time2)][x_limit[1]]),
                   max(time1[!is.na(time1)][x_limit[2]],
                       time2[!is.na(time2)][x_limit[2]]))
      ) +
      labs(x = "",
           y = "") +
      guides(color = guide_legend(title = "",
                                  override.aes = list(size = 3)),
             size = "none") +
      scale_color_manual(values = color_list,
                         labels = c("x" = x_name,
                                    "y" = y_name,
                                    "NO" = "Non matched"))
    
    if (add_point) {
      plot =
        plot +
        geom_point(
          data = value,
          aes(
            x = time,
            y = value,
            group = class,
            color = matched,
            size = matched
          ),
          shape = 16,
          show.legend = TRUE
        )
    }
    
    if (add_connect_line) {
      ###get the segment data
      segment_data =
        purrr::map(seq_along(idx), function(i) {
          if (length(idx[[i]]) > 0) {
            data.frame(
              time1 = time1[i],
              x = x[i],
              time2 = time2[idx[[i]]],
              y = y[idx[[i]]]
            )
          }
        }) %>%
        dplyr::bind_rows()
      
      plot =
        plot +
        geom_segment(
          data = segment_data,
          aes(
            x = time1,
            y = x,
            xend = time2,
            yend = y
          ),
          color = x_color,
          show.legend = FALSE
        )
    }
    
    plot =
      plot +
      base_theme +
      theme(
        legend.position = "top",
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          size = 10
        ),
        axis.line.x = element_blank(),
        plot.margin = margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0,
          unit = "pt"
        )
      )
    plot
  }


time_plot_multiple =
  function(x,
           x_limit = c(1, 1000000),
           class_color,
           time_gap = 4) {
    if (x_limit[2] > length(sort(unique(x$accurate_time)))) {
      x_limit[2] <-
        length(sort(unique(x$accurate_time)))
    }
    
    time1 = sort(unique(x$accurate_time))[x_limit[1]]
    time2 = sort(unique(x$accurate_time))[x_limit[2]]
    
    x =
      x %>%
      dplyr::filter(accurate_time >= time1 & accurate_time < time2)
    
    sun_rise =
      lubridate::ymd_hms(paste(unique(lubridate::date(x$accurate_time)), c("6:00:00")),
                         tz = lubridate::tz(x$accurate_time))
    sun_set =
      lubridate::ymd_hms(paste(unique(lubridate::date(x$accurate_time)), c("18:00:00")),
                         tz = lubridate::tz(x$accurate_time))
    
    day_night_df =
      data.frame(start = sun_rise,
                 end = sun_set) %>%
      dplyr::filter()
    
    plot =
      ggplot() +
      geom_rect(
        mapping = aes(
          xmin = start,
          xmax = end,
          ymin = -Inf,
          ymax = Inf
        ),
        fill = "lightyellow",
        data = day_night_df,
        show.legend = FALSE
      ) +
      geom_line(
        data = x,
        aes(
          x = accurate_time,
          y = value,
          group = class,
          color = class
        ),
        show.legend = FALSE
      ) +
      facet_grid(rows = vars(class), scales = "free_y")
    
    plot =
      plot +
      scale_x_datetime(
        breaks = scales::date_breaks(paste(time_gap, "hour")),
        date_labels = "%a %H:%M",
        timezone = "America/Los_Angeles"
      ) +
      labs(x = "",
           y = "") +
      guides(color = guide_legend(title = "",
                                  override.aes = list(size = 3)),
             size = "none") +
      scale_color_manual(values = class_color)
    
    plot =
      plot +
      base_theme +
      theme(
        legend.position = "top",
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          size = 10
        ),
        axis.line.x = element_blank(),
        plot.margin = margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0,
          unit = "pt"
        )
      )
    plot
  }



base_theme =
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text =  ggplot2::element_text(size = 12),
    axis.title =  ggplot2::element_text(size = 13),
    panel.grid.minor =  ggplot2::element_blank(),
    plot.background =  ggplot2::element_rect(fill = "transparent"),
    panel.background =  ggplot2::element_rect(fill = "transparent"),
    strip.text =  ggplot2::element_text(size = 12)
  )




# Create the vector with color values
subject_color <- c("#FF6F00", "#EB4E00", "#D82D00", "#C01405", "#7B3F3C", "#366B73", "#09889F", "#396E9C",
                  "#685399", "#854998", "#746698", "#638398", "#708E8D", "#A97C71", "#E26B55", "#E97762",
                  "#BF9E97", "#94C6CB", "#9DC9D5", "#C7B2C1", "#F29BAD", "#D07F88", "#8D605B", "#4A412E",
                  "#5B6954", "#82A28F", "#A9DCCA", "#7FB5A9", "#4C847E", "#1A5354")

# Assign names to the vector elements
names(subject_color) <- c("DCAMPAAA", "DCAMPAAB", "DCAMPAAC", "DCAMPAAD", "DCAMPAAE", "DCAMPAAF", "DCAMPAAG", "DCAMPAAH",
                         "DCAMPAAI", "DCAMPAAJ", "DCAMPAAK", "DCAMPAAL", "DCAMPAAM", "DCAMPAAO", "DCAMPAAP", "DCAMPAAQ",
                         "DCAMPAAR", "DCAMPAAS", "DCAMPAAT", "DCAMPAAU", "DCAMPAAV", "DCAMPAAW", "DCAMPAAX", "DCAMPAAY",
                         "DCAMPAAZ", "DCAMPABA", "DCAMPABB", "DCAMPABC", "DCAMPABH", "DCAMPABK")







# 
# calculate_lagged_correlation <-
#   function(x,
#            y,
#            time1,
#            time2,
#            time_tol = 1,
#            step = 1 / 60,
#            min_matched_sample = 10,
#            progressbar = TRUE,
#            all_idx = NULL,
#            threads = 10,
#            cor_method = c("spearman", "pearson"),
#            match_method = c("loop", "vector")) {
#     cor_method = match.arg(cor_method)
#     match_method <-
#       match.arg(match_method)
#     if (length(x) == 0 | length(y) == 0) {
#       return(NULL)
#     }
#     ##time_tol unit is hour
#     ##step unit is hour
#     x = as.numeric(x)
#     
#     y = as.numeric(y)
#     
#     time_window1 <-
#       seq(from = step / 2, to = time_tol, by = step)
#     
#     time_window2 <-
#       -rev(seq(from = step / 2, to = time_tol, by = step))
#     
#     time_window <-
#       sort(c(time_window2, time_window1))
#     
#     temp_fun1 <-
#       function(temp_idx,
#                time_window,
#                x,
#                y,
#                time1,
#                time2) {
#         idx =
#           time1 %>%
#           purrr::map(function(temp_time1) {
#             # cat(match(temp_time1, time1), " ")
#             diff_time =
#               difftime(temp_time1, time2, units = "hours")
#             which(diff_time > time_window[temp_idx] &
#                     diff_time <= time_window[temp_idx + 1])
#           })
#       }
#     
#     temp_fun2 <-
#       function(temp_idx,
#                time_window,
#                x,
#                y,
#                time1,
#                time2) {
#         idx <-
#           match_sample(
#             time1 = time1,
#             time2 = time2 + mean(c(time_window[temp_idx],
#                                    time_window[temp_idx + 1])) * 3600,
#             time_window = step / 2,
#             slice_length = 10000
#           )
#         idx$time2 <-
#           idx$time2 - mean(c(time_window[temp_idx],
#                              time_window[temp_idx + 1]) * 3600)
#         
#         idx$diff_time <-
#           idx$diff_time + mean(c(time_window[temp_idx],
#                                  time_window[temp_idx + 1]))
#         idx
#       }
#     
#     if (get_os() == "windows") {
#       bpparam = BiocParallel::SnowParam(workers = threads,
#                                         progressbar = TRUE)
#     } else{
#       bpparam = BiocParallel::MulticoreParam(workers = threads,
#                                              progressbar = TRUE)
#     }
#     
#     old_all_idx <-
#       all_idx
#     
#     if (is.null(all_idx)) {
#       if (match_method == "loop") {
#         all_idx <-
#           BiocParallel::bplapply(
#             X = seq_along(time_window)[-length(time_window)],
#             FUN = temp_fun1,
#             time_window = time_window,
#             x = x,
#             y = y,
#             time1 = time1,
#             time2 = time2,
#             BPPARAM = bpparam
#           )
#       } else{
#         all_idx <-
#           BiocParallel::bplapply(
#             X = seq_along(time_window)[-length(time_window)],
#             FUN = temp_fun2,
#             time_window = time_window,
#             x = x,
#             y = y,
#             time1 = time1,
#             time2 = time2,
#             BPPARAM = bpparam
#           )
#         
#         all_idx <-
#           all_idx %>%
#           purrr::map(function(x) {
#             diff_index1 <-
#               setdiff(seq_len(length(time1)),
#                       x$index1)
#             if (length(diff_index1) > 0) {
#               additional_data <-
#                 data.frame(
#                   index1 = diff_index1,
#                   index2 = NA,
#                   time1 = NA,
#                   time2 = NA,
#                   diff_time = NA
#                 )
#               x <-
#                 rbind(x, additional_data) %>%
#                 dplyr::arrange(index1)
#               
#             }
#             
#             unique(x$index1) %>%
#               purrr::map(function(idx1) {
#                 x$index2[x$index1 == idx1][!is.na(x$index2[x$index1 == idx1])]
#               })
#           })
#       }
#     }
#     
#     all_cor_result <-
#       purrr::map(all_idx, function(idx) {
#         temp_y <-
#           lapply(idx, function(x) {
#             mean(y[x])
#           }) %>%
#           unlist()
#         
#         temp_x = x[which(!is.na(temp_y))]
#         temp_y = temp_y[which(!is.na(temp_y))]
#         if (length(temp_x) < min_matched_sample) {
#           return(NA)
#         } else{
#           tryCatch(
#             expr = cor.test(temp_x, temp_y, method = cor_method),
#             error = function(na) {
#               return(NA)
#             }
#           )
#         }
#       })
#     
#     all_cor_p =
#       all_cor_result %>%
#       purrr::map(function(x) {
#         # if (class(all_cor_result[[1]]) != "htest") {
#         if (!is(all_cor_result[[1]], "htest")) {
#           return(NA)
#         } else{
#           x$p.value
#         }
#       }) %>%
#       unlist()
#     
#     all_cor <-
#       all_cor_result %>%
#       purrr::map(function(x) {
#         # if (class(all_cor_result[[1]]) != "htest") {
#         if (!is(all_cor_result[[1]], "htest")) {
#           return(NA)
#         } else{
#           x$estimate
#         }
#       }) %>%
#       unlist() %>%
#       unname()
#     
#     which_max_idx =
#       which.max(abs(all_cor))
#     
#     max_idx <-
#       all_idx[[which_max_idx]]
#     
#     shift_time =
#       paste("(",
#             paste(round(time_window[-length(time_window)] * 60, 2),
#                   round(time_window[-1] * 60, 2), sep = ','),
#             "]", sep = "")
#     
#     which_global_idx =
#       purrr::map(shift_time, function(x) {
#         x =
#           stringr::str_replace(x, "\\(", "") %>%
#           stringr::str_replace("\\]", "") %>%
#           stringr::str_split(",") %>%
#           `[[`(1) %>%
#           as.numeric()
#         x[1] < 0 & x[2] > 0
#       }) %>%
#       unlist() %>%
#       which()
#     
#     global_idx <-
#       all_idx[[which_global_idx]]
#     
#     global_cor <-
#       all_cor[which_global_idx]
#     global_cor_p = all_cor_p[which_global_idx]
#     
#     parameter =
#       new(
#         Class = "tidymass_parameter",
#         pacakge_name = "laggedcor",
#         function_name = "calculate_lagged_correlation",
#         parameter = list(
#           time_tol = time_tol,
#           step = step,
#           min_matched_sample = min_matched_sample,
#           progressbar = progressbar,
#           threads = threads,
#           cor_method = cor_method
#         ),
#         time = Sys.time()
#       )
#     
#     object <- new(
#       Class = "lagged_cor_result",
#       x = x,
#       time1 = time1,
#       y = y,
#       time2 = time2,
#       idx = all_idx,
#       all_cor = all_cor,
#       all_cor_p = all_cor_p,
#       shift_time = shift_time,
#       which_max_idx = which_max_idx,
#       which_global_idx = which_global_idx,
#       max_idx = max_idx,
#       max_cor = all_cor[which_max_idx],
#       global_idx = global_idx,
#       global_cor = global_cor,
#       parameter = parameter
#     )
#     
#     return(object)
#     
#   }
