
library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(RColorBrewer)
source("./R/stats_summary.R")
source("./R/stats_funs.R")
source("./R/quick_plot.R")


## Folders
# my_work_folder <- "E:/TimCheung/Dropbox (Personal)/"
my_work_folder <- "C:/Users/timhc/Dropbox/"

proj_folder <- "/Work/DLC/Cir_Sq_OpField_v2-TC-2022-10-18/vid_analyse/DLC_bout_csv_output/"

file_path_BL_S1_1 <- "/DLC-bout_CJXT1-2_NoTreat-S1-1_timebins.csv"
file_path_BL_S1_2 <- "/DLC-bout_CJXT1-2_NoTreat-S1-2_timebins.csv"

file_path_BL_S1_1 <- paste0(my_work_folder, proj_folder, file_path_BL_S1_1)
file_path_BL_S1_2 <- paste0(my_work_folder, proj_folder, file_path_BL_S1_2)

# Read CSV to dataframe
data_BL_S1_1 <- readr::read_csv(file_path_BL_S1_1) %>%
    tibble::add_column(sess = "S1_1", sess_name = "S1_avg")

data_BL_S1_2 <- readr::read_csv(file_path_BL_S1_2) %>%
    tibble::add_column(sess = "S1_2", sess_name = "S1_avg")

data_combo <- dplyr::bind_rows(data_BL_S1_1, data_BL_S1_2)

data_combo <- data_combo %>%
    mutate(MouseID_sess_name = paste0(MouseID, "_", sess_name))

# Re-order factors for plotting
my_col_factor <- c('MouseID', '6OHDA_side', 'sess', 'sess_name', 'Ofield', 'start_time_in_min', 'end_time_in_min')
data_combo[my_col_factor] <- lapply(data_combo[my_col_factor], factor)

# Pull out non-character columns (these are usually data variables) vs. character columns (these are usually factors)
my_col_fact <- data_combo %>% dplyr::summarise_all(class) %>% stringr::str_detect("character|factor")
my_col_fact <- my_col_fact | colnames(data_combo) == "start_time_in_min" | colnames(data_combo) == "end_time_in_min"
my_colnames_factor <- colnames(data_combo[my_col_fact])
my_colnames_non_factor <- colnames(data_combo[!my_col_fact])

##########
exclude_cw_ccw <- FALSE
my_colour_set <- RColorBrewer::brewer.pal(9, 'Set1')
my_curr_var <- 1

for (i in 1){ #(i in 1:length(my_colnames_non_factor)) {

    # go thru each column variables
    my_colnames_test <- append(my_colnames_factor, my_colnames_non_factor[i])
    my_var <- my_colnames_non_factor[i]
    data_test <- data_combo[my_colnames_test] %>% rename(var = all_of(my_var))

    contains_cw_ccw = stringr::str_detect(my_var, "cw_") | stringr::str_detect(my_var, "ccw_")
    if (exclude_cw_ccw & contains_cw_ccw) {
        next
    } else {

        # print(paste0(my_curr_var, ": ", my_var))
        cat("\n*****", my_curr_var, ":", my_var, "*****\n")

        # summarize
        data_test <- data_test %>%
            group_by(MouseID, start_time_in_min) %>%
            calc_mean_sem() %>%
            select(MouseID, start_time_in_min, var_mean) %>%
            mutate(sess_name = 'S1_avg') %>%
            rename(var = var_mean)

        y_max = max(data_test$var, 0) * 1.05
        y_min = min(data_test$var, 0) * 1.05
        my_ylim <- ggplot2::ylim(y_min, y_max)

        data_plot <- data_test

        my_plot <- quick_lineplot_mean_sem_indi(
            data_plot, start_time_in_min, var, sess_name, MouseID) +
            xlab("time") +
            ggplot2::theme(axis.title.y = element_blank(), legend.position='none') +
            ggtitle(my_var) +
            my_ylim

        print(my_plot)

        my_curr_var <- my_curr_var + 1
    }

}






# library(readr)
# library(dplyr)
# library(stringr)
# library(tibble)
# library(tidyr)
# library(RColorBrewer)
# source("./R/stats_summary.R")
# source("./R/stats_funs.R")
# source("./R/quick_plot.R")
#
#
# ## Folders
# my_work_folder <- "E:/TimCheung/Dropbox (Personal)/"
# # my_work_folder <- "C:/Users/timhc/Dropbox/"
#
# proj_folder <- "/Work/DLC/Cir_Sq_OpField_v2-TC-2022-10-18/vid_analyse/DLC_bout_csv_output/"
#
# file_path_BL_paired <- "/DLC-bout_CJXT53_54_Baseline-S4-paired_timebins.csv"
# file_path_probe_paired <- "/DLC-bout_CJXT53_54_Probe-S1-paired_timebins.csv"
# file_path_LD1_S1 <- "/DLC-bout_CJXT53_54_LD1-S1-paired_timebins.csv"
# file_path_LD1_S5 <- "/DLC-bout_CJXT53_54_LD1-S5-paired_timebins.csv"
# file_path_BL_unpaired <- "/DLC-bout_CJXT53_54_Baseline-S4-unpaired_timebins.csv"
# file_path_probe_unpaired <- "/DLC-bout_CJXT53_54_Probe-S1-unpaired_timebins.csv"
#
# file_path_BL_paired <- paste0(my_work_folder, proj_folder, file_path_BL_paired)
# file_path_probe_paired <- paste0(my_work_folder, proj_folder, file_path_probe_paired)
# file_path_LD1_S1 <- paste0(my_work_folder, proj_folder, file_path_LD1_S1)
# file_path_LD1_S5 <- paste0(my_work_folder, proj_folder, file_path_LD1_S5)
# file_path_BL_unpaired <- paste0(my_work_folder, proj_folder, file_path_BL_unpaired)
# file_path_probe_unpaired <- paste0(my_work_folder, proj_folder, file_path_probe_unpaired)
#
# # Read CSV to dataframe
# data_BL_paired <- readr::read_csv(file_path_BL_paired) %>%
#     tibble::add_column(sess_name = "BL_paired", sess = "BL", context = "paired", .after = "6OHDA_side")
#
# data_probe_paired <- readr::read_csv(file_path_probe_paired) %>%
#     tibble::add_column(sess_name = "probe_S1_paired", sess = "probe_S1", context = "paired", .after = "6OHDA_side")
#
# data_BL_unpaired <-  readr::read_csv(file_path_BL_unpaired) %>%
#     tibble::add_column(sess_name = "BL_unpaired", sess = "BL", context = "unpaired", .after = "6OHDA_side")
#
# data_probe_unpaired <-  readr::read_csv(file_path_probe_unpaired) %>%
#     tibble::add_column(sess_name = "probe_S1_unpaired", sess = "probe_S1", context = "unpaired", .after = "6OHDA_side")
#
# data_LD1_S1 <- readr::read_csv(file_path_LD1_S1) %>%
#     tibble::add_column(sess_name = "LD1_S1", sess = "LD1_S1", context = "paired", .after = "6OHDA_side")
#
# data_LD1_S5 <- readr::read_csv(file_path_LD1_S5) %>%
#     tibble::add_column(sess_name = "LD1_S5", sess = "LD1_S5", context = "paired", .after = "6OHDA_side")
#
# data_combo <- dplyr::bind_rows(data_BL_paired, data_probe_paired,
#                                # data_LD1_S1, data_LD1_S5,
#                                data_BL_unpaired, data_probe_unpaired)
#
# data_combo <- data_combo %>%
#     mutate(MouseID_sess_name = paste0(MouseID, "_", sess_name))
#
# # Re-order factors for plotting
# my_col_factor <- c('MouseID', '6OHDA_side', 'sess_name', 'sess', 'context', 'start_time_in_min', 'end_time_in_min')
# # my_col_factor_order <- c('BL_paired', 'LD1_S1', 'LD1_S5', 'probe_S1_paired', 'BL_unpaired', 'probe_S1_unpaired')
# my_col_factor_order <- c('BL_paired', 'probe_S1_paired', 'BL_unpaired', 'probe_S1_unpaired')
# data_combo[my_col_factor] <- lapply(data_combo[my_col_factor], factor)
# data_combo <- data_combo %>%
#     mutate(sess_name = forcats::fct_relevel(sess_name, my_col_factor_order))
#
# # Pull out non-character columns (these are usually data variables) vs. character columns (these are usually factors)
# my_col_fact <- data_combo %>% dplyr::summarise_all(class) %>% stringr::str_detect("character|factor")
# my_col_fact <- my_col_fact | colnames(data_combo) == "start_time_in_min" | colnames(data_combo) == "end_time_in_min"
# my_colnames_factor <- colnames(data_combo[my_col_fact])
# my_colnames_non_factor <- colnames(data_combo[!my_col_fact])
#
#
# my_colour_set <- RColorBrewer::brewer.pal(9, 'Set1')
#
# exclude_cw_ccw <- TRUE
#
# my_curr_var <- 1
#
#
# for (i in 1:length(my_colnames_non_factor)) {
#
#     # go thru each column variables
#     my_colnames_test <- append(my_colnames_factor, my_colnames_non_factor[i])
#     my_var <- my_colnames_non_factor[i]
#     data_test <- data_combo[my_colnames_test] %>% rename(var = all_of(my_var))
#
#     contains_cw_ccw = stringr::str_detect(my_var, "cw_") | stringr::str_detect(my_var, "ccw_")
#     if (exclude_cw_ccw & contains_cw_ccw) {
#         next
#     } else {
#
#         cat("\n###", my_curr_var, ":", my_var, "###\n")
#         y_max = max(data_test$var)
#         y_min = min(data_test$var)
#         my_ylim <- ggplot2::ylim(y_min, y_max)
#
#         data_plot <- data_test %>%
#             filter(context == "paired")
#
#         my_plot <- quick_lineplot_mean_sem_indi(
#             data_plot, start_time_in_min, var, sess_name, MouseID) +
#             xlab("time") +
#             theme(axis.title.y = element_blank()) +
#             ggtitle(my_var)
#         my_plot <- my_plot +
#             scale_colour_manual(values=my_colour_set[c(1, 3)]) +
#             scale_fill_manual(values=my_colour_set[c(1, 3)]) +
#             my_ylim
#
#         print(my_plot)
#
#         data_plot <- data_test %>%
#             filter(context == "unpaired")
#
#         my_plot <- quick_lineplot_mean_sem_indi(
#             data_plot, start_time_in_min, var, sess_name, MouseID) +
#             xlab("time") +
#             theme(axis.title.y = element_blank()) +
#             ggtitle(my_var)
#         my_plot <- my_plot +
#             scale_colour_manual(values=my_colour_set[c(2, 4)]) +
#             scale_fill_manual(values=my_colour_set[c(2, 4)]) +
#             my_ylim
#
#         print(my_plot)
#
#
#         my_curr_var <- my_curr_var + 1
#     }
#
# }
#
#
# #######################
# paste0(my_curr_var, ": ", my_var)
#
# # summarize
# by_sess_name_data_test <- data_test %>%
#     group_by(sess_name) %>%
#     calc_mean_sem()
# by_sess_name_data_test
#
# # plot graph
# my_plot <- nb_barplot(data_test, sess_name, var) +
#     theme(axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
#           axis.title.x = element_blank(),
#           axis.title.y = element_blank(),
#           legend.position = "none") +
#     ggtitle(my_var)
# my_plot
#
#
# # t-tests, BL vs. Probe-S1
# pull_grp = "paired"
# data_test_diff_paired <- data_test %>%
#     dplyr::filter(context==pull_grp) %>%
#     dplyr::select(!sess_name) %>%
#     tidyr::pivot_wider(names_from="sess", values_from="var") %>%
#     dplyr::mutate(diff = probe_S1 - BL)
#
# pull_grp = "unpaired"
# data_test_diff_unpaired <- data_test %>%
#     dplyr::filter(context==pull_grp) %>%
#     dplyr::select(!sess_name) %>%
#     tidyr::pivot_wider(names_from="sess", values_from="var") %>%
#     dplyr::mutate(diff = probe_S1 - BL)
#
# print("BL vs. Probe-S1, paired context")
# one_sample_ttest(data_test_diff_paired$diff)
#
# print("BL vs. Probe-S1, unpaired context")
# one_sample_ttest(data_test_diff_unpaired$diff)
#
# print("Diff (paired vs. unpaired) of Probe-BL diff score")
# one_sample_ttest(data_test_diff_paired$diff - data_test_diff_unpaired$diff)
#
# # t-tests, paired vs. unpaired
# pull_grp = "BL"
# data_test_diff_BL <- data_test %>%
#     dplyr::filter(sess==pull_grp) %>%
#     dplyr::select(!sess_name) %>%
#     tidyr::pivot_wider(names_from="context", values_from="var") %>%
#     dplyr::mutate(diff = paired - unpaired)
#
# pull_grp = "probe_S1"
# data_test_diff_probe <- data_test %>%
#     dplyr::filter(sess==pull_grp) %>%
#     dplyr::select(!sess_name) %>%
#     tidyr::pivot_wider(names_from="context", values_from="var") %>%
#     dplyr::mutate(diff = paired - unpaired)
#
# print("Paired vs. unpaired context, BL sess")
# one_sample_ttest(data_test_diff_BL$diff)
#
# print("Paired vs. unpaired context, Probe-S1 sess")
# one_sample_ttest(data_test_diff_probe$diff)
#
# print("Diff (BL vs. Probe) of paired-unpaired diff score")
# one_sample_ttest(data_test_diff_probe$diff - data_test_diff_BL$diff)
#
#
#
# # overall ANOVA
# my_stats <- ez::ezANOVA(
#     data_test,
#     dv = .(var),
#     wid = .(MouseID),
#     within = .(sess, context),
#     detailed = TRUE,
#     type = 3,
#     return_aov = TRUE
# )
#
# print(my_stats[1])
# print(my_stats[2])
#
#
#
# oneway_anova_wn(data_test, subID="MouseID", wn_factor="sess", btw_factor = NULL, depvar="var")
