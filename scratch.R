
# Setup
## Pre-load packages
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
my_work_folder <- "E:/TimCheung/Dropbox (Personal)/"
# my_work_folder <- "C:/Users/timhc/Dropbox/"

proj_folder <- "/Work/DLC/Cir_Sq_OpField_v2-TC-2022-10-18/vid_analyse/DLC_bout_csv_output/"

file_path_BL_paired <- "/DLC-bout_CJXT53_54_Baseline-S4-paired.csv"
file_path_probe_paired <- "/DLC-bout_CJXT53_54_Probe-S1-paired.csv"
file_path_BL_unpaired <- "/DLC-bout_CJXT53_54_Baseline-S4-unpaired.csv"
file_path_probe_unpaired <- "/DLC-bout_CJXT53_54_Probe-S1-unpaired.csv"

file_path_BL_paired <- paste0(my_work_folder, proj_folder, file_path_BL_paired)
file_path_probe_paired <- paste0(my_work_folder, proj_folder, file_path_probe_paired)
file_path_BL_unpaired <- paste0(my_work_folder, proj_folder, file_path_BL_unpaired)
file_path_probe_unpaired <- paste0(my_work_folder, proj_folder, file_path_probe_unpaired)

# Read CSV to dataframe
data_BL_paired <- readr::read_csv(file_path_BL_paired) %>%
    tibble::add_column(sess_name = "BL_paired", sess = "BL", context = "paired")

data_probe_paired <- readr::read_csv(file_path_probe_paired) %>%
    tibble::add_column(sess_name = "probe_S1_paired", sess = "probe_S1", context = "paired")

data_BL_unpaired <-  readr::read_csv(file_path_BL_unpaired) %>%
    tibble::add_column(sess_name = "BL_unpaired", sess = "BL", context = "unpaired")

data_probe_unpaired <-  readr::read_csv(file_path_probe_unpaired) %>%
    tibble::add_column(sess_name = "probe_S1_unpaired", sess = "probe_S1", context = "unpaired")

data_combo <- dplyr::bind_rows(data_BL_paired, data_probe_paired,
                               data_BL_unpaired, data_probe_unpaired)

# Re-order factors for plotting
my_col_factor <- c('MouseID', '6OHDA_side', 'sess_name', 'sess', 'context')
my_col_factor_order <- c('BL_paired', 'probe_S1_paired', 'BL_unpaired', 'probe_S1_unpaired')
data_combo[my_col_factor] <- lapply(data_combo[my_col_factor], factor)
data_combo <- data_combo %>%
    mutate(sess_name = forcats::fct_relevel(sess_name, my_col_factor_order))

# Pull out non-character columns (these are usually data variables) vs. character columns (these are usually factors)
my_col_fact <- data_combo %>% dplyr::summarise_all(class) %>% stringr::str_detect("character|factor")
my_colnames_factor <- colnames(data_combo[my_col_fact])
my_colnames_non_factor <- colnames(data_combo[!my_col_fact])


###########


exclude_cw_ccw <- TRUE
my_colour_set <- RColorBrewer::brewer.pal(9, 'Set1')
my_curr_var <- 1

for (i in 1:length(my_colnames_non_factor)) {

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
        by_sess_name_data_test <- data_test %>%
            group_by(sess_name) %>%
            calc_mean_sem()
        by_sess_name_data_test

        # plot graph
        my_plot <- quick_barplot(data_test, sess_name, var) +
            theme(axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.position = "none") +
            ggtitle(my_var) +
            scale_colour_manual(values=my_colour_set[c(1:4)]) +
            scale_fill_manual(values=my_colour_set[c(1:4)])
        print(my_plot)

        # t-tests, BL vs. Probe-S1
        pull_grp = "paired"
        data_test_diff_paired <- data_test %>%
            dplyr::filter(context==pull_grp) %>%
            dplyr::select(!c(sess_name, Ofield)) %>%
            tidyr::pivot_wider(names_from="sess", values_from="var") %>%
            dplyr::mutate(diff = probe_S1 - BL)

        pull_grp = "unpaired"
        data_test_diff_unpaired <- data_test %>%
            dplyr::filter(context==pull_grp) %>%
            dplyr::select(!c(sess_name, Ofield)) %>%
            tidyr::pivot_wider(names_from="sess", values_from="var") %>%
            dplyr::mutate(diff = probe_S1 - BL)

        cat("\n*** BL vs. Probe-S1, paired context ***\n")
        one_sample_ttest(data_test_diff_paired$diff)

        cat("\n*** BL vs. Probe-S1, unpaired context ***\n")
        one_sample_ttest(data_test_diff_unpaired$diff)

        cat("\n*** Diff (paired vs. unpaired) of Probe-BL diff score ***\n")
        one_sample_ttest(data_test_diff_paired$diff - data_test_diff_unpaired$diff)

        # t-tests, paired vs. unpaired
        pull_grp = "BL"
        data_test_diff_BL <- data_test %>%
            dplyr::filter(sess==pull_grp) %>%
            dplyr::select(!c(sess_name, Ofield)) %>%
            tidyr::pivot_wider(names_from="context", values_from="var") %>%
            dplyr::mutate(diff = paired - unpaired)

        pull_grp = "probe_S1"
        data_test_diff_probe <- data_test %>%
            dplyr::filter(sess==pull_grp) %>%
            dplyr::select(!c(sess_name, Ofield)) %>%
            tidyr::pivot_wider(names_from="context", values_from="var") %>%
            dplyr::mutate(diff = paired - unpaired)

        cat("\n*** Paired vs. unpaired context, BL sess ***\n")
        one_sample_ttest(data_test_diff_BL$diff)

        cat("\n*** Paired vs. unpaired context, Probe-S1 sess ***\n")
        one_sample_ttest(data_test_diff_probe$diff)

        cat("\n*** Diff (BL vs. Probe) of paired-unpaired diff score ***\n")
        one_sample_ttest(data_test_diff_probe$diff - data_test_diff_BL$diff)


        my_curr_var <- my_curr_var + 1
    }

}
