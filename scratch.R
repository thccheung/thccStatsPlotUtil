library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)


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

data_BL_paired <- readr::read_csv(file_path_BL_paired) %>%
    tibble::add_column(sess_name = "BL_paired", sess = "BL", context = "paired")
data_probe_paired <- readr::read_csv(file_path_probe_paired) %>%
    tibble::add_column(sess_name = "probe_S1_paired", sess = "probe_S1", context = "paired")
data_BL_unpaired <-  readr::read_csv(file_path_BL_unpaired) %>%
    tibble::add_column(sess_name = "BL_unpaired", sess = "BL", context = "unpaired")
data_probe_unpaired <-  readr::read_csv(file_path_probe_unpaired) %>%
    tibble::add_column(sess_name = "probe_S1_unpaired", sess = "probe_S1", context = "unpaired")

data_cat <- dplyr::bind_rows(data_BL_paired, data_probe_paired, data_BL_unpaired, data_probe_unpaired)

my_col_factor <- c('MouseID', '6OHDA_side', 'sess_name', 'sess', 'context')
data_cat[my_col_factor] <- lapply(data_cat[my_col_factor], factor)

my_col_char <- data_cat %>% dplyr::summarise_all(class) %>% stringr::str_detect("character|factor")
my_colnames_char <- colnames(data_cat[my_col_char])
my_colnames_non_char <- colnames(data_cat[!my_col_char])

my_test_i <- 1
my_colnames_test <- append(my_colnames_char, my_colnames_non_char[my_test_i])
my_var <- my_colnames_non_char[my_test_i]
data_test <- data_cat[my_colnames_test] %>% rename(var = all_of(my_var))

# summarize
by_sess_name_data_test <- data_test %>%
    group_by(sess_name) %>%
    calc_mean_sem()

print(my_var)
print(by_sess_name_data_test)

# overall ANOVA
my_stats <- ez::ezANOVA(
    data_test,
    dv = .(var),
    wid = .(MouseID),
    within = .(sess, context),
    detailed = TRUE,
    type = 3,
    return_aov = TRUE
)

print(my_stats[1])

pull_grp = "paired"
data_test %>%
    dplyr::filter(context==pull_grp) %>%
    dplyr::select(!sess_name) %>%
    tidyr::pivot_wider(names_from="sess", values_from="var") %>%
    dplyr::mutate(diff = probe_S1 - BL) %>%
    calc_mean_sem()
