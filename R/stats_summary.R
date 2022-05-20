#' Calculate mean and SEM from a grouped dataframe
#'
#' @param df_grouped A dataframe, grouped by appropriate factor(s)
#'
#' @return A dataframe, with each group's mean, SD, SEM
#'
#' @example
#' calc_mean_sem(my_dataframe)
#'
#' @export
#'
calc_mean_sem <- function(df_grouped) {

    df <- df_grouped %>%
        dplyr::summarise(dplyr::across(where(is.double), list(
            mean = ~ mean(.x, na.rm=TRUE), sd = ~ sd(.x, na.rm=TRUE),
            sem = ~ sd(.x, na.rm=TRUE)/sqrt(sum(!is.na(.x)))
            )),
            ncount = dplyr::n()) %>%
        dplyr::ungroup()

    return(df)
}
