#' Calculate mean and SEM from a grouped dataframe, with NA removed from SEM
#' calculation.
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
        dplyr::summarise(
            dplyr::across(where(is.double)|where(is.integer),
                          list(mean = ~ mean(.x, na.rm=TRUE),
                               sd = ~ sd(.x, na.rm=TRUE),
                               sem = ~ sd(.x, na.rm=TRUE)/sqrt(sum(!is.na(.x))),
                               median = ~ median(.x, na.rm=TRUE)
                          )),
            ncount = dplyr::n()) %>%
        dplyr::ungroup()

    return(df)
}
