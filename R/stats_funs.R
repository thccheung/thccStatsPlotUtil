#' Run parametric and non-parametric ANOVA, with post-hoc comparisons
#'
#' @param df_data A tibble dataframe
#' @param subID Column name of subject ID
#' @param group Column name of group assignment
#' @param depvar Column name of dependent variable
#' @param remove_NaN Logical - whether to remove NaNs or NAs from the dependent
#' variable. Default = TRUE.
#'
#' @return Prints the results of parametric ANOVA and Tukey HSD post-hoc, and
#' then prints the results of non-parametric ANOVA and multiple-t-tests without
#' pooling variance, with Holm-Bonferroni correction
#'
#' @export
#'
oneway_anova <- function(df_data, subID="SubID",
                         group="Group", depvar="Depvar", remove_NaN=TRUE){

    # rename col names if necessary
    my_data <- df_data %>%
        dplyr::rename(subID = {{ subID }},
                      group = {{ group }},
                      depvar = {{ depvar }})

    if (remove_NaN) {
        na_data <- my_data %>%
            dplyr::ungroup() %>%
            dplyr::select(depvar) %>%
            dplyr::filter(is.na(depvar)) %>%
            dplyr::tally()

        if (na_data$n > 0) {
            cat("\nRemoved", na_data$n, "NA from data.\n")
        }

        my_data <- my_data %>%
            dplyr::filter(!is.na(depvar)) %>%
            dplyr::mutate(subID = factor(subID))
    }

    # run parametric ANOVA
    cat("\nParametric ANOVA\n")

    my_stats <- ez::ezANOVA(
        my_data,
        dv = .(depvar),
        wid = .(subID),
        between = .(group),
        detailed = TRUE,
        type = 3,
        return_aov = TRUE
    )

    # Print ANOVA results
    print(my_stats[1])

    # Print Levene's variance homogeneity test
    print(my_stats[2])

    # Print Tukey's HSD post hoc test
    print(stats::TukeyHSD(my_stats[[3]]))


    # run non-parametric ANOVA
    cat("\nNon-parametric ANOVA (Kruskal-wallis test)\n")
    print(stats::kruskal.test(depvar ~ group, data = my_data))

    # Re-run t-test without pooled variance
    print('Run multiple t-tests without pooling variances, using "Holm" correction')
    print(
        stats::pairwise.t.test(my_data$depvar, my_data$group,
                        p.adjust.method = "holm",
                        pool.sd = FALSE, paired = FALSE,
                        alternative = c("two.sided")
        )
    )

}


#' Run one-sample t-test, based on normality test result. If fail, run
#' non-parameteric Mann-Whitney-Wilcoxon test. Otherwise, run parametric t-test.
#' Input is numeric vector.
#'
#' @param x A vector of numbers
#' @param mu A number of true value of the mean to test against
#'
#' @return Prints the results of non-parametric t-test (Mann-Whitney-
#' Wilcoxon) if failed Shapiro-Wilk normality test. Otherwise prints the
#' results of parametric t-test
#'
#' @export
#'
one_sample_ttest <- function(x, mu = 0) {

    ncount_non_na = sum(!is.na(x))

    # run Shapiro-Wilk normality test
    if (ncount_non_na >= 3 && ncount_non_na <= 5000) {
        cat("\nFirst test normality to see if non-parametric t-test needed\n")
        norm_test <- stats::shapiro.test(x)
        print(norm_test)
    } else {
        cat("\nDid not run Shapiro-wilk normality test due to N being", ncount_non_na, ".\n")
        cat("\nShapiro-wilk normality test requires N to be between 3-5000.\n")
        norm_test <- list(p.value = 0)
    }

    if (ncount_non_na <= 1) {
        cat("\nDid not run one-sample t-test due to too few N.\n")
        return()
    }

    if (norm_test$p.value < 0.05) {
        cat("\nRunning Mann-Whitney-Wilcoxon (non-param T-test) because failed Shapiro-Wilk\n")
        my_stat = stats::wilcox.test(x, alternative = c("two.sided"),
                              mu = 0, paired = FALSE, exact = TRUE, correct = TRUE,
                              conf.int = TRUE, conf.level = 0.95)
        print(my_stat)
    } else {
        cat("\nPassed Shapiro-Wilk, so run parametric t-test\n")
        my_stat = stats::t.test(x, alternative = c("two.sided"),
                         mu = 0, paired = FALSE)
        print(my_stat)
    }

}




#' Run multiple parametric one-sample t-tests against a fixed constant, then
#' give P-adjusted if specified
#'
#' @param df_data A tibble dataframe
#' @param group Column name of group assignment
#' @param depvar Column name of dependent variable
#' @param p_adj_method Correction method for multiple comparison. Default =
#' "Depvar". See stats::p.adjust.
#' @param mu A number of true value of the mean to test against
#'
#' @return Prints the results of multiple parametric t-tests, with p-value
#' adjusted based on the user-defined method.
#'
#' @export
#'
run_multi_1sample_ttest <- function(df_data, group="Group", depvar="Depvar",
                                    p_adj_method="holm", mu=0) {

    my_grp <- df_data %>%
        dplyr::ungroup() %>%
        dplyr::select({{ group }})

    # Pull out data by group, then pull out just Depvar
    my_data_split = split(df, my_grp) %>%
        purrr::map({{ depvar }})

    # Use Purr's "Map" function to run t.test in vectorized form
    my_ttest_out <- purrr::map(my_data_split,
                               ~stats::t.test(., alternative = c("two.sided"),
                                              mu = mu, paired = FALSE))

    print(sprintf("One-sample t-tests against %s, by Grp", mu))
    print(my_ttest_out)

    # Pull out p.value
    my_pval <- purrr::map(my_ttest_out, "p.value") %>%
        unlist() %>%
        stats::p.adjust(method = p_adj_method)

    print(sprintf("P-values after correcting for multiple-comparisons using %s
                  method", p_adj_method))
    print(my_pval)

}


#' Run two-sample t-test. First run F-test to test equal variance. If fail, run
#' Welch t-test with non-pooled variance. Otherwise pool variance.
#'
#' @param data_1
#' @param data_2
#'
#' @return Prints the results of two-sample t-test.
#'
#' @export
#'
two_sample_ttest <- function(data_1, data_2) {

    data_1 <- unlist(data_1)
    data_2 <- unlist(data_2)

    # Use F-test to test variance difference
    F_out = stats::var.test(data_1, data_2, ratio = 1,
                            alternative = "two.sided")
    F_pval = F_out$p.value
    print(F_out)

    # Run 2-sample t-test
    # If variance is the same, run t-test with pooled variance
    # Other use individual variance (less power)
    if (F_pval >= 0.05) {
        print("Using pooled variance because variance difference non-significant")
        T_out = stats::t.test(data_1, data_2, alternative = c("two.sided"),
                       mu = 0, paired = FALSE, var.equal = TRUE,
                       conf.level = 0.95)
    } else {
        print("Not using pooled variance because variance difference is significant")
        T_out = stats::t.test(data_1, data_2, alternative = c("two.sided"),
                       mu = 0, paired = FALSE, var.equal = FALSE,
                       conf.level = 0.95)
    }

    return(T_out)
    print(T_out)

}


oneway_anova_wn <- function(df_data, subID="SubID", wn_factor="wn_factor",
                            btw_factor=NULL, depvar="Depvar", remove_NaN=TRUE) {

    # rename col names if necessary
    my_data <- df_data %>%
        dplyr::rename(subID = {{ subID }},
                      wn_factor = {{ wn_factor }},
                      btw_factor = {{ btw_factor }},
                      depvar = {{ depvar }})

    if (remove_NaN) {
        na_data <- my_data %>%
            dplyr::ungroup() %>%
            dplyr::select(depvar) %>%
            dplyr::filter(is.na(depvar)) %>%
            dplyr::tally()

        if (na_data$n > 0) {
            cat("\nRemoved", na_data$n, "NA from data.\n")
        }

        my_data_na <- my_data %>%
            dplyr::filter(is.na(depvar)) %>%
            dplyr::select(subID) %>%
            dplyr::distinct(subID)

        if (nrow(my_data_na) > 0) {
            my_data <- my_data %>%
                dplyr::anti_join(my_data_na, by = join_by(subID)) %>%
                dplyr::mutate(subID = factor(subID))
        }
    }

    nsub <- nrow(distinct(my_data, subID))
    if (nsub <= 1) {

        cat("\nDid not run stats because there is only", nsub, "subjects\n")
        return()

    }

    # run parametric ANOVA
    cat("\nParametric ANOVA\n")

    if (is.null(btw_factor)) {
        my_stats <- ez::ezANOVA(
            my_data,
            dv = .(depvar),
            wid = .(subID),
            within = .(wn_factor),
            detailed = TRUE,
            type = 3,
            return_aov = TRUE
        )
    } else {
        my_stats <- ez::ezANOVA(
            my_data,
            dv = .(depvar),
            wid = .(subID),
            within = .(wn_factor),
            between = .(btw_factor),
            detailed = TRUE,
            type = 3,
            return_aov = TRUE
        )
    }

    print(my_stats)

    # run non-parametric ANOVA
    cat("\nNon-parametric ANOVA (Kruskal-wallis test)\n")
    print(stats::friedman.test(depvar ~ wn_factor | subID, data = my_data))

    # Re-run t-test without pooled variance
    print('Run multiple t-tests without pooling variances, using "Holm" correction')
    print(
        stats::pairwise.t.test(my_data$depvar, my_data$wn_factor,
                               p.adjust.method = "holm",
                               pool.sd = FALSE, paired = TRUE,
                               alternative = c("two.sided")
        )
    )
}
