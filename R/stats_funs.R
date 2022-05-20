#library(ez)
#library(tidyverse)

#' Run parametric and non-parametric ANOVA, with post-hoc comparisons
#'
#' @param df_data A tibble dataframe
#' @param SubID Column name of subject ID
#' @param Group Column name of group assignment
#' @param Depvar Column name of dependent variable
#' @param remove_NaN Logical - whether to remove NaNs or NAs from the dependent variable. Default = TRUE.
#'
oneway_anova <- function(df_data, SubID="SubID",
                         Group="Group", Depvar="Depvar", remove_NaN=TRUE){

    # rename colnames if necessary
    my_data <- df_data %>%
        dplyr::rename(SubID = {{ SubID }},
                      Group = {{ Group }},
                      Depvar = {{ Depvar }})

    if (remove_NaN) {
        na_data <- my_data %>%
            dplyr::ungroup() %>%
            dplyr::select(Depvar) %>%
            dplyr::filter(is.na(Depvar)) %>%
            dplyr::tally()

        if (na_data$n > 0) {
            cat("\nRemoved", na_data$n, "NA from data.\n")
        }

        my_data <- my_data %>%
            filter(!is.na(Depvar)) %>%
            mutate(SubID = factor(SubID))
    }

    # run parametric ANOVA
    cat("\nParametric ANOVA\n")

    my_stats <- ezANOVA(
        my_data,
        dv = .(Depvar),
        wid = .(SubID),
        between = .(Group),
        detailed=TRUE,
        type= 3,
        return_aov = TRUE
    )

    # Print ANOVA reults
    print(my_stats[1])

    # Print Levene's variance homogeneity test
    print(my_stats[2])

    # Print Tukey's HSD post hoc test
    print(TukeyHSD(my_stats[[3]]))


    # run non-parametric ANOVA
    cat("\nNon-parametric ANOVA (Kruskal-wallis test)\n")
    print(kruskal.test(Depvar ~ Group, data = my_data))

    # Re-run t-test without pooled variance
    print('Run multiple t-tests without pooling variances, using "Holm" correction')
    print(
        pairwise.t.test(my_data$Depvar, my_data$Group,
                        p.adjust.method = "holm",
                        pool.sd = FALSE, paired = FALSE,
                        alternative = c("two.sided")
        )
    )

}


#' Run one-sample t-test, based on normality test result. If fail, run
#' non-parameteric Mann-Whitney-Wilcoxon test. Otherwise, run parametric t-test.
#' Input is numeric vector.

one_sample_ttest <- function(x) {

    # run shapiro-wilk normality test
    cat("\nFirst test normality to see if non-parametric t-test needed\n")
    norm_test <- shapiro.test(x)
    print(norm_test)


    if (norm_test$p.value < 0.05) {
        cat("\nRunning Mann-Whitney-Wilcoxon (non-param T-test) because failed Shapiro-Wilk\n")
        my_stat = wilcox.test(x, alternative = c("two.sided"),
                              mu = 0, paired = FALSE, exact = TRUE, correct = TRUE,
                              conf.int = TRUE, conf.level = 0.95)
        print(my_stat)
    } else {
        cat("\nPassed Shapiro-Wilk, so run parametric t-test\n")
        my_stat = t.test(x, alternative = c("two.sided"),
                         mu = 0, paired = FALSE)
        print(my_stat)
    }

}


#' Run multiple 1 sample t-tests against a fixed constant, then give P-adjusted
#' if specified
run_multi_1sample_ttest <- function(df, group="Group", depvar="Depvar",
                                    p_adj_method="holm", mu=0) {

    my_grp <- my_data %>%
        ungroup() %>%
        select({{ group }})

    # Pull out data by group, then pull out just Depvar
    my_data_split = split(df, my_grp) %>%
        map({{ depvar }})

    # Use Purr's "Map" function to run t.test in vectorized form
    my_ttest_out <- map(my_data_split, ~t.test(.,
                                               alternative = c("two.sided"),
                                               mu = mu,
                                               paired = FALSE))

    print(sprintf("One-sample t-tests against %s, by Grp", mu))
    print(my_ttest_out)

    # Pull out p.value
    my_pval <- map(my_ttest_out, "p.value") %>%
        unlist() %>%
        p.adjust(method = p_adj_method)

    print(sprintf("P-values after correcting for multiple-comparisons using %s method", p_adj_method))
    print(my_pval)

}

#' Run two-sample t-test. First run F-test to test equal variance. If fail, run
#' Welch t-test with non-pooled variance. Otherwise pool variance.
two_sample_ttest <- function(data_1, data_2) {

    data_1 <- unlist(data_1)
    data_2 <- unlist(data_2)

    # Use F-test to test variance difference
    F_out = var.test(data_1, data_2, ratio = 1, alternative = "two.sided")
    F_pval = F_out$p.value
    print(F_out)

    # Run 2-sample t-test
    # If variance is the same, run t-test with pooled variance
    # Other use individual variance (less power)
    if (F_pval >= 0.05) {
        print("Using pooled variance because variance difference non-significant")
        T_out = t.test(data_1, data_2, alternative = c("two.sided"),
                       mu = 0, paired = FALSE, var.equal = TRUE,
                       conf.level = 0.95)
    } else {
        print("Not using pooled variance because variance difference is significant")
        T_out = t.test(data_1, data_2, alternative = c("two.sided"),
                       mu = 0, paired = FALSE, var.equal = FALSE,
                       conf.level = 0.95)
    }

    return(T_out)
    print(T_out)

}
