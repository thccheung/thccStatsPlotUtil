# library(tidyverse)
# library(broom)

#' calculate mean and SEM from a grouped dataframe
calc_mean_sem <- function(df_grouped) {

    df_grouped <- df_grouped %>%
        dplyr::summarise(dplyr::across(where(is.double), list(
            mean = ~ mean(.x, na.rm=TRUE), sd = ~ sd(.x, na.rm=TRUE),
            sem = ~ sd(.x, na.rm=TRUE)/sqrt(sum(!is.na(.x)))
        )),
        ncount = dplyr::n()) %>%
        dplyr::ungroup()

    return(df_grouped)
}

#' generate bootstrap samples of linear regression fit
#' Outputs:
#' bs_samp: bootstrap samples
#' glance: model fit statistics
bootstrap_lm <- function(x, y, n_bootstrap) {

    n_smp <- length(x)
    bs_smp <- sample(n_smp, n_smp*n_bootstrap, TRUE)
    x_bs <- x[bs_smp] %>%
        matrix(nrow=n_smp, ncol=n_bootstrap)
    y_bs <- y[bs_smp] %>%
        matrix(nrow=n_smp, ncol=n_bootstrap)

    out_bootstrap_lm <- vector("list", length=n_bootstrap)
    out_glance <- vector("list", length=n_bootstrap)

    for (i in seq_len(n_bootstrap)) {
        out_bootstrap_lm[[i]] <- lm(y_bs[,i] ~ x_bs[,i])
        out_glance[[i]] <- broom::glance(out_bootstrap_lm[[i]])
        if (i %% 500 == 0) {
            cat("generating", i, "th bootstrap sample\n")
        }
    }

    out_glance <- dplyr::bind_rows(out_glance)
    output <- list(bs_samp = out_bootstrap_lm,
                   glance = out_glance)
    return(output)
}

#' generate bootstrap samples of linear regression fit
#' Outputs:
#' bs_samp: bootstrap samples
#' glance: model fit statistics
bootstrap_corr <- function(x, y, n_bootstrap) {

    n_smp <- length(x)
    bs_smp <- sample(n_smp, n_smp*n_bootstrap, TRUE)

    x_bs <- x[bs_smp] %>%
        matrix(nrow=n_smp, ncol=n_bootstrap)
    y_bs <- y[bs_smp] %>%
        matrix(nrow=n_smp, ncol=n_bootstrap)

    # reshape bootstrap samples from matrices into nested lists
    # for parallel processing with purrr:map2
    x_bs <- split(x_bs, rep(1:n_bootstrap, each=n_smp))
    y_bs <- split(y_bs, rep(1:n_bootstrap, each=n_smp))
    output <- purrr::map2(x_bs, y_bs, cor) %>%
        unlist()

    return(output)
}
