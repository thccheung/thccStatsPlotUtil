#' Generate bootstrap samples
#'
#' @param x A scalar or vector, to be bootstrapped n times
#' @param n_boot A scalar, the number of bootstrap samples to generate
#'
#' @return A matrix. Each row is a bootstrap sample.
#'
#' @example
#' bootstrap_generate(c(1:5), 10)
#'
bootstrap_generate <- function(x, n_boot) {

    n_smp <- length(x)
    bs_smp <- sample(n_smp, n_smp*n_boot, TRUE)
    output <- x[bs_smp] %>%
        matrix(nrow=n_boot, ncol=n_smp)

    return(output)
}


#' Generate bootstrap samples of simple linear regression
#'
#' @param x A vector, the independent variable in regression
#' @param y A vector, the dependent variable in regression
#' @param n_boots A scalar, the number of bootstrap samples to generate
#'
#' @return A list, with [1] $bs_smp: each boostrap regression model sample;
#' [2] $glance: a dataframe of bootstrapped regression model summary
#'
#' @export
#'
bootstrap_lm <- function(x, y, n_boots) {

    if (length(x) != length(y)) {
        stop("Length of x and y must be the same")
    }

    x_bs <- bootstrap_generate(x, n_boots)
    y_bs <- bootstrap_generate(y, n_boots)

    out_bootstrap_lm <- vector("list", length=n_boots)
    out_glance <- vector("list", length=n_boots)

    for (i in seq_len(n_boots)) {
        out_bootstrap_lm[[i]] <- lm(y_bs[i,] ~ x_bs[i,])
        out_glance[[i]] <- broom::glance(out_bootstrap_lm[[i]])
        if (i %% 500 == 0) {
            cat("generating", i, "th bootstrap sample\n")
        }
    }

    out_glance <- dplyr::bind_rows(out_glance)
    output <- list(bs_smp = out_bootstrap_lm,
                   glance = out_glance)

    return(output)
}


#' Generate bootstrap samples of correlation
#' (See also https://stackoverflow.com/questions/58393608/bootstrapped-correlation-in-r)
#'
#' @param x A vector, a variable to be correlated
#' @param y A vector, another variable to be correlated
#' @param n_boots A scalar, the number of bootstrap samples to generate
#'
#' @return A numeric of length n_boots of bootstrapped pearson correlation
#' coefficients
#'
#' @export
#'
bootstrap_corr <- function(x, y, n_boots) {

    n_smp <- length(x)
    if (length(x) != length(y)) {
        stop("Length of x and y must be the same")
    }

    x_bs <- bootstrap_generate(x, n_boots)
    y_bs <- bootstrap_generate(y, n_boots)

    # reshape bootstrap samples from matrices into nested lists
    # for parallel processing with purrr:map2
    x_bs <- split(x_bs, rep(1:n_boots, n_smp))
    y_bs <- split(y_bs, rep(1:n_boots, n_smp))
    output <- purrr::map2(x_bs, y_bs, cor) %>%
        unlist()

    return(output)
}
