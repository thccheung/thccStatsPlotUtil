#library(tidyverse)
#source("./R/util.R")

#' Plot crossbar graph with 1 grouping variable
nb_barplot <- function(df_data, grpvar, yplot) {

    plot_data <- df_data %>%
        rename(Depvar = {{ yplot }}) %>%  # fix dependent var colname to "Depvar"
        group_by({{ grpvar }})

    plot_data_summ <- calc_mean_sem(plot_data)

    my_plot <- ggplot() +

        geom_point(data = df_data,
                   mapping = aes(x = {{ grpvar }},
                                 y = Depvar,
                                 colour = {{ grpvar }}),
                   position = ggplot2::position_dodge2(0.25)) +

        geom_crossbar(data = plot_data_summ,
                      mapping = aes(x = {{ grpvar }}, y = Depvar_mean,
                                    ymin = Depvar_mean, ymax = Depvar_mean,
                                    colour = {{ grpvar }})) +

        geom_errorbar(data = plot_data_summ,
                      mapping = aes(x = {{ grpvar }},
                                    ymin = Depvar_mean - Depvar_sem,
                                    ymax = Depvar_mean + Depvar_sem,
                                    colour = {{ grpvar }}),
                      width = 0.5)

    return(my_plot)
}
