library(ggplot2)
#source("./R/util.R")

#' Plot crossbar graph with 1 grouping variable
nb_barplot <- function(df_data, grpvar, yplot) {

    plot_data <- df_data %>%
        rename(Depvar = {{ yplot }}) %>%  # fix dependent var colname to "Depvar"
        group_by({{ grpvar }})

    plot_data_summ <- calc_mean_sem(plot_data)

    my_plot <- ggplot() +

        geom_point(data = plot_data,
                   mapping = aes(x = {{ grpvar }},
                                 y = Depvar,
                                 colour = {{ grpvar }}),
                   alpha = 1/2,
                   stroke = 0,
                   size = 2,
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

    my_plot <- my_plot +
        theme(axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
              axis.title.x = element_blank(),
              legend.position = "none"
        )

    return(my_plot)
}
