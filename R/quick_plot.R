library(ggplot2)
source("./R/stats_summary.R")

#' Plot crossbar graph with 1 grouping variable
quick_barplot <- function(df_data, groupvar, yplot) {

    plot_data <- df_data %>%
        rename(my_y = {{ yplot }}) %>%  # fix dependent var colname to "my_y"
        group_by({{ groupvar }})

    plot_data_summ <- calc_mean_sem(plot_data)

    my_plot <- ggplot() +

        geom_point(data = plot_data,
                   mapping = aes(x = {{ groupvar }},
                                 y = my_y,
                                 colour = {{ groupvar }}),
                   alpha = 1/2,
                   stroke = 0,
                   size = 2,
                   position = ggplot2::position_dodge2(0.25)) +

        geom_crossbar(data = plot_data_summ,
                      mapping = aes(x = {{ groupvar }}, y = my_y_mean,
                                    ymin = my_y_mean, ymax = my_y_mean,
                                    colour = {{ groupvar }})) +

        geom_errorbar(data = plot_data_summ,
                      mapping = aes(x = {{ groupvar }},
                                    ymin = my_y_mean - my_y_sem,
                                    ymax = my_y_mean + my_y_sem,
                                    colour = {{ groupvar }}),
                      width = 0.5)

    my_plot <- my_plot +
        theme(axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
              axis.title.x = element_blank(),
              legend.position = "none"
        )

    return(my_plot)
}


quick_lineplot_mean_sem_indi <- function(df_data, xplot, yplot, groupvar, indivar) {

    plot_data <- df_data %>%
        dplyr::rename(my_x = {{ xplot }}) %>%
        dplyr::rename(my_y = {{ yplot }}) %>%
        dplyr::rename(my_group = {{groupvar}}) %>%
        dplyr::rename(my_indi = {{indivar}}) %>%
        dplyr::group_by(my_group, my_x)

    plot_data <- plot_data %>%
        dplyr::mutate(my_indi_bygroup = paste0(my_indi, "_", my_group))

    # summarize
    plot_data_summarized <- plot_data %>%
        calc_mean_sem()

    my_plot <- ggplot2::ggplot() +

        ggplot2::geom_line(
            data = plot_data,
            mapping = ggplot2::aes(
                x = my_x,
                y = my_y,
                group = my_indi_bygroup,
                colour = my_group
            ),
            alpha = 1 / 4,
            linewidth = 1,
            position = ggplot2::position_dodge2(0.25)
        ) +

        ggplot2::geom_line(
            data = plot_data_summarized,
            mapping = ggplot2::aes(
                x = my_x,
                y = my_y_mean,
                group = my_group,
                colour = my_group
            ),
            alpha = 1,
            linewidth = 1.5
        ) +

        ggplot2::geom_ribbon(
            data = plot_data_summarized,
            mapping = ggplot2::aes(
                x = my_x,
                ymin = my_y_mean - my_y_sem,
                ymax = my_y_mean + my_y_sem,
                group = my_group,
                fill = my_group
            ),
            alpha = 0.3
        )

    return(my_plot)
}
