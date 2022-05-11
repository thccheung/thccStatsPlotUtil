library(tidyverse)
library(cowplot)
library(RColorBrewer)

my_txt_size_sm <- 5
my_txt_size_bg <- 5

#' Generate a named list for downstream ggplot2 setup
get_plot_setup <- function(){
    
    plot_setup <- list(
        show_legend = TRUE,
        crossbar_size = 0.15, crossbar_width = 0.8,
        errorbar_size = 0.3, errorbar_width = 0.4,
        pt_size = 0, pt_stroke = 1.2, pt_alpha = 1/2, pt_shape = 16,
        line_size = 0.2,
        ribbon_fill = "gray50", ribbon_alpha = 1/7,
        pd = position_dodge(0.3),
        pt_pd = position_dodge(0.3),
        y_trans = "identity", y_breaks = waiver(), y_title = waiver(),
        x_breaks = waiver(), x_lim = NULL, 
        x_expand = waiver(),
        y_title = waiver()
    )
    
    
    # theme setup
    plot_theme = theme_classic() +
        
        theme(
            axis.line = element_line(size = rel(0.6), colour = "black"),
            axis.ticks = element_line(size = rel(0.5), colour = "black"),
            axis.ticks.length = unit(1.5, "pt"),
            axis.title.x = element_text(size = my_txt_size_bg, vjust = 2, face = "plain"),
            axis.title.y = element_text(size = my_txt_size_bg, vjust = 0, face = "plain"),
            axis.text.x = element_text(size = my_txt_size_sm, angle = 0,
                                       hjust = 0.5, vjust = 1,
                                       colour = "black",
                                       margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "pt")),
            axis.text.y = element_text(size = my_txt_size_sm, angle = 0,
                                       hjust = 1, vjust = 0.5, 
                                       colour = "black",
                                       margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "pt")),
            
            legend.justification = c(0, 1),
            legend.position = c(0, 1),
            legend.key = element_blank(),
            legend.key.size = unit(1, "lines"),
            legend.key.width = unit(1, "pt"),
            legend.background = element_blank(),
            legend.spacing = NULL,
            legend.spacing.x = unit(3, "pt"),
            legend.spacing.y = unit(-1, "pt"),
            legend.text = element_text(size = 9, hjust = 0.5, vjust = 0.5),
            legend.text.align = 0,
            legend.title = element_text(size = 9, 
                                        hjust = 0, vjust = 1, face = "plain"),
            legend.title.align = 0,
            plot.title = element_text(size = 5, hjust = 0.5, vjust = 0, face = "plain"),
            
            # Panel setting (data plot area)
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid = element_blank(),
            panel.grid.minor = element_blank()
        )
    
    plot_setup$plot_theme <- plot_theme
    
    return(plot_setup)
}


#' Prepare dataframe mean + SEM for plotting
get_plot_df_old <- function(plot_select, df_grpsumm, df_indi) {
    
    plot_select_group <- c(stringr::str_c(plot_select, "_mean"),
                           stringr::str_c(plot_select, "_sem"))
    
    # select depvar cols, and rename
    plot_df_group <- df_grpsumm %>% 
        select(Group, lesion_side, all_of(plot_select_group)) %>% 
        rename(my_mean = contains("_mean"),
               my_sem = contains("_sem"),
               my_x = lesion_side,
               my_group = Group)
    
    plot_df_indi <- df_indi %>% 
        select(SubID, Group, lesion_side, all_of(plot_select)) %>% 
        rename(my_x = lesion_side,
               my_y = all_of(plot_select),
               my_group = SubID,
               my_colour = Group) %>% 
        arrange(my_colour)  # sort order by plot colour
    
    # reorder SubID factor to plot points in order
    plot_df_indi$my_group <- fct_inorder(plot_df_indi$my_group)
    
    return(list(group_df = plot_df_group, 
                indi_df = plot_df_indi)
    )
}


#' Prepare dataframe mean + SEM for plotting
get_plot_df <- function(plot_select, df_grpsumm, df_indi, factors) {
    
    if (length(factors) > 2) {
        stop("length of 'factors' need to be 2 (2 factor jitterdodge) or 1")
    } else if (length(factors) == 1) {
        factors = rep(factors, 2)
    }
    factor_1 <- factors[1]
    factor_2 <- factors[2]
    
    plot_select_group <- c(stringr::str_c(plot_select, "_mean"),
                           stringr::str_c(plot_select, "_sem"))
    
    # select depvar cols, and rename
    plot_df_group <- df_grpsumm %>% 
        select(all_of(factor_1), all_of(factor_2), all_of(plot_select_group)) %>% 
        rename(my_mean = contains("_mean"),
               my_sem = contains("_sem")) %>% 
        mutate(my_group = .data[[factor_1]],
               my_x = .data[[factor_2]])
    
    plot_df_indi <- df_indi %>% 
        select(SubID, all_of(factor_1), all_of(factor_2), all_of(plot_select)) %>% 
        mutate(my_x = .data[[factor_2]]) %>% 
        rename(my_y = all_of(plot_select),
               my_group = SubID,
               my_colour = .data[[factor_1]]) %>% 
        arrange(my_colour)  # sort order by plot colour
    
    # reorder SubID factor to plot points in order
    plot_df_indi$my_group <- fct_inorder(plot_df_indi$my_group)
    
    return(list(group_df = plot_df_group, 
                indi_df = plot_df_indi)
    )
}


#' Plot crossbar graph with jitterdodge with 2 factors my_x and my_group
plot_bargraph_jittdodge <- function(df_group, df_indi, plot_setup) {
    
    # parse plot_setup list
    show_legend <- plot_setup$show_legend
    crossbar_size <- plot_setup$crossbar_size
    crossbar_width  <- plot_setup$crossbar_width
    errorbar_size <- plot_setup$errorbar_size
    errorbar_width <- plot_setup$errorbar_width
    pt_size  <- plot_setup$pt_size
    pt_stroke <- plot_setup$pt_stroke
    pt_alpha <- plot_setup$pt_alpha
    pt_shape <- plot_setup$pt_shape
    pt_pd  <- plot_setup$pt_pd
    pd <- plot_setup$pd
    plot_title <- plot_setup$plot_title
    x_label <- plot_setup$x_label
    y_title <- plot_setup$y_title
    x_breaks <- plot_setup$x_breaks
    y_lim <- plot_setup$y_lim
    y_trans <- plot_setup$y_trans
    y_breaks <- plot_setup$y_breaks
    x_lim <- plot_setup$x_lim
    x_expand <- plot_setup$x_expand
    plot_theme <- plot_setup$plot_theme
    
    
    # run ggplot
    my_plot <- ggplot() +
        
        geom_crossbar(data = df_group,
                      mapping = aes(x = my_x, y = my_mean, 
                                    ymin = my_mean, ymax = my_mean,
                                    fill = my_group,
                                    colour = my_group),
                      size = crossbar_size, alpha = 1, width = crossbar_width,
                      position = pd,
                      show.legend = show_legend
        ) +
        
        geom_errorbar(data = df_group,
                      mapping = aes(x = my_x, 
                                    ymin = my_mean - my_sem,
                                    ymax = my_mean + my_sem,
                                    group = my_group, 
                                    colour = my_group),
                      size = errorbar_size, alpha = 1, width = errorbar_width, 
                      position = pd,
                      show.legend = show_legend
        ) 
    
    if (!is.null(df_indi)) {
        
        my_plot <- my_plot +
            
            geom_point(data = df_indi,
                       mapping = aes(x = my_x,
                                     y = my_y,
                                     group = my_group,
                                     colour = my_colour),
                       size = pt_size, stroke = pt_stroke, alpha = pt_alpha, 
                       position = pt_pd, 
                       shape = pt_shape,
                       show.legend = FALSE)
        
    }
    
    my_plot <- my_plot +
        scale_x_discrete(labels = x_label, limits = x_lim, breaks = x_breaks,
                         expand = x_expand) +
        scale_y_continuous(limits = y_lim, trans = y_trans, breaks = y_breaks) +
        ylab(y_title) +
        ggtitle(plot_title) +
        plot_theme
    
    if (!is.null(plot_setup$colour)) {
        my_plot <- my_plot +
            scale_colour_manual(values = plot_setup$colour) +
            scale_fill_manual(values = plot_setup$colour)
    }
    
    return(my_plot)
    
}


#' Plot scatter plot with linear fit and SEM as ribbon
plot_scatter_linfit <- function(df_pt, df_pred, plot_setup) {
    
    # parse plot_setup list
    show_legend <- plot_setup$show_legend
    crossbar_size <- plot_setup$crossbar_size
    crossbar_width  <- plot_setup$crossbar_width
    errorbar_size <- plot_setup$errorbar_size
    errorbar_width <- plot_setup$errorbar_width
    pt_size  <- plot_setup$pt_size
    pt_stroke <- plot_setup$pt_stroke
    pt_alpha <- plot_setup$pt_alpha
    pt_shape <- plot_setup$pt_shape
    pt_pd  <- plot_setup$pt_pd
    ribbon_fill <- plot_setup$ribbon_fill
    ribbon_alpha <- plot_setup$ribbon_alpha
    pd <- plot_setup$pd
    plot_title <- plot_setup$plot_title
    x_title <- plot_setup$x_title
    x_label <- plot_setup$x_label
    x_lim <- plot_setup$x_lim
    x_breaks <- plot_setup$x_breaks
    y_title <- plot_setup$y_title
    y_trans <- plot_setup$y_trans
    y_lim <- plot_setup$y_lim
    y_breaks <- plot_setup$y_breaks
    plot_theme <- plot_setup$plot_theme
    
    my_plot <- ggplot() +
        geom_ribbon(data = df_pred, 
                    aes(x = Depvar_x, ymin = lwr, ymax = upr),
                    fill = ribbon_fill, alpha = ribbon_alpha) +
        geom_line(data = df_pred, 
                  aes(x = Depvar_x, y = fit),
                  linetype = "dashed") +
        geom_point(data = df_pt, aes(x = Depvar_x, y= Depvar_y,
                                     colour = Grp_col),
                   size = pt_size, stroke = pt_stroke, alpha = pt_alpha, 
                   shape = pt_shape,
                   show.legend = show_legend) +
        scale_y_continuous(limits = y_lim, breaks = y_breaks) +
        scale_x_continuous(limits = x_lim) +
        ylab(y_title) +
        xlab(x_title) +
        plot_theme +
        theme(legend.title = element_blank())
    
    if (!is.null(plot_setup$colour)) {
        my_plot <- my_plot +
            scale_colour_manual(values = plot_setup$colour) +
            scale_fill_manual(values = plot_setup$colour)
    }
    
    return(my_plot)
    
}


#' Plot scatter plot with linear fit and SEM as lines
plot_scatter_linfit_lines <- function(df_pt, df_pred, plot_setup, plot_ci=TRUE) {
    
    # parse plot_setup list
    show_legend <- plot_setup$show_legend
    crossbar_size <- plot_setup$crossbar_size
    crossbar_width  <- plot_setup$crossbar_width
    errorbar_size <- plot_setup$errorbar_size
    errorbar_width <- plot_setup$errorbar_width
    pt_size  <- plot_setup$pt_size
    pt_stroke <- plot_setup$pt_stroke
    pt_alpha <- plot_setup$pt_alpha
    pt_shape <- plot_setup$pt_shape
    pt_pd  <- plot_setup$pt_pd
    ribbon_fill <- plot_setup$ribbon_fill
    ribbon_alpha <- plot_setup$ribbon_alpha
    line_size <- plot_setup$line_size
    pd <- plot_setup$pd
    plot_title <- plot_setup$plot_title
    x_title <- plot_setup$x_title
    x_label <- plot_setup$x_label
    x_lim <- plot_setup$x_lim
    x_breaks <- plot_setup$x_breaks
    y_title <- plot_setup$y_title
    y_trans <- plot_setup$y_trans
    y_lim <- plot_setup$y_lim
    y_breaks <- plot_setup$y_breaks
    plot_theme <- plot_setup$plot_theme
    
    if (plot_ci == TRUE) {
        
        my_plot <- ggplot() +
            geom_line(data = df_pred,
                      aes(x = Depvar_x, y = upr),
                      linetype = "22", size = line_size) +
            geom_line(data = df_pred,
                      aes(x = Depvar_x, y = lwr),
                      linetype = "22", size = line_size) +
            geom_line(data = df_pred, 
                      aes(x = Depvar_x, y = fit),
                      linetype = "solid", size = line_size) +
            geom_point(data = df_pt, aes(x = Depvar_x, y= Depvar_y,
                                         colour = Grp_col),
                       size = pt_size, stroke = pt_stroke, shape = pt_shape,
                       alpha = pt_alpha, fill = NA, 
                       show.legend = show_legend) +
            scale_y_continuous(limits = y_lim, breaks = y_breaks) +
            scale_x_continuous(limits = x_lim, breaks = x_breaks) +
            ylab(y_title) +
            xlab(x_title) +
            plot_theme +
            theme(legend.title = element_blank())
        
    } else {
        
        my_plot <- ggplot() +
            geom_line(data = df_pred, 
                      aes(x = Depvar_x, y = fit),
                      linetype = "solid", size = line_size) +
            geom_point(data = df_pt, aes(x = Depvar_x, y= Depvar_y,
                                         colour = Grp_col),
                       size = pt_size, stroke = pt_stroke, shape = pt_shape,
                       alpha = pt_alpha, fill = NA, 
                       show.legend = show_legend) +
            scale_y_continuous(limits = y_lim, breaks = y_breaks) +
            scale_x_continuous(limits = x_lim, breaks = x_breaks) +
            ylab(y_title) +
            xlab(x_title) +
            plot_theme +
            theme(legend.title = element_blank())
        
    }
    
    
    
    if (!is.null(plot_setup$colour)) {
        my_plot <- my_plot +
            scale_colour_manual(values = plot_setup$colour) +
            scale_fill_manual(values = plot_setup$colour)
    }
    
    return(my_plot)
    
}


#' Plot asymmetric crossbar graph with jitterdodge with 2 factors my_x and my_group
plot_asym_bargraph_jittdodge <- function(df_group, df_indi, plot_setup) {
    
    # parse plot_setup list
    show_legend <- plot_setup$show_legend
    crossbar_size <- plot_setup$crossbar_size
    crossbar_width  <- plot_setup$crossbar_width
    errorbar_size <- plot_setup$errorbar_size
    errorbar_width <- plot_setup$errorbar_width
    pt_size  <- plot_setup$pt_size
    pt_stroke <- plot_setup$pt_stroke
    pt_alpha <- plot_setup$pt_alpha
    pt_shape <- plot_setup$pt_shape
    pt_pd  <- plot_setup$pt_pd
    pd <- plot_setup$pd
    plot_title <- plot_setup$plot_title
    x_label <- plot_setup$x_label
    y_title <- plot_setup$y_title
    x_breaks <- plot_setup$x_breaks
    y_lim <- plot_setup$y_lim
    y_trans <- plot_setup$y_trans
    y_breaks <- plot_setup$y_breaks
    x_lim <- plot_setup$x_lim
    x_expand <- plot_setup$x_expand
    plot_theme <- plot_setup$plot_theme
    
    
    # run ggplot
    my_plot <- ggplot() +
        
        geom_crossbar(data = df_group,
                      mapping = aes(x = my_x, y = my_mean, 
                                    ymin = my_mean, ymax = my_mean,
                                    fill = my_group,
                                    colour = my_group),
                      size = crossbar_size, alpha = 1, width = crossbar_width,
                      position = pd,
                      show.legend = show_legend
        ) +
        
        geom_errorbar(data = df_group,
                      mapping = aes(x = my_x, 
                                    ymin = my_lwr,
                                    ymax = my_upr,
                                    group = my_group, 
                                    colour = my_group),
                      size = errorbar_size, alpha = 1, width = errorbar_width, 
                      position = pd,
                      show.legend = show_legend
        ) 
    
    if (!is.null(df_indi)) {
        
        my_plot <- my_plot +
            
            geom_point(data = df_indi,
                       mapping = aes(x = my_x,
                                     y = my_y,
                                     group = my_group,
                                     colour = my_colour),
                       size = pt_size, stroke = pt_stroke, alpha = pt_alpha, 
                       position = pt_pd, 
                       shape = pt_shape,
                       show.legend = FALSE)
        
    }
    
    my_plot <- my_plot +
        scale_x_discrete(labels = x_label, limits = x_lim, breaks = x_breaks,
                         expand = x_expand) +
        scale_y_continuous(limits = y_lim, trans = y_trans, breaks = y_breaks) +
        ylab(y_title) +
        ggtitle(plot_title) +
        plot_theme
    
    if (!is.null(plot_setup$colour)) {
        my_plot <- my_plot +
            scale_colour_manual(values = plot_setup$colour) +
            scale_fill_manual(values = plot_setup$colour)
    }
    
    return(my_plot)
    
}

