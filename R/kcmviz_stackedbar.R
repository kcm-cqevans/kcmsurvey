#' This function creates a stacked bar plot
#'
#' @param data A data frame containing the agreement data.
#' @param outcome_var The outcome variable representing the proportion of agreement.
#' @param prop_labels Labels for the proportions.
#' @param var_labels Labels for the variables.
#' @param value_labels Labels for the values.
#' @param main_title The main title of the plot.
#' @param subtitle The subtitle of the plot.
#' @param source_info Information about the data source.
#' @param rev_values Logical indicating whether to reverse the order of value labels.
#' @param rev_variables Logical indicating whether to reverse the order of variable labels.
#' @param hide_small_values Logical indicating whether to hide small values in the plot.
#' @param order_bars Logical indicating whether to order the bars in the plot.
#' @param subtitle_h_just Horizontal justification of the subtitle.
#' @param fixed_aspect_ratio Logical indicating whether to maintain a fixed aspect ratio for the plot.
#' @param color_scheme A vector of colors for the plot.
#' @param label_size The size of the labels.
#' @param text_position The position of the text labels.
#' @import ggrepel
#' @import ggtext
#' @import ggplot2
#' @return A ggplot object representing the agreement visualization.
#' @export
kcmviz_stackedbar <-  function(data, outcome_var = data$prop, prop_labels = data$proplabel,
                                 var_labels = data$element_var, value_labels = data$response_category,
                                 main_title = "",
                                 subtitle = "",
                                 source_info = "",
                                 rev_values = FALSE,
                                 rev_variables = FALSE,
                                 hide_small_values = TRUE,
                                 order_bars = FALSE,
                                 subtitle_h_just = 0,
                                 fixed_aspect_ratio = TRUE,
                                 legendnrow = 1,
                                 color_scheme = NULL,
                                 colors=NULL){

  if (!is.null(colors)) {
    # Use custom colors if provided
    colors <- colors
  } else if (!is.null(color_scheme)) {
    # Define color schemes
    color_schemes <- list(
      "agree_dis5" = c("#FF0000", "#FF6666",  "#FFCC33", "#338585", "#006666"),
      "agree_dis4" = c("#FF0000", "#FF6666",  "#338585", "#006666"),
      "categorical" = c("#2D708E", "#008381", "#C74E49", "#784885", "#a43d6a","#202020")
    )

    # Select color scheme based on input
    colors <- color_schemes[[color_scheme]]
  } else {
    stop("Either 'colors' or 'color_scheme' must be provided.") }


   if(!inherits(var_labels, "character") & !inherits(var_labels, "factor")){
    var_labels = as.character(var_labels)
    data$element_vars = as.character(data$element_var)
  }
  if(!inherits(value_labels, "character") & !inherits(value_labels, "factor")){
    value_labels = as.character(value_labels)
    data$response_category = as.character(data$response_category)
  }

  mycolors = rev(colors[seq_along(unique(value_labels))])

  if(rev_values == TRUE){
    value_labels = factor(value_labels, levels = unique(value_labels))
  } else{
    value_labels = factor(value_labels, levels = rev(unique(value_labels)))
  }
  positions = rev(unique(var_labels))
  update_geom_defaults("text", list(family = "roboto"))
  if(order_bars == TRUE){
    var_labels = factor(var_labels, levels = unique(var_labels))
    data = data.frame(var_labels, value_labels, outcome_var, prop_labels)
    ggplot(data, aes(y = outcome_var, x = var_labels,
                     fill = reorder(value_labels, outcome_var), label = prop_labels)) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[1], ]) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[2], ]) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[3], ]) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[1], ],
                aes(label = prop_labels),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[2], ],
                aes(label = ifelse(outcome_var >= 5, prop_labels, NA)),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[3], ],
                aes(label = ifelse(outcome_var >= 5, prop_labels, NA)),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[4], ],
                aes(label = ifelse(outcome_var >= 5, prop_labels, NA)),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[1], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4, family = "roboto",
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[2], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4, family = "roboto",
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[3], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4, family = "roboto",
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[4], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4, family = "roboto",
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      coord_flip() +
      scale_fill_manual(values = mycolors, guide=guide_legend(reverse = TRUE, nrow = legendnrow)) +
      scale_x_discrete(limits = positions, expand = c(0, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(title = main_title,
           y = "",
           x = " ",
           caption = source_info,
           subtitle = subtitle) +
      theme(text = element_text(size = 14, family = "roboto"),
            plot.title = element_text(size = 17, family = "nunito", face = "bold"),
            plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "nunito", color="#36454F"),
            plot.subtitle = element_text(size = 14, family = "nunito-light", color="#36454F"),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(margin=margin(r=0)),
            axis.ticks = element_blank(),
            # aspect.ratio = aspect_ratio,
            axis.text = element_text(size = 14, family = "roboto", color = "#36454F", margin=margin(r=5)),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            legend.position = "top",
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.text = element_text(family = "Playfair Display", color = "#36454F"),
            legend.title = element_blank(),
            legend.justification='left',
            legend.key.size = unit(1, "line"),
            legend.margin = margin(t=5,b=5, 0, subtitle_h_just)) +
      {if(fixed_aspect_ratio)theme(aspect.ratio = 0.35)}
  } else{
    ggplot(data, aes(fill = value_labels, y = outcome_var, x = var_labels, label = prop_labels)) +
      geom_bar(position = "stack", stat = "identity", width = 0.6) +
      geom_text(label = ifelse(outcome_var >= 5, prop_labels, NA),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      ggrepel::geom_text_repel(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4, family = "nunito",
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      coord_flip() +
      scale_fill_manual(values = mycolors, guide=guide_legend(reverse = TRUE)) +
      scale_x_discrete(limits = positions, expand = c(0, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(title = main_title,
           y = "",
           x = " ",
           caption = source_info,
           subtitle = subtitle) +
      theme(text = element_text(size = 14, family = "roboto"),
            plot.title = element_text(size = 17, family = "nunito", face = "bold"),
            plot.caption = element_text(size = 10.5, hjust = 0, vjust = 2, family = "roboto-light", color="#36454F"),
            plot.subtitle = element_text(size = 14, family = "nunito-light", color="#36454F"),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(margin=margin(r=0)),
            axis.ticks = element_blank(),
            # aspect.ratio = aspect_ratio,
            axis.text = element_text(size = 14, family = "roboto", color = "#36454F", margin=margin(r=5)),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            legend.position = "top",
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.text = element_text(family = "Playfair Display", color = "#36454F"),
            legend.title = element_blank(),
            legend.justification='left',
            legend.key.size = unit(1, "line"),
            legend.margin = margin(t=5,b=5, 0, subtitle_h_just)) +
      {if(fixed_aspect_ratio)theme(aspect.ratio = 0.35)}
  }
}
