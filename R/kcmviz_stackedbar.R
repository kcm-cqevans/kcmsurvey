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
                               colors=NULL,
                               textsize_xaxis=16,
                               textsize_yaxis=16  ){

  if (!is.null(colors)) {
    # Use custom colors if provided
    colors <- colors
  } else if (!is.null(color_scheme)) {
    # Define color schemes
    color_schemes <- list(
      "agree_dis5" = c("#FF0000", "#FF6666",  "#FFCC33", "#338585", "#006666"),
      "agree_dis4" = c("#FF0000", "#FF6666",  "#338585", "#006666"),
      "categorical" = c("#D67619",  "#006848", "#4B2884", "#FDB71A", "#264d5e"),
      "continuous" = c("#125A56",  "#00767B", "#238F9D", "#42A7C6","#FD9A44","#F57634","#E94C1F", "#555555")
    )
    # Select color scheme based on input
    colors <- color_schemes[[color_scheme]]
  } else {
    stop("Either 'colors' or 'color_scheme' must be provided.") }
  mycolors = colors[seq_along(unique(value_labels))]
  update_geom_defaults("text", list(family = "nunito"))
  ggplot(data, aes(fill = value_labels, y = outcome_var, x = var_labels, label = prop_labels)) +
    geom_bar(position = "stack", stat = "identity", width = 0.6) +
    geom_text(label = ifelse(outcome_var >= 5, prop_labels, NA),
              position = position_stack(vjust = 0.5), color = "#FFFFFF",
              fontface = "bold", size = 5) +
    coord_flip() +
    scale_fill_manual(values = mycolors, guide=guide_legend(reverse = TRUE)) +
    scale_x_discrete(limits = rev, expand = c(0, 0)) +
    scale_y_continuous(labels=function(y) paste0(y, '%'), expand = c(0.01, 0.01)) +
    labs(title = main_title,
         y = "",
         x = " ",
         caption = source_info,
         subtitle = subtitle) +
    theme(text = element_text(size = 14, family = "lato"),
          plot.title = element_text(size = 17,  face = "bold"),
          plot.caption = element_text(size = 10.5, hjust = 0, vjust = 2, , color="#36454F"),
          plot.subtitle = element_text(size = 14,  color="#36454F"),
          plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
          #axis.text.x = element_blank(),
          axis.line.x = element_line(linewidth = 0.6, linetype = "solid", colour = "black"),
          axis.text.x = element_text(size = textsize_xaxis,  color = "black"),
          axis.text.y = element_text(margin=margin(r=0), color="black"),
          axis.ticks = element_blank(),
          # aspect.ratio = aspect_ratio,
          axis.text = element_text(size = 14,  color = "#36454F", margin=margin(r=5)),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_line(color = "#585860",
                                            size = 0.35,
                                            linetype = 2),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.text = element_text(size=12,family = "Playfair Display", color = "black"),
          legend.title = element_blank(),
          legend.justification='left',
          legend.key.size = unit(1, "line"),
          legend.margin = margin(t=5,b=5, 0, subtitle_h_just)) +
    {if(fixed_aspect_ratio)theme(aspect.ratio = 0.35)}
}

