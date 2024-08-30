

#' Title
#'
#' @param data
#' @param element_var
#' @param outcome_var
#' @param lower_bound
#' @param upper_bound
#' @param label_var
#' @param groupby_var
#' @param ymin
#' @param ymax
#' @param main_title
#' @param source_info
#' @param subtitle
#' @param sort
#' @param horiz
#' @param y_label
#' @param x_label
#' @param color_scheme
#' @param label_size
#' @param text_position
#'
#' @return A nice pretty graph
#' @export
#'

kcmviz_clusterbar<- function(data,
                           element_var = data$element_var, outcome_var = data$prop,
                           lower_bound = data$prop_low, upper_bound = data$prop_upp,
                           label_var = data$proplabel, groupby_var = data$groupby_var,
                           ymin = 0,
                           ymax = 100,
                           main_title = "",
                           source_info = "",
                           subtitle = "",
                           sort = "",
                           horiz=TRUE,
                           y_label = "",
                           x_label = "",
                           color_scheme = c("#D67619", "#264d5e", "#006848", "#4B2884", "#FDB71A", "#784885", "#a43d6a"),
                           label_size = 4.25,
                           text_position = 0.75){
  fill_colors = paste0(color_scheme, "")
  if(sort == "group1"){
    data = data %>%
      group_by(groupby_var) %>%
      mutate(rank = rank(-prop)) %>%
      arrange(groupby_var, rank)
  } else if(sort == "group2"){
    data = data %>%
      group_by(groupby_var) %>%
      mutate(rank = rank(-prop)) %>%
      arrange(match(groupby_var, unique(groupby_var)[2]), rank)
  } else if(sort == "group3"){
    data = data %>%
      group_by(groupby_var) %>%
      mutate(rank = rank(-prop)) %>%
      arrange(match(groupby_var, unique(groupby_var)[3]), rank)
  } else if(sort == "alpha"){
    data = data[order(data$element_var),]
  }
  if(horiz==TRUE){
    update_geom_defaults("text", list(family = "inter"))
    ggplot(data=data, aes(x=factor(element_var, levels = unique(element_var)), y=prop, fill = groupby_var, color = groupby_var)) +
      geom_bar(position = "dodge", stat="identity", width = 0.75) +
      geom_text(aes(label=label_var, y = prop, group = groupby_var),
                position = position_dodge(width = text_position),
                vjust=-.5, size = label_size, fontface = "bold",
                show.legend = FALSE) +
      scale_fill_manual(values = fill_colors) +
      scale_color_manual(values = color_scheme) +
      scale_y_continuous(limits = c(ymin, ymax), expand = expansion(mult = 0.002)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
      labs(title=main_title,
           y = y_label,
           x = x_label,
           caption = source_info) +
      {if(subtitle != "")labs(subtitle = subtitle)}+
      theme(text = element_text(size = 20, family = "inter"),
            plot.title = element_text(size = 20, family = "inter", face = "bold"),
            plot.caption = element_text(size = 16, vjust = 2, hjust = 0.02, family = "inter", color="#585860"),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_line(linewidth = 0.6, linetype = "solid", colour = "#dddddf"),
            axis.text = element_text(size = 20, family = "inter-light", color = "black"),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "top",
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.title = element_blank(),
            legend.justification='left',
            legend.margin = margin(t=0, b=0),
            legend.text = element_markdown(family = "inter-light", size=15))
  }
  else if(horiz==FALSE){
    update_geom_defaults("text", list(family = "inter"))
    ggplot(data=data, aes(x=factor(element_var, levels = unique(element_var)), y=prop, fill = groupby_var, color = groupby_var)) +
      geom_bar(position = "dodge", stat="identity", width = 0.75) +
      geom_text(aes(label=label_var, y = prop, group = groupby_var),
                position = position_dodge(width = text_position),
                hjust=-.05, size = label_size, fontface = "bold",
                show.legend = FALSE) +
      scale_fill_manual(values = fill_colors) +
      scale_color_manual(values = color_scheme) +
      scale_y_continuous(limits = c(ymin, ymax), expand = expansion(mult = 0.002)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 45)) +
      labs(title=main_title,
           y = y_label,
           x = x_label,
           caption = source_info) +
      {if(subtitle != "")labs(subtitle = subtitle)}+
      theme(text = element_text(size = 20, family = "inter"),
            plot.title = element_text(size = 20, family = "inter", face = "bold"),
            plot.caption = element_text(size = 16, vjust = 2, hjust = 0.02, family = "inter", color="#585860"),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_line(linewidth = 0.6, linetype = "solid", colour = "#dddddf"),
            axis.text = element_text(size = 16, family = "inter-light", color = "black"),
            #axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.position = c(1.1,-0.2),             # try c(y,x) instead of named position. i.e. c(1.1,-0.2)
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.title = element_blank(),
            legend.justification='left',
            legend.margin = margin(t=0, b=0),
            legend.text = element_markdown(family = "inter-light", size=15)) + coord_flip() }
}





svyreshape_long <- function(data, pivot_cols = NULL, not_pivot_cols = NULL, drop_cols = NULL, drop_na = FALSE) {
  # Validate parameters
  if (!is.null(pivot_cols) && !is.null(not_pivot_cols)) {
    stop("Specify either pivot_cols or not_pivot_cols, not both.")
  }

  if (is.null(pivot_cols) && is.null(not_pivot_cols)) {
    stop("At least one of pivot_cols or not_pivot_cols must be provided.")
  }

  if (!is.null(drop_cols)) {
    if (!all(drop_cols %in% names(data))) {
      stop("All drop_cols must be column names in data.")
    }
    # Drop specified columns
    data <- select(data, -all_of(drop_cols))
  }

  cols_to_pivot <- if (!is.null(not_pivot_cols)) {
    setdiff(names(data), not_pivot_cols)
  } else {
    pivot_cols
  }

  # Reshape the data
  data_long <- data %>%
    pivot_longer(cols = all_of(cols_to_pivot), names_to = "element_var", values_to = "response_category") %>%
    as.data.frame()

  if (drop_na) {
    # Drop rows with NA in 'response' after reshaping
    data_long <- data_long %>% filter(!is.na(response))
  }

  return(data_long)
}
