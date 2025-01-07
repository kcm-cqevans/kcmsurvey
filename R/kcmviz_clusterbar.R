

#' KCM Data Visualization - Clustered Bar Chart
#' For showing proportions of an X variable across group Z.
#' For example, you might might to show the proportion of people who cite each barrier to transit use across their rider status.
#'
#' @param data Data frame that you are referencing
#' @param element_var The axis variable, in the example above - this will be the column that holds all of the barrier types.
#' @param lower_bound lower bound of confidence intervals
#' @param upper_bound upper bound of confidence intervals
#' @param prop This is the calculated proportion, without formatting for display
#' @param proplabel The labelled proportion variable. This is the actual value you will be displaying, ideally in format XX.X%
#' @param groupby_var This is the grouping variable. In the example above, this will be rider status.
#' @param ymin Value 0-100 where you want you graph axis to start, can be left blank
#' @param ymax Value 0-100 where you want you graph axis to end, can be left blank
#' @param main_title Title of your graph, goes top left
#' @param source_info Source information, goes bottom left. Example "Source: Rider/Non-Rider 2024"
#' @param subtitle Subtitle - if applicable.
#' @param sort Sort - the way you want to order bars -- default is alphabetically.
#' @param horiz Do you want your graph to be horizontal (i.e., with bars that go up and down)? Or graph to be vertical (so that bars goes left to right?)
#' @param y_label Label for Y axis
#' @param x_label Label for X axis
#' @param color_scheme Color choices, you can specify as "color_scheme=c(#color, #color)" or you can use the default colors.
#' @param label_size size of labels on top of bars
#' @param text_position position of text on top of bars
#' @param textsize_yaxis Text size of y axis
#' @param textsize_xaxis Text size of x axis
#'
#' @return A nice pretty graph
#' @export
#'

kcmviz_clusterbar<- function(data,
                             element_var = data$element_var, prop = data$prop
                             lower_bound = data$prop_low, upper_bound = data$prop_upp,
                             proplabel = data$proplabel, groupby_var = data$groupby_var,
                             ymin = 0,
                             ymax = 100,
                             main_title = "",
                             source_info = "",
                             subtitle = "",
                             sort = "",
                             horiz=TRUE,
                             y_label = "",
                             x_label = "",
                             color_scheme = c("#D67619",  "#006848", "#4B2884", "#FDB71A", "#264d5e","#784885", "#a43d6a"),
                             label_size = 4.25,
                             text_position = 0.75,
                             textsize_yaxis = 16,
                             textsize_xaxis=16){
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
      geom_text(aes(label=proplabel, y = prop, group = groupby_var),
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
      theme(text = element_text(size = 16, family = "inter"),
            plot.title = element_text(size = 20, family = "inter", face = "bold"),
            plot.caption = element_text(size = 16, vjust = 2, hjust = 0.02, family = "inter", color="#585860"),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_line(linewidth = 0.6, linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.6, linetype = "solid", colour = "black"),
            axis.text.x = element_text(size = textsize_xaxis, family = "inter-light", color = "black"),
            axis.text.y = element_text(size = textsize_yaxis, family = "inter-light", color = "black"),
            axis.ticks = element_blank(),
            panel.grid.major.y = element_line(color = "#585860",
                                              size = 0.35,
                                              linetype = 2),
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
      geom_text(aes(label=proplabel, y = prop, group = groupby_var),
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
      theme(text = element_text(size = 16, family = "inter"),
            plot.title = element_text(size = 20, family = "inter", face = "bold"),
            plot.caption = element_text(size = 16, vjust = 2, hjust = 0.02, family = "inter", color="#585860"),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_line(linewidth = 0.6, linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.6, linetype = "solid", colour = "black"),
            axis.text.x = element_text(size = textsize_xaxis, family = "inter-light", color = "black"),
            axis.text.y = element_text(size = textsize_yaxis, family = "inter-light", color = "black"),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_line(color = "#585860",
                                              size = 0.35,
                                              linetype = 2),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.position = "top",
            legend.title = element_blank(),
            legend.justification='left',
            legend.margin = margin(t=0, b=0, l=0, r=0),
            legend.text = element_markdown(family = "inter-light", size=15)) + coord_flip(expand=TRUE) }
}




