#' KCM Survey Data - Bar Graph Visualization
#'
#' This function creates bar graphs for showing the distribution of responses -- both across single-response survey
#' data questions as well as multiple select responses.
#' @param data This is the datagram that will be used for the visualization. It should include columns for the category,
#' as well as the proportion (prop) and the nicely labeled proportion (prop_label). Using the "svycollapse" function, you can
#' create the data frame necessary to create all the variables.
#' @param prop This is the proportion of responses that fall in the category. For categorical variables, it'll be the
#' proportion of respondents that fit in each category (adding to 100%). For multiple select, you'll need to make sure to
#' transpose to long form. Then, it'll be the proportion of respondents who selected yes or no.
#' @param proplabel This is the same as the proportion, except with formatting and labeling appropriate for graphs.
#' @param element_var This is the variable used for the axis -- the descriptive variable.
#' @param ymin This is the minimum of the y-axis for the graph
#' @param ymax This is the maximum of the y-axis for the graph
#' @param main_title This is the main title of the grpah
#' @param subtitle This is the subtitle of the graph (i.e indicating a subgroup or more description)
#' @param source_info This is the source of the data for the graph. If it is for Q2 of 2023 Rider/Non-Rider - you can
#' add source_info="Source: Rider/Non-Rider, Q2 2023"
#' @param order This is specifying whether the order of the bars are in ascending or descending order.
#' @param color_scheme Default color = #FDB71A
#' @param horiz If horiz=TRUE, the graph bars are horizontal and y-axis = %. If horiz= FALSE, then graph is vertical with
#' x axis = %.
#' @param textsize_yaxis text size of x axis
#' @param textsize_xaxis text size of y axis
#' @import ggrepel
#' @import ggtext
#' @import ggplot2
#' @import ggtext
#' @return Pretty graph
#' @export
#'
kcmviz_bar<-function(data, prop = data$prop, proplabel = data$proplabel,
                      element_var = data$element_var,
                      ymin = 0,
                      ymax = 100,
                      main_title = "",
                      subtitle = "",
                      source_info = "",
                      order = "ascend",
                      color_scheme = "#006633",
                      horiz = TRUE,
                      textsize_yaxis = 16,
                      textsize_xaxis=16){
  if(order == "ascend"){
    data = data[order(+data$prop), ]
    element_var = element_var[order(+prop)]
    proplabel = proplabel[order(+prop)]
    prop = prop[order(+prop)]
  }
  else if(order == "descend"){
    data = data[order(-data$prop), ]
    element_var = element_var[order(-prop)]
    proplabel = proplabel[order(-prop)]
    prop = prop[order(-prop)]
  }
  if(horiz==TRUE){
    update_geom_defaults("text", list(family = "inter"))
    ggplot(data, aes(x=factor(element_var, levels = element_var), y = prop)) +
      geom_bar(stat = "identity", color = color_scheme, fill = color_scheme, width = 0.75) +
      geom_text(aes(label=proplabel), vjust=-0.5, size = 6, fontface = "bold", color = color_scheme) +
      scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0.3), labels = function(x) paste0(x, "%")) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
      labs(title=main_title,
           y = "",
           x = "",
           caption = source_info,
           subtitle = subtitle) +
      theme(text = element_text(size = 18, family = "inter"),
            plot.title = element_text(size = 24, family = "inter", face = "bold"),
            plot.caption = element_text(size = 16, hjust = 0.02, vjust = 2, family = "inter", color="#585860"),
            plot.subtitle = element_text(size = 18, family = "inter-light", color="#242424"),
            axis.line = element_line(size=0.5, color="black"),
            #axis.title.y = element_blank(),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            #axis.ticks = element_blank(),
            panel.grid.major.y = element_line(color = "#585860",
                                              size = 0.35,
                                              linetype = 2),
            axis.text.x = element_text(size = textsize_xaxis, family = "inter-light", color = "black"),
            axis.text.y = element_text(size = textsize_yaxis, family = "inter-light", color = "black"),
            #panel.grid = element_blank(),
            panel.background = element_rect(fill = "white"))
  }
  else if(horiz==FALSE){
    update_geom_defaults("text", list(family = "inter"))
    ggplot(data, aes(x=factor(element_var, levels = element_var), y = prop)) +
      geom_bar(stat = "identity", color = color_scheme, fill = color_scheme, width = 0.75) +
      geom_text(aes(label=proplabel), vjust=0.5, hjust=-0.1, size = 6, fontface = "bold", color = color_scheme) +
      scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0.3), labels = function(x) paste0(x, "%")) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
      labs(title=main_title,
           y = "",
           x = "",
           caption = source_info,
           subtitle = subtitle) +
      theme(text = element_text(size = 16, family = "inter"),
            plot.title = element_text(size = 24, family = "inter", face = "bold"),
            plot.caption = element_text(size = 16, hjust = 0.02, vjust = 2, family = "inter", color="#585860"),
            plot.subtitle = element_text(size = 18, family = "inter-light", color="#242424"),
            #axis.title.y = element_blank(),
            axis.line = element_line(size=0.5, color="black"),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            # axis.ticks = element_blank(),
            panel.grid.major.x = element_line(color = "#585860",
                                              size = 0.35,
                                              linetype = 2),
            axis.text.x = element_text(size = textsize_xaxis, family = "inter-light", color = "black"),
            axis.text.y = element_text(size = textsize_yaxis, family = "inter-light", color = "black"),
            #panel.grid = element_blank(),
            panel.background = element_rect(fill = "white")) + coord_flip()}

}
