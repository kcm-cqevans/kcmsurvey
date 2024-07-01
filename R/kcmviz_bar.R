#' Title
#'
#' @param data
#' @param outcome_var
#' @param label_var
#' @param cat_var
#' @param ymin
#' @param ymax
#' @param lang
#' @param main_title
#' @param subtitle
#' @param source_info
#' @param order
#' @param color_scheme
#' @param horiz
#' @import ggrepel
#' @import ggtext
#' @import ggplot2
#' @import ggtext
#' @return
#' @export
#'
#' @examples
kcmviz_bar<-function(data, outcome_var = data$prop, label_var = data$proplabel,
         cat_var = data$element_var,
         ymin = 0,
         ymax = 100,
         lang = "en",
         main_title = "",
         subtitle = "",
         source_info = "",
         order = "ascend",
         color_scheme = "#FDB71A",
         horiz = TRUE){
  if(order == "ascend"){
    data = data[order(+data$prop), ]
    cat_var = cat_var[order(+outcome_var)]
    label_var = label_var[order(+outcome_var)]
    outcome_var = outcome_var[order(+outcome_var)]
  }
  else if(order == "descend"){
    data = data[order(-data$prop), ]
    cat_var = cat_var[order(-outcome_var)]
    label_var = label_var[order(-outcome_var)]
    outcome_var = outcome_var[order(-outcome_var)]
  }
  if(horiz==TRUE){
    update_geom_defaults("text", list(family = "inter"))
    ggplot(data, aes(x=factor(cat_var, levels = cat_var), y = outcome_var)) +
      geom_bar(stat = "identity", color = color_scheme, fill = color_scheme, width = 0.75) +
      geom_text(aes(label=label_var), vjust=-0.5, size = 6, fontface = "bold", color = color_scheme) +
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
            axis.title.y = element_blank(),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            axis.ticks = element_blank(),
            axis.text = element_text(size = 20, family = "inter-light", color = "black"),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.x = element_blank())
  }
  else if(horiz==FALSE){
    update_geom_defaults("text", list(family = "inter"))
    ggplot(data, aes(x=factor(cat_var, levels = cat_var), y = outcome_var)) +
      geom_bar(stat = "identity", color = color_scheme, fill = color_scheme, width = 0.75) +
      geom_text(aes(label=label_var), vjust=0.5, hjust=-0.1, size = 6, fontface = "bold", color = color_scheme) +
      scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0.3), labels = function(x) paste0(x, "%")) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
      labs(title=main_title,
           y = "",
           x = "",
           caption = source_info,
           subtitle = subtitle) +
      theme(text = element_text(size = 18, family = "inter"),
            plot.title = element_text(size = 24, family = "inter", face = "bold"),
            plot.caption = element_text(size = 16, hjust = 0.02, vjust = 2, family = "inter", color="#585860"),
            plot.subtitle = element_text(size = 18, family = "inter-light", color="#242424"),
            axis.title.y = element_blank(),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            axis.ticks = element_blank(),
            axis.text = element_text(size = 20, family = "inter-light", color = "black"),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.x = element_blank()) + coord_flip()}

}
