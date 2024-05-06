#' Add Google fonts for visualization
#'
#' This function adds Google fonts for visualization purposes using sysfonts and showtext.
#'
#' @keywords internal
#' @export
kcmviz_fonts <- function() {
  sysfonts::font_add_google("lato", "lato")
  sysfonts::font_add_google("inter", "inter")
  sysfonts::font_add_google("roboto", "roboto")
  sysfonts::font_add_google("inter tight", "inter tight")
  sysfonts::font_add_google("inter", family = "inter-light", regular.wt = 300)
  showtext::showtext_auto()
}
