% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kcmviz_clusterbar.R
\name{kcmviz_clusterbar}
\alias{kcmviz_clusterbar}
\title{KCM Data Visualization - Clustered Bar Chart
For showing proportions of an X variable across group Z.
For example, you might might to show the proportion of people who cite each barrier to transit use across their rider status.}
\usage{
kcmviz_clusterbar(
  data,
  element_var = data$element_var,
  prop = data$prop,
  lower_bound = data$prop_low,
  upper_bound = data$prop_upp,
  proplabel = data$proplabel,
  groupby_var = data$groupby_var,
  ymin = 0,
  ymax = 100,
  main_title = "",
  source_info = "",
  subtitle = "",
  sort = "",
  horiz = TRUE,
  y_label = "",
  x_label = "",
  color_scheme = c("#390854", "#F57F29", "#FDB71A", "#31859F", "#006633"),
  label_size = 4.25,
  text_position = 0.75,
  textsize_yaxis = 16,
  textsize_xaxis = 16
)
}
\arguments{
\item{data}{Data frame that you are referencing}

\item{element_var}{The axis variable, in the example above - this will be the column that holds all of the barrier types.}

\item{prop}{This is the calculated proportion, without formatting for display}

\item{lower_bound}{lower bound of confidence intervals}

\item{upper_bound}{upper bound of confidence intervals}

\item{proplabel}{The labelled proportion variable. This is the actual value you will be displaying, ideally in format XX.X\%}

\item{groupby_var}{This is the grouping variable. In the example above, this will be rider status.}

\item{ymin}{Value 0-100 where you want you graph axis to start, can be left blank}

\item{ymax}{Value 0-100 where you want you graph axis to end, can be left blank}

\item{main_title}{Title of your graph, goes top left}

\item{source_info}{Source information, goes bottom left. Example "Source: Rider/Non-Rider 2024"}

\item{subtitle}{Subtitle - if applicable.}

\item{sort}{Sort - the way you want to order bars -- default is alphabetically.}

\item{horiz}{Do you want your graph to be horizontal (i.e., with bars that go up and down)? Or graph to be vertical (so that bars goes left to right?)}

\item{y_label}{Label for Y axis}

\item{x_label}{Label for X axis}

\item{color_scheme}{Color choices, you can specify as "color_scheme=c(#color, #color)" or you can use the default colors.}

\item{label_size}{size of labels on top of bars}

\item{text_position}{position of text on top of bars}

\item{textsize_yaxis}{Text size of y axis}

\item{textsize_xaxis}{Text size of x axis}
}
\value{
A nice pretty graph
}
\description{
KCM Data Visualization - Clustered Bar Chart
For showing proportions of an X variable across group Z.
For example, you might might to show the proportion of people who cite each barrier to transit use across their rider status.
}
