\name{sjp.scatter}
\alias{sjp.scatter}
\title{Plot (grouped) scatter plots}
\usage{
sjp.scatter(x, y, grp = NULL, title = NULL, titleSize = 1.3,
  titleColor = "black", legendTitle = NULL, legendLabels = NULL,
  axisTitle.x = NULL, axisTitle.y = NULL, axisTitleColor = "black",
  axisTitleSize = 1.3, axisTickMarkSize = 1, axisTickMarkColor = "grey50",
  breakTitleAt = 50, breakLegendTitleAt = 20, breakLegendLabelsAt = 20,
  pointAlpha = 0.5, pointSize = 3, pointColors = NULL,
  legendPos = "right", legendSize = 1, legendBorderColor = "white",
  legendBackColor = "white", showTickMarkLabels.x = TRUE,
  showTickMarkLabels.y = TRUE, showTickMarks = TRUE,
  majorGridColor = NULL, minorGridColor = NULL, hideGrid.x = FALSE,
  hideGrid.y = FALSE, borderColor = NULL, axisColor = NULL,
  showGroupFitLine = FALSE, showTotalFitLine = FALSE, showSE = FALSE,
  fitmethod = "lm", useJitter = FALSE, autojitter = TRUE,
  jitterRatio = 0.15, showRug = FALSE, hideLegend = FALSE, theme = NULL,
  useFacetGrid = FALSE, printPlot = TRUE)
}
\arguments{
  \item{x}{A vector (variable) indicating the x positions.}

  \item{y}{A vector (variable) indicating the y positions.}

  \item{grp}{A grouping variable. If not \code{NULL}, the
  scatter plot will be grouped. See examples below. Default
  is \code{NULL}, i.e. not grouping is done.}

  \item{title}{Title of the diagram, plotted above the
  whole diagram panel.  Use \code{"auto"} to automatically
  detect variable names that will be used as title (see
  \code{\link{sji.setVariableLabels}}) for details).}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{legendTitle}{Title of the diagram's legend.}

  \item{legendLabels}{Labels for the guide/legend.}

  \item{axisTitle.x}{A label (title) for the x axis.  Use
  \code{"auto"} to automatically detect variable names that
  will be used as title (see
  \code{\link{sji.setVariableLabels}}) for details).}

  \item{axisTitle.y}{A label (title) for the y axis.  Use
  \code{"auto"} to automatically detect variable names that
  will be used as title (see
  \code{\link{sji.setVariableLabels}}) for details).}

  \item{axisTitleColor}{The color of the x and y axis
  labels. Refers to \code{axisTitle.x} and
  \code{axisTitle.y}, not to the tick mark or category
  labels.}

  \item{axisTitleSize}{The size of the x and y axis labels.
  Refers to \code{axisTitle.x} and \code{axisTitle.y}, not
  to the tick mark or category labels.}

  \item{axisTickMarkSize}{The size of tick mark values of
  both x and y axis. Default is 1, recommended values range
  between 0.5 and 3.0}

  \item{axisTickMarkColor}{User defined color for tick mark
  values. If not specified, a default mid gray color will
  be used for the labels.}

  \item{breakTitleAt}{Wordwrap for diagram title.
  Determines how many chars of the title are displayed in
  one line and when a line break is inserted into the
  title.}

  \item{breakLegendTitleAt}{Wordwrap for diagram legend
  title. Determines how many chars of the legend's title
  are displayed in one line and when a line break is
  inserted.}

  \item{breakLegendLabelsAt}{Wordwrap for diagram legend
  labels. Determines how many chars of the legend labels
  are displayed in one line and when a line break is
  inserted.}

  \item{pointAlpha}{The alpha values of scattered points.
  Useful to better cope with overplotting. Default is 0.5}

  \item{pointSize}{The size of scattered points.}

  \item{pointColors}{The color(s) of scattered points. If
  \code{grp} is not \code{NULL}, groups are indicated by
  different colors, thus a vector with multiple color
  values has to be supplied. By default, the \code{Set1}
  palette of diverging palette type is chosen (see
  \url{http://colorbrewer2.org}).}

  \item{legendPos}{The position of the legend, if a legend
  is drawn. Use \code{"bottom"}, \code{"top"},
  \code{"left"} or \code{"right"} to position the legend
  above, below, on the left or right side of the diagram.
  Right positioning is default.}

  \item{legendSize}{The text size of the legend. Default is
  1. Relative size, so recommended values are from 0.3 to
  2.5}

  \item{legendBorderColor}{Color of the legend's border.
  Default is \code{"white"}, so no visible border is
  drawn.}

  \item{legendBackColor}{Fill color of the legend's
  background. Default is \code{"white"}, so no visible
  background is drawn.}

  \item{showTickMarkLabels.x}{Whether x axis tick mark
  labels should be shown or not.}

  \item{showTickMarkLabels.y}{Whether y axis tick mark
  labels should be shown or not.}

  \item{showTickMarks}{Whether tick marks of axes should be
  shown or not.}

  \item{showGroupFitLine}{If \code{TRUE}, a fitted line for
  each group is drawn. See \code{fitmethod} to change the
  fit method of the fitted lines.}

  \item{showTotalFitLine}{If \code{TRUE}, a fitted line for
  the overall scatterplot is drawn. See \code{fitmethod} to
  change the fit method of the fitted line.}

  \item{showSE}{If \code{TRUE}, a shaded region indicating
  the standard error of the fitted lines will be added.}

  \item{fitmethod}{By default, a linear method
  (\code{"lm"}) is used for fitting the fit lines. Possible
  values are for instance: \itemize{ \item \code{"lm"}
  \item \code{"glm"} \item \code{"loess"} \item
  \code{"auto"} } (see
  \url{http://docs.ggplot2.org/current/stat_smooth.html}
  for more details).}

  \item{useJitter}{If \code{TRUE}, points will be jittered
  (to avoid overplotting).}

  \item{autojitter}{If \code{TRUE}, points will be jittered
  according to an overlap-estimation. A matrix of \code{x}
  and \code{y} values is created and the amount of cells
  (indicating a unique point position) is calculated.  If
  more than 15\% (see \code{jitterRatio}) of the
  approximated amount of unique point coordinates seem to
  overlap, they are automatically jittered.}

  \item{jitterRatio}{The ratio of tolerated overlapping
  (see \code{autojitter}). If approximated amount of
  overlapping points exceed this ration, they are
  automatically jittered. Default is 0.15. Valid values
  range between 0 and 1.}

  \item{showRug}{If \code{TRUE}, a marginal rug plot is
  displayed in the graph (see
  \url{http://docs.ggplot2.org/current/geom_rug.html} for
  more details).)}

  \item{hideLegend}{Indicates whether legend (guide) should
  be shown or not.}

  \item{borderColor}{User defined color of whole diagram
  border (panel border).}

  \item{axisColor}{User defined color of axis border (y-
  and x-axis, in case the axes should have different colors
  than the diagram border).}

  \item{majorGridColor}{Specifies the color of the major
  grid lines of the diagram background.}

  \item{minorGridColor}{Specifies the color of the minor
  grid lines of the diagram background.}

  \item{hideGrid.x}{If \code{TRUE}, the x-axis-gridlines
  are hidden. Default is \code{FALSE}.}

  \item{hideGrid.y}{If \code{TRUE}, the y-axis-gridlines
  are hidden. Default is \code{FALSE}.}

  \item{theme}{Specifies the diagram's background theme.
  Default (parameter \code{NULL}) is a gray background with
  white grids.  \itemize{ \item Use \code{"bw"} for a white
  background with gray grids \item \code{"classic"} for a
  classic theme (black border, no grids) \item
  \code{"minimal"} for a minimalistic theme (no border,gray
  grids) or \item \code{"none"} for no borders, grids and
  ticks.  }}

  \item{useFacetGrid}{\code{TRUE} when each scatter plot
  group should be plotted as single facet instead of an
  integrated single graph. Only applies if \code{grp} is
  not \code{NULL}. Each category of \code{grp} will be
  plotted in an own facet.}

  \item{printPlot}{If \code{TRUE} (default), plots the
  results as graph. Use \code{FALSE} if you don't want to
  plot any graphs. In either case, the ggplot-object will
  be returned as value.}
}
\value{
(Insisibily) returns the ggplot-object with the complete
plot (\code{plot}) as well as the data frame that was used
for setting up the ggplot-object (\code{df}).
}
\description{
Display scatter plot of two variables. Adding a grouping
variable to the scatter plot is possible. Furthermore,
fitted lines can be added for each group as well as for the
overall plot.
}
\examples{
# load sample date
data(efc)

# simple scatter plot, auto-jittering
sjp.scatter(efc$e16sex,efc$neg_c_7)

# simple scatter plot, no jittering needed
sjp.scatter(efc$c160age,efc$e17age)

# grouped scatter plot
sjp.scatter(efc$c160age,efc$e17age, efc$e42dep)

# grouped and jittered scatter plot with marginal rug plot
sjp.scatter(efc$e16sex,efc$neg_c_7, efc$c172code, showRug=TRUE)

# grouped and labelled scatter plot
sjp.scatter(efc$c160age,efc$e17age, efc$e42dep, title="Scatter Plot",
            legendTitle=sji.getVariableLabels(efc)['e42dep'],
            legendLabels=sji.getValueLabels(efc)[['e42dep']],
            axisTitle.x=sji.getVariableLabels(efc)['c160age'],
            axisTitle.y=sji.getVariableLabels(efc)['e17age'],
            showGroupFitLine=TRUE)

# grouped and labelled scatter plot as facets
sjp.scatter(efc$c160age,efc$e17age, efc$e42dep, title="Scatter Plot",
            legendTitle=sji.getVariableLabels(efc)['e42dep'],
            legendLabels=sji.getValueLabels(efc)[['e42dep']],
            axisTitle.x=sji.getVariableLabels(efc)['c160age'],
            axisTitle.y=sji.getVariableLabels(efc)['e17age'],
            showGroupFitLine=TRUE, useFacetGrid=TRUE, showSE=TRUE)

# -------------------------------
# auto-detection of labels
# -------------------------------
efc <- sji.setVariableLabels(efc, sji.getVariableLabels(efc))
sjp.scatter(efc$c160age,efc$e17age, efc$e42dep,
            title="auto", axisTitle.x="auto", axisTitle.y="auto")
}
\references{
\itemize{ \item \url{http://rpubs.com/sjPlot/sjpscatter}
\item
\url{http://strengejacke.wordpress.com/sjplot-r-package/} }
}
\seealso{
\code{\link{sjp.lm1}} \cr \code{\link{sjp.reglin}}
}

