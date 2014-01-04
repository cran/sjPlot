\name{sjp.likert}
\alias{sjp.likert}
\title{Plot likert scales as centered stacked bars}
\usage{
sjp.likert(items, legendLabels, orderBy = NULL, reverseOrder = FALSE,
  dropLevels = NULL, weightBy = NULL, weightByTitleString = NULL,
  hideLegend = FALSE, title = NULL, titleSize = 1.3,
  titleColor = "black", legendTitle = NULL, includeN = TRUE,
  axisLabels.y = NULL, axisLabelSize = 1.1, axisLabelAngle.x = 0,
  axisLabelColor = "gray30", valueLabelSize = 4,
  valueLabelColor = "black", breakTitleAt = 50, breakLabelsAt = 30,
  breakLegendTitleAt = 30, breakLegendLabelsAt = 28, gridRange = 1,
  gridBreaksAt = 0.2, diagramMargins = TRUE, barWidth = 0.5,
  barColor = NULL, colorPalette = "GnBu", barAlpha = 1,
  borderColor = NULL, axisColor = NULL, barOutline = FALSE,
  outlineColor = "black", majorGridColor = NULL, minorGridColor = NULL,
  hideGrid.x = FALSE, hideGrid.y = FALSE, axisTitle.x = NULL,
  axisTitle.y = NULL, axisTitleColor = "black", axisTitleSize = 1.3,
  theme = NULL, showTickMarks = FALSE, showValueLabels = TRUE,
  showItemLabels = TRUE, showSeparatorLine = FALSE,
  separatorLineColor = "grey80", separatorLineSize = 0.3,
  legendPos = "right", legendSize = 1, legendBorderColor = "white",
  legendBackColor = "white", flipCoordinates = TRUE, returnPlot = FALSE)
}
\arguments{
  \item{items}{A data frame with each column representing
  one likert-item.}

  \item{legendLabels}{A list or vector of strings that
  indicate the likert-scale-categories and which appear as
  legend text.}

  \item{orderBy}{Indicates whether the \code{items} should
  be ordered by total sum of positive or negative answers.
  Use \code{"pos"} to order descending by sum of positive
  answers, \code{"neg"} for sorting descending negative
  answers or \code{NULL} (default) for no sorting.}

  \item{reverseOrder}{If \code{TRUE}, the item order
  (positive/negative) are reversed. Default is
  \code{FALSE}.}

  \item{dropLevels}{Indicates specific factor levels that
  should be dropped from the items before the likert scale
  is plotted. Default is \code{NULL}, hence all factor
  levels are included. Exampe to drop first factor level:
  \code{dropLevels=c(1)}.}

  \item{weightBy}{A weight factor that will be applied to
  weight all cases from \code{items}.}

  \item{weightByTitleString}{If a weight factor is supplied
  via the parameter \code{weightBy}, the diagram's title
  may indicate this with a remark. Default is \code{NULL},
  so the diagram's title will not be modified when cases
  are weighted. Use a string as parameter, e.g.:
  \code{weightByTitleString=" (weighted)"}}

  \item{hideLegend}{Indicates whether legend (guide) should
  be shown or not.}

  \item{title}{Title of the diagram, plotted above the
  whole diagram panel.}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{legendTitle}{Title of the diagram's legend.}

  \item{includeN}{If \code{TRUE} (default), the N of each
  item is included into axis labels.}

  \item{axisLabels.y}{Labels for the y-axis (the labels of
  the \code{items}). These parameters must be passed as
  list! Example: \code{axisLabels.y=list(c("Q1", "Q2",
  "Q3"))}}

  \item{axisLabelSize}{The size of category labels at the
  axes. Default is 1.1, recommended values range between
  0.5 and 3.0}

  \item{axisLabelAngle.x}{Angle for axis-labels.}

  \item{axisLabelColor}{User defined color for axis labels.
  If not specified, a default dark gray color palette will
  be used for the labels.}

  \item{valueLabelSize}{The size of value labels in the
  diagram. Default is 4, recommended values range between 2
  and 8}

  \item{valueLabelColor}{The color of value labels in the
  diagram. Default is black.}

  \item{breakTitleAt}{Wordwrap for diagram title.
  Determines how many chars of the title are displayed in
  one line and when a line break is inserted into the
  title.}

  \item{breakLabelsAt}{Wordwrap for diagram labels.
  Determines how many chars of the category labels are
  displayed in one line and when a line break is inserted.}

  \item{breakLegendTitleAt}{Wordwrap for diagram legend
  title. Determines how many chars of the legend's title
  are displayed in one line and when a line break is
  inserted.}

  \item{breakLegendLabelsAt}{Wordwrap for diagram legend
  labels. Determines how many chars of the legend labels
  are displayed in one line and when a line break is
  inserted.}

  \item{gridRange}{Sets the limit of the x-axis-range.
  Default is 1, so the x-scale ranges from zero to 100
  percent on both sides from the center. Valid values range
  from 0 (0 percent) to 1 (100 percent).}

  \item{gridBreaksAt}{Sets the breaks on the y axis, i.e.
  at every n'th position a major grid is being printed.
  Valid values range from 0 to 1.}

  \item{diagramMargins}{If \code{TRUE} (default), the
  diagram has margins, i.e. the y-axis is not exceeded to
  the diagram's boundaries.}

  \item{barWidth}{Width of bars. Recommended values for
  this parameter are from 0.4 to 1.5}

  \item{barColor}{User defined color for bars.  If not
  specified (\code{NULL}), a default red-green color
  palette for four(!) categories will be used for the bar
  charts. You can use pre-defined color-sets that are
  independent from the amount of categories: \cr If
  barColor is \code{"brown"}, a brown-marine-palette will
  be used. \cr If barColor is \code{"violet"}, a
  violet-green palette will be used. \cr If barColor is
  \code{"pink"}, a pink-green palette will be used. \cr If
  barColor is \code{"brewer"}, use the \code{colorPalette}
  parameter to specify a palette of the color brewer \cr
  Else specify your own color values as vector (e.g.
  \code{barColor=c("darkred", "red", "green",
  "darkgreen")})}

  \item{colorPalette}{If \code{barColor} is
  \code{"brewer"}, specify a color palette from the color
  brewer here. All color brewer palettes supported by
  ggplot are accepted here.}

  \item{barAlpha}{Specify the transparancy (alpha value) of
  bars.}

  \item{borderColor}{User defined color of whole diagram
  border (panel border).}

  \item{barOutline}{If \code{TRUE}, each bar gets a colored
  outline. Default is \code{FALSE}.}

  \item{outlineColor}{The color of the bar outline. Only
  applies, if \code{barOutline} is set to \code{TRUE}.}

  \item{majorGridColor}{Specifies the color of the major
  grid lines of the diagram background.}

  \item{minorGridColor}{Specifies the color of the minor
  grid lines of the diagram background.}

  \item{hideGrid.x}{If \code{TRUE}, the x-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{hideGrid.y}{If \code{TRUE}, the y-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{axisColor}{User defined color of axis border (y-
  and x-axis, in case the axes should have different colors
  than the diagram border).}

  \item{axisTitle.x}{A label for the x axis. Useful when
  plotting histograms with metric scales where no category
  labels are assigned to the x axis.}

  \item{axisTitle.y}{A label for the y axis. Useful when
  plotting histograms with metric scales where no category
  labels are assigned to the y axis.}

  \item{axisTitleColor}{The color of the x and y axis
  labels. refers to \code{axisTitle.x} and
  \code{axisTitle.y}, not to the tick mark or category
  labels.}

  \item{axisTitleSize}{The size of the x and y axis labels.
  refers to \code{axisTitle.x} and \code{axisTitle.y}, not
  to the tick mark or category labels.}

  \item{showValueLabels}{Whether counts and percentage
  values should be plotted to each bar}

  \item{showItemLabels}{Whether x axis text (category
  names) should be shown or not}

  \item{showTickMarks}{Whether tick marks of axes should be
  shown or not}

  \item{showSeparatorLine}{If \code{TRUE}, a line is drawn
  to visually "separate" each bar in the diagram.}

  \item{separatorLineColor}{The color of the separator
  line. Only applies, if \code{showSeparatorLine} is
  \code{TRUE}}

  \item{separatorLineSize}{The size of the separator line.
  only applies, if \code{showSeparatorLine} is \code{TRUE}}

  \item{legendPos}{The position of the legend. Default is
  \code{"right"}. Use one of the following values:
  \code{"right"}, \code{"left"}, \code{"bottom"},
  \code{"top"}.}

  \item{legendSize}{The size of the legend.}

  \item{legendBorderColor}{The border color of the legend.}

  \item{legendBackColor}{The background color of the
  legend.}

  \item{theme}{Specifies the diagram's background theme.
  Default (parameter \code{NULL}) is a gray background with
  white grids. Use \code{"bw"} for a white background with
  gray grids, \code{"classic"} for a classic theme (black
  border, no grids), \code{"minimal"} for a minimalistic
  theme (no border, gray grids) or \code{"none"} for no
  borders, grids and ticks.  The ggplot-object can be
  returned with \code{returnPlot} set to \code{TRUE} in
  order to further modify the plot's theme.}

  \item{flipCoordinates}{If \code{TRUE}, the x and y axis
  are swapped.}

  \item{returnPlot}{If \code{TRUE}, the ggplot-object with
  the complete plot will be returned (and not plotted).
  Default is \code{FALSE}, hence the ggplot object will be
  plotted, not returned.}
}
\value{
The ggplot-object with the complete plot in case
\code{returnPlot} is \code{TRUE}.
}
\description{
Plot likert scales as centered stacked bars. "Neutral"
categories (odd-numbered categories) will be removed from
the plot.
}
\note{
Transformation of data and ggplot-code taken from
\url{http://statisfactions.com/2012/improved-net-stacked-distribution-graphs-via-ggplot2-trickery/}
}
\examples{
# prepare data for dichotomous likert scale, 5 items
likert_2 <- data.frame(as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.3,0.7))),
                       as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.6,0.4))),
                       as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.25,0.75))),
                       as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.9,0.1))),
                       as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.35,0.65))))
# create labels
levels_2 <- list(c("Disagree", "Agree"))

# prepare data for 4-category likert scale, 5 items
likert_4 <- data.frame(as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.2,0.3,0.1,0.4))),
                       as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.5,0.25,0.15,0.1))),
                       as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.25,0.1,0.4,0.25))),
                       as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.1,0.4,0.4,0.1))),
                       as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.35,0.25,0.15,0.25))))
# create labels
levels_4 <- list(c("Strongly disagree", "Disagree", "Agree", "Strongly Agree"))

# prepare data for 6-category likert scale, 5 items
likert_6 <- data.frame(
               as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.2,0.1,0.1,0.3,0.2,0.1))),
               as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.15,0.15,0.3,0.1,0.1,0.2))),
               as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.2,0.25,0.05,0.2,0.2,0.2))),
               as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.2,0.1,0.1,0.4,0.1,0.1))),
               as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.1,0.4,0.1,0.3,0.05,0.15))))
# create labels
levels_6 <- list(c("Very strongly disagree", "Strongly disagree", "Disagree",
                   "Agree", "Strongly Agree", "Very strongly agree"))

# create item labels
items <- list(c("Q1", "Q2", "Q3", "Q4", "Q5"))

# plot dichotomous likert scale, ordered by "negative" values
sjp.likert(likert_2, legendLabels=levels_2, axisLabels.y=items, orderBy="neg")

# plot 4-category-likert-scale, no order
sjp.likert(likert_4, legendLabels=levels_4, axisLabels.y=items)

# plot 4-category-likert-scale, ordered by positive values and in brown color scale
sjp.likert(likert_6, legendLabels=levels_6, barColor="brown",
           axisLabels.y=items, orderBy="pos")
}
\references{
\url{http://strengejacke.wordpress.com/sjplot-r-package/}
\cr \cr
\url{http://strengejacke.wordpress.com/2013/07/17/plotting-likert-scales-net-stacked-distributions-with-ggplot-rstats/}
}
\seealso{
\code{\link{sjp.stackfrq}}
}

