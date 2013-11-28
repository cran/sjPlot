\name{sjp.xtab}
\alias{sjp.xtab}
\title{Plot proportional crosstables}
\usage{
  sjp.xtab(y, x, title = NULL, titleSize = 1.3,
    titleColor = "black", legendTitle = NULL,
    weightBy = NULL, weightByTitleString = NULL,
    type = "bars", tableIndex = "col", reverseX = FALSE,
    maxYlim = TRUE, upperYlim = NULL, axisLabels.x = NULL,
    axisLabelColor.x = "darkgray", axisLabelAngle.x = 0,
    axisLabelSize.x = 1.1, legendLabels = NULL,
    valueLabelSize = 4, valueLabelColor = "black",
    valueLabelPosOnTop = TRUE, stringTotal = "Total",
    breakTitleAt = 50, breakLabelsAt = 12,
    breakLegendTitleAt = 20, breakLegendLabelsAt = 20,
    gridBreaksAt = 0.2, barWidth = 0.7, barSpace = 0.1,
    barPosition = "dodge", barColor = NULL,
    colorPalette = "GnBu", barAlpha = 1, lineType = 1,
    lineSize = 1, lineAlpha = 1, lineDotSize = 3,
    smoothLines = FALSE, borderColor = NULL,
    axisColor = NULL, barOutline = FALSE,
    outlineColor = "black", majorGridColor = NULL,
    minorGridColor = NULL, hideGrid.x = FALSE,
    hideGrid.y = FALSE, showValueLabels = TRUE,
    showCategoryLabels = TRUE, showTickMarks = TRUE,
    showTableSummary = TRUE, tableSummaryPos = "r",
    showTotalColumn = TRUE, hideLegend = FALSE,
    axisTitle.x = NULL, axisTitle.y = NULL,
    axisTitleColor = "black", axisTitleSize = 1.3,
    theme = NULL, flipCoordinates = FALSE,
    returnPlot = FALSE)
}
\arguments{
  \item{y}{The variable which proportions (percentage
  values) should be plotted. The percentage proportion
  (within table row, table column or complete table, see
  parameter \code{tableIndex} of this variable are along
  the y-axis, the variable's categories on the x-axis.}

  \item{x}{The grouping variable, where each value
  represents a single bar chart within each category of the
  \code{y} variable.}

  \item{weightBy}{A weight factor that will be applied to
  weight all cases from \code{y}.}

  \item{weightByTitleString}{If a weight factor is supplied
  via the parameter \code{weightBy}, the diagram's title
  may indicate this with a remark. Default is \code{NULL},
  so the diagram's title will not be modified when cases
  are weighted. Use a string as parameter, e.g.:
  \code{weightByTitleString=" (weighted)"}.}

  \item{type}{The plot type. may be either \code{"b"},
  \code{"bar"}, \code{"bars"} (default) for bar charts, or
  \code{"l"}, \code{"line"}, \code{"lines"} for line
  diagram.}

  \item{tableIndex}{Indicates which data from the
  proportional table should be plotted. Use \code{"row"}
  for calculating row percentages, \code{"col"} for column
  percentages and \code{"cell"} for cell percentages.  Only
  when \code{tableIndex} is \code{"col"}, an additional bar
  chart with the total sum of each column (i.e.  of each
  category on the x-axis) can be added with the parameter
  \code{showTotalColumn}.}

  \item{barPosition}{Indicates whether bars should be
  positioned side-by-side (default, or use \code{"dodge"}
  as parameter) or stacked (use \code{"stack"} as
  parameter).}

  \item{hideLegend}{Indicates whether legend (guide) should
  be shown or not. Default is \code{FALSE}, thus the legend
  is shown.}

  \item{reverseX}{Whether the categories along the x-axis
  should apper in reversed order or not.}

  \item{maxYlim}{Indicates how to calculate the maximum
  limit of the y-axis.  If \code{TRUE}, the y-axes ranges
  from 0 to 100%.  If \code{FALSE}, the maximum y-axis
  depends on the highest percentage value of a variable's
  answer category. In this case, the y-axis breaks may
  change, depending on the variable.}

  \item{upperYlim}{Uses a pre-defined upper limit for the
  y-axis. Overrides the \code{maxYlim} parameter.}

  \item{title}{Title of the diagram, plotted above the
  whole diagram panel.}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{legendTitle}{Title of the diagram's legend.}

  \item{axisLabels.x}{Labels for the x-axis breaks.}

  \item{legendLabels}{Labels for the guide/legend.}

  \item{axisLabelSize.x}{The size of category labels at the
  axes. Default is 1.1, recommended values range between
  0.5 and 3.0.}

  \item{valueLabelSize}{The size of value labels in the
  diagram. Default is 4, recommended values range between 2
  and 8.}

  \item{axisLabelAngle.x}{Angle for axis-labels.}

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

  \item{gridBreaksAt}{Sets the breaks on the y axis, i.e.
  at every n'th position a major grid is being printed.
  Valid values range from 0 to 1.}

  \item{barWidth}{Width of bars. Recommended values for
  this parameter are from 0.4 to 1.5.}

  \item{barSpace}{Spacing between bars. If unchanges, the
  grouped bars are sticked together and have no space in
  between. Recommended values for this parameter are from 0
  to 0.5.}

  \item{barColor}{User defined color for bars.  If not
  specified (\code{NULL}), a default red-green-yellow color
  palette will be used for the bar charts.  If barColor is
  \code{"gs"}, a greyscale will be used.  If barColor is
  \code{"bw"}, a monochrome white filling will be used.  If
  barColor is \code{"brewer"}, use the \code{colorPalette}
  parameter to specify a palette of the color brewer Else
  specify your own color values as vector (e.g.
  \code{barColor=c("#f00000", "#00ff00", "#0080ff")}).}

  \item{colorPalette}{If \code{barColor} is
  \code{"brewer"}, specify a color palette from the color
  brewer here. All color brewer palettes supported by
  ggplot are accepted here.}

  \item{barAlpha}{Specify the transparancy (alpha value) of
  bars.}

  \item{lineType}{The linetype when using line diagrams.
  Only applies, when parameter \code{type} is set to
  \code{"lines"}.}

  \item{lineSize}{The size of lines in a line diagram. Only
  applies, when parameter \code{type} is set to
  \code{"lines"}.}

  \item{lineAlpha}{The alpha value of lines in a line
  diagram. Only applies, when parameter \code{type} is set
  to \code{"lines"}.}

  \item{lineDotSize}{Size of dots. Only applies, when
  parameter \code{type} is set to \code{"lines"}.}

  \item{smoothLines}{Prints a smooth line curve. Only
  applies, when parameter \code{type} is set to
  \code{"lines"}.}

  \item{axisLabelColor.x}{User defined color for axis
  labels. If not specified, a default dark gray color
  palette will be used for the labels.}

  \item{borderColor}{User defined color of whole diagram
  border (panel border).}

  \item{axisColor}{User defined color of axis border (y-
  and x-axis, in case the axes should have different colors
  than the diagram border).}

  \item{barOutline}{If \code{TRUE}, each bar gets a colored
  outline. Default is \code{FALSE}.}

  \item{outlineColor}{The color of the bar outline. Only
  applies, if \code{barOutline} is set to \code{TRUE}}

  \item{majorGridColor}{specifies the color of the major
  grid lines of the diagram background}

  \item{minorGridColor}{specifies the color of the minor
  grid lines of the diagram background}

  \item{hideGrid.x}{If \code{TRUE}, the x-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{hideGrid.y}{If \code{TRUE}, the y-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{showValueLabels}{Whether counts and percentage
  values should be plotted to each bar}

  \item{valueLabelPosOnTop}{Whether value labels should be
  displayed on top of dodged bars or inside the bars.
  Default is \code{TRUE}, i.e. the value labels are
  displayed on top of the bars. Only applies if parameter
  \code{barPosition} is \code{dodge} (default).}

  \item{stringTotal}{The string for the legend label when a
  total-column is added. Only applies if
  \code{showTotalColumn} is \code{TRUE}. Default is
  \code{"Total"}.}

  \item{showCategoryLabels}{Whether x axis text (category
  names) should be shown or not.}

  \item{showTickMarks}{Whether tick marks of axes should be
  shown or not.}

  \item{showTableSummary}{If \code{TRUE} (default), a
  summary of the cross tabulation with N, chi-square, df
  and p-value is printed to the upper right corner of the
  diagram. Only applies to bar-charts or dot-plots, i.e.
  when parameter \code{type} is either \code{"bars"} or
  \code{"dots"}.}

  \item{tableSummaryPos}{Position of the model summary
  which is printed when \code{showTableSummary} is
  \code{TRUE}. Default is \code{"r"}, i.e. it's printed to
  the upper right corner. Use \code{"l"} for upper left
  corner.}

  \item{showTotalColumn}{if \code{tableIndex} is
  \code{"col"}, an additional bar chart with the sum within
  each category and it's percentages will be added to each
  category.}

  \item{valueLabelColor}{The color of the value labels
  (numbers) inside the diagram.}

  \item{axisTitle.x}{A label for the x axis. useful when
  plotting histograms with metric scales where no category
  labels are assigned to the x axis.}

  \item{axisTitle.y}{A label for the y axis. useful when
  plotting histograms with metric scales where no category
  labels are assigned to the y axis.}

  \item{axisTitleColor}{The color of the x and y axis
  labels. refers to \code{axisTitle.x} and
  \code{axisTitle.y}, not to the tick mark or category
  labels.}

  \item{axisTitleSize}{The size of the x and y axis labels.
  refers to \code{axisTitle.x} and \code{axisTitle.y}, not
  to the tick mark or category labels.}

  \item{theme}{Specifies the diagram's background theme.
  default (parameter \code{NULL}) is a gray background with
  white grids. Use \code{"bw"} for a white background with
  gray grids, \code{"classic"} for a classic theme (black
  border, no grids), \code{"minimal"} for a minimalistic
  theme (no border, gray grids) or \code{"none"} for no
  borders, grids and ticks.}

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
  Plot proportional crosstables of two variables as ggplot
  diagram.
}
\examples{
# create 4-category-items
x <- sample(1:4, 100, replace=TRUE)
# create 3-category-items
y <- sample(1:3, 100, replace=TRUE)

# plot "cross tablulation" of x and y
sjp.xtab(y,x)

# plot "cross tablulation" of x and y, including labels
sjp.xtab(y,x, axisLabels.x=c("low", "mid", "high"),
         legendLabels=c("Grp 1", "Grp 2", "Grp 3", "Grp 4"))

# plot "cross tablulation" of x and y as stacked proportional bars
sjp.xtab(y,x, tableIndex="row", barPosition="stack", flipCoordinates=TRUE)

# grouped bars with EUROFAMCARE sample dataset
# dataset was importet from an SPSS-file, using:
# efc <- sji.SPSS("efc.sav", enc="UTF-8")
data(efc)
efc.val <- sji.getValueLabels(efc)
efc.var <- sji.getVariableLabels(efc)

sjp.xtab(efc$e42dep,
         efc$e16sex,
         title=efc.var['e42dep'],
         axisLabels.x=efc.val[['e42dep']],
         legendTitle=efc.var['e16sex'],
         legendLabels=efc.val[['e16sex']])

sjp.xtab(efc$e16sex,
         efc$e42dep,
         title=efc.var['e16sex'],
         axisLabels.x=efc.val[['e16sex']],
         legendTitle=efc.var['e42dep'],
         legendLabels=efc.val[['e42dep']])

sjp.xtab(efc$e16sex,
         efc$e42dep,
         title=efc.var['e16sex'],
         axisLabels.x=efc.val[['e16sex']],
         legendTitle=efc.var['e42dep'],
         legendLabels=efc.val[['e42dep']],
         tableIndex="row",
         barPosition="stack",
         flipCoordinates=TRUE)
}
\references{
  \url{http://strengejacke.wordpress.com/sjplot-r-package/}
  \cr \cr
  \url{http://strengejacke.wordpress.com/2013/04/18/examples-for-sjplotting-functions-including-correlations-and-proportional-tables-with-ggplot-rstats/}
}

