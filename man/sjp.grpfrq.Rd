\name{sjp.grpfrq}
\alias{sjp.grpfrq}
\title{Plot grouped or stacked frequencies}
\usage{
  sjp.grpfrq(varCount, varGroup, weightBy = NULL,
    weightByTitleString = NULL, interactionVar = NULL,
    type = "bars", dotSize = 4, hideLegend = FALSE,
    maxYlim = FALSE, upperYlim = NULL,
    useFacetGrid = FALSE, title = NULL, titleSize = 1.3,
    titleColor = "black", legendTitle = NULL,
    axisLabels.x = NULL, axisLabelSize = 1.1,
    axisLabelColor = "gray30", axisLabelAngle.x = 0,
    interactionVarLabels = NULL, legendLabels = NULL,
    valueLabelSize = 4, valueLabelColor = "black",
    breakTitleAt = 50, breakLabelsAt = 12,
    breakLegendTitleAt = 20, breakLegendLabelsAt = 20,
    gridBreaksAt = NULL, barPosition = "dodge",
    barWidth = 0.6, barSpace = 0.1, barColor = NULL,
    barAlpha = 1, innerBoxPlotWidth = 0.15,
    innerBoxPlotDotSize = 3, colorPalette = "GnBu",
    lineType = 1, lineSize = 1, lineAlpha = 1,
    smoothLines = FALSE, borderColor = NULL,
    axisColor = NULL, barOutline = FALSE,
    outlineColor = "black", majorGridColor = NULL,
    minorGridColor = NULL, hideGrid.x = FALSE,
    hideGrid.y = FALSE, showValueLabels = TRUE,
    showPercentageValues = TRUE, showAxisLabels.x = TRUE,
    showTickMarks = TRUE, showPlotAnnotation = TRUE,
    showMeanIntercept = FALSE, showMeanValue = TRUE,
    showStandardDeviation = FALSE, showTableSummary = TRUE,
    showGroupCount = FALSE, tableSummaryPos = "r",
    meanInterceptLineType = 2, meanInterceptLineSize = 0.5,
    axisTitle.x = NULL, axisTitle.y = NULL,
    axisTitleColor = "black", axisTitleSize = 1.3,
    theme = NULL, legendPos = "right", legendSize = 1,
    legendBorderColor = "white", legendBackColor = "white",
    flipCoordinates = FALSE, omitNA = TRUE,
    returnPlot = FALSE)
}
\arguments{
  \item{varCount}{The variable which frequencies should be
  plotted. The counts of this variable are along the
  y-axis, the variable's categories on the x-axis.}

  \item{varGroup}{the grouping variable, where each value
  represents a single bar chart within each category of the
  \code{varCount} variable.}

  \item{weightBy}{A weight factor that will be applied to
  weight all cases from \code{varCount}.}

  \item{weightByTitleString}{If a weight factor is supplied
  via the parameter \code{weightBy}, the diagram's title
  may indicate this with a remark. Default is \code{NULL},
  so the diagram's title will not be modified when cases
  are weighted. Use a string as parameter, e.g.:
  \code{weightByTitleString=" (weighted)"}.}

  \item{interactionVar}{An interaction variable which can
  be used for box plots. Divides each category indicated by
  \code{varGroup} into the factors of
  \code{interactionVar}, so that each category of
  \code{varGroup} is subgrouped into
  \code{interactionVar}'s categories. Only applies when
  parameter \code{type} is \code{box} or \code{violin}
  (resp. their alternative strings like \code{"boxplot"},
  \code{"boxplots"} or \code{"v"}).}

  \item{barPosition}{Indicates whether bars should be
  positioned side-by-side (default, or use \code{"dodge"}
  as parameter) or stacked (use \code{"stack"} as
  parameter).  If \code{type} is \code{"histogram"}, you
  can use either \code{"dodge"} (default value), which
  displays the bars side-by-side, or \code{"identity"},
  which results in overlaying bars. In the latter case,
  it's recommended to adjust the \code{barAlpha} value.}

  \item{type}{The plot type. May be either \code{"b"},
  \code{"bar"}, \code{"bars"} (default) for bar charts,
  \code{"l"}, \code{"line"}, \code{"lines"} for line
  diagram, \code{"d"}, \code{"dot"}, \code{"dots"} for dot
  plots, \code{"h"}, \code{"hist"}, \code{"histogram"} for
  grouped histograms, \code{"box"}, \code{"boxplot"},
  \code{"boxplots"} for box plots or \code{"v"},
  \code{"violin"} for violin box plots}

  \item{dotSize}{Size of dots. Applies only when
  \code{type} is set to \code{"dots"}.}

  \item{hideLegend}{Indicates whether legend (guide) should
  be shown or not.}

  \item{maxYlim}{Indicates how to calculate the maximum
  limit of the y-axis.  If \code{TRUE}, the upper y-limit
  corresponds to the amount of cases, i.e. y-axis for each
  plot of a data base are the same.  If \code{FALSE}
  (default), the maximum y-axis depends on the highest
  count of a variable's answer category. In this case, the
  y-axis breaks may change, depending on the variable.}

  \item{upperYlim}{Uses a pre-defined upper limit for the
  y-axis. Overrides the \code{maxYlim} parameter.}

  \item{useFacetGrid}{\code{TRUE} when bar charts should be
  plotted as facet grids instead of integrated single bar
  charts. Ideal for larger amount of groups.}

  \item{title}{Title of the diagram, plotted above the
  whole diagram panel.}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{legendTitle}{Title of the diagram's legend.}

  \item{axisLabels.x}{Labels for the x-axis breaks. Passed
  as vector of strings.  Example:
  \code{axisLabels.x=c("Label1", "Label2", "Label3")}.
  Note: If you use the \code{\link{sji.SPSS}} function and
  the \code{\link{sji.getValueLabels}} function, you
  receive a list object with label string. The labels may
  also be passed as list object. They will be unlisted and
  converted to character vector automatically.}

  \item{interactionVarLabels}{Labels for the x-axis breaks
  when having interaction variables included.  These labels
  replace the \code{axisLabels.x}. Only applies, when using
  box or violin plots (i.e. \code{"type"} is \code{"box"}
  or \code{"violin"}) and \code{interactionVar} is not
  \code{NULL}.  Example: See \code{axisLabels.x}.}

  \item{legendLabels}{Labels for the guide/legend.
  Example: See \code{axisLabels.x}.}

  \item{axisLabelSize}{The size of axis labels of both x
  and y axis. Default is 1.1, recommended values range
  between 0.5 and 3.0}

  \item{valueLabelSize}{The size of value labels in the
  diagram. Default is 4, recommended values range between 2
  and 8}

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
  at every n'th position a major grid is being printed.}

  \item{barWidth}{Width of bars. Recommended values for
  this parameter are from 0.4 to 1.5}

  \item{innerBoxPlotWidth}{The width of the inner box plot
  that is plotted inside of violin plots. Only applies if
  \code{type} is \code{"violin"}. Default value is 0.15}

  \item{innerBoxPlotDotSize}{Size of mean dot insie a
  violin plot. Applies only when \code{type} is set to
  \code{"violin"}.}

  \item{barSpace}{Spacing between bars. Default value is
  0.1. If 0 is used, the grouped bars are sticked together
  and have no space in between. Recommended values for this
  parameter are from 0 to 0.5}

  \item{barColor}{User defined color for bars.  If not
  specified (\code{NULL}), a default red-green-yellow color
  palette will be used for the bar charts.  If barColor is
  \code{"gs"}, a greyscale will be used.  If barColor is
  \code{"bw"}, a monochrome white filling will be used.  If
  barColor is \code{"brewer"}, use the \code{colorPalette}
  parameter to specify a palette of the color brewer.  Else
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

  \item{smoothLines}{Prints a smooth line curve. Only
  applies, when parameter \code{type} is set to
  \code{"lines"}.}

  \item{axisLabelColor}{User defined color for axis labels.
  If not specified, a default dark gray color palette will
  be used for the labels.}

  \item{borderColor}{User defined color of whole diagram
  border (panel border).}

  \item{axisColor}{User defined color of axis border (y-
  and x-axis, in case the axes should have different colors
  than the diagram border).}

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

  \item{showValueLabels}{Whether counts and percentage
  values should be plotted to each bar.}

  \item{showPercentageValues}{\code{TRUE} if percentage
  values should be plotted to each bar, if \code{FALSE},
  percentage-values are removed.}

  \item{showAxisLabels.x}{Whether x axis text (category
  names) should be shown or not.}

  \item{showTickMarks}{Whether tick marks of axes should be
  shown or not.}

  \item{showPlotAnnotation}{If \code{TRUE}, the groups of
  dots in a dot-plot are highlighted with a shaded
  rectangle.}

  \item{showMeanIntercept}{if \code{TRUE}, a vertical line
  in histograms is drawn to indicate the mean value of the
  count variables. Only applies to histogram-charts.}

  \item{showMeanValue}{If \code{TRUE} (default value), the
  mean value is printed to the vertical line that indicates
  the mean value of the count variables. Only applies to
  histogram-charts.}

  \item{showStandardDeviation}{If \code{TRUE}, the standard
  deviation is annotated as shaded rectangle around the
  mean intercept line. Only applies to histogram-charts.
  The shaded rectangles have borders in the group colors,
  so it's easier to see which shaded area belongs to which
  mean value resp. group}

  \item{showTableSummary}{If \code{TRUE} (default), a
  summary of the cross tabulation with N, chi-square, df
  and p-value is printed to the upper right corner of the
  diagram. Only applies to bar-charts or dot-plots, i.e.
  when parameter \code{type} is either \code{"bars"} or
  \code{"dots"}.}

  \item{showGroupCount}{if \code{TRUE}, the count within
  each group is added to the category labels (e.g.
  \code{"Cat 1 (n=87)"}).  Default value is \code{FALSE}.}

  \item{tableSummaryPos}{Position of the model summary
  which is printed when \code{showTableSummary} is
  \code{TRUE}. Default is \code{"r"}, i.e. it's printed to
  the upper right corner. Use \code{"l"} for upper left
  corner.}

  \item{meanInterceptLineType}{The linetype of the mean
  intercept line. Only applies to histogram-charts and when
  \code{showMeanIntercept} is \code{TRUE}.}

  \item{meanInterceptLineSize}{The size of the mean
  intercept line. Only applies to histogram-charts and when
  \code{showMeanIntercept} is \code{TRUE}.}

  \item{valueLabelColor}{The color of the value labels
  (numbers) inside the diagram.}

  \item{axisTitle.x}{A label for the x axis. Useful when
  plotting histograms with metric scales where no category
  labels are assigned to the x axis.}

  \item{axisTitle.y}{A label for the y axis. Useful when
  plotting histograms with metric scales where no category
  labels are assigned to the y axis.}

  \item{axisTitleColor}{The color of the x and y axis
  labels. Refers to \code{axisTitle.x} and
  \code{axisTitle.y}, not to the tick mark or category
  labels.}

  \item{axisTitleSize}{The size of the x and y axis labels.
  Refers to \code{axisTitle.x} and \code{axisTitle.y}, not
  to the tick mark or category labels.}

  \item{theme}{Specifies the diagram's background theme.
  default (parameter \code{NULL}) is a gray background with
  white grids. Use \code{"bw"} for a white background with
  gray grids, \code{"classic"} for a classic theme (black
  border, no grids), \code{"minimal"} for a minimalistic
  theme (no border, gray grids) or \code{"none"} for no
  borders, grids and ticks.}

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

  \item{flipCoordinates}{If \code{TRUE}, the x and y axis
  are swapped.}

  \item{omitNA}{If \code{TRUE}, missings are not included
  in the frequency calculation and diagram plot.}

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
  Plot grouped or stacked frequencies of variables as
  bar/dor graphs, box or violin plots, histograms etc.
  using ggplot.
}
\examples{
# histogram plot
sjp.grpfrq(discoveries, sample(1:3, length(discoveries), replace=TRUE), type="hist",
           showValueLabels=FALSE, showMeanIntercept=TRUE)

# histrogram with EUROFAMCARE sample dataset
data(efc)
efc.val <- sji.getValueLabels(efc)
efc.var <- sji.getVariableLabels(efc)
sjp.grpfrq(efc$e17age,
           efc$e16sex,
           title=efc.var['e17age'],
           legendTitle=efc.var['e16sex'],
           legendLabels=efc.val[['e16sex']],
           type="hist",
           showValueLabels=FALSE)

# boxplot
sjp.grpfrq(ChickWeight$weight, as.numeric(ChickWeight$Diet), type="box")

# violin plot
sjp.grpfrq(ChickWeight$weight, as.numeric(ChickWeight$Diet), type="v")

# grouped bars
sjp.grpfrq(sample(1:3, length(ChickWeight$Diet), replace=TRUE),
           as.numeric(ChickWeight$Diet), barSpace=0.2)

# grouped bars with EUROFAMCARE sample dataset
# dataset was importet from an SPSS-file, using:
# efc <- sji.SPSS("efc.sav", enc="UTF-8")
data(efc)
efc.val <- sji.getValueLabels(efc)
efc.var <- sji.getVariableLabels(efc)
sjp.grpfrq(efc$e42dep,
           efc$e16sex,
           title=efc.var['e42dep'],
           axisLabels.x=efc.val[['e42dep']],
           legendTitle=efc.var['e16sex'],
           legendLabels=efc.val[['e16sex']])

# grouped bars using the maximum y-limit
sjp.grpfrq(efc$e42dep,
           efc$e16sex,
           title=efc.var['e42dep'],
           axisLabels.x=efc.val[['e42dep']],
           legendTitle=efc.var['e16sex'],
           legendLabels=efc.val[['e16sex']],
           maxYlim=TRUE)

# grouped bars using necessary y-limit
sjp.grpfrq(efc$e16sex,
           efc$e42dep,
           title=efc.var['e16sex'],
           axisLabels.x=efc.val[['e16sex']],
           legendTitle=efc.var['e42dep'],
           legendLabels=efc.val[['e42dep']])

# box plots with interaction variable
sjp.grpfrq(efc$e17age,
           efc$e42dep,
           interactionVar=efc$e16sex,
           title=paste(efc.var['e17age'], "by", efc.var['e42dep'], "and", efc.var['e16sex']),
           axisLabels.x=efc.val[['e17age']],
           interactionVarLabels=efc.val[['e16sex']],
           legendTitle=efc.var['e42dep'],
           legendLabels=efc.val[['e42dep']],
           type="box")
}
\references{
  \url{http://strengejacke.wordpress.com/sjplot-r-package/}
  \cr \cr
  \url{http://strengejacke.wordpress.com/2013/03/05/easily-plotting-grouped-bars-with-ggplot}
}

