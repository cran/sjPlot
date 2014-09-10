\name{sjp.frq}
\alias{sjp.frq}
\title{Plot frequencies of (count) variables}
\usage{
sjp.frq(varCount, title = NULL, titleSize = 1.3, titleColor = "black",
  weightBy = NULL, weightByTitleString = NULL, interactionVar = NULL,
  maxYlim = FALSE, upperYlim = NULL, order = "none", type = "bars",
  axisLabels.x = NULL, interactionVarLabels = NULL, axisLabelAngle.x = 0,
  axisLabelSize = 1.1, axisLabelColor = "gray30", valueLabelSize = 4,
  valueLabelColor = "black", breakTitleAt = 50, breakLabelsAt = 12,
  gridBreaksAt = NULL, barWidth = 0.6, dotSize = 4, barColor = NULL,
  barAlpha = 1, barOutline = FALSE, barOutlineSize = 0.2,
  innerBoxPlotWidth = 0.15, innerBoxPlotDotSize = 3, borderColor = NULL,
  axisColor = NULL, barOutlineColor = "black", majorGridColor = NULL,
  minorGridColor = NULL, hideGrid.x = FALSE, hideGrid.y = FALSE,
  expand.grid = FALSE, showValueLabels = TRUE, showCountValues = TRUE,
  showPercentageValues = TRUE, showAxisLabels.x = TRUE,
  showAxisLabels.y = TRUE, showTickMarks = TRUE,
  showMeanIntercept = FALSE, showMeanValue = TRUE,
  showStandardDeviation = TRUE, showNormalCurve = FALSE,
  showStandardNormalCurve = FALSE, adjustNormalCurve.x = FALSE,
  meanInterceptLineType = 2, meanInterceptLineSize = 0.5,
  normalCurveColor = "red", normalCurveSize = 0.8, normalCurveAlpha = 0.4,
  axisTitle.x = NULL, axisTitle.y = NULL, axisTitleColor = "black",
  axisTitleSize = 1.3, startAxisAt = "auto", hist.skipZeros = FALSE,
  autoGroupAt = NULL, theme = NULL, flipCoordinates = FALSE,
  labelPos = "outside", na.rm = TRUE, printPlot = TRUE)
}
\arguments{
  \item{varCount}{The variable which frequencies should be
  plotted.}

  \item{title}{Title of diagram as string. Example:
  \code{title=c("my title")}.  Use \code{"auto"} to
  automatically detect variable names that will be used as
  title (see \code{\link{sji.setVariableLabels}}) for
  details).}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{weightBy}{A weight factor that will be applied to
  weight all cases from \code{varCount}.  default is
  \code{NULL}, so no weights are used.}

  \item{weightByTitleString}{If a weight factor is supplied
  via the parameter \code{weightBy}, the diagram's title
  may indicate this with a remark. Default is \code{NULL},
  so the diagram's title will not be modified when cases
  are weighted. Use a string as parameter, e.g.:
  \code{weightByTitleString=" (weighted)"}.}

  \item{interactionVar}{An interaction variable which can
  be used for box plots. Divides the observations in
  \code{varCount} into the factors (sub groups) of
  \code{interactionVar}. Only applies when parameter
  \code{"type"} is \code{"box"} or \code{"violin"} (resp.
  their alternative strings like \code{"boxplot"},
  \code{"boxplots"} or \code{"v"}).}

  \item{maxYlim}{Indicates how to calculate the maximum
  limit of the y-axis.  If \code{TRUE}, the upper y-limit
  corresponds to the amount of cases, i.e. y-axis for each
  plot of a data base are the same.  If \code{FALSE}
  (default), the maximum y-axis depends on the highest
  count of a variable's answer category. In this case, the
  y-axis breaks may change, depending on the variable.}

  \item{upperYlim}{Uses a pre-defined upper limit for the
  y-axis. Overrides the \code{maxYlim} parameter.}

  \item{order}{Determines whether categories on x-axis
  should be order according to the frequencies or not.
  Default is \code{"none"}, so categories are not ordered
  by frequency. Use \code{"asc"} or \code{"desc"} for
  sorting categories ascending or descending in relation to
  the frequencies.}

  \item{type}{Specifies the type of distribution plot that
  will be plotted.  \itemize{ \item \code{"bar"},
  \code{"bars"} or \code{"b"} for simple bars (the default
  setting).  \item \code{"dots"} or \code{"dot"} for a dot
  plot.  \item \code{"h"}, \code{"hist"} or
  \code{"histogram"} for a histogram.  \item \code{"line"},
  \code{"lines"} or \code{"l"} for a histogram with filled
  area with line.  \item \code{"dens"}, \code{"d"} or
  \code{"density"} for a density plot.  \item \code{"box"},
  \code{"boxplot"} or \code{"boxplots"} for box plots.
  \item \code{"v"} or \code{"violin"} for violin plots.  }}

  \item{axisLabels.x}{Labels for the x-axis breaks.
  Example: \code{axisLabels.x=c("Label1", "Label2",
  "Label3")}.  Note: If you use the \code{\link{sji.SPSS}}
  function and the \code{\link{sji.getValueLabels}}
  function, you receive a list object with label string.
  The labels may also be passed as list object. They will
  be unlisted and converted to character vector
  automatically.}

  \item{interactionVarLabels}{Labels for the x-axis breaks
  when having interaction variables included.  These labels
  replace the \code{axisLabels.x}. Only applies, when using
  box or violin plots (i.e. \code{"type"} is \code{"box"}
  or \code{"violin"}) and \code{interactionVar} is not
  \code{NULL}.  Example: See \code{axisLabels.x}.}

  \item{axisLabelAngle.x}{Angle for axis-labels.}

  \item{axisLabelSize}{The size of axis labels of both x
  and y axis. Default is 1.1, recommended values range
  between 0.5 and 3.0.}

  \item{valueLabelSize}{The size of value labels in the
  diagram. Default is 4, recommended values range between 2
  and 8.}

  \item{breakTitleAt}{Determines how many chars of the
  title are displayed in one line and when a line break is
  inserted into the title.}

  \item{breakLabelsAt}{Determines how many chars of the
  labels are displayed in one line and when a line break is
  inserted into the axis labels.}

  \item{gridBreaksAt}{Sets the breaks on the y axis, i.e.
  at every n'th position a major grid is being printed.}

  \item{barWidth}{Width of bars. Default is 0.6,
  recommended values range from 0.2 to 2.0}

  \item{dotSize}{The size of dots in case of dot-plots
  (\code{type="dots"}).}

  \item{innerBoxPlotWidth}{The width of the inner box plot
  that is plotted inside of violin plots. Only applies if
  \code{type} is \code{"violin"}. Default value is 0.15}

  \item{innerBoxPlotDotSize}{Size of mean dot insie a
  violin plot. Applies only when \code{type} is set to
  \code{"violin"} or \code{"box"}.}

  \item{barColor}{User defined color for bars. If not
  specified, a default blue color palette will be used for
  the bar charts.}

  \item{barAlpha}{Specify the transparancy (alpha value) of
  bars.}

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

  \item{barOutlineSize}{The size of the bar outlines. Only
  applies if \code{barOutline} is \code{TRUE}.  Default is
  0.2}

  \item{barOutlineColor}{The color of the bar outline. Only
  applies, if \code{barOutline} is \code{TRUE}.}

  \item{majorGridColor}{Specifies the color of the major
  grid lines of the diagram background.}

  \item{minorGridColor}{Specifies the color of the minor
  grid lines of the diagram background.}

  \item{hideGrid.x}{If \code{TRUE}, the x-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{hideGrid.y}{If \code{TRUE}, the y-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{expand.grid}{If \code{TRUE}, the plot grid is
  expanded, i.e. there is a small margin between axes and
  plotting region. Default is \code{FALSE}.}

  \item{showValueLabels}{Whether counts and percentage
  values should be plotted to each bar. Default is
  \code{TRUE}.}

  \item{showCountValues}{If \code{TRUE} (default), count
  values are be plotted to each bar. If \code{FALSE}, count
  values are removed.}

  \item{showPercentageValues}{If \code{TRUE} (default),
  percentage values are be plotted to each bar, if
  \code{FALSE}, percentage-values are removed.}

  \item{showAxisLabels.x}{Whether x axis labels (category
  names) should be shown or not.}

  \item{showAxisLabels.y}{Whether y axis labels (count
  values) should be shown or not.}

  \item{showTickMarks}{Whether tick marks of axes should be
  shown or not.}

  \item{showMeanIntercept}{If \code{TRUE}, a vertical line
  in histograms is drawn to indicate the mean value of the
  count variables. Only applies to histogram-charts.}

  \item{showMeanValue}{If \code{TRUE} (default value), the
  mean value is printed to the vertical line that indicates
  the mean value of the count variables. Only applies to
  histogram-charts.}

  \item{showStandardDeviation}{If \code{TRUE}, the standard
  deviation is annotated as shaded rectangle around the
  mean intercept line. Only applies to histogram-charts.}

  \item{meanInterceptLineType}{The linetype of the mean
  intercept line. Only applies to histogram-charts and when
  \code{showMeanIntercept} is \code{TRUE}.}

  \item{meanInterceptLineSize}{The size of the mean
  intercept line. Only applies to histogram-charts and when
  \code{showMeanIntercept} is \code{TRUE}.}

  \item{showNormalCurve}{If \code{TRUE}, a normal curve,
  which is adjusted to the data, is plotted over the
  histogram or density plot. Default is \code{FALSE}. Only
  applies when histograms or density plots are plotted (see
  \code{type}).}

  \item{showStandardNormalCurve}{If \code{TRUE}, a normal
  curve, which is not adjusted to the data (thus
  representing a "true" standard normal curve, which is,
  however, still just an approximation), is plotted over
  the histogram or density plot. Default is \code{FALSE}.
  Only applies when histograms or density plots are plotted
  (see \code{type}).}

  \item{adjustNormalCurve.x}{If \code{TRUE} and
  \code{showStandardNormalCurve} is also \code{TRUE}, the
  x-axis-start of the standard normal curve starts with the
  x-axis limits of the graph. This is only necessary, if
  minimum value of \code{varCount} is larger than 0 or 1.}

  \item{normalCurveColor}{Specify the color of the normal
  curve line. Only applies if \code{showNormalCurve} is
  \code{TRUE}.}

  \item{normalCurveSize}{Specifiy the size of the normal
  curve line. Only applies if \code{showNormalCurve} is
  \code{TRUE}.}

  \item{normalCurveAlpha}{Specify the transparancy (alpha
  value) of the normal curve. Only applies if
  \code{showNormalCurve} is \code{TRUE}.}

  \item{valueLabelColor}{The color of the value labels
  (numbers) inside the digram.}

  \item{axisTitle.x}{A label for the x axis. useful when
  plotting histograms with metric scales where no category
  labels are assigned to the x axis.  Use \code{"auto"} to
  automatically detect variable names that will be used as
  title (see \code{\link{sji.setVariableLabels}}) for
  details).}

  \item{axisTitle.y}{A label for the y axis. useful when
  plotting histograms with metric scales where no category
  labels are assigned to the y axis.}

  \item{axisTitleColor}{The color of the x and y axis
  labels. Refers to \code{axisTitle.x} and
  \code{axisTitle.y}, not to the tick mark or category
  labels.}

  \item{axisTitleSize}{the size of the x and y axis labels.
  Refers to \code{axisTitle.x} and \code{axisTitle.y}, not
  to the tick mark or category labels. Default is 1.3.}

  \item{hist.skipZeros}{If \code{TRUE}, zero counts
  (categories with no answer) in \code{varCout} are omitted
  when drawing histrograms, and the mapping is changed to
  \code{\link{stat_bin}}. Only applies to histograms (see
  \code{type}). Use this parameter to get identical results
  to the default \code{\link{qplot}} or
  \code{\link{geom_histogram}} histogram plots of ggplot.
  You may need to adjust the \code{barWidth} parameter for
  better visual results (which, by ggplot-default, is 1/30
  of the x-axis-range).}

  \item{startAxisAt}{Determines the first value on the
  x-axis. By default, this value is set to \code{"auto"},
  i.e. the value range on the x axis starts with the lowest
  value of \code{varCount}.  If you set \code{startAxisAt}
  to 1, you may have zero counts if the lowest value of
  \code{varCount} is larger than 1 and hence no bars
  plotted for these values in such cases.}

  \item{autoGroupAt}{A value indicating at which length of
  unique values of \code{varCount} the variable is
  automatically grouped into smaller units (see
  \code{\link{sju.groupVar}}). If \code{varCount} has large
  numbers of unique values, too many bars for the graph
  have to be plotted. Hence it's recommended to group such
  variables. For example, if \code{autoGroupAt} is 50, i.e.
  if \code{varCount} has 50 and more unique values it will
  be grouped using \code{\link{sju.groupVar}} with
  \code{groupsize="auto"} parameter. By default, the
  maximum group count is 30. However, if \code{autoGroupAt}
  is less than 30, \code{autoGroupAt} groups are built.
  Default value for \code{autoGroupAt} is \code{NULL}, i.e.
  auto-grouping is off.}

  \item{theme}{Specifies the diagram's background theme.
  Default (parameter \code{NULL}) is a gray background with
  white grids.  \itemize{ \item Use \code{"bw"} for a white
  background with gray grids \item \code{"classic"} for a
  classic theme (black border, no grids) \item
  \code{"minimal"} for a minimalistic theme (no border,gray
  grids) \item \code{"none"} for no borders, grids and
  ticks or \item \code{"themr"} if you are using the
  \code{ggthemr} package (in such cases, you may use the
  \code{ggthemr::swatch} function to retrieve theme-colors
  for the \code{barColor} parameter) } See
  \url{http://rpubs.com/sjPlot/custplot} for details and
  examples.}

  \item{flipCoordinates}{If \code{TRUE}, the x and y axis
  are swapped. Default is \code{FALSE}.}

  \item{labelPos}{If \code{flipCoordinates} is \code{TRUE},
  use this parameter to specify value label position.  Can
  be either \code{"inside"} or \code{"outside"} (default).
  You may specify initial letter only. If
  \code{flipCoordinates} is \code{FALSE}, this parameter
  will be ignored.}

  \item{na.rm}{If \code{TRUE}, missings are not included in
  the frequency calculation and diagram plot.}

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
Plot frequencies of a (count) variable as bar graph,
histogram, box plot etc. using ggplot.
}
\examples{
# ---------------
# boxplot
# ---------------
sjp.frq(ChickWeight$weight, type="box")

# ---------------
# histogram
# ---------------
sjp.frq(discoveries, type="hist", showMeanIntercept=TRUE)
# histogram with minimal theme and w/0 labels
sjp.frq(discoveries, type="hist", showMeanIntercept=TRUE,
        theme="minimal", minorGridColor="white",
        showTickMarks=FALSE, hideGrid.x=TRUE,
        showValueLabels=FALSE)

# ---------------
# violin plot
# ---------------
sjp.frq(ChickWeight$weight, type="v")

# ---------------
# bar plot
# ---------------
sjp.frq(ChickWeight$Diet)
sjp.frq(ChickWeight$Diet, maxYlim=TRUE)

# ---------------
# bar plot with EUROFAMCARE sample dataset
# dataset was importet from an SPSS-file, using:
# efc <- sji.SPSS("efc.sav", enc="UTF-8")
# ---------------
data(efc)
efc.val <- sji.getValueLabels(efc)
efc.var <- sji.getVariableLabels(efc)
sjp.frq(as.factor(efc$e15relat),
        title=efc.var[['e15relat']],
        axisLabels.x=efc.val['e15relat'],
        axisLabelAngle.x=90)

# bar plot with EUROFAMCARE sample dataset
# grouped variable
ageGrp <- sju.groupVar(efc$e17age)
ageGrpLab <- sju.groupVarLabels(efc$e17age)
sjp.frq(ageGrp,
        title=efc.var[['e17age']],
        axisLabels.x=ageGrpLab)
# minimal theme
sjp.frq(ageGrp,
        title=efc.var[['e17age']],
        axisLabels.x=ageGrpLab,
        theme="minimal",
        minorGridColor="white",
        showTickMarks=FALSE,
        hideGrid.x=TRUE)

# ---------------
# box plots with interaction variable
# the following example is equal to the function call
# sjp.grpfrq(efc$e17age, efc$e16sex, type="box")
# ---------------
sjp.frq(efc$e17age,
        title=paste(efc.var[['e17age']], "by", efc.var[['e16sex']]),
        interactionVar=efc$e16sex,
        interactionVarLabels=efc.val['e16sex'],
        type="box")

# -------------------------------------------------
# auto-detection of value labels and variable names
# -------------------------------------------------
efc <- sji.setVariableLabels(efc, sji.getVariableLabels(efc))

# negative impact scale, ranging from 7-28, assuming that
# variable scale (lowest value) starts with 1
sjp.frq(efc$neg_c_7, startAxisAt=1, title="auto")

# negative impact scale, ranging from 7-28, using
# automatic detection of start index of x-axis
sjp.frq(efc$neg_c_7, axisTitle.x="auto")

# -------------------------------------------------
# Simulate ggplot-default histogram, using "hist.skipZeros"
# and adjusted "barWidth".
# -------------------------------------------------
sjp.frq(efc$c160age, type="h", hist.skipZeros=TRUE, barWidth=1)
}
\references{
\itemize{ \item \url{http://rpubs.com/sjPlot/sjpfrq} \item
\url{http://strengejacke.wordpress.com/sjplot-r-package/} }
}
\seealso{
\code{\link{sjt.frq}}
}

