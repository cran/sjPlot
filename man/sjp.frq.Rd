\name{sjp.frq}
\alias{sjp.frq}
\title{Plot frequencies of (count) variables}
\usage{
  sjp.frq(varCount, title = NULL, titleSize = 1.3,
    titleColor = "black", weightBy = NULL,
    weightByTitleString = NULL, interactionVar = NULL,
    maxYlim = FALSE, upperYlim = NULL, order = "none",
    type = "bars", axisLabels.x = NULL,
    interactionVarLabels = NULL, axisLabelAngle.x = 0,
    axisLabelSize = 1.1, axisLabelColor = "gray30",
    valueLabelSize = 4, valueLabelColor = "black",
    breakTitleAt = 50, breakLabelsAt = 12,
    gridBreaksAt = NULL, barWidth = 0.6, barColor = NULL,
    barAlpha = 1, barOutline = FALSE, barOutlineSize = 0.2,
    innerBoxPlotWidth = 0.15, innerBoxPlotDotSize = 3,
    borderColor = NULL, axisColor = NULL,
    outlineColor = "black", majorGridColor = NULL,
    minorGridColor = NULL, hideGrid.x = FALSE,
    hideGrid.y = FALSE, showValueLabels = TRUE,
    showPercentageValues = TRUE, showAxisLabels.x = TRUE,
    showTickMarks = TRUE, showMeanIntercept = FALSE,
    showMeanValue = TRUE, showStandardDeviation = TRUE,
    showNormalCurve = FALSE, meanInterceptLineType = 2,
    meanInterceptLineSize = 0.5, normalCurveColor = "red",
    normalCurveSize = 0.7, normalCurveAlpha = 0.4,
    axisTitle.x = NULL, axisTitle.y = NULL,
    axisTitleColor = "black", axisTitleSize = 1.3,
    theme = NULL, flipCoordinates = FALSE, omitNA = TRUE,
    returnPlot = FALSE)
}
\arguments{
  \item{varCount}{The variable which frequencies should be
  plotted.}

  \item{title}{Title of diagram as string.  Example:
  \code{title=c("my title")}}

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
  will be plotted. \cr \code{"bar"}, \code{"bars"} or
  \code{"b"} for simple bars (the default setting). \cr
  \code{"h"}, \code{"hist"} or \code{"histogram"} for a
  histogram. \cr \code{"line"}, \code{"lines"} or
  \code{"l"} for a histogram with filled area with line.
  \cr \code{"dens"}, \code{"d"} or \code{"density"} for a
  density plot. \cr \code{"box"}, \code{"boxplot"} or
  \code{"boxplots"} for box plots. \cr \code{"v"} or
  \code{"violin"} for violin plots.}

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
  title are displayed in one line and when a line break is
  inserted into the title.}

  \item{gridBreaksAt}{Sets the breaks on the y axis, i.e.
  at every n'th position a major grid is being printed.}

  \item{barWidth}{Width of bars. Default is 0.6,
  recommended values range from 0.2 to 2.0}

  \item{innerBoxPlotWidth}{The width of the inner box plot
  that is plotted inside of violin plots. Only applies if
  \code{type} is \code{"violin"}. Default value is 0.15}

  \item{innerBoxPlotDotSize}{Size of mean dot insie a
  violin plot. Applies only when \code{type} is set to
  \code{"violin"}.}

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

  \item{outlineColor}{The color of the bar outline. Only
  applies, if \code{barOutline} is \code{TRUE}.}

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
  values should be plotted to each bar. If \code{FALSE},
  percentage values are removed.}

  \item{showAxisLabels.x}{Whether x axis text (category
  names) should be shown or not.}

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

  \item{showNormalCurve}{If \code{TRUE}, a normal curve is
  plotted over the histogram. Default is \code{FALSE}. Only
  applies when histograms are plotted.}

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
  labels are assigned to the x axis.}

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

  \item{theme}{Specifies the diagram's background theme.
  default (parameter \code{NULL}) is a gray background with
  white grids. Use \code{"bw"} for a white background with
  gray grids, \code{"classic"} for a classic theme (black
  border, no grids), \code{"minimal"} for a minimalistic
  theme (no border, gray grids) or \code{"none"} for no
  borders, grids and ticks.}

  \item{flipCoordinates}{If \code{TRUE}, the x and y axis
  are swapped. Default is \code{FALSE}.}

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
  Plot frequencies of a (count) variable as bar graph,
  histogram, box plot etc. using ggplot.
}
\examples{
# boxplot
sjp.frq(ChickWeight$weight, type="box")

# histogram
sjp.frq(discoveries, type="hist", showMeanIntercept=TRUE)
# histogram with minimal theme
sjp.frq(discoveries, type="hist", showMeanIntercept=TRUE,
        theme="minimal", minorGridColor="white",
        showTickMarks=FALSE, hideGrid.x=TRUE)

# violin plot
sjp.frq(ChickWeight$weight, type="v")

# bar plot
sjp.frq(ChickWeight$Diet)
sjp.frq(ChickWeight$Diet, maxYlim=TRUE)

# bar plot with EUROFAMCARE sample dataset
# dataset was importet from an SPSS-file, using:
# efc <- sji.SPSS("efc.sav", enc="UTF-8")
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

# box plots with interaction variable
# the following example is equal to the function call
# sjp.grpfrq(efc$e17age, efc$e16sex, type="box")
sjp.frq(efc$e17age,
        title=paste(efc.var[['e17age']], "by", efc.var[['e16sex']]),
        interactionVar=efc$e16sex,
        interactionVarLabels=efc.val['e16sex'],
        type="box")
}
\references{
  \url{http://strengejacke.wordpress.com/sjplot-r-package/}
  \cr \cr
  \url{http://strengejacke.wordpress.com/2013/02/25/simplify-frequency-plots-with-ggplot-in-r-rstats/}
}

