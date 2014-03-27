\name{sjp.lm}
\alias{sjp.lm}
\title{Plot beta coefficients of lm}
\usage{
sjp.lm(fit, sort = "beta", title = NULL, titleSize = 1.3,
  titleColor = "black", axisLabels.y = NULL, showAxisLabels.y = TRUE,
  axisLabelSize = 1.1, axisLabelColor = "gray30",
  axisTitle.x = "Estimates", axisTitleSize = 1.4,
  axisTitleColor = c("#444444"), axisLimits = NULL,
  valueLabelColor = "grey20", valueLabelColorNS = "grey50",
  valueLabelSize = 4.5, valueLabelAlpha = 0.8, axisLabelAngle.x = 0,
  axisLabelAngle.y = 0, errorBarColor = "#3366a0", errorBarWidth = 0,
  errorBarSize = 0.8, pointColor = "#3366a0", pointSize = 3,
  pointColorStdBeta = "#cc5533", pointSizeStdBeta = 3,
  stdBetaLineType = 2, stdBetaLineAlpha = 0.3, interceptLineType = 2,
  interceptLineColor = "grey70", breakTitleAt = 50, breakLabelsAt = 12,
  gridBreaksAt = NULL, borderColor = NULL, axisColor = NULL,
  theme = NULL, flipCoordinates = TRUE, majorGridColor = NULL,
  minorGridColor = NULL, hideGrid.x = FALSE, hideGrid.y = FALSE,
  showTickMarks = TRUE, showValueLabels = TRUE, labelDigits = 2,
  showPValueLabels = TRUE, showModelSummary = TRUE,
  showStandardBeta = FALSE, showStandardBetaLine = FALSE,
  printPlot = TRUE)
}
\arguments{
  \item{fit}{The model of the linear regression
  (lm-Object).}

  \item{title}{Diagram's title as string.  Example:
  \code{title=c("my title")}}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{sort}{Determines whether the predictors are sorted
  by beta-values (default, or use \code{"beta"} as
  parameter) or by standardized beta values (use
  \code{"std"}).}

  \item{axisLabels.y}{Labels of the predictor variables
  (independent vars) that are used for labelling the axis.
  Passed as vector of strings.  Example:
  \code{axisLabels.y=c("Label1", "Label2", "Label3")}.
  Note: If you use the \code{\link{sji.SPSS}} function and
  the \code{\link{sji.getValueLabels}} function, you
  receive a list object with label string. The labels may
  also be passed as list object. They will be unlisted and
  converted to character vector automatically.}

  \item{showAxisLabels.y}{Whether x axis text (category
  names, predictor labels) should be shown (use
  \code{TRUE}) or not. Default is \code{TRUE}}

  \item{axisLabelSize}{The size of value labels in the
  diagram. Default is 4, recommended values range between 2
  and 8.}

  \item{axisLabelColor}{The color of the category labels
  (predictor labels). Default is a dark grey (grey30).}

  \item{axisTitle.x}{A label for the x axis. Default is
  \code{"Estimates"}.}

  \item{axisTitleColor}{The color of the x axis label.
  Default is a dark grey.}

  \item{axisTitleSize}{The size of the x axis label.
  Default is 1.4.}

  \item{axisLimits}{Defines the range of the axis where the
  beta coefficients and their confidence intervalls are
  drawn. By default, the limits range from the lowest
  confidence interval to the highest one, so the diagram
  has maximum zoom. Use your own values as 2-value-vector,
  for instance: \code{limits=c(-0.8,0.8)}.}

  \item{valueLabelColor}{Colour of the values (significant
  beta coefficients) inside the diagrams. Only applies,
  when parameter \code{showValueLabels} is set to
  \code{TRUE}. Use any valid colour value, e.g.
  \code{valueLabelColor="grey50"} or
  \code{valueLabelColor=c("#cc3366")}.}

  \item{valueLabelColorNS}{Colour of the non significant
  values (non significant beta coefficients) inside the
  diagrams.  Only applies, when parameter
  \code{showValueLabels} is set to \code{TRUE}. Use any
  valid colour value, e.g.  \code{valueLabelColor="grey50"}
  or \code{valueLabelColor=c("#cc3366")}.}

  \item{valueLabelSize}{Size of the value labels. Default
  is 4.5. Recommended Values range from 2 to 8}

  \item{valueLabelAlpha}{The alpha level (transparancy) of
  the value labels. Default is 0.8, use any value from 0 to
  1.}

  \item{axisLabelAngle.x}{Angle for axis-labels where the
  estimates are printed. Note that due to the coordinate
  flip, the acutal y-axis with estimates labels are
  appearing on the x-axis.}

  \item{axisLabelAngle.y}{Angle for axis-labels, passed as
  numeric value.}

  \item{errorBarColor}{The color of the error bars that
  indicate the confidence intervalls of the
  beta-coefficients}

  \item{errorBarWidth}{The width of the error bar ends.
  Default is 0}

  \item{errorBarSize}{The size of the error bar. Default is
  0.8}

  \item{pointColor}{The colour of the points that indicate
  the beta-value.}

  \item{pointSize}{The size of the points that indicate the
  beta-value. Default is 3.}

  \item{pointColorStdBeta}{The colour of the points that
  indicate the standardized beta-value.}

  \item{pointSizeStdBeta}{The size of the points that
  indicate the standardized beta-value. Default is 3.}

  \item{stdBetaLineType}{The standardized beta-value dots
  are connected by a thin line for a better overview. With
  this parameter you can specify the line type.}

  \item{stdBetaLineAlpha}{The alpha-value for the line that
  connects the standardized beta-value dots.}

  \item{interceptLineType}{The linetype of the intercept
  line (zero point). Default is \code{2} (dashed line).}

  \item{interceptLineColor}{The color of the intercept
  line. Default value is \code{"grey70"}.}

  \item{breakTitleAt}{Wordwrap for diagram title.
  Determines how many chars of the title are displayed in
  one line and when a line break is inserted into the
  title}

  \item{breakLabelsAt}{Wordwrap for diagram labels.
  Determines how many chars of the category labels are
  displayed in one line and when a line break is inserted}

  \item{gridBreaksAt}{Sets the breaks on the y axis, i.e.
  at every n'th position a major grid is being printed.
  Default is \code{NULL}, so \code{\link{pretty}} gridbeaks
  will be used.}

  \item{borderColor}{User defined color of whole diagram
  border (panel border).}

  \item{axisColor}{User defined color of axis border (y-
  and x-axis, in case the axes should have different colors
  than the diagram border).}

  \item{theme}{Specifies the diagram's background theme.
  Default (parameter \code{NULL}) is a gray background with
  white grids.  \itemize{ \item Use \code{"bw"} for a white
  background with gray grids \item \code{"classic"} for a
  classic theme (black border, no grids) \item
  \code{"minimal"} for a minimalistic theme (no border,gray
  grids) or \item \code{"none"} for no borders, grids and
  ticks.  }}

  \item{flipCoordinates}{If \code{TRUE} (default),
  predictors are plotted on the left y-axis and estimate
  values are plotted on the x-axis.}

  \item{majorGridColor}{Specifies the color of the major
  grid lines of the diagram background.}

  \item{minorGridColor}{Specifies the color of the minor
  grid lines of the diagram background.}

  \item{hideGrid.x}{If \code{TRUE}, the x-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{hideGrid.y}{If \code{TRUE}, the y-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{showTickMarks}{Whether tick marks of axes should be
  shown or not}

  \item{showValueLabels}{Whether the beta and standardized
  beta values should be plotted to each dot or not.}

  \item{labelDigits}{The amount of digits for rounding the
  estimations (see \code{showValueLabels}).  Default is 2,
  i.e. estimators have 2 digits after decimal point.}

  \item{showPValueLabels}{Whether the significance levels
  of each coefficient should be appended to values or not}

  \item{showModelSummary}{If \code{TRUE} (default), a
  summary of the regression model with Intercept, R-square,
  F-Test and AIC-value is printed to the lower right corner
  of the diagram.}

  \item{showStandardBeta}{Whether or not the dots for the
  standardized beta values should be plotted to the
  diagram.}

  \item{showStandardBetaLine}{Whether or not the connecting
  line for the standardized beta values should be plotted
  to the diagram. Default is \code{FALSE}.}

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
Plot beta coefficients of linear regressions with
confidence intervalls as dot plot (forest plot).
Additionally, the standardized beta values are plotted as
red dots.
}
\note{
Based on an an idea from surefoss:
\url{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/}
}
\examples{
# fit linear model
fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)

# plot estimates with CI and standardized beta-values
sjp.lm(fit, gridBreaksAt=2)

# plot estimates with CI without standardized beta-values
# and with narrower tick marks (because "gridBreaksAt" was not specified)
sjp.lm(fit, showStandardBeta=FALSE)
}
\references{
\itemize{ \item
\url{http://strengejacke.wordpress.com/sjplot-r-package/}
\item
\url{http://strengejacke.wordpress.com/2013/03/22/plotting-lm-and-glm-models-with-ggplot-rstats/}
}
}
\seealso{
\code{\link{sjp.lm.ma}} \cr \code{\link{sjp.reglin}} \cr
\code{\link{sjp.lm.int}} \cr \code{\link{sjp.scatter}} \cr
\code{\link{sju.betaCoef}}
}

