\name{sjp.glm}
\alias{sjp.glm}
\title{Plot odds ratios (forest plots)}
\usage{
sjp.glm(fit, sortOdds = TRUE, title = NULL, titleSize = 1.3,
  titleColor = "black", axisLabels.y = NULL, axisLabelSize = 1.1,
  axisLabelAngle.x = 0, axisLabelAngle.y = 0, axisLabelColor = "gray30",
  axisTitle.x = "Odds Ratios", axisTitleSize = 1.2,
  axisTitleColor = c("#444444"), axisLimits = NULL, breakTitleAt = 50,
  breakLabelsAt = 12, gridBreaksAt = 0.5, transformTicks = FALSE,
  type = "dots", hideErrorBars = FALSE, errorBarWidth = 0,
  errorBarSize = 0.8, errorBarLineType = 1, pointSize = 3,
  colorPalette = "Paired", barColor = NULL, barWidth = 0.3,
  barAlpha = 1, valueLabelColor = "black", valueLabelSize = 4.5,
  valueLabelAlpha = 1, axisColor = NULL, borderColor = NULL,
  barOutline = FALSE, outlineColor = "black", interceptLineType = 2,
  interceptLineColor = "grey70", majorGridColor = NULL,
  minorGridColor = NULL, hideGrid.x = FALSE, hideGrid.y = FALSE,
  theme = NULL, flipCoordinates = TRUE, showIntercept = FALSE,
  showAxisLabels.y = TRUE, showTickMarks = TRUE, showValueLabels = TRUE,
  labelDigits = 2, showPValueLabels = TRUE, showModelSummary = TRUE,
  returnPlot = FALSE)
}
\arguments{
  \item{fit}{The fitted model of a logistic regression
  (glm-Object).}

  \item{sortOdds}{If \code{TRUE} (default), the odds ratios
  are ordered according their OR value from highest first
  to lowest last. Use \code{FALSE} if you don't want to
  change the order of the predictors.}

  \item{title}{Diagram's title as string.  Example:
  \code{title=c("my title")}}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{axisLabels.y}{Labels of the predictor variables
  (independent vars, odds) that are used for labelling the
  axis. Passed as vector of strings.  Example:
  \code{axisLabels.y=c("Label1", "Label2", "Label3")} Note:
  If you use the \code{\link{sji.SPSS}} function and the
  \code{\link{sji.getValueLabels}} function, you receive a
  \code{list} object with label string. The labels may also
  be passed as list object. They will be unlisted and
  converted to character vector automatically.}

  \item{axisLabelSize}{The size of value labels in the
  diagram. Default is 1.1, recommended values range between
  0.7 and 3.0}

  \item{showAxisLabels.y}{Whether odds names (predictor
  labels) should be shown or not.}

  \item{showTickMarks}{Whether tick marks of axes should be
  shown or not.}

  \item{axisTitle.x}{A label ("title") for the x axis.}

  \item{axisTitleColor}{The color of the x axis label.}

  \item{axisTitleSize}{The size of the x axis label.}

  \item{axisLimits}{Defines the range of the axis where the
  beta coefficients and their confidence intervalls are
  drawn. By default, the limits range from the lowest
  confidence interval to the highest one, so the diagram
  has maximum zoom. Use your own values as 2-value-vector,
  for instance: \code{limits=c(-0.8,0.8)}.}

  \item{axisLabelAngle.x}{Angle for axis-labels where the
  odds ratios are printed. Note that due to the coordinate
  flip, the acutal y-axis with odds ratio labels are
  appearing on the x-axis.}

  \item{axisLabelAngle.y}{Angle for axis-labels where the
  predictor labels (\code{axisLabels.y}) are printed. Note
  that due to the coordinate flip, the acutal x-axis with
  predictor labels are appearing on the y-axis.}

  \item{breakTitleAt}{Wordwrap for diagram title.
  Determines how many chars of the title are displayed in
  one line and when a line break is inserted into the
  title}

  \item{breakLabelsAt}{Wordwrap for diagram labels.
  Determines how many chars of the category labels are
  displayed in one line and when a line break is inserted}

  \item{gridBreaksAt}{Sets the breaks on the y axis, i.e.
  at every n'th position a major grid is being printed.
  Default is 0.5}

  \item{transformTicks}{if \code{TRUE}, the grid bars have
  exponential distances, i.e. they visually have the same
  distance from one grid bar to the next. Default is
  \code{FALSE} which means that grids are plotted on every
  \code{gridBreaksAt}'s position, thus the grid bars become
  narrower with higher odds ratio values.}

  \item{type}{Indicates Whether the Odds Ratios should be
  plotted as \code{"dots"} (aka forest plots, default) or
  as \code{"bars"}.}

  \item{hideErrorBars}{If \code{TRUE}, the error bars that
  indicate the confidence intervals of the odds ratios are
  not shown. Only applies if parameter \code{type} is
  \code{bars}. Default value is \code{FALSE}.}

  \item{pointSize}{The size of the points that indicate the
  beta-value. Default is 3.}

  \item{barColor}{A vector with colors for representing the
  odds values (i.e. points and error bars in case the
  parameter \code{type} is \code{"dots"} or the bar charts
  in case of \code{"bars"}). The first color value
  indicates odds ratio values larger than 1, the second
  color value indicates odds ratio values lower or equal to
  1.  Default colors is a blue/red-scheme. You can also
  use: \itemize{ \item \code{"bw"} or \code{"black"} for
  only one colouring in almost black \item \code{"gray"},
  \code{"grey"} or \code{"gs"} for a grayscale \item
  \code{"brewer"} for colours from the color brewer
  palette.  } If barColors is \code{"brewer"}, use the
  \code{colorPalette} parameter to specify a palette of the
  color brewer.  Else specify your own color values as
  vector (e.g. \code{barColors=c("#f00000", "#00ff00")}).}

  \item{colorPalette}{If parameter \code{barColor} is
  \code{brewer}, specify a color palette from the color
  brewer here.  All color brewer palettes supported by
  ggplot are accepted here.}

  \item{barWidth}{The width of the bars in bar charts. only
  applies if parameter \code{type} is \code{bars}. Default
  is 0.5}

  \item{barAlpha}{The alpha value of the bars in bar
  charts. only applies if parameter \code{type} is
  \code{bars}. Default is 1}

  \item{axisLabelColor}{Colour of the tick labels at the
  axis (variable names, odds names).}

  \item{valueLabelColor}{The colour of the odds values.
  These values are printed above the plots respectively
  beside the bar charts. default color is \code{"black"}.}

  \item{valueLabelSize}{Size of the value labels. Drfault
  is 4.5. Recommended Values range from 2 to 8}

  \item{valueLabelAlpha}{The alpha level (transparancy) of
  the value labels. Default is 1, use any value from 0 to
  1.}

  \item{axisColor}{User defined color of axis border (y-
  and x-axis, in case the axes should have different colors
  than the diagram border).}

  \item{borderColor}{User defined color of whole diagram
  border (panel border).}

  \item{barOutline}{If \code{TRUE}, each bar gets a colored
  outline. only applies if parameter \code{type} is
  \code{bars}.  Default is \code{FALSE}.}

  \item{outlineColor}{The color of the bar outline. Only
  applies, if \code{barOutline} is set to \code{TRUE}.
  Default is black.}

  \item{interceptLineType}{The linetype of the intercept
  line (zero point). Default is \code{2} (dashed line).}

  \item{interceptLineColor}{The color of the intercept
  line. Default value is \code{"grey70"}.}

  \item{errorBarWidth}{The width of the error bar ends.
  Default is \code{0}}

  \item{errorBarSize}{The size (thickness) of the error
  bars. Default is \code{0.8}}

  \item{errorBarLineType}{The linetype of error bars.
  Default is \code{1} (solid line).}

  \item{majorGridColor}{Specifies the color of the major
  grid lines of the diagram background.}

  \item{minorGridColor}{Specifies the color of the minor
  grid lines of the diagram background.}

  \item{hideGrid.x}{If \code{TRUE}, the x-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{hideGrid.y}{If \code{TRUE}, the y-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{theme}{Specifies the diagram's background theme.
  Default (parameter \code{NULL}) is a gray background with
  white grids.  \itemize{ \item Use \code{"bw"} for a white
  background with gray grids \item \code{"classic"} for a
  classic theme (black border, no grids) \item
  \code{"minimal"} for a minimalistic theme (no border,gray
  grids) or \item \code{"none"} for no borders, grids and
  ticks.  } The ggplot-object can be returned with
  \code{returnPlot} set to \code{TRUE} in order to further
  modify the plot's theme.}

  \item{flipCoordinates}{If \code{TRUE} (default),
  predictors are plotted on the left y-axis and estimate
  values are plotted on the x-axis.}

  \item{showIntercept}{If \code{TRUE}, the intercept of the
  fitted model is also plotted.  Default is \code{FALSE}.
  Please note that due to exp-transformation of estimates,
  the intercept in some cases can not be calculated, thus
  the function call is interrupted and no plot printed.}

  \item{showValueLabels}{Whether the beta and standardized
  beta values should be plotted to each dot or not.}

  \item{labelDigits}{The amount of digits for rounding the
  estimations (see \code{showValueLabels}).  Default is 2,
  i.e. estimators have 2 digits after decimal point.}

  \item{showPValueLabels}{Whether the significance levels
  of each coefficient should be appended to values or not.}

  \item{showModelSummary}{If \code{TRUE} (default), a
  summary of the regression model with Intercept, R-square,
  F-Test and AIC-value is printed to the lower right corner
  of the diagram.}

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
Plot odds ratios with confidence intervalls as bar chart or
dot plot
}
\note{
Based on the script from surefoss:
\url{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/}
}
\examples{
# prepare dichotomous dependent variable
y <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)

# fit model
fitOR <- glm(y ~ swiss$Education + swiss$Examination + swiss$Infant.Mortality + swiss$Catholic,
             family=binomial(link="logit"))

# print Odds Ratios as dots
sjp.glm(fitOR)

# print Odds Ratios as bars
sjp.glm(fitOR, type="bars")


# -------------------------------
# Predictors for negative impact
# of care. Data from the EUROFAMCARE
# sample dataset
# -------------------------------
data(efc)
# retrieve predictor variable labels
labs <- sji.getVariableLabels(efc)
predlab <- c(labs[['c161sex']],
             labs[['e42dep']],
             paste0(labs[['c172code']], " (mid)"),
             paste0(labs[['c172code']], " (high)"))
# create binary response
y <- ifelse(efc$neg_c_7<median(na.omit(efc$neg_c_7)), 0, 1)
# create dummy variables for educational status
edu.mid <- ifelse(efc$c172code==2, 1, 0)
edu.high <- ifelse(efc$c172code==3, 1, 0)
# create data frame for fitted model
df <- na.omit(as.data.frame(cbind(y,
                as.factor(efc$c161sex),
                as.factor(efc$e42dep),
                as.factor(edu.mid),
                as.factor(edu.high))))
# fit model
fit <- glm(y ~., data=df, family=binomial(link="logit"))
# plot odds
sjp.glm(fit, title=labs[['neg_c_7']], axisLabels.y=predlab)
}
\references{
\itemize{ \item
\url{http://strengejacke.wordpress.com/sjplot-r-package/}
\item
\url{http://strengejacke.wordpress.com/2013/03/22/plotting-lm-and-glm-models-with-ggplot-rstats/}
\item
\url{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/}
}
}
\seealso{
\code{\link{sjp.glm.ma}}
}

