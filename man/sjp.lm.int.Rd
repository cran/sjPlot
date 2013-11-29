\name{sjp.lm.int}
\alias{sjp.lm.int}
\title{Plot interaction terms of linear models}
\usage{
  sjp.lm.int(fit, smooth = "none", diff = FALSE,
    swapPredictors = FALSE, title = NULL, titleSize = 1.3,
    titleColor = "black", fillColor = "grey",
    fillAlpha = 0.4, lowerBoundColor = "#3366cc",
    upperBoundColor = "#cc3300", lineColor = "#33cc66",
    axisTitle.x = NULL, axisTitle.y = NULL,
    axisLabelColor = "gray30", axisLabelSize = 1.1,
    axisTitleColor = "black", axisTitleSize = 1.3,
    legendLabels = NULL, legendLabelSize = 0.9,
    legendLabelColor = "black", showValueLabels = FALSE,
    valueLabelSize = 4, valueLabelColor = "black",
    valueLabelAlpha = 0.8, breakTitleAt = 50,
    breakLegendLabelsAt = 20, gridBreaksAt = NULL,
    theme = NULL, showTickMarks = TRUE, borderColor = NULL,
    axisColor = NULL, majorGridColor = NULL,
    minorGridColor = NULL, hideGrid.x = FALSE,
    hideGrid.y = FALSE)
}
\arguments{
  \item{fit}{the fitted linear model (lm) object, including
  interaction terms}

  \item{smooth}{smoothes the regression line in case it is
  not linear. Default is \code{none}, so no smoothing is
  applied. Use \code{loess} for loess-smoothing or
  \code{lm} to force linear regression lines.}

  \item{diff}{if \code{FALSE} (default), the minimum and
  maximum interaction effects of predictor 2 on predictor 1
  are shown (one line each). if \code{TRUE}, only the
  difference between minimum and maximum interaction effect
  is shown (single line)}

  \item{swapPredictors}{if \code{TRUE}, the predictor with
  less unique values is printed along the x-axis. Default
  is \code{FALSE}, so the predictor with more unique values
  is printed along the x-axis.}

  \item{title}{a default title used for the plots. Default
  value is \code{NULL}, which means that each plot's title
  includes the dependent variable as well as the names of
  the interaction terms.}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{fillColor}{fill color of the shaded area between
  the minimum and maximum lines. Default is \code{grey}.
  Either set \code{fillColor} to \code{NULL} or use 0 for
  \code{fillAlpha} if you want to hide the shaded area.}

  \item{fillAlpha}{alpha value (transparancy) of the shaded
  area between the minimum and maximum lines. Default is
  0.4.  Use either 0 or set \code{fillColor} to \code{NULL}
  if you want to hide the shaded area.}

  \item{lowerBoundColor}{the color of the line indicating
  the lower bound of the interaction term.  Default value
  is \code{"#3366cc"} (blue-like)}

  \item{upperBoundColor}{the color of the line indicating
  the upper bound of the interaction term.  Default value
  is \code{"#cc3300"} (red-like)}

  \item{lineColor}{the color of the line indicating the
  upper difference between lower and upper bound of
  interaction terms. Only applies if \code{diff} is
  \code{TRUE}.  Default value is \code{"#33cc66"}
  (green-like)}

  \item{axisTitle.x}{a default title used for the x-axis.
  Default value is \code{NULL}, which means that each
  plot's x-axis uses the predictor's name as title.}

  \item{axisTitle.y}{a default title used for the y-axis.
  Default value is \code{NULL}, which means that each
  plot's y-axis uses the dependent variable's name as
  title.}

  \item{axisLabelColor}{the color value for the axis labels
  at the tick marks. Default value is \code{"darkgray"}.}

  \item{axisLabelSize}{The size of axis labels. Default is
  1.1, recommended values range between 0.5 and 3.0}

  \item{axisTitleColor}{the color value for the axis titles
  (both x and y). Default value is \code{"black"}.}

  \item{axisTitleSize}{The size of axis titles (both x and
  y). Default is 1.3, recommended values range between 0.5
  and 3.0}

  \item{legendLabels}{Labels for the guide/legend. Default
  is \code{NULL}, so the name of the predictor with
  min/max-effect is used as legend label.}

  \item{legendLabelSize}{The size of legend labels. Default
  is 0.9, recommended values range between 0.5 and 3.0}

  \item{legendLabelColor}{user defined color for legend
  labels. If not specified, black will be used for the
  labels}

  \item{showValueLabels}{if \code{TRUE}, value labels are
  plotted along the lines. Default is \code{FALSE}.}

  \item{valueLabelSize}{size of the value labels. Default
  is 4. Recommended Values range from 2 to 8}

  \item{valueLabelColor}{colour of the values inside the
  diagrams. Only applies, when parameter
  \code{showValueLabels} is set to \code{TRUE}. Use any
  valid colour value, e.g. \code{valueLabelColor="grey50"}
  or \code{valueLabelColor=c("#cc3366")}. Default is
  \code{"black"}.}

  \item{valueLabelAlpha}{the alpha level (transparancy) of
  the value labels. Default is 0.8, use any value from 0 to
  1.}

  \item{breakTitleAt}{Wordwrap for diagram's title.
  Determines how many chars of the title are displayed in
  one line and when a line break is inserted. Default is
  \code{50}.}

  \item{breakLegendLabelsAt}{Wordwrap for diagram legend
  labels. Determines how many chars of the legend labels
  are displayed in one line and when a line break is
  inserted. Default is \code{20}.}

  \item{gridBreaksAt}{Sets the breaks on the y axis, i.e.
  at every n'th position a major grid is being printed.
  Default is \code{NULL}.}

  \item{theme}{specifies the diagram's background theme.
  default (parameter \code{NULL}) is a gray background with
  white grids. Use \code{bw} for a white background with
  gray grids, \code{classic} for a classic theme (black
  border, no grids), \code{minimal} for a minimalistic
  theme (no border, gray grids) or \code{none} for no
  borders, grids and ticks.}

  \item{showTickMarks}{Whether tick marks of axes should be
  shown or not}

  \item{borderColor}{user defined color of whole diagram
  border (panel border)}

  \item{axisColor}{user defined color of axis border (y-
  and x-axis, in case the axes should have different colors
  than}

  \item{majorGridColor}{specifies the color of the major
  grid lines of the diagram background}

  \item{minorGridColor}{specifies the color of the minor
  grid lines of the diagram background}

  \item{hideGrid.x}{If \code{TRUE}, the x-axis-gridlines
  are hidden. Default if \code{FALSE}.}

  \item{hideGrid.y}{If \code{TRUE}, the y-axis-gridlines
  are hidden. Default if \code{FALSE}.}
}
\description{
  Plot regression curves of significant interaction terms
  in linear models (lm).
}
\examples{
# Note that the data sets used in this example may not be perfectly suitable for
# fitting linear models. I just used them because they are part of the R-software.

# fit "dummy" model.
fit <- lm(weight ~ Time * Diet, data=ChickWeight, x=TRUE)

# show summary to see significant interactions
summary(fit)

# plot regression line of interaction terms
sjp.lm.int(fit)
# plot regression line of interaction terms, including value labels
sjp.lm.int(fit, showValueLabels=TRUE)


# fit "dummy" model
fit <- lm(Fertility ~ .*., data=swiss, na.action=na.omit, x=TRUE)

# show summary to see significant interactions
summary(fit)

# plot regression line of interaction terms
sjp.lm.int(fit)
# plot smoothes regression line of interaction terms
sjp.lm.int(fit, smooth="loess")
# plot linear regression line of interaction terms
sjp.lm.int(fit, smooth="lm")
}
\references{
  \url{http://strengejacke.wordpress.com/sjplot-r-package/}
  \cr \cr
  \url{http://strengejacke.wordpress.com/2013/10/31/visual-interpretation-of-interaction-terms-in-linear-models-with-ggplot-rstats/}
}
\seealso{
  \code{\link{sjp.lm}} \cr \code{\link{sjp.reglin}} \cr
  \code{\link{sjp.lm.ma}}
}

