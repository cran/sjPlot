\name{sjp.lm1}
\alias{sjp.lm1}
\title{Plot regression line of fitted lm}
\usage{
sjp.lm1(fit, data, title = NULL, titleSize = 1.3, titleColor = "black",
  breakTitleAt = 50, axisLabel.x = NULL, axisLabel.y = NULL,
  breakLabelsAt = 12, axisLabelSize = 1.1, axisLabelColor = "gray30",
  axisTitleSize = 1.4, axisTitleColor = c("#444444"), lineColor = "blue",
  modsumLabelColor = "grey20", modsumLabelSize = 4.5, showCI = TRUE,
  ciLevel = 0.95, pointAlpha = 0.2, pointColor = "black",
  showScatterPlot = TRUE, showLoess = FALSE, loessLineColor = "red",
  showLoessCI = FALSE, loessCiLevel = 0.95, showModelSummary = TRUE,
  borderColor = NULL, axisColor = NULL, theme = NULL,
  majorGridColor = NULL, minorGridColor = NULL, hideGrid.x = FALSE,
  hideGrid.y = FALSE, showTickMarks = TRUE, printPlot = TRUE)
}
\arguments{
  \item{fit}{The model of the linear regression
  (lm-Object).}

  \item{data}{The data/dataset/dataframe used to fit the
  model}

  \item{title}{Diagram's title as string.  Example:
  \code{title=c("my title")}}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{breakTitleAt}{Wordwrap for diagram title.
  Determines how many chars of the title are displayed in
  one line and when a line break is inserted into the
  title}

  \item{axisLabel.x}{Labels of the predictor (independent
  variable) that is used for labelling the axis. Passed as
  string.  Example: \code{axisLabel.x=c("My Predictor
  Var")}.  Note: If you use the \code{\link{sji.SPSS}}
  function and the \code{\link{sji.getVariableLabels}}
  function, you receive a character vector with variable
  label strings. You can use it like so:
  \code{axisLabel.x=sji.getVariableLabels(efc)['quol_5']}}

  \item{axisLabel.y}{Labels of the response (dependent
  variable) that is used for labelling the axis. Passed as
  string.  Example: \code{axisLabel.y=c("My Depdendent
  Var")}.  Note: If you use the \code{\link{sji.SPSS}}
  function and the \code{\link{sji.getVariableLabels}}
  function, you receive a character vector with variable
  label strings. You can use it like so:
  \code{axisLabel.y=sji.getVariableLabels(efc)['neg_c_7']}}

  \item{breakLabelsAt}{Wordwrap for axis labels. Determines
  how many chars of the category labels are displayed in
  one line and when a line break is inserted}

  \item{axisLabelSize}{The size of axis tick marks Default
  is 1.1.}

  \item{axisLabelColor}{The color of the axis tick marks.
  Default is a dark grey (grey30).}

  \item{axisTitleColor}{The color of the axis labels
  (response and predictor label). Default is a dark grey.}

  \item{axisTitleSize}{The size of the axis label (response
  and predictor label). Default is 1.4.}

  \item{lineColor}{The color of the regression line.
  Default is \code{"blue"}.}

  \item{modsumLabelColor}{Colour of the model summary
  inside the diagrams. Only applies, when parameter
  \code{showModelSummary} is set to \code{TRUE}. Use any
  valid colour value, e.g. \code{modsumLabelColor="grey50"}
  or \code{valueLabelColor=c("#cc3366")}.}

  \item{modsumLabelSize}{Size of the model summary text.
  Default is 4.5. Recommended Values range from 2 to 8}

  \item{showCI}{If \code{TRUE} (default), a confidence
  region for the regression line will be plotted. Use
  \code{ciLevel} to specifiy the confidence level.}

  \item{ciLevel}{The confidence level of the confidence
  region. Only applies when \code{showCI} is \code{TRUE}.
  Default is 0.95.}

  \item{pointAlpha}{The alpha values of the scatter plot's
  point-geoms.  Default is 0.2.}

  \item{pointColor}{The color of the scatter plot's
  point-geoms. Only applies when \code{showScatterPlot} is
  \code{TRUE}. Default is \code{"black"}.}

  \item{showScatterPlot}{If \code{TRUE} (default), a
  scatter plot of response and predictor values for each
  predictor of the fitted model \code{fit} is plotted.}

  \item{showLoess}{If \code{TRUE}, an additional
  loess-smoothed line is plotted.}

  \item{loessLineColor}{The color of the loess-smoothed
  line. Default is \code{"red"}. Only applies, if
  \code{showLoess} is \code{TRUE}.}

  \item{showLoessCI}{If \code{TRUE}, a confidence region
  for the loess-smoothed line will be plotted. Default is
  \code{FALSE}. Use \code{loessCiLevel} to specifiy the
  confidence level.  Only applies, if \code{showLoess} is
  \code{TRUE}.}

  \item{loessCiLevel}{The confidence level of the
  loess-line's confidence region.  Only applies, if
  \code{showLoessCI} is \code{TRUE}. Default is 0.95.}

  \item{showModelSummary}{If \code{TRUE} (default), a
  summary of the regression model with Intercept, R-square,
  F-Test and AIC-value is printed to the lower right corner
  of the diagram.}

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
Plot a regression line with confidence interval for a
fitted model with only one predictor (i.e. \code{lm(y~y)}).
This function may plot two lines: The resulting linear
regression line including confidence interval (in blue) by
default, and a loess-smoothed line without confidence
interval (in red) if parameter \code{showLoess} is
\code{TRUE}.  The better the linear relationship of
predictor and response is, the more both lines should
overlap (i.e. the red loess-smoothed line is almost
linear). \cr \cr Furthermore, a scatter plot of response
and predictor values is plotted.
}
\examples{
# load sample data
data(efc)
# fit model
fit <- lm(neg_c_7 ~ quol_5, data=efc, na.action=na.omit)
# plot regression line
sjp.lm1(fit, efc)
# plot regression line with label strings
sjp.lm1(fit,
        efc,
        axisLabel.x=sji.getVariableLabels(efc)['quol_5'],
        axisLabel.y=sji.getVariableLabels(efc)['neg_c_7'],
        showLoess=TRUE)
}
\seealso{
\code{\link{sjp.lm}} \cr \code{\link{sjp.reglin}} \cr
\code{\link{sjp.scatter}}
}

