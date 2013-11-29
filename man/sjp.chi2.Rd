\name{sjp.chi2}
\alias{sjp.chi2}
\title{Plot Pearson's Chi2-Test of multiple contingency tables}
\usage{
  sjp.chi2(df,
    title = "Pearson's Chi2-Test of Independence",
    titleSize = 1.3, titleColor = "black",
    axisLabels = NULL, valueLabelColor = "black",
    valueLabelSize = 4.5, valueLabelAlpha = 1,
    outlineColor = "black", outlineSize = 0.5,
    axisColor = NULL, axisLabelSize = 1.1,
    axisLabelColor = "gray30", axisLabelAngle.x = 0,
    axisLabelAngle.y = 0, breakTitleAt = 50,
    breakLabelsAt = 12, hideLegend = TRUE,
    legendTitle = NULL, returnPlot = FALSE)
}
\arguments{
  \item{df}{a data frame of (dichotomous) factor
  variables.}

  \item{title}{Title of the diagram, plotted above the
  whole diagram panel}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{axisLabels}{Labels for the x- andy y-axis}

  \item{valueLabelColor}{the color of the value labels
  (numbers) inside the diagram}

  \item{valueLabelSize}{The size of value labels in the
  diagram. Default is 4.5, recommended values range between
  2 and 8}

  \item{valueLabelAlpha}{specify the transparancy (alpha
  value) of value labels}

  \item{outlineColor}{defines the outline color of geoms
  (circles or tiles). Default is black.}

  \item{outlineSize}{defines the outline size of geoms
  (circles or tiles). Default is 1.}

  \item{axisColor}{user defined color of axis border (y-
  and x-axis, in case the axes should have different colors
  than the diagram border)}

  \item{axisLabelSize}{The size of variable labels at the
  axes. Default is 1.1, recommended values range between
  0.5 and 3.0}

  \item{axisLabelColor}{user defined color for axis labels.
  If not specified, a default dark gray color palette will
  be used for the labels}

  \item{axisLabelAngle.x}{angle for x-axis-labels}

  \item{axisLabelAngle.y}{angle for y-axis-labels}

  \item{breakTitleAt}{Wordwrap for diagram title.
  Determines how many chars of the title are displayed in
  one line and when a line break is inserted into the
  title}

  \item{breakLabelsAt}{Wordwrap for diagram labels.
  Determines how many chars of the category labels are
  displayed in one line and when a line break is inserted}

  \item{hideLegend}{show or hide the legend. The legend
  indicates the strength of correlations by gradient colour
  fill.}

  \item{legendTitle}{the legend title, provided as string,
  e.g. \code{legendTitle=c("Strength of correlation")}.
  Default is \code{NULL}, hence no legend title is used.}

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
  Plot Pearson's Chi2-Test of multiple contingency tables
  as ellipses or tiles.  Requires a data frame with
  dichotomous (dummy) variables.  Calculation of
  Chi2-matrix taken from following blog-posting:
  \url{http://talesofr.wordpress.com/2013/05/05/ridiculously-photogenic-factors-heatmap-with-p-values/}
}
\examples{
# create data frame with 5 dichotomous (dummy) variables
df <- data.frame(as.factor(sample(1:2, 100, replace=TRUE)),
                 as.factor(sample(1:2, 100, replace=TRUE)),
                 as.factor(sample(1:2, 100, replace=TRUE)),
                 as.factor(sample(1:2, 100, replace=TRUE)),
                 as.factor(sample(1:2, 100, replace=TRUE)))
# create variable labels
items <- list(c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5"))

# plot Chi2-contingency-table
sjp.chi2(df, axisLabels=items)
}
\references{
  \url{http://strengejacke.wordpress.com/sjplot-r-package/}
  \cr
  \url{http://talesofr.wordpress.com/2013/05/05/ridiculously-photogenic-factors-heatmap-with-p-values/}
}

