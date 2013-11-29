\name{sjp.aov1}
\alias{sjp.aov1}
\title{Plot One-Way-Anova tables}
\usage{
  sjp.aov1(depVar, grpVar, meansums = FALSE, type = "dots",
    hideErrorBars = FALSE, title = NULL, titleSize = 1.3,
    titleColor = "black", axisLabels.y = NULL,
    reverseOrder = FALSE, stringIntercept = "(Intercept)",
    showAxisLabels.y = TRUE, axisLabelSize = 1.1,
    axisLabelColor = "gray30", axisTitle.x = NULL,
    axisTitleSize = 1.2, axisTitleColor = c("#444444"),
    axisLimits = NULL, valueLabelColor = "grey20",
    valueLabelColorNS = "grey50", valueLabelSize = 4.5,
    valueLabelAlpha = 0.8, axisLabelAngle.y = 0,
    errorBarColor = NULL, errorBarWidth = 0,
    errorBarSize = 0.8, errorBarLineType = 1,
    pointColor = "#3366a0", pointSize = 3,
    barColor = "#3366a0", barWidth = 0.5, barAlpha = 1,
    barOutline = FALSE, outlineColor = "black",
    breakTitleAt = 50, breakLabelsAt = 12,
    gridBreaksAt = NULL, borderColor = NULL,
    axisColor = NULL, theme = NULL, majorGridColor = NULL,
    minorGridColor = NULL, hideGrid.x = FALSE,
    hideGrid.y = FALSE, showTickMarks = TRUE,
    showValueLabels = TRUE, showPValueLabels = TRUE,
    showModelSummary = TRUE, returnPlot = FALSE)
}
\arguments{
  \item{depVar}{The dependent variable. Will be used with
  following formular: \code{aov(depVar ~ grpVar)}}

  \item{grpVar}{The grouping variable, as unordered factor.
  Will be used with following formular: \code{aov(depVar ~
  grpVar)}}

  \item{meansums}{If \code{TRUE}, the values reported are
  the true group mean values. If \code{FALSE} (default),
  the values are reported in the standard way, i.e. the
  values indicate the difference of the group mean in
  relation to the intercept (reference group).}

  \item{type}{Indicates Whether the group means should be
  plotted as \code{"dots"} (aka forest plots, default) or
  as \code{"bars"}.}

  \item{hideErrorBars}{If \code{TRUE}, the error bars that
  indicate the confidence intervals of the group means are
  not shown. Only applies if parameter \code{type} is
  \code{"bars"}. Default value is \code{FALSE}.}

  \item{title}{Diagram's title as string.  Example:
  \code{title=c("my title")}}

  \item{titleSize}{The size of the plot title. Default is
  1.3.}

  \item{titleColor}{The color of the plot title. Default is
  \code{"black"}.}

  \item{axisLabels.y}{Value labels of the grouping variable
  \code{grpVar} that are used for labelling the grouping
  variable axis. Passed as vector of strings.  Example:
  \code{axisLabels.y=c("Label1", "Label2", "Label3")}. \cr
  Note: If you use the \code{\link{sji.SPSS}} function and
  the \code{\link{sji.getValueLabels}} function, you
  receive a list object with label string. The labels may
  also be passed as list object. They will be unlisted and
  converted to character vector automatically. See examples
  below. \cr Note: In case \code{type} is \code{"bars"},
  the \code{grpVar} will be plotted along the x-axis.}

  \item{reverseOrder}{If \code{TRUE}, the order of the
  factor categories (groups) is reversed.  Default is
  \code{FALSE}.}

  \item{stringIntercept}{A string that indicates the
  reference group (intercept), that is appended to the
  value label of the grouping variable. Default is
  \code{"(Intercept)"}.}

  \item{showAxisLabels.y}{Whether y axis text (category
  value) should be shown (use \code{TRUE}) or not. Default
  is \code{TRUE}.}

  \item{axisLabelSize}{The size of value labels in the
  diagram. Default is 4, recommended values range between 2
  and 8.}

  \item{axisLabelColor}{The color of the category labels
  (predictor labels). Default is a dark grey (grey30).}

  \item{axisTitle.x}{A label for the x axis. Default is
  \code{NULL}, which means no x-axis title.}

  \item{axisTitleColor}{The color of the x axis label.}

  \item{axisTitleSize}{The size of the x axis label.
  Default is 1.2.}

  \item{axisLimits}{Defines the range of the axis where the
  beta coefficients and their confidence intervalls are
  drawn. By default, the limits range from the lowest
  confidence interval to the highest one, so the diagram
  has maximum zoom. Use your own values as 2-value-vector,
  for instance: \code{limits=c(-0.8,0.8)}.}

  \item{valueLabelColor}{Colour of the values inside the
  diagram. Only applies, when parameter
  \code{showValueLabels} is set to \code{TRUE}. Use any
  valid colour value, e.g. \code{valueLabelColor="grey50"}
  or \code{valueLabelColor=c("#cc3366")}. Default is
  \code{"grey20"}.}

  \item{valueLabelColorNS}{Colour of the non significant
  values inside the diagram.  Only applies, when parameter
  \code{showValueLabels} is set to \code{TRUE}. Use any
  valid colour value, e.g.  \code{valueLabelColor="grey50"}
  or \code{valueLabelColor=c("#cc3366")}. Default is
  \code{"grey50"}.}

  \item{valueLabelSize}{Size of the value labels. Drfault
  is 4.5. Recommended Values range from 2 to 8}

  \item{valueLabelAlpha}{The alpha level (transparancy) of
  the value labels. Default is 0.8, use any value from 0 to
  1.}

  \item{axisLabelAngle.y}{Angle for axis-labels, passed as
  numeric value.}

  \item{errorBarColor}{The color of the error bars that
  indicate the confidence intervalls of the group means.
  Default is \code{NULL}, which means that if \code{type}
  is \code{"dots"}, the \code{pointColor} value will be
  used as error bar color. In case \code{type} is
  \code{"bars"}, black will be used as error bar color.}

  \item{errorBarWidth}{The width of the error bar ends.
  Default is 0.}

  \item{errorBarSize}{The size of the error bar. Default is
  0.8.}

  \item{errorBarLineType}{The linetype of error bars.
  Default is \code{1} (solid line).}

  \item{pointColor}{The colour of the points that indicate
  the beta-value.}

  \item{pointSize}{The size of the points that indicate the
  beta-value. Default is 3.}

  \item{barColor}{The color of the bars in bar charts. Only
  applies if parameter \code{type} is \code{"bars"}.
  Default is \code{"#3366a0"}.}

  \item{barWidth}{The width of the bars in bar charts. Only
  applies if parameter \code{type} is \code{"bars"}.
  Default is 0.5}

  \item{barAlpha}{The alpha value of the bars in bar
  charts. Only applies if parameter \code{type} is
  \code{"bars"}. Default is 1}

  \item{barOutline}{If \code{TRUE}, each bar gets a colored
  outline. Only applies if parameter \code{type} is
  \code{bars}.  Default is \code{FALSE}.}

  \item{outlineColor}{The color of the bar outline. Only
  applies, if \code{barOutline} is set to \code{TRUE}.
  Default is black.}

  \item{breakTitleAt}{Wordwrap for diagram title.
  Determines how many chars of the title are displayed in
  one line and when a line break is inserted into the
  title}

  \item{breakLabelsAt}{Wordwrap for diagram labels.
  Determines how many chars of the category labels are
  displayed in one line and when a line break is inserted}

  \item{gridBreaksAt}{Sets the breaks on the y axis, i.e.
  at every n'th position a major grid is being printed.
  Default is 2.}

  \item{borderColor}{User defined color of whole diagram
  border (panel border).}

  \item{axisColor}{User defined color of axis border (y-
  and x-axis, in case the axes should have different colors
  than the diagram border).}

  \item{theme}{specifies The diagram's background theme.
  default (parameter \code{NULL}) is a gray background with
  white grids. Use \code{"bw"} for a white background with
  gray grids, \code{"classic"} for a classic theme (black
  border, no grids), \code{"minimal"} for a minimalistic
  theme (no border, gray grids) or \code{"none"} for no
  borders, grids and ticks.}

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

  \item{showValueLabels}{Whether the value labels (mean
  differences) should be plotted to each dot or not.}

  \item{showPValueLabels}{Whether the significance levels
  of each category/group should be appended to values or
  not.}

  \item{showModelSummary}{If \code{TRUE} (default), a
  summary of the anova model with Sum of Squares between
  groups (ssb), Sum of Squares within groups (ssw),
  multiple and adjusted R-square and F-Test is printed to
  the lower right corner of the diagram. Default is
  \code{TRUE}.}

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
  Plot One-Way-Anova table sum of squares (SS) of each
  factor level (group) against the dependent variable. The
  SS of the factor variable against the dependent variable
  (variance within and between groups) is printed to the
  model summary.
}
\examples{
data(efc)
sjp.aov1(efc$c12hour, as.factor(efc$e42dep))


data(efc)
efc.val <- sji.getValueLabels(efc)
efc.var <- sji.getVariableLabels(efc)
sjp.aov1(efc$c12hour,
         as.factor(efc$e42dep),
         axisLabels.y=efc.val['e42dep'],
         axisTitle.x=efc.var[['c12hour']])

sjp.aov1(efc$c12hour,
         as.factor(efc$e42dep),
         axisLabels.y=efc.val['e42dep'],
         title=efc.var[['c12hour']],
         type="bars",
         meansums=TRUE,
         hideErrorBars=TRUE,
         theme="minimal",
         minorGridColor="white",
         showTickMarks=FALSE,
         showModelSummary=FALSE,
         hideGrid.x=TRUE)
}
\seealso{
  \link{sju.aov1.levene}
}

