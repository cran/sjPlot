# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("Row", "Column", "p.value"))


#' @title Plot Pearson's Chi2-Test of multiple contingency tables
#' @name sjp.chi2
#' @references \url{http://strengejacke.wordpress.com/sjplot-r-package/} \cr
#'             \url{http://talesofr.wordpress.com/2013/05/05/ridiculously-photogenic-factors-heatmap-with-p-values/}
#' 
#' @description Plot Pearson's Chi2-Test of multiple contingency tables as ellipses or tiles. 
#'                Requires a data frame with dichotomous (dummy) variables.
#'                Calculation of Chi2-matrix taken from following blog-posting:
#'                \url{http://talesofr.wordpress.com/2013/05/05/ridiculously-photogenic-factors-heatmap-with-p-values/}
#' 
#' @param df a data frame of (dichotomous) factor variables.
#' @param title Title of the diagram, plotted above the whole diagram panel
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param axisLabels Labels for the x- andy y-axis
#' @param valueLabelColor the color of the value labels (numbers) inside the diagram
#' @param valueLabelSize The size of value labels in the diagram. Default is 4.5, recommended values range
#'          between 2 and 8
#' @param valueLabelAlpha specify the transparancy (alpha value) of value labels
#' @param outlineColor defines the outline color of geoms (circles or tiles). Default is black.
#' @param outlineSize defines the outline size of geoms (circles or tiles). Default is 1.
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border)
#' @param axisLabelSize The size of variable labels at the axes. Default is 1.1, recommended values range
#'          between 0.5 and 3.0
#' @param axisLabelColor user defined color for axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels
#' @param axisLabelAngle.x angle for x-axis-labels
#' @param axisLabelAngle.y angle for y-axis-labels
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param hideLegend show or hide the legend. The legend indicates the strength of correlations
#'          by gradient colour fill.
#' @param legendTitle the legend title, provided as string, e.g. \code{legendTitle=c("Strength of correlation")}.
#'          Default is \code{NULL}, hence no legend title is used.
#' @param returnPlot If \code{TRUE}, the ggplot-object with the complete plot will be returned (and not plotted).
#'          Default is \code{FALSE}, hence the ggplot object will be plotted, not returned.
#' @return The ggplot-object with the complete plot in case \code{returnPlot} is \code{TRUE}.
#' 
#' @examples
#' # create data frame with 5 dichotomous (dummy) variables
#' df <- data.frame(as.factor(sample(1:2, 100, replace=TRUE)),
#'                  as.factor(sample(1:2, 100, replace=TRUE)),
#'                  as.factor(sample(1:2, 100, replace=TRUE)),
#'                  as.factor(sample(1:2, 100, replace=TRUE)),
#'                  as.factor(sample(1:2, 100, replace=TRUE)))
#' # create variable labels
#' items <- list(c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5"))
#' 
#' # plot Chi2-contingency-table
#' sjp.chi2(df, axisLabels=items)
#' 
#' @import ggplot2
#' @importFrom plyr adply
#' @export
sjp.chi2 <- function(df,
                     title="Pearson's Chi2-Test of Independence",
                     titleSize=1.3,
                     titleColor="black",
                     axisLabels=NULL,
                     valueLabelColor="black",
                     valueLabelSize=4.5,
                     valueLabelAlpha=1,
                     outlineColor="black",
                     outlineSize=0.5,
                     axisColor=NULL, 
                     axisLabelSize=1.1,
                     axisLabelColor="gray30",
                     axisLabelAngle.x=0, 
                     axisLabelAngle.y=0, 
                     breakTitleAt=50, 
                     breakLabelsAt=12, 
                     hideLegend=TRUE,
                     legendTitle=NULL,
                     returnPlot=FALSE) {
  # ----------------------------------------------------------------
  # Calculation of Chi2-matrix taken from following blog-posting:
  # http://talesofr.wordpress.com/2013/05/05/ridiculously-photogenic-factors-heatmap-with-p-values/
  # ----------------------------------------------------------------
  combos <- expand.grid(rep(list(1:ncol(df)), 2 )) # combinations with repetitions
  combos <- as.matrix(combos)
  combos <- t(combos) # transpose matrix
  # ----------------------------------------------------------------
  # when 2 variables are *not* significant, they are independent
  # ----------------------------------------------------------------
  m <- adply(combos, 2, function(x) {
    test <- chisq.test(df[, x[1]], df[, x[2]])
    out <- data.frame("Row" = colnames(df)[x[1]], "Column" = colnames(df[x[2]]),
                      "Chi.Square" = round(test$statistic, 4), "df"= test$parameter, 
                      "p.value" = round(test$p.value, 4))
    return(out)
  })
  
  
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(axisLabels)) {
    axisLabels <- row.names(m)
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  # Help function that unlists a list into a vector
  unlistlabels <- function(lab) {
    dummy <- unlist(lab)
    labels <- c()
    for (i in 1:length(dummy)) {
      labels <- c(labels, as.character(dummy[i]))
    }
    return (labels)
  }
  if (!is.null(axisLabels) && is.list(axisLabels)) {
    axisLabels <- unlistlabels(axisLabels)
  }
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) {
    pattern <- c(paste('(.{1,', breakTitleAt, '})(\\s|$)', sep=""))
    title <- gsub(pattern, '\\1\n', title)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels)) {
    pattern <- c(paste('(.{1,', breakLabelsAt, '})(\\s|$)', sep=""))
    for (n in 1:length(axisLabels))
      axisLabels[n] <- gsub(pattern, '\\1\n', axisLabels[n])
  }
  # --------------------------------------------------------
  # start with base plot object here
  # --------------------------------------------------------
  chiPlot <- ggplot(data=m, aes(x=Row, y=Column, fill=p.value, label=p.value))
  # --------------------------------------------------------
  # determine the geom type, either points when "type" is "circles"
  # --------------------------------------------------------
  # check whether we have an outline color
  if (is.null(outlineColor)) {
    geot <- geom_tile()
  }
  # ... and apply colour-attribute
  else {
    geot <- geom_tile(size=outlineSize, colour=outlineColor)
  }
  chiPlot <- chiPlot +
    geot +
    scale_x_discrete(labels=axisLabels) +
    scale_y_discrete(labels=axisLabels) +
    scale_fill_gradient2(low=rgb(128,205,193, maxColorValue=255), mid="white", high=rgb(5,113,176, maxColorValue=255), midpoint=0.05) +
    geom_text(label=sprintf("%.3f", m$p.value), colour=valueLabelColor, alpha=valueLabelAlpha, size=valueLabelSize) +
    labs(title=title, x=NULL, y=NULL, fill=legendTitle) +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.text.x = element_text(angle=axisLabelAngle.x),
          axis.text.y = element_text(angle=axisLabelAngle.y),
          panel.grid.minor = element_line(colour="white"),
          panel.grid.minor = element_line(colour="white"),
          plot.title = element_text(size=rel(titleSize), colour=titleColor))
  if (!is.null(axisColor)) {
    chiPlot <- chiPlot + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (hideLegend) {
    chiPlot <- chiPlot + 
      guides(fill=FALSE)
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (returnPlot) {
    return(chiPlot)
  }
  else {
    plot(chiPlot)
  }
}
