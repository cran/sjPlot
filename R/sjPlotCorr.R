# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("ordx", "ordy"))



#' @title Plot correlation matrix
#' @name sjp.corr
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2013/04/18/examples-for-sjplotting-functions-including-correlations-and-proportional-tables-with-ggplot-rstats/}
#'             }
#'             
#' @description Plot correlations as ellipses or tiles. Required parameter is either 
#'                a data frame or a computed \code{\link{cor}}-object. In case of ellipses, the
#'                ellipses size indicates the strength of the correlation. Furthermore,
#'                blue and red colors indicate positive or negative correlations, where
#'                stronger correlations are darkened.
#' 
#' @param data A correlation object, built with the R-\code{\link{cor}}-function, or a data frame
#'          which correlations should be calculated.
#' @param title Title of the diagram, plotted above the whole diagram panel.
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param axisLabels Labels for the x- andy y-axis.
#' @param type Indicates whether the geoms of correlation values should be plotted
#'          as \code{"circle"} (default) or as \code{"tile"}.
#' @param sortCorrelations If \code{TRUE} (default), the axis labels are sorted
#'          according to the correlation strength. If \code{FALSE}, axis labels
#'          appear in order of how variables were included in the cor-computation or
#'          data frame.
#' @param decimals Indicates how many decimal values after comma are printed when
#'          the values labels are shown. Default is 3. Only applies when
#'          \code{showCorrelationValueLabels} is \code{TRUE}.
#' @param missingDeletion Indicates how missing values are treated. May be either
#'          \code{"listwise"} (default) or \code{"pairwise"}.
#' @param corMethod Indicates the correlation computation method. May be one of
#'          \code{"spearman"} (default), \code{"pearson"} or \code{"kendall"}.
#' @param geomAlpha Specify the transparancy (alpha value) of geom objects (circles or tiles).
#'          Default is 0.8.
#' @param valueLabelColor The color of the value labels (numbers) inside the diagram.
#'          Default is \code{"black"}.
#' @param valueLabelSize The size of value labels in the diagram. Default is 4.5, recommended values range
#'          between 2 and 8.
#' @param valueLabelAlpha Specify the transparancy (alpha value) of value labels.
#'          Default is 1.
#' @param circleSize Specifies the circle size factor. The circle size depends on the correlation
#'          value multiplicated with this factor. Default is 15.
#' @param outlineColor Defines the outline color of geoms (circles or tiles). Default is black.
#' @param outlineSize Defines the outline size of geoms (circles or tiles). Default is 1.
#' @param axisColor User defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border).
#' @param borderColor User defined color of whole diagram border (panel border).
#' @param axisLabelSize The size of variable labels at the axes. Default is 1.1, recommended values range
#'          between 0.5 and 3.0.
#' @param axisLabelColor User defined color for axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels.
#' @param axisLabelAngle.x Angle for x-axis-labels.
#' @param axisLabelAngle.y Angle for y-axis-labels.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title. Default is 50.
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted. Default is 12.
#' @param hideDiagCircle If \code{TRUE} (default), the geoms of the diagonal correlations
#'          (self-correlations with value "1") are not plotted. Only applies if parameter
#'          \code{type} is \code{"circle"}.
#' @param hideLegend Show or hide the legend. The legend indicates the strength of correlations
#'          by gradient colour fill. Default is \code{TRUE}, hence the legend is hidden.
#' @param legendTitle The legend title, provided as string, e.g. \code{legendTitle=c("Strength of correlation")}.
#'          Default is \code{NULL}, hence no legend title is used.
#' @param showCorrelationValueLabels Whether correlation values should be plotted to each geom
#' @param showCorrelationPValues Whether significance levels (p-values) of correlations should 
#'          be plotted to each geom.
#' @param pvaluesAsNumbers If \code{TRUE}, the significance levels (p-values) are printed as numbers.
#'          if \code{FALSE} (default), asterisks are used.
#' @param showTickMarks Whether tick marks should be plotted or not. Default is \code{FALSE}.
#' @param fillColor A color palette for fillng the geoms. If not specified, the 5th diverging color palette
#'          from the color brewer palettes (RdBu) is used, resulting in red colors for negative and blue colors
#'          for positive correlations, that become lighter the weaker the correlations are. Use any
#'          color palette that is suitbale for the \code{scale_fill_gradientn} parameter of ggplot2.
#' @param majorGridColor Specifies the color of the major grid lines of the diagram background.
#' @param minorGridColor Specifies the color of the minor grid lines of the diagram background.
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids.
#'          \itemize{
#'          \item Use \code{"bw"} for a white background with gray grids
#'          \item \code{"classic"} for a classic theme (black border, no grids)
#'          \item \code{"minimal"} for a minimalistic theme (no border,gray grids) or 
#'          \item \code{"none"} for no borders, grids and ticks.
#'          }
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}) and the original correlation matrix
#'           (\code{corr.matrix}).
#'          
#' @examples
#' # create data frame with 5 random variables
#' df <- as.data.frame(cbind(rnorm(10), rnorm(10), rnorm(10), rnorm(10), rnorm(10)))
#' 
#' # plot correlation matrix using circles
#' sjp.corr(df)
#' 
#' # plot correlation matrix using square tiles without diagram background
#' sjp.corr(df, type="tile", theme="none")
#' 
#' 
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' data(efc)
#' 
#' # retrieve variable and value labels
#' varlabs <- sji.getVariableLabels(efc)
#' 
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc)=="c83cop2")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc)=="c88cop7")
#'  
#' # create data frame with COPE-index scale
#' df <- as.data.frame(efc[,c(start:end)])
#' colnames(df) <- varlabs[c(start:end)]
#'
#' # we have high correlations here, because all items
#' # belong to one factor. See example from "sjp.pca". 
#' sjp.corr(df, type="tile", theme="none", outlineColor="white", hideLegend=FALSE)
#'  
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom scales brewer_pal
#' @export
sjp.corr <- function(data,
                     title=NULL,
                     titleSize=1.3,
                     titleColor="black",
                     axisLabels=NULL,
                     type="circle",
                     sortCorrelations=TRUE,
                     decimals=3,
                     missingDeletion="listwise",
                     corMethod="spearman",
                     geomAlpha=0.8,
                     valueLabelColor="black",
                     valueLabelSize=4.5,
                     valueLabelAlpha=1,
                     circleSize=15,
                     outlineColor="black",
                     outlineSize=1,
                     axisColor=NULL, 
                     borderColor=NULL, 
                     axisLabelSize=1.1,
                     axisLabelColor="gray30",
                     axisLabelAngle.x=0, 
                     axisLabelAngle.y=0, 
                     breakTitleAt=50, 
                     breakLabelsAt=12, 
                     hideDiagCircle=TRUE,
                     hideLegend=TRUE,
                     legendTitle=NULL,
                     showCorrelationValueLabels=TRUE,
                     showCorrelationPValues=TRUE,
                     pvaluesAsNumbers=FALSE,
                     showTickMarks=FALSE,
                     fillColor=NULL,
                     majorGridColor=NULL,
                     minorGridColor=NULL,
                     theme=NULL,
                     printPlot=TRUE) {
  # ----------------------------
  # check for valid parameter
  # ----------------------------
  if (corMethod!="pearson" && corMethod!="spearman" && corMethod!= "kendall") {
    stop("Parameter 'corMethod' must be one of: pearson, spearman or kendall")
  }
  # ----------------------------
  # check if user has passed a data frame
  # or a pca object
  # ----------------------------
  if (class(data)=="matrix") {
    corr <- data
    cpvalues <- NULL
  }
  else {
    # missing deletion corresponds to
    # SPSS listwise
    if (missingDeletion=="listwise") {
      data <- na.omit(data)
      corr <- cor(data, method=corMethod)
    }
    # missing deletion corresponds to
    # SPSS pairwise
    else {
      corr <- cor(data, method=corMethod, use="pairwise.complete.obs")
    }
    #---------------------------------------
    # if we have a data frame as parameter,
    # compute p-values of significances
    #---------------------------------------
    computePValues <- function(df) {
      cp <- c()
      for (i in 1:ncol(df)) {
        pv <- c()
        for (j in 1:ncol(df)) {
          test <- cor.test(df[,i], df[,j], alternative="two.sided", method=corMethod)
          pv <- cbind(pv, round(test$p.value,4))
        }
        cp <- rbind(cp, pv)
      }
      return (cp)
    }
    cpvalues <- computePValues(data)
  }
  
  
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(axisLabels)) {
    axisLabels <- row.names(corr)
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
    title <- sju.wordwrap(title, breakTitleAt)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels)) {
    axisLabels <- sju.wordwrap(axisLabels, breakLabelsAt)
  }

  
  # --------------------------------------------------------
  # order correlations from highest to lowest correlation coefficient
  # --------------------------------------------------------
  if (sortCorrelations) {
    neword <- order(corr[1,])
    orderedCorr <- corr[neword, neword]
    # order variable labels as well
    axisLabels <- axisLabels[neword]
    if (!is.null(cpvalues)) {
      cpvalues <- cpvalues[neword, neword]
    }
  }
  else {
    orderedCorr <- rev(corr)
    axisLabels <- rev(axisLabels)
    if (!is.null(cpvalues)) {
      cpvalues <- rev(cpvalues)
    }
  }  
  # --------------------------------------------------------
  # prepare a ordering-index-column needed for the data frame
  # that is passed to the ggplot
  # --------------------------------------------------------
  yo <- c()
  for (i in 1:nrow(corr)) {
    yo <- c(yo, c(rep(i,nrow(corr))))
  }
  # --------------------------------------------------------
  # melt correlation matrix and create data frame
  # --------------------------------------------------------
  # first, save original matrix for return value
  oricor <- orderedCorr
  orderedCorr <- melt(orderedCorr)
  if (!is.null(cpvalues)) cpvalues <- melt(cpvalues)
  # bind additional information like order for x- and y-axis
  # as well as the size of plotted points
  orderedCorr <- cbind(orderedCorr, ordx=c(1:nrow(corr)), ordy=yo, psize=c(exp(abs(orderedCorr$value))*circleSize))
  # if the diagonal circles should be hidden, set their point size to 0
  if (hideDiagCircle) {
    orderedCorr$psize[which(orderedCorr$value>=0.999)] <- 0
  }

  orderedCorr$ordx <- as.factor(orderedCorr$ordx)
  orderedCorr$ordy <- as.factor(orderedCorr$ordy)
  

  # --------------------------------------------------------
  # add column with significance value
  # --------------------------------------------------------
  cpv <- c()
  if (!is.null(cpvalues)) {
    if (!pvaluesAsNumbers) {
      for (cpi in 1:nrow(cpvalues)) {
        cva <- cpvalues$value[cpi]
        if (cva>=0.05) cpv <- c(cpv, "")
        else if (cva>=0.01 && cva<0.05) cpv <- c(cpv, "*")
        else if (cva>=0.001 && cva<0.01) cpv <- c(cpv, "**")
        else if (cva<0.001) cpv <- c(cpv, "***")
      }
    }
    else {
      cpv <- c(sprintf("\n(%.3f)", round(cpvalues$value,3)))
    }
  }
  else {
    cpv <- c("")
  }
  orderedCorr$ps <- cpv

  
  # --------------------------------------------------------
  # Set theme and default grid colours. grid colours
  # might be adjusted later
  # --------------------------------------------------------
  if (is.null(theme)) {
    ggtheme <- theme_gray()
  }
  else if (theme=="bw") {
    ggtheme <- theme_bw()
  }
  else if (theme=="classic") {
    ggtheme <- theme_classic()
  }
  else if (theme=="minimal") {
    ggtheme <- theme_minimal()
  }
  else if (theme=="none") {
    ggtheme <- theme_minimal()
    majorGridColor <- c("white")
    minorGridColor <- c("white")
    showTickMarks <-FALSE
  }
  
  
  # --------------------------------------------------------
  # Set up grid colours
  # --------------------------------------------------------
  majorgrid <- NULL
  minorgrid <- NULL
  if (!is.null(majorGridColor)) {
    majorgrid <- element_line(colour=majorGridColor)
  }
  if (!is.null(minorGridColor)) {
    minorgrid <- element_line(colour=minorGridColor)
  }
  
  
  # --------------------------------------------------------
  # Set up visibility oftick marks
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showCorrelationValueLabels) {
    correlationValueLabels <- c("")
    correlationPValues <- c("")
  }
  else {
    correlationValueLabels <- c(round(orderedCorr$value,decimals))
    if (showCorrelationPValues) {
      correlationPValues <- orderedCorr$ps
    }
    else {
      correlationPValues <- c("")
    }
  }
  
  
  cat(sprintf("Computing correlation using %s-method with %s-deletion...\n", corMethod, missingDeletion))
  
  # --------------------------------------------------------
  # start with base plot object here
  # --------------------------------------------------------
  corrPlot <- ggplot(data=orderedCorr, aes(x=ordx, y=ordy, fill=value))
  # --------------------------------------------------------
  # determine the geom type, either points when "type" is "circles"
  # --------------------------------------------------------
  if (type=="circle") {
    # check whether we have an outline color
    if (is.null(outlineColor)) {
      geop <- geom_point(shape=21, size=orderedCorr$psize, alpha=geomAlpha)
    }
    # ... and apply colour-attribute
    else {
      geop <- geom_point(shape=21, size=orderedCorr$psize, alpha=geomAlpha, colour=outlineColor)
    }
    corrPlot <- corrPlot + 
      geop
  }
  # --------------------------------------------------------
  # or boxes / tiles when "type" is "tile"
  # --------------------------------------------------------
  else {
    # check whether we have an outline color
    if (is.null(outlineColor)) {
      geot <- geom_tile(alpha=geomAlpha)
    }
    # ... and apply colour-attribute
    else {
      geot <- geom_tile(size=outlineSize, colour=outlineColor, alpha=geomAlpha)
    }
    corrPlot <- corrPlot +
      geot
  }
  # fill gradient colour from distinct color brewer palette. negative correlations are dark
  # red, positive corr. are dark blue, and they become lighter the closer they are to a
  # correlation coefficient of zero
  if (is.null(fillColor)) {
    corrPlot <- corrPlot +
      scale_x_discrete(labels=axisLabels) +
      scale_y_discrete(labels=axisLabels) +
      # set limits to (-1,1) to make sure the whole color palette is used
      scale_fill_gradientn(colours=brewer_pal("div",5)(5), limits=c(-1,1))
  }
  else {
    corrPlot <- corrPlot +
      # set limits to (-1,1) to make sure the whole color palette is used
      scale_fill_gradientn(colours=fillColor, limits=c(-1,1))
  }
  corrPlot <- corrPlot +
    geom_text(label=sprintf("%s%s", correlationValueLabels, correlationPValues), colour=valueLabelColor, alpha=valueLabelAlpha, size=valueLabelSize) +
    labs(title=title, x=NULL, y=NULL, fill=legendTitle) +
    ggtheme +
    # set font size for axes.
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.text.x = element_text(angle=axisLabelAngle.x),
          axis.text.y = element_text(angle=axisLabelAngle.y),
          plot.title = element_text(size=rel(titleSize), colour=titleColor))
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      corrPlot <- corrPlot + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      cat("\nParameter 'borderColor' can only be applied to 'bw' theme.\n")
    }
  }
  if (!is.null(axisColor)) {
    corrPlot <- corrPlot + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    corrPlot <- corrPlot + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    corrPlot <- corrPlot + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideLegend) {
    corrPlot <- corrPlot + 
      guides(fill=FALSE)
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) plot(corrPlot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpcorr",
                       list(plot = corrPlot,
                            df = orderedCorr,
                            corr.matrix = oricor)))
}
