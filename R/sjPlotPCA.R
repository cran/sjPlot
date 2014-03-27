#' @title Plot PCA results
#' @name sjp.pca
#' @references \url{http://strengejacke.wordpress.com/sjplot-r-package/} \cr \cr
#'             \url{http://strengejacke.wordpress.com/2013/07/08/plotting-principal-component-analysis-with-ggplot-rstats/}
#' 
#' @description Performes a principle component analysis on a data frame or matrix and plots 
#'                the factor solution as ellipses or tiles. \cr \cr In case a data frame is used as 
#'                parameter, the cronbach's alpha value for each factor scale will be calculated,
#'                i.e. all variables with the highest loading for a factor are taken for the
#'                reliability test. The result is an alpha value for each factor dimension.
#' 
#' @seealso \code{\link{sjt.pca}} \cr
#'          \code{\link{sju.reliability}} \cr
#'          \code{\link{sjt.itemanalysis}} \cr
#'          \code{\link{sju.cronbach}}
#' 
#' @param data A data frame with factors (each columns one variable) that should be used 
#'          to compute a PCA, or a \code{\link{prcomp}} object.
#' @param numberOfFactors A predefined number of factors to use for the calculating the varimax
#'          rotation. By default, this value is \code{NULL} and the amount of factors is
#'          calculated according to the Kaiser-criteria. See paramater \code{plotEigenvalues}.
#' @param factorLoadingTolerance Specifies the minimum difference a variable needs to have between
#'          factor loadings (components) in order to indicate a clear loading on just one factor and not
#'          diffusing over all factors. For instance, a variable with 0.8, 0.82 and 0.84 factor loading 
#'          on 3 possible factors can not be clearly assigned to just one factor and thus would be removed
#'          from the principal component analysis. By default, the minimum difference of loading values
#'          between the highest and 2nd highest factor should be 0.1
#' @param plotEigenvalues If \code{TRUE}, a plot showing the Eigenvalues according to the
#'          Kaiser criteria is plotted to determine the number of factors.
#' @param digits The amount of decimals used. Default is 2.
#' @param title Title of the diagram, plotted above the whole diagram panel.
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param axisLabels.y The item labels that are printed on the y-axis. If no item labels are
#'          provided (default), the data frame's column names are used. Item labels must
#'          be a string vector, e.g.: \code{axisLabels.y=c("Var 1", "Var 2", "Var 3")}.
#' @param type Indicates whether \code{"circle"} (default) or \code{"tile"} geoms
#'          should be used for plotting.
#' @param geomAlpha Specify the transparancy (alpha value) of geom objects (circles or tiles).
#'          Default is 0.8.
#' @param valueLabelColor The color of the value labels (numbers) inside the diagram.
#'          Default is \code{"black"}.
#' @param valueLabelSize The size of value labels in the diagram. Default is 4.5, recommended values range
#'          between 2 and 8.
#' @param valueLabelAlpha Specify the transparancy (alpha value) of value labels.
#'          Default is 0.8
#' @param circleSize Specifies the circle size factor. The circle size depends on the correlation
#'          value multiplicated with this factor. Default is 10.
#' @param outlineColor Defines the outline color of geoms (circles or tiles). Default is \code{"black"}.
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
#' @param hideLegend Show or hide the legend. The legend indicates the strength of correlations
#'          by gradient colour fill. Default is \code{TRUE}, hence the legend is hidden.
#' @param legendTitle The legend title, provided as string, e.g. \code{legendTitle=c("Factor loading")}.
#'          Default is \code{NULL}, hence no legend title is used.
#' @param showValueLabels Whether factor loading values should be plotted to each geom.
#'          Default is \code{TRUE}.
#' @param showTickMarks Whether tick marks should be plotted or not. Default is \code{FALSE}.
#' @param showCronbachsAlpha If \code{TRUE} (default), the cronbach's alpha value for each factor scale will be calculated,
#'          i.e. all variables with the highest loading for a factor are taken for the
#'          reliability test. The result is an alpha value for each factor dimension.
#'          Only applies when \code{data} is a data frame and no \code{\link{prcomp}} object.
#' @param fillColor A color palette for fillng the geoms. If not specified, the 5th diverging color palette
#'          from the color brewer palettes (RdBu) is used, resulting in red colors for negative and blue colors
#'          for positive factor loadings, that become lighter the weaker the loadings are. Use any
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
#' @return (Invisibly) returns a \code{\link{structure}} with
#'          \itemize{
#'            \item the varimax-rotated factor loading matrix (\code{varim})
#'            \item the column indices of removed variables (for more details see next list item) (\code{removed.colindex})
#'            \item an updated data frame containing all factors that have a clear loading on a specific scale in case \code{data} was a data frame (See parameter \code{factorLoadingTolerance} for more details) (\code{removed.df})
#'            \item the ggplot-object (\code{plot}),
#'            \item the data frame that was used for setting up the ggplot-object (\code{df}).
#'            }
#' 
#' @note This PCA uses the \code{\link{prcomp}} function and the \code{\link{varimax}} rotation.
#' 
#' @examples
#' # randomly create data frame with 7 items, each consisting of 4 categories
#' likert_4 <- data.frame(sample(1:4, 500, replace=TRUE, prob=c(0.2,0.3,0.1,0.4)),
#'                        sample(1:4, 500, replace=TRUE, prob=c(0.5,0.25,0.15,0.1)),
#'                        sample(1:4, 500, replace=TRUE, prob=c(0.4,0.15,0.25,0.2)),
#'                        sample(1:4, 500, replace=TRUE, prob=c(0.25,0.1,0.4,0.25)),
#'                        sample(1:4, 500, replace=TRUE, prob=c(0.1,0.4,0.4,0.1)),
#'                        sample(1:4, 500, replace=TRUE),
#'                        sample(1:4, 500, replace=TRUE, prob=c(0.35,0.25,0.15,0.25)))
#'
#' # Create variable labels
#' colnames(likert_4) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")
#' 
#' # plot results from PCA as square-tiled "heatmap"
#' sjp.pca(likert_4)
#' 
#' # manually compute PCA
#' pca <- prcomp(na.omit(likert_4), retx=TRUE, center=TRUE, scale.=TRUE)
#' # plot results from PCA as circles, including Eigenvalue-diagnostic.
#' # note that this plot does not compute the Cronbach's Alpha
#' sjp.pca(pca, plotEigenvalues=TRUE, type="circle")
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
#' start <- which(colnames(efc)=="c82cop1")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc)=="c90cop9")
#'  
#' # create data frame with COPE-index scale
#' df <- as.data.frame(efc[,c(start:end)])
#' colnames(df) <- varlabs[c(start:end)]
#' 
#' sjp.pca(df)
#' 
#' # -------------------------------
#' # auto-detection of labels
#' # -------------------------------
#' efc <- sji.setVariableLabels(efc, varlabs)
#' sjp.pca(efc[,c(start:end)])
#' 
#' 
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom scales brewer_pal
#' @export
sjp.pca <- function(data,
                    numberOfFactors=NULL,
                    factorLoadingTolerance=0.1,
                    plotEigenvalues=FALSE,
                    digits=2,
                    title=NULL,
                    titleSize=1.3,
                    titleColor="black",
                    axisLabels.y=NULL,
                    type="tile",
                    geomAlpha=0.8,
                    valueLabelColor="black",
                    valueLabelSize=4.5,
                    valueLabelAlpha=1,
                    circleSize=10,
                    outlineColor="black",
                    outlineSize=0.2,
                    axisColor=NULL, 
                    borderColor=NULL, 
                    axisLabelSize=1.1,
                    axisLabelColor="gray30",
                    axisLabelAngle.x=0, 
                    axisLabelAngle.y=0, 
                    breakTitleAt=50, 
                    breakLabelsAt=20, 
                    hideLegend=TRUE,
                    legendTitle=NULL,
                    showValueLabels=TRUE,
                    showTickMarks=FALSE,
                    showCronbachsAlpha=TRUE,
                    fillColor=NULL,
                    majorGridColor=NULL,
                    minorGridColor=NULL,
                    theme=NULL,
                    printPlot=TRUE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(axisLabels.y) && is.data.frame(data)) {
    # if yes, iterate each variable
    for (i in 1:ncol(data)) {
      # retrieve variable name attribute
      vn <- autoSetVariableLabels(data[,i])
      # if variable has attribute, add to variableLabel list
      if (!is.null(vn)) {
        axisLabels.y <- c(axisLabels.y, vn)
      }
      else {
        # else break out of loop
        axisLabels.y <- NULL
        break
      }
    }
  }
  # ----------------------------
  # check if user has passed a data frame
  # or a pca object
  # ----------------------------
  if (class(data)=="prcomp") {
    pcadata <- data
    dataframeparam <- FALSE
  }
  else {
    pcadata <- prcomp(na.omit(data), retx=TRUE, center=TRUE, scale.=TRUE)
    dataframeparam <- TRUE
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) {
    axisLabels.y <- unlistlabels(axisLabels.y)
  }
  # ----------------------------
  # calculate eigenvalues
  # ----------------------------
  pcadata.eigenval <- pcadata$sdev^2
  # ----------------------------
  # retrieve best amount of factors according
  # to Kaiser-critearia, i.e. factors with eigen value > 1
  # ----------------------------
  pcadata.kaiser <- which(pcadata.eigenval<1)[1]-1
  # ----------------------------
  # plot eigenvalues
  # ----------------------------
  if (plotEigenvalues) {
    # create data frame with eigen values
    mydat <- as.data.frame(cbind(xpos=1:length(pcadata.eigenval), eigen=pcadata.eigenval))
    # plot eigenvalues as line curve
    eigenplot <- 
      # indicate eigen vlaues > 1
      ggplot(mydat, aes(x=xpos, y=eigen, colour=eigen>1)) +
        geom_line() + geom_point() +
        geom_hline(y=1, linetype=2, colour="grey50") +
        # print best number of factors according to eigen value
        annotate("text", label=sprintf("Factors: %i", pcadata.kaiser), x=Inf, y=Inf, vjust=2, hjust=1.2) +
        scale_x_continuous(breaks=c(seq(1,nrow(mydat), by=2))) +
        labs(title=NULL, y="Eigenvalue", x="Number of factors")
    plot(eigenplot)
    # print statistics
    cat("\n--------------------------------------------\n")
    print(summary(pcadata))
    cat("\nEigenvalues:\n")
    print(pcadata.eigenval)
    cat("--------------------------------------------\n")
  }
  # --------------------------------------------------------
  # varimax rotation, retrieve factor loadings
  # --------------------------------------------------------
  # check for predefined number of factors
  if (!is.null(numberOfFactors) && is.numeric(numberOfFactors)) {
    pcadata.kaiser <- numberOfFactors
  }
  pcadata.varim = varimaxrota(pcadata, pcadata.kaiser)
  # pcadata.varim = varimax(loadings(pcadata))
  # create data frame with factor loadings
  df <- as.data.frame(pcadata.varim$loadings[,1:ncol(pcadata.varim$loadings)])
  # df <- as.data.frame(pcadata.varim$rotmat[,1:pcadata.kaiser])
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(axisLabels.y)) {
    axisLabels.y <- row.names(df)
  }
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- sju.wordwrap(axisLabels.y, breakLabelsAt)
  }
  # --------------------------------------------------------
  # this function checks which items have unclear factor loadings,
  # i.e. which items do not strongly load on a single factor but
  # may load almost equally on several factors
  # --------------------------------------------------------
  getRemovableItems <- function(dataframe) {
    # clear vector
    removers <- c()
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in 1:nrow(dataframe)) {
      # get factor loadings for each item
      rowval <- as.numeric(abs(df[i,]))
      # retrieve highest loading
      maxload <- max(rowval)
      # retrieve 2. highest loading
      max2load <- sort(rowval, TRUE)[2]
      # check difference between both
      if (abs(maxload-max2load)<factorLoadingTolerance) {
        # if difference is below the tolerance,
        # remeber row-ID so we can remove that items
        # for further PCA with updated data frame
        removers <- c(removers, i)
      }
    }
    # return a vector with index numbers indicating which items
    # have unclear loadings
    return (removers)
  }
  # --------------------------------------------------------
  # this function retrieves a list with the column index ("factor" index)
  # where each case of the data frame has its highedt factor loading.
  # So we know to which "group" (factor dimension) each case of the 
  # data frame belongs to according to the pca results
  # --------------------------------------------------------
  getItemLoadings <- function(dataframe) {
    # clear vector
    itemloading <- c()
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in 1:nrow(dataframe)) {
      # get factor loadings for each item
      rowval <- abs(df[i,])
      # retrieve highest loading and remeber that column
      itemloading <- c(itemloading, which(rowval==max(rowval)))
    }
    # return a vector with index numbers indicating which items
    # loads the highest on which factor
    return (itemloading)
  }
  # --------------------------------------------------------
  # this function calculates the cronbach's alpha value for
  # each factor scale, i.e. all variables with the highest loading
  # for a factor are taken for the reliability test. The result is
  # an alpha value for each factor dimension
  # --------------------------------------------------------
  getCronbach <- function(dataframe, itemloadings) {
    # clear vector
    cbv <- c()
    # iterate all highest factor loadings of items
    for (n in 1:length(unique(itemloadings))) {
      # calculate cronbach's alpha for those cases that all have the
      # highest loading on the same factor
      cbv <- as.data.frame(rbind(cbv, cbind(nr=n, sju.cronbach(na.omit(dataframe[,which(itemloadings==n)])))))
    }
    # just for vertical position adjustment when we print the alpha values
    vpos <- rep(c(-0.25, -1), nrow(cbv))
    cbv <- cbind(cbv, vpos[1:nrow(cbv)])
    names(cbv) <- c("nr", "alpha", "vpos")
    # cbv now contains the factor numbers and the related alpha values
    # for each "factor dimension scale"
    return(cbv)
  }
  # ----------------------------------
  # Cronbach's Alpha can only be calculated when having a data frame
  # with each component / variable as column
  # ----------------------------------
  if (dataframeparam) {
    # get alpha values
    alphaValues <- getCronbach(data, getItemLoadings(df))
  }
  else {
    cat("\nCronbach's Alpha can only be calculated when having a data frame with each component / variable as column\n")
    showCronbachsAlpha <- FALSE
  }
  # retrieve those items that have unclear factor loadings, i.e.
  # which almost load equally on several factors. The tolerance
  # that indicates which difference between factor loadings is
  # considered as "equally" is defined via factorLoadingTolerance
  removableItems <- getRemovableItems(df)
  # rename columns, so we have numbers on x axis
  names(df) <- c(1:ncol(df))
  # convert to long data
  df <- melt(df)
  # we need new columns for y-positions and point sizes
  df <- cbind(df, ypos=c(1:nrow(pcadata.varim$loadings)), psize=c(exp(abs(df$value))*circleSize))
  # rename first column for more intuitive name
  colnames(df)[1] <- c("xpos")
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
  if (!showValueLabels) {
    valueLabels <- c("")
  }
  else {
    valueLabels <- sprintf("%.*f", digits, df$value)
  }
  # --------------------------------------------------------
  # start with base plot object here
  # --------------------------------------------------------
  heatmap <- ggplot(data=df, aes(x=xpos, y=ypos, fill=value))
  # --------------------------------------------------------
  # determine the geom type, either points when "type" is "circles"
  # --------------------------------------------------------
  if (type=="circle") {
    # check whether we have an outline color
    if (is.null(outlineColor)) {
      geo <- geom_point(shape=21, size=df$psize, alpha=geomAlpha)
    }
    # ... and apply colour-attribute
    else {
      geo <- geom_point(shape=21, size=df$psize, alpha=geomAlpha, colour=outlineColor)
    }
  }
  # --------------------------------------------------------
  # or boxes / tiles when "type" is "tile"
  # --------------------------------------------------------
  else {
    # check whether we have an outline color
    if (is.null(outlineColor)) {
      geo <- geom_tile()
    }
    # ... and apply colour-attribute
    else {
      geo <- geom_tile(size=outlineSize, colour=outlineColor)
    }
  }
  heatmap <- heatmap +
    geo +
    scale_y_reverse(breaks=c(seq(1, length(axisLabels.y), by=1)), labels=axisLabels.y)
  # --------------------------------------------------------
  # fill gradient colour from distinct color brewer palette. negative correlations are dark
  # red, positive corr. are dark blue, and they become lighter the closer they are to a
  # correlation coefficient of zero
  # --------------------------------------------------------
  if (is.null(fillColor)) {
    heatmap <- heatmap +
      # set limits to (-1,1) to make sure the whole color palette is used
      scale_fill_gradientn(colours=brewer_pal("div",5)(5), limits=c(-1,1))
      # scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0)
  }
  else {
    heatmap <- heatmap +
      # set limits to (-1,1) to make sure the whole color palette is used
      scale_fill_gradientn(colours=fillColor, limits=c(-1,1))
      # scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0)
  }
  heatmap <- heatmap +
    geom_text(label=valueLabels, colour=valueLabelColor, alpha=valueLabelAlpha, size=valueLabelSize) +
    labs(title=title, x=NULL, y=NULL, fill=legendTitle) +
    ggtheme +
    # set font size for axes.
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.text.x = element_text(angle=axisLabelAngle.x),
          axis.text.y = element_text(angle=axisLabelAngle.y),
          plot.title = element_text(size=rel(titleSize), colour=titleColor))
  # --------------------------------------------------------
  # show cronbach's alpha value for each scale 
  # --------------------------------------------------------
  if (showCronbachsAlpha) {
    heatmap <- heatmap +
      # annotate("text", x=alphaValues$nr, y=Inf, parse=TRUE, label=sprintf("alpha == %.2f", alphaValues$alpha), size=0.9*valueLabelSize, colour=axisLabelColor, vjust=alphaValues$vpos)
      annotate("text", x=alphaValues$nr, y=Inf, parse=TRUE, label=sprintf("alpha == %.*f", digits, alphaValues$alpha), size=0.9*valueLabelSize, colour=axisLabelColor, vjust=-0.5)
  }
  # --------------------------------------------------------
  # the panel-border-property can only be applied to the bw-theme
  # --------------------------------------------------------
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      heatmap <- heatmap + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      cat("\nParameter 'borderColor' can only be applied to 'bw' theme.\n")
    }
  }
  # --------------------------------------------------------
  # apply theme properties like axis and grid colors
  # --------------------------------------------------------
  if (!is.null(axisColor)) {
    heatmap <- heatmap + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    heatmap <- heatmap + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    heatmap <- heatmap + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideLegend) {
    heatmap <- heatmap + 
      guides(fill=FALSE)
  }
  # --------------------------------------------------------
  # print plot
  # --------------------------------------------------------
  if (printPlot) plot(heatmap)
  # --------------------------------------------------------
  # if we have a data frame, all factors which do not clearly
  # load on a specific dimension (see patameter "factorLoadingTolerance")
  # will be removed and the updated data frame will be returned.
  # the user may calculate another PCA with the updated data frame
  # in order to get more clearly factor loadings
  # --------------------------------------------------------
  remdf <- NULL
  if (class(data)=="data.frame") {
    cat("\nFollowing items have been removed:\n")
    if (!is.null(removableItems)) {
      print(colnames(data)[removableItems])
      remdf <- data[,c(-removableItems)]
    }
    else {
      cat("none.\n")
    }
  }
  # --------------------------------------------------------
  # return structure with various results
  # --------------------------------------------------------
  invisible (structure(class = "sjcpca",
                       list(varim = pcadata.varim,
                            removed.colindex = removableItems,
                            removed.df = remdf,
                            plot = heatmap,
                            df = df)))
}
