# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("xpos", "value", "Var2", "grp", "prc", "fg", "cprc", "se", "group"))


#' @title Compute quick cluster analysis
#' @name sjc.qclus
#' @description Compute a quick kmeans or hierarchical cluster analysis and displays "cluster characteristics"
#'                as graph.
#'                \enumerate{
#'                \item If \code{method} is \code{kmeans}, this function first determines the optimal group count via gap statistics (unless parameter \code{groupcount} is specified), using the \link{sjc.kgap} function.
#'                \item Than a cluster analysis is performed by running the \link{sjc.cluster} function to determine the cluster groups.
#'                \item After that, all variables in \code{data} are scaled and centered. The mean value of these z-scores within each cluster group is calculated to see how certain characteristics (variables) in a cluster group differ in relation to other cluster groups.
#'                \item These results are shown in a graph.
#'                }
#'                
#' @seealso \code{\link{sjc.cluster}} \cr
#'          \code{\link{sjc.kgap}} \cr
#'          \code{\link{sjc.elbow}} \cr
#'          \code{\link{sjc.grpdisc}}
#'
#' @param data The data frame containing all variables that should be used for the
#'          cluster analysis.
#' @param groupcount The amount of groups (clusters) that should be retrieved. By default
#'          (\code{NULL}), the optimal amount of clusters is calculated using the gap statistics
#'          (see \code{\link{sjc.kgap}}. However, this works only with kmeans as \code{method}. If
#'          \code{method} is \code{"h"}, you have to specify a groupcount. Use the \code{\link{sjc.elbow}}-function 
#'          to determine the group-count depending on the elbow-criterion. Use \code{\link{sjc.grpdisc}}-function 
#'          to inspect the goodness of grouping.
#' @param method The method for computing the cluster analysis. By default (\code{"k"}), a
#'          kmeans cluster analysis will be computed. Use \code{"h"} to compute a hierarchical
#'          cluster analysis.
#' @param distance The distance measure to be used when \code{"method"} is \code{"h"} (for hierarchical
#'          clustering). This must be one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, 
#'          \code{"canberra"}, \code{"binary"} or \code{"minkowski"}. See \link{dist}.
#'          By default, method is \code{"k"} and this parameter will be ignored.
#' @param agglomeration The agglomeration method to be used when \code{"method"} is \code{"h"} (for hierarchical
#'          clustering). This should be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, 
#'          \code{"mcquitty"}, \code{"median"} or \code{"centroid"}. Default is \code{"ward"}. See \link{hclust}.
#'          By default, method is \code{"k"} and this parameter will be ignored.
#' @param showAccuracy If \code{TRUE}, the \link{sjc.grpdisc} function will be called,
#'          which computes a linear discriminant analysis on the classified cluster groups and plots a 
#'          bar graph indicating the goodness of classification for each group.
#' @param title Title of diagram as string.
#'          Example: \code{title=c("my title")}
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param axisLabels.x Labels for the x-axis breaks.
#'          Example: \code{axisLabels.x=c("Label1", "Label2", "Label3")}.
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getValueLabels}} function, you receive a
#'          list object with label string. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param axisLabelAngle.x Angle for axis-labels.
#' @param axisLabelSize The size of axis labels of both x and y axis. Default is 1.1, recommended values range
#'          between 0.5 and 3.0.
#' @param axisLabelColor User defined color for axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels.
#' @param axisTitle.x A label for the x axis. useful when plotting histograms with metric scales where no category labels
#'          are assigned to the x axis.
#' @param axisTitle.y A label for the y axis. useful when plotting histograms with metric scales where no category labels
#'          are assigned to the y axis.
#' @param axisTitleColor The color of the x and y axis labels. Refers to \code{axisTitle.x} and \code{axisTitle.y}, not to the tick mark 
#'          or category labels.
#' @param axisTitleSize the size of the x and y axis labels. Refers to \code{axisTitle.x} and \code{axisTitle.y}, not to the tick mark 
#'          or category labels. Default is 1.3.
#' @param breakTitleAt Determines how many chars of the title are displayed in 
#'          one line and when a line break is inserted into the title.
#' @param breakLabelsAt Determines how many chars of the labels are displayed in 
#'          one line and when a line break is inserted into the axis labels.
#' @param breakLegendTitleAt Determines how many chars of the legend title are displayed in 
#'          one line and when a line break is inserted into the legend title.
#' @param breakLegendLabelsAt Determines how many chars of the legend labels are displayed in 
#'          one line and when a line break is inserted into the axis labels.
#' @param facetCluster If \code{TRUE}, each cluster group will be represented by an own panel.
#'          Default is \code{FALSE}, thus all cluster groups are plotted in a single graph.
#' @param barColor User defined color for bars.
#'          \itemize{
#'            \item If not specified (\code{NULL}), a default color palette will be used for the bar charts.
#'            \item If barColor is \code{"gs"}, a greyscale will be used.
#'            \item If barColor is \code{"bw"}, a monochrome white filling will be used.
#'            \item If barColor is \code{"brewer"}, use the \code{colorPalette} parameter to specify a palette of the color brewer.
#'            }
#'          Else specify your own color values as vector (e.g. \code{barColor=c("#f00000", "#00ff00", "#0080ff")}).
#' @param barAlpha Specify the transparancy (alpha value) of bars.
#' @param colorPalette If \code{barColor} is \code{"brewer"}, specify a color palette from the color brewer here. All color brewer 
#'          palettes supported by ggplot are accepted here.
#' @param barWidth Width of bars. Recommended values for this parameter are from 0.4 to 1.5
#' @param barSpace Spacing between bars. Default value is 0.1. If 0 is used, the grouped bars are sticked together and have no space
#'          in between. Recommended values for this parameter are from 0 to 0.5
#' @param barOutline If \code{TRUE}, each bar gets a colored outline. Default is \code{FALSE}.
#' @param outlineColor The color of the bar outline. Only applies, if \code{barOutline} is set to \code{TRUE}.
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids.
#'          \itemize{
#'            \item Use \code{"bw"} for a white background with gray grids
#'            \item \code{"classic"} for a classic theme (black border, no grids)
#'            \item \code{"minimal"} for a minimalistic theme (no border,gray grids) or 
#'            \item \code{"none"} for no borders, grids and ticks.
#'          }
#'          The ggplot-object can be returned with \code{returnPlot} set to \code{TRUE} in order to further
#'          modify the plot's theme.
#' @param borderColor User defined color of whole diagram border (panel border).
#' @param axisColor User defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border).
#' @param hideLegend Indicates whether legend (guide) should be shown or not.
#' @param showTickMarks Whether tick marks of axes should be shown or not.
#' @param showAxisLabels.x Whether x axis labels (cluster variables) should be shown or not.
#' @param showAxisLabels.y Whether y axis labels (z scores) should be shown or not.
#' @param showGroupCount if \code{TRUE} (default), the count within each cluster group is added to the 
#'          legend labels (e.g. \code{"Group 1 (n=87)"}).
#' @param legendTitle Title of the diagram's legend.
#' @param legendLabels Labels for the guide/legend. Example: See \code{axisLabels.x}. If \code{legendLabels}
#'          is \code{NULL} (default), the standard string \code{"Group <nr>"} will be used.
#' @param legendPos The position of the legend, if a legend is drawn. Use \code{"bottom"}, \code{"top"}, \code{"left"}
#'          or \code{"right"} to position the legend above, below, on the left or right side of the diagram. Right
#'          positioning is default.
#' @param legendSize The text size of the legend. Default is 1. Relative size, so recommended values are from 0.3 to
#'          2.5
#' @param legendBorderColor Color of the legend's border. Default is \code{"white"}, so no visible border is drawn.
#' @param legendBackColor Fill color of the legend's background. Default is \code{"white"}, so no visible background is drawn.
#' @param majorGridColor Specifies the color of the major grid lines of the diagram background.
#' @param minorGridColor Specifies the color of the minor grid lines of the diagram background.
#' @param hideGrid.x If \code{TRUE}, the x-axis-gridlines are hidden. Default is \code{FALSE}.
#' @param hideGrid.y If \code{TRUE}, the y-axis-gridlines are hidden. Default is \code{FALSE}.
#' @param flipCoordinates If \code{TRUE}, the x and y axis are swapped.
#'
#' @return An object of type \link{structure} containing the used data frame for plotting, 
#'           the ggplot object, the number of found cluster (as calculated by \link{sjc.kgap})
#'           and the group classification (as calculated by \link{sjc.cluster}).
#' 
#' @examples
#' # K-means clustering of mtcars-dataset
#' sjc.qclus(mtcars)
#' 
#' # K-means clustering of mtcars-dataset with 4 pre-defined
#' # groups in a faceted panel
#' sjc.qclus(mtcars, groupcount=4, facetCluster=TRUE)
#' 
#' @import ggplot2
#' @export
sjc.qclus <- function(data,
                      groupcount=NULL,
                      method="k",
                      distance="euclidean", 
                      agglomeration="ward",
                      showAccuracy=FALSE,
                      title=NULL,
                      titleSize=1.3,
                      titleColor="black",
                      axisLabels.x=NULL,
                      axisLabelAngle.x=0, 
                      axisLabelSize=1.1,
                      axisLabelColor="gray30", 
                      axisTitle.x="Cluster group characteristics",
                      axisTitle.y="Mean of z-scores",
                      axisTitleColor="black",
                      axisTitleSize=1.3,
                      breakTitleAt=40,
                      breakLabelsAt=12,
                      breakLegendTitleAt=20, 
                      breakLegendLabelsAt=20,
                      facetCluster=FALSE,
                      barColor=NULL,
                      barAlpha=1,
                      colorPalette="GnBu",
                      barWidth=0.5,
                      barSpace=0.1,
                      barOutline=FALSE, 
                      outlineColor="black", 
                      theme=NULL,
                      borderColor=NULL, 
                      axisColor=NULL, 
                      hideLegend=FALSE,
                      showTickMarks=TRUE,
                      showAxisLabels.x=TRUE,
                      showAxisLabels.y=TRUE,
                      showGroupCount=TRUE,
                      legendTitle=NULL,
                      legendLabels=NULL,
                      legendPos="right",
                      legendSize=1,
                      legendBorderColor="white",
                      legendBackColor="white",
                      majorGridColor=NULL,
                      minorGridColor=NULL,
                      hideGrid.x=FALSE,
                      hideGrid.y=FALSE,
                      flipCoordinates=FALSE) {
  # remove missings
  data <- na.omit(data)
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
  if (!is.null(axisLabels.x) && is.list(axisLabels.x)) {
    axisLabels.x <- unlistlabels(axisLabels.x)
  }
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }
  # check for valid parameter
  if (is.null(axisLabels.x)) {
    axisLabels.x <- colnames(data)
  }
  # --------------------------------------------------------
  # Trim labels and title to appropriate size
  # --------------------------------------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)    
  }
  # check length of x-axis title and split longer string at into new lines
  if (!is.null(axisTitle.x)) {
    axisTitle.x <- sju.wordwrap(axisTitle.x, breakTitleAt)    
  }
  # check length of y-axis title and split longer string at into new lines
  if (!is.null(axisTitle.y)) {
    axisTitle.y <- sju.wordwrap(axisTitle.y, breakTitleAt)    
  }
  # check length of legend title and split longer string at into new lines
  if (!is.null(legendTitle)) {
    legendTitle <- sju.wordwrap(title, breakLegendTitleAt)    
  }
  # check length of y-axis title and split longer string at into new lines
  if (!is.null(legendLabels)) {
    legendLabels <- sju.wordwrap(legendLabels, breakLegendLabelsAt)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  axisLabels.x <- sju.wordwrap(axisLabels.x, breakLabelsAt)
  # ---------------------------------------------
  # check for auto-groupcount
  # ---------------------------------------------
  if (is.null(groupcount)) {
    # check whether method is kmeans. hierarchical clustering
    # requires a specified groupcount
    if (method!="k") {
      cat("\nCannot compute hierarchical cluster analysis when 'groupcount' is NULL. Using kmeans clustering instead.\n")
      method <- "k"
    }
    # retrieve optimal group count via gap statistics
    kgap <- sjc.kgap(data, plotResults=F)
    # save group counts
    groupcount <- kgap$solution
  }
  # ---------------------------------------------
  # auto-set legend labels
  # ---------------------------------------------
  if (is.null(legendLabels)) {
    legendLabels <- sprintf("Group %i", c(1:groupcount))
  }
  # ---------------------------------------------
  # run cluster analysis with claculated group count
  # ---------------------------------------------
  grp <- sjc.cluster(data, groupcount, method, distance, agglomeration)
  # ---------------------------------------------
  # Add group count to legend labels
  # ---------------------------------------------
  if (showGroupCount) {
    # iterate legend labels
    for (i in 1:length(legendLabels)) {
      # add group count to each label
      legendLabels[i] <- paste0(legendLabels[i], " (n=", length(which(grp==i)), ")")
    }
  }
  # scale data
  z <- scale(data)
  # retrieve column count
  colnr <- ncol(data)
  # init data frame
  df <- data.frame()
  # ---------------------------------------------
  # iterate all columns to calculate group means
  # ---------------------------------------------
  for (cnt in 1:colnr) {
    # retrieve column data
    coldat <- z[,cnt]
    # ---------------------------------------------
    # iterate groups
    # ---------------------------------------------
    for (i in 1:groupcount) {
      # retrieve column values for each group
      grpvalues <- coldat[which(grp==i)]
      # calculate mean
      mw <- mean(grpvalues, na.rm=TRUE)
      df <- rbind(df, cbind(x=cnt, y=mw, group=i))
    }
    # df %.% group_by(grp) %.% summarise(mean(a))
  }
  # --------------------------------------------------------
  # factor for discrete scale
  # --------------------------------------------------------
  df$group <- as.factor(df$group)
  # --------------------------------------------------------
  # show goodness of classification
  # --------------------------------------------------------
  if (showAccuracy) {
    sjc.grpdisc(data, grp, groupcount)
  }
  # --------------------------------------------------------
  # Prepare fill colors
  # --------------------------------------------------------
  if (is.null(barColor)) {
    scalecolors <- scale_fill_discrete(labels=legendLabels)
  }
  else if (barColor=="gs") {
    scalecolors <- scale_fill_grey(labels=legendLabels)
  }
  else if (barColor=="brewer") {
    # remember to specify the "colorPalette" if you use "brewer" as "barColor"
    scalecolors <- scale_fill_brewer(palette=colorPalette, labels=legendLabels)
  }
  else if (barColor=="bw") {
    barColor <- rep("white", length(legendLabels))
    scalecolors <- scale_fill_manual(values=barColor, labels=legendLabels)
  }
  else {
    scalecolors <- scale_fill_manual(values=barColor, labels=legendLabels)
  }
  # --------------------------------------------------------
  # check whether bars should have an outline
  # --------------------------------------------------------
  if (!barOutline) {
    outlineColor <- waiver()
  }
  # --------------------------------------------------------
  # Set theme and default grid colours. grid colours
  # might be adjusted later
  # --------------------------------------------------------
  hideGridColor <- c("white")
  if (is.null(theme)) {
    ggtheme <- theme_gray()
    hideGridColor <- c("gray90")
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
  # Hide or show Tick Marks and Axis Labels (x axis text) 
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showAxisLabels.x) {
    axisLabels.x <- NULL
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
  hidegrid <- element_line(colour=hideGridColor)
  # --------------------------------------------------------
  # create plot
  # --------------------------------------------------------
  gp <- 
    ggplot(df, aes(x=x, y=y, fill=group)) +
      geom_bar(stat="identity", position=position_dodge(barWidth+barSpace), colour=outlineColor, width=barWidth, alpha=barAlpha) +
      scale_x_discrete(breaks=c(1:colnr), limits=c(1:colnr), labels=axisLabels.x) +
      labs(title=title, x=axisTitle.x, y=axisTitle.y, fill=legendTitle)
  # --------------------------------------------------------
  # set colour palette for filling bars
  # --------------------------------------------------------
  if (!is.null(scalecolors)) {
    gp <- gp + scalecolors
  }
  # --------------------------------------------------------
  # hide y-axis labels
  # --------------------------------------------------------
  if (!showAxisLabels.y) {
    gp <- gp + scale_y_continuous(labels=NULL)    
  } 
  # --------------------------------------------------------
  # set axis label sizes and colors
  # --------------------------------------------------------
  gp <- gp +
    ggtheme +
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
            axis.text.x = element_text(angle=axisLabelAngle.x),
            axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor),
            plot.title = element_text(size=rel(titleSize), colour=titleColor))
  # --------------------------------------
  # set position and size of legend
  # --------------------------------------
  if (!hideLegend) {
    gp <- gp +
      theme(legend.position = legendPos,
            legend.text = element_text(size=rel(legendSize)),
            legend.background = element_rect(colour=legendBorderColor, fill=legendBackColor))
  }
  else {
    # --------------------------------------------------------
    # Hide Legend
    # --------------------------------------------------------
    gp <- gp + guides(fill=FALSE)
  }
  # --------------------------------------------------------
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  # --------------------------------------------------------
  if (flipCoordinates) {
    gp <- gp + coord_flip()
  }
  # --------------------------------------------------------
  # border/axis color
  # --------------------------------------------------------
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      gp <- gp + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      cat("\nParameter 'borderColor' can only be applied to 'bw' theme.\n")
    }
  }
  if (!is.null(axisColor)) {
    gp <- gp + 
      theme(axis.line = element_line(colour=axisColor))
  }
  # --------------------------------------------------------
  # grid color/style
  # --------------------------------------------------------
  if (!is.null(minorgrid)) {
    gp <- gp + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    gp <- gp + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideGrid.x) {
    gp <- gp + 
      theme(panel.grid.major.x = hidegrid,
            panel.grid.minor.x = hidegrid)
  }
  if (hideGrid.y) {
    gp <- gp + 
      theme(panel.grid.major.y = hidegrid,
            panel.grid.minor.y = hidegrid)
  }
  # --------------------------------------------------------
  # use facets
  # --------------------------------------------------------
  if (facetCluster) {
    gp <- gp + facet_wrap(~group)
  }
  # --------------------------------------------------------
  # plot
  # --------------------------------------------------------
  plot(gp)
  # --------------------------------------------------------
  # return values
  # --------------------------------------------------------
  invisible (structure(class = "sjcqclus",
                       list(data = df,
                            groupcount = groupcount,
                            classification = grp,
                            plot = gp)))
}


#' @title Compute hierarchical or kmeans cluster analysis
#' @name sjc.cluster
#' @description Compute hierarchical or kmeans cluster analysis and returns the group
#'                association for each observation as vector.
#' @seealso \code{\link{sjc.dend}} \cr
#'          \code{\link{sjc.grpdisc}} \cr
#'          \code{\link{sjc.elbow}}
#'
#' @param data The data frame containing all variables that should be used for the
#'          cluster analysis.
#' @param groupcount The amount of groups (clusters) that should be retrieved. Following functions
#'          may be helpful for estimating the amount of clusters:
#'          \itemize{
#'            \item Use \code{\link{sjc.elbow}}-function to determine the group-count depending on the elbow-criterion.
#'            \item If using kmeans as \code{method}, use \code{\link{sjc.kgap}}-function to determine the group-count according to the gap-statistic.
#'            \item If using hierarchical as \code{method} (default), use  \code{\link{sjc.dend}}-function to inspect different cluster group solutions.
#'            \item Use \code{\link{sjc.grpdisc}}-function to inspect the goodness of grouping (accuracy of classification).
#'            }
#' @param method Indicates the clustering method. If \code{"h"} (default), a hierachical 
#'          clustering using the ward method is computed. Use any other parameter to compute
#'          a k-means clustering.
#' @param distance The distance measure to be used when \code{"method"} is \code{"h"} (for hierarchical
#'          clustering). This must be one of \code{"euclidean"} (default), \code{"maximum"}, \code{"manhattan"}, 
#'          \code{"canberra"}, \code{"binary"} or \code{"minkowski"}. See \link{dist}.
#' @param agglomeration The agglomeration method to be used when \code{"method"} is \code{"h"} (for hierarchical
#'          clustering). This should be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, 
#'          \code{"mcquitty"}, \code{"median"} or \code{"centroid"}. Default is \code{"ward"}. See \link{hclust}.
#' @return The group classification for each observation as vector. This group
#'           classification is needed for \code{\link{sjc.grpdisc}}-function to
#'           check the goodness of classification.
#' 
#' @examples
#' # Hierarchical clustering of mtcars-dataset
#' groups <- sjc.cluster(mtcars, 5)
#' 
#' # K-means clustering of mtcars-dataset
#' groups <- sjc.cluster(mtcars, 5, method="k")
#' 
#' @import ggplot2
#' @export
sjc.cluster <- function(data, groupcount, method="h", distance="euclidean", agglomeration="ward") {
  # Prepare Data
  # listwise deletion of missing
  data <- na.omit(data) 
  # --------------------------------------------------
  # Ward Hierarchical Clustering
  # --------------------------------------------------
  if (method=="h") {
    # distance matrix
    d <- dist(data, method=distance)
    # hierarchical clustering, using ward
    hc <- hclust(d, method=agglomeration) 
    # cut tree into x clusters
    groups <- cutree(hc, k=groupcount)
  }
  else {
    km <- kmeans(data, groupcount)
    # return cluster assignment
    groups <- km$cluster
  }
  # return group assignment
  return(groups)
}


#' @title Compute hierarchical cluster analysis and visualize group classification
#' @name sjc.dend
#' @description Computes a hierarchical cluster analysis and plots a hierarchical
#'                dendrogram with highlighting rectangles around the classified groups.
#'                Can be used, for instance, as visual tool to verify the elbow-criterion
#'                (see \code{\link{sjc.elbow}}).
#'                
#' @seealso \code{\link{sjc.cluster}} \cr
#'          \code{\link{sjc.grpdisc}} \cr
#'          \code{\link{sjc.elbow}}
#'
#' @param data The data frame containing all variables that should be used for the
#'          cluster analysis.
#' @param groupcount The amount of groups (clusters) that should be used.
#'          \itemize{
#'            \item Use \code{\link{sjc.elbow}}-function to determine the group-count depending on the elbow-criterion.
#'            \item If using kmeans as \code{method}, use \code{\link{sjc.kgap}}-function to determine the group-count according to the gap-statistic.
#'            \item Use \code{\link{sjc.grpdisc}}-function to inspect the goodness of grouping (accuracy of classification).
#'          }
#'          Solutions for multiple cluster groups can be plotted, for instance with \code{"groupcount=c(3:6)"}.
#' @param distance The distance measure to be used. This must be one of \code{"euclidean"} (default), 
#'          \code{"maximum"}, \code{"manhattan"}, \code{"canberra"}, \code{"binary"} or 
#'          \code{"minkowski"}. See \link{dist}.
#' @param agglomeration The agglomeration method to be used. This should be one of
#'          \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, 
#'          \code{"mcquitty"}, \code{"median"} or \code{"centroid"}. Default is 
#'          \code{"ward"}. See \link{hclust}.
#' 
#' @importFrom scales brewer_pal
#' @examples
#' # Plot dendrogram of hierarchical clustering of mtcars-dataset
#' # and show group classification
#' sjc.dend(mtcars, 5)
#' 
#' # Plot dendrogram of hierarchical clustering of mtcars-dataset
#' # and show group classification for 2 to 4 groups
#' sjc.dend(mtcars, 2:4)
#' 
#' @export
sjc.dend <- function(data, groupcount, distance="euclidean", agglomeration="ward") {
  # Prepare Data
  # listwise deletion of missing
  data <- na.omit(data) 
  # --------------------------------------------------
  # Ward Hierarchical Clustering
  # --------------------------------------------------
  # distance matrix
  d <- dist(data, method=distance)
  # hierarchical clustering, using ward
  hc <- hclust(d, method=agglomeration) 
  # display simple dendrogram
  plot(hc, main="Cluster Dendrogramm", xlab=sprintf("Hierarchical Cluster Analysis, %s-Method", agglomeration))
  # now plot overlaying rectangles, depending on the amount of groupcounts
  gl <- length(groupcount)
  if (gl>1) {
    # retrieve different colors
    color <- brewer_pal("qual", "Set1")(gl)
    # iterate all groupcounts
    for (cnt in 1:gl) {
      k <- groupcount[cnt]
      # retrieve cluster
      cluster <- cutree(hc, k=k)
      # create table with cluster groups
      clustab <- table(cluster)[unique(cluster[hc$order])]
      m <- c(0, cumsum(clustab))
      which <- 1L:k
      # draw dendrogram with red borders around the clusters 
      # source code taken from "rect.hclust" from base-package
      for (n in seq_along(which)) {
        rect(m[which[n]] + 0.46 + (cnt*0.2), 
             par("usr")[3L], 
             m[which[n] + 1] + 0.53 - (cnt*0.2), 
             mean(rev(hc$height)[(k-1):k]), 
             border=color[cnt],
             lwd=2)
      }
    }
  }
  else {
    # draw dendrogram with red borders around the clusters 
    rect.hclust(hc, k=groupcount, border="red")
  }
}


#' @title Compute a linear discriminant analysis on classified cluster groups
#' @name sjc.grpdisc
#' @description Computes linear discriminant analysis on classified cluster groups.
#'                This function plots a bar graph indicating the goodness of classification
#'                for each group.
#' @seealso \code{\link{sjc.dend}} \cr
#'          \code{\link{sjc.cluster}} \cr
#'          \code{\link{sjc.elbow}}
#'
#' @param data The data frame containing all variables that should be used for the
#'          check for goodness of classification of a cluster analysis.
#' @param groups The group classification of the cluster analysis that was returned
#'          from the \code{\link{sjc.cluster}}-function.
#' @param groupcount The amount of groups (clusters) that should be used. Use
#'          the \code{\link{sjc.elbow}}-function to determine the group-count depending
#'          on the elbow-criterion.
#' @param showTotalCorrect If \code{TRUE} (default), a vertical line indicating the
#'          overall goodness of classification is added to the plot, so one can see
#'          whether a certain group is below or above the average classification goodness.
#'          
#' @examples
#' # retrieve group classification from hierarchical cluster analysis
#' # on the mtcars data set (5 groups)
#' groups <- sjc.cluster(mtcars, 5)
#' 
#' # plot goodness of group classificatoin
#' sjc.grpdisc(mtcars, groups, 5)
#' 
#' @importFrom MASS lda
#' @import ggplot2
#' @export
sjc.grpdisc <- function(data, groups, groupcount, showTotalCorrect=TRUE) {
  # Prepare Data
  # listwise deletion of missing
  data <- na.omit(data) 
  xval <- cbind(1:groupcount)-0.25
  xplotval <- cbind(1:groupcount)
  # ---------------------------------------------------------------
  # compute discriminant analysis of groups on original data frame
  # ---------------------------------------------------------------
  disc <- lda(groups ~ ., data=data, na.action="na.omit", CV=TRUE)
  # ---------------------------------------------------------------
  # Assess the accuracy of the prediction
  # percent correct for each category of groups
  # ---------------------------------------------------------------
  ct <- table(groups, disc$class)
  dg <- diag(prop.table(ct, 1))
  # print correct percentage for each category of groups
  print(dg)
  # ---------------------------------------------------------------
  # print barplot for correct percentage for each category of groups
  # ---------------------------------------------------------------
  perc <- round(100*dg,2)
  percrest <- round(100-perc,2)
  counts <- rbind(perc, percrest)
  # total correct percentage
  totalcorrect <- sum(diag(prop.table(ct)))
  # round total percentages and transform to percent value
  totalcorrect <- round(100*totalcorrect,2)
  print(totalcorrect)
  
  # create three data columns for data frame which is
  # needed to plot the barchart with ggplot
  newdat <- NULL
  tmpdat <- NULL
  filldat <- NULL
  labeldat <- NULL
  
  # data frame has flexible count of rows, depending on
  # the amount of groups in the lda
  for (i in 1:groupcount) {
    # first columns indicates the two parts of each group
    # (correct percentage and remaining percentage untill 100%)
    newdat <- rbind(newdat, c(paste("g",i,sep="")))
    newdat <- rbind(newdat, c(paste("g",i,sep="")))
    # second columns contains the percentage of lda
    # followed by the remaining percentage to 100%
    tmpdat <- rbind(tmpdat, perc[i])
    tmpdat <- rbind(tmpdat, percrest[i])
    # third columns indicates both which data row contains
    # the lda-percentage and which one the remaining percentage
    filldat <- rbind(filldat, "1")
    filldat <- rbind(filldat, "2")
    # last column is created for printing the label-values
    # we need only on percentage value, otherwise double labels are
    # printed
    labeldat <- rbind(labeldat, perc[i])
    labeldat <- rbind(labeldat, 0)
  }
  # create data frame
  mydat <- data.frame(filldat, newdat, tmpdat, labeldat)
  # name columns
  names(mydat) <- c("fg", "grp", "prc", "cprc")
  # fillgroup-indicator ($fg) needs to be a factor
  mydat$fg <- factor(mydat$fg)
  # plot bar charts, stacked proportional
  # this works, because we always have two "values" (variables)
  # for the X-axis in the $grp-columns indicating a group
  classplot <- ggplot(mydat, aes(x=grp, y=prc, fill=fg)) +
    # use stat identity to show value, not count of $prc-variable
    # draw no legend!
    geom_bar(stat="identity", colour="black", show_guide=FALSE) +
    # fill bars
    scale_fill_manual(values=c("#235a80", "#80acc8")) +
    # give chart and X-axis a title
    labs(title="Accuracy of cluster group classification (in %)", x="cluster groups", y=NULL) +
    # print value labels into bar chart
    geom_text(aes(label=cprc, y=cprc), vjust=1.2, colour="white", size=4.5) +
    # larger font size for axes
    theme(axis.line = element_line(colour="gray"), 
          axis.text = element_text(size=rel(1.2)), 
          axis.title = element_text(size=rel(1.2))) +
    # set ticks
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    # change range on x-axis, so the text annotation is visible and
    # beside the bars and not printed into them
    coord_cartesian(ylim=c(0,100), xlim=c(-0.5,groupcount+1))
  if (showTotalCorrect) {
    classplot <- classplot +
    # set line across all bars which indicates the total percentage of
    # correct assigned cases
    geom_hline(yintercept=totalcorrect, linetype=2, colour="#333333") +
      # print text annotation
      annotate("text", x=0, y=totalcorrect, vjust=1.2, label=paste("overall", c(totalcorrect), sep="\n"), size=5, colour="#333333")
  }
  print(classplot)
}


#' @title Plot elbow values of a k-means cluster analysis
#' @name sjc.elbow
#' @description Plot elbow values of a k-means cluster analysis. This function
#'                computes a k-means cluster analysis on the provided data frame
#'                and produces two plots: one with the different elbow values
#'                and a second plot that maps the differences between each
#'                "step" (i.e. between elbow values) on the y-axis. An
#'                increase in the second plot may indicate the elbow criterion.
#' 
#' @seealso \code{\link{sjc.kgap}} \cr
#'          \code{\link{sjc.dend}} \cr
#'          \code{\link{sjc.cluster}} \cr
#'          \code{\link{sjc.grpdisc}}
#'
#' @param data The data frame containing all variables that should be used for 
#'          determining the elbow criteria.
#' @param steps The maximum group-count for the k-means cluster analysis for
#'          which the elbow-criterion should be displayed. Default is \code{15}.
#' @param showDiff If \code{TRUE}, an additional plot with the differences between 
#'          each fusion step of the Elbow criterion calculation is shown. This plot
#'          may help identifying the "elbow". Default for this parameter is \code{FALSE}.
#'          
#' @examples
#' # plot elbow values of mtcars dataset
#' sjc.elbow(mtcars)
#' 
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
sjc.elbow <- function (data, steps=15, showDiff=FALSE) {
  # Prepare Data
  # listwise deletion of missing
  data <- na.omit(data) 
  # define line linecolor
  lcol <- rgb(128,172,200, maxColorValue=255)
  # calculate elbow values (sum of squares)
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:steps) wss[i] <- sum(kmeans(data,centers=i)$withinss)
  # round and print elbow values
  wssround <- round(wss,0)
  dfElbowValues <- as.data.frame(melt(wssround))
  dfElbowValues <- cbind(dfElbowValues, xpos=1:nrow(dfElbowValues))
  # calculate differences between each step
  diff <- c()
  for (i in 2:steps) diff <- cbind(diff,wssround[i-1]-wssround[i])
  dfElbowDiff <- as.data.frame(melt(diff))
  # --------------------------------------------------
  # Plot diagram with sum of squares
  # all pointes are connected with a line
  # a bend the in curve progression might indicate elbow
  # --------------------------------------------------
  plot(ggplot(dfElbowValues, aes(x=xpos, y=value, label=value)) + 
    geom_line(colour=lcol) + 
    geom_point(colour=lcol, size=3) +
    geom_text(hjust=-0.3, size=4) +
    labs(title="Elbow criterion (sum of squares)", x="Number of clusters", y="elbow value"))
  # --------------------------------------------------
  # Plot diagram with differences between each step
  # increasing y-value on x-axis (compared to former y-values)
  # might indicate elbow
  # --------------------------------------------------
  if (showDiff) {
    plot(ggplot(dfElbowDiff, aes(x=Var2, y=value, label=value)) + 
           geom_line(colour=lcol) + 
           geom_point(colour=lcol, size=3) +
           geom_text(hjust=-0.3, size=4) +
           labs(title="Elbow criterion (differences between sum of squares)", x="difference to previews cluster", y="delta"))
  }
}


#' @title Compute gap statistics for k-means-cluster
#' @name sjc.kgap
#' @description An implementation of the gap statistic algorithm from Tibshirani, Walther, and Hastie's
#'                "Estimating the number of clusters in a data set via the gap statistic".
#'                Source code was taken from the \code{\link{clusGap}} function of the
#'                cluster-package (\url{http://cran.r-project.org/web/packages/cluster/index.html}).
#'                
#' @seealso \code{\link{sjc.elbow}} \cr
#'          \code{\link{clusGap}}
#' 
#' @param x A matrix, where rows are observations and columns are individual dimensions, 
#'          to compute and plot the gap statistic (according to a uniform reference distribution).
#' @param max The maximum number of clusters to consider, must be at least two. Default
#'          is 10.
#' @param B integer, number of Monte Carlo ("bootstrap") samples. Default is 100.
#' @param SE.factor [When \code{method} contains "SE"] Determining the optimal 
#'          number of clusters, Tibshirani et al. proposed the "1 S.E."-rule. 
#'          Using an SE.factor f, the "f S.E."-rule is used, more generally.
#' @param method A character string indicating how the "optimal" number of clusters, 
#'          k^, is computed from the gap statistics (and their standard deviations), 
#'          or more generally how the location k^ of the maximum of f[k] should be 
#'          determined. Default is \code{"Tibs2001SEmax"}. Possible value are:
#'          \itemize{
#'            \item \code{"globalmax"} simply corresponds to the global maximum, i.e., is which.max(f).
#'            \item \code{"firstmax"} gives the location of the first local maximum.
#'            \item \code{"Tibs2001SEmax"} uses the criterion, Tibshirani et al(2001) proposed: "the smallest k such that f(k) >= f(k+1) - s_{k+1}". Note that this chooses k = 1 when all standard deviations are larger than the differences f(k+1) - f(k).
#'            \item \code{"firstSEmax"} is the location of the first f() value which is not larger than the first local maximum minus SE.factor * SE.f[], i.e, within an "f S.E." range of that maximum (see also SE.factor).
#'            \item \code{"globalSEmax"} (used in Dudoit and Fridlyand (2002), supposedly following Tibshirani's proposition) is the location of the first f() value which is not larger than the global maximum minus SE.factor * SE.f[], i.e, within an "f S.E." range of that maximum (see also SE.factor).
#'            }
#' @param plotResults If \code{TRUE} (default), a graph visualiting the gap statistic will
#'          be plotted. Use \code{FALSE} to omit the plot.
#' 
#' @return An object of type \link{structure} containing the used data frame for plotting, the ggplot object
#'           and the number of found cluster.
#' 
#' @note Source code was taken from the \code{\link{clusGap}} function of the
#'         cluster-package (\url{http://cran.r-project.org/web/packages/cluster/index.html}).
#' 
#' @references \itemize{
#'              \item Tibshirani R, Walther G, Hastie T (2001) Estimating the number of clusters in a data set via gap statistic. J. R. Statist. Soc. B, 63, Part 2, pp. 411-423
#'              \item Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., Hornik, K.(2013). cluster: Cluster Analysis Basics and Extensions. R package version 1.14.4. (\url{http://cran.r-project.org/web/packages/cluster/index.html})
#'             }
#' 
#' @examples
#' # plot gap statistic and determine best number of clusters
#' # in mtcars dataset
#' sjc.kgap(mtcars)
#' 
#' # and in iris dataset
#' sjc.kgap(iris[,1:4])
#' 
#' @import ggplot2
#' @export
sjc.kgap <- function(x, max=10, B=100, SE.factor=1, method="Tibs2001SEmax", plotResults=TRUE) {
  # Prepare Data
  # listwise deletion of missing
  x <- na.omit(x) 
  
  gap <- clusGap(x, kmeans, max, B)

  stopifnot((K <- nrow(T <-gap$Tab)) >= 1, SE.factor >= 0)
  cat("Clustering Gap statistic [\"clusGap\"].\n",
      sprintf("B=%d simulated reference sets, k = 1..%d\n",gap$B, K), sep="")
  nc <- maxSE(f = T[,"gap"], SE.f = T[,"SE.sim"],
              method=method, SE.factor=SE.factor)
  cat(sprintf(" --> Number of clusters (method '%s'%s): %d\n",
              method, if(grepl("SE", method))
                sprintf(", SE.factor=%g",SE.factor) else "", nc))
  # point size for cluster solution
  nclus <- rep(2, max)
  nclus[nc] <- 4
  # coliur  for cluster solution
  cclus <- rep("black", max)
  cclus[nc] <- "#cc3366"
  # create data frame
  df <- data.frame(x=1:max, y=gap$Tab[,'gap'], se=gap$Tab[,'SE.sim'], psize=nclus, pcol=cclus)
  # plot cluster solution
  gp <- ggplot(df, aes(x=x, y=y)) + 
    geom_errorbar(aes(ymin=y-se, ymax=y+se), width=0, size=0.5, colour="#3366cc") +
    geom_line(colour="gray50") +
    geom_point(colour=df$pcol, size=df$psize) +
    scale_x_discrete(breaks=c(1:nrow(df))) +
    labs(x="Number of clusters", y="Gap", title=sprintf("Estimation of clusters (gap statistics)\n%i-cluster solution found",nc)) +
    theme_classic()
  if (plotResults) {
    plot(gp)
  }
  # return value
  invisible(structure(class = "sjckgap",
                      list(data = df,
                      plot = gp,
                      solution = nc)))
}


# -----------------------------------------------------------------------------
# Source code of the following function was taken from the cluster-package
#
# Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., Hornik, K.(2013). 
# cluster: Cluster Analysis Basics and Extensions. R package version 1.14.4.
# 
# http://cran.r-project.org/web/packages/cluster/index.html
# -----------------------------------------------------------------------------

#### Originally from orphaned package SLmisc
#### (Version: 1.4.1, 2007-04-12, Maintainer: Matthias Kohl <kohl@sirs-lab.com>)
#### License: GPL (version 2 or later)
####
#### which said
####  "function corresponds to function gap in package SAGx"

## MM: SAGx is now in Bioconductor --- 1.10.1{devel} or 1.11.1{release}
##     had gap() *corrected* to re-cluster using FUNcluster --> see ./gap-SAGx.R.~orig~
##
## MM: Package 'lga' -- has gap() and lga and robust lga [-> UBC]
##    - it uses  boot() nicely  [2012-01: ORPHANED because  Justin Harrington is amiss]
## MM: renamed arguments, and changed almost everything

clusGap <- function (x, FUNcluster, K.max, B = 100, verbose = interactive(), ...)
{
  stopifnot(is.function(FUNcluster), length(dim(x)) == 2, K.max >= 2,
            (n <- nrow(x)) >= 1, (p <- ncol(x)) >= 1)
  if(B != (B. <- as.integer(B)) || (B <- B.) <= 0)
    stop("'B' has to be a positive integer")
  
  if(is.data.frame(x))
    x <- as.matrix(x)
  ii <- seq_len(n)
  W.k <- function(X, kk) {
    clus <- if(kk > 1) FUNcluster(X, kk, ...)$cluster else rep.int(1L, nrow(X))
    ##                 ---------- =  =       -------- kmeans() has 'cluster'; pam() 'clustering'
    0.5* sum(vapply(split(ii, clus),
                    function(I) { xs <- X[I,, drop=FALSE]
                                  sum(dist(xs)/nrow(xs)) }, 0.))
  }
  logW <- E.logW <- SE.sim <- numeric(K.max)
  if(verbose) cat("Clustering k = 1,2,..., K.max (= ",K.max,"): .. ", sep='')
  for(k in 1:K.max)
    logW[k] <- log(W.k(x, k))
  if(verbose) cat("done\n")
  
  ## Scale 'x' into "hypercube" -- we later fill with H0-generated data
  xs <- scale(x, center=TRUE, scale=FALSE)
  m.x <- rep(attr(xs,"scaled:center"), each = n)# for back transforming
  V.sx <- svd(xs)$v
  rng.x1 <- apply(xs %*% V.sx, # = transformed(x)
                  2, range)
  
  logWks <- matrix(0., B, K.max)
  if(verbose) cat("Bootstrapping, b = 1,2,..., B (= ", B,
                  ")  [one \".\" per sample]:\n", sep="")
  for (b in 1:B) {
    ## Generate "H0"-data as "parametric bootstrap sample" :
    z1 <- apply(rng.x1, 2,
                function(M, nn) runif(nn, min=M[1], max=M[2]),
                nn=n)
    z <- tcrossprod(z1, V.sx) + m.x # back transformed
    for(k in 1:K.max) {
      logWks[b,k] <- log(W.k(z, k))
    }
    if(verbose) cat(".", if(b %% 50 == 0) paste(b,"\n"))
  }
  if(verbose && (B %% 50 != 0)) cat("",B,"\n")
  E.logW <- colMeans(logWks)
  SE.sim <- sqrt((1 + 1/B) * apply(logWks, 2, var))
  structure(class = "clusGap",
            list(Tab = cbind(logW, E.logW, gap = E.logW - logW, SE.sim),
                 ## K.max == nrow(T)
                 n = n, B = B, FUNcluster=FUNcluster))
}

## lga/R/gap.R   --- has for Tibshirani et al (2001):
## ElogWks[k,] <- c(mean(BootOutput), sqrt(var(BootOutput)*(1+1/B)))
## GAP[k] <- ElogWks[k,1] - logWks[k]
## if (k > 1)
##     if(GAP[k-1] >= GAP[k]-ElogWks[k,2] & !doall)
##         finished <- TRUE
##  so they effectively only look for the *first* (local) maximum which ..
## MM: <==> diff(GAP) = GAP[k] - GAP[k-1] <= +SE.sim[k]


## criteria.DandF() -- Dudoit and Fridlyand (2002)
## ---------------- looks at the *global* maximum and then to the left..
## y <- x$data
## crit <- diff(y[which.max(y[,"Gap"]), c("Sks", "Gap")])
## nclust <- min(which(y[,"Gap"] > crit))
## return(ifelse(nclust == nrow(y), NA, nclust))

maxSE <- function(f, SE.f,
                  method = c("firstSEmax", "Tibs2001SEmax",
                             "globalSEmax", "firstmax", "globalmax"),
                  SE.factor = 1)
{
  method <- match.arg(method)
  stopifnot((K <- length(f)) >= 1, K == length(SE.f), SE.f >= 0, SE.factor >= 0)
  fSE <- SE.factor * SE.f
  switch(method,
         "firstmax" = { ## the first local maximum  (== firstSEmax with SE.factor == 0)
           decr <- (dg <- diff(f)) <= 0 # length K-1
           if(any(decr)) which.max(decr) else K # the first TRUE, or K
         },
         "globalmax" = {
           which.max(f)
         },
         "Tibs2001SEmax" = { ## The one Tibshirani et al (2001) proposed:
           ## "the smallest k such that f(k) >= f(k+1) - s_{k+1}"
           g.s <- f - fSE
           if(any(mp <- f[-K] >= g.s[-1])) which.max(mp) else K
         },
         "firstSEmax" = { ## M.Maechler(2012): rather ..
           ## look at the first *local* maximum and then to the left ..:
           decr <- (dg <- diff(f)) <= 0 # length K-1
           nc <- if(any(decr)) which.max(decr) else K # the first TRUE, or K
           if(any(mp <- f[seq_len(nc - 1)] >= f[nc] - fSE[nc]))
             which(mp)[1]
           else nc
         },
         "globalSEmax" = { ## Dudoit and Fridlyand (2002) *thought* Tibshirani proposed..
           ## in 'lga', see criteria.DandF():
           ## looks at the *global* maximum and then to the left..
           nc <- which.max(f)
           if(any(mp <- f[seq_len(nc - 1)] >= f[nc] - fSE[nc]))
             which(mp)[1]
           else nc
         })
}
