#' @title Plot (grouped) scatter plots
#' @name sjp.scatter
#'             
#' @description Display scatter plot of two variables. Adding a grouping variable to
#'                the scatter plot is possible. Furthermore, fitted lines can be added
#'                for each group as well as for the overall plot.
#'
#' @references \itemize{
#'              \item \url{http://rpubs.com/sjPlot/sjpscatter}
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              }
#'              
#' @seealso \code{\link{sjp.lm1}} \cr
#'          \code{\link{sjp.reglin}}
#' 
#' @param x A vector (variable) indicating the x positions.
#' @param y A vector (variable) indicating the y positions.
#' @param grp A grouping variable. If not \code{NULL}, the scatter plot will be grouped. See
#'          examples below. Default is \code{NULL}, i.e. not grouping is done.
#' @param title Title of the diagram, plotted above the whole diagram panel.
#'          Use \code{"auto"} to automatically detect variable names that will be used as title
#'          (see \code{\link{sji.setVariableLabels}}) for details).
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param legendTitle Title of the diagram's legend.
#' @param legendLabels Labels for the guide/legend.
#' @param axisTitle.x A label (title) for the x axis.
#'          Use \code{"auto"} to automatically detect variable names that will be used as title
#'          (see \code{\link{sji.setVariableLabels}}) for details).
#' @param axisTitle.y A label (title) for the y axis.
#'          Use \code{"auto"} to automatically detect variable names that will be used as title
#'          (see \code{\link{sji.setVariableLabels}}) for details).
#' @param axisTitleColor The color of the x and y axis labels. Refers to \code{axisTitle.x} and \code{axisTitle.y},
#'          not to the tick mark or category labels.
#' @param axisTitleSize The size of the x and y axis labels. Refers to \code{axisTitle.x} and \code{axisTitle.y},
#'          not to the tick mark or category labels.
#' @param axisTickMarkSize The size of tick mark values of both x and y axis. Default is 1, recommended values range
#'          between 0.5 and 3.0
#' @param axisTickMarkColor User defined color for tick mark values. If not specified, a default mid gray
#'          color will be used for the labels.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title.
#' @param breakLegendTitleAt Wordwrap for diagram legend title. Determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted.
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted.
#' @param pointAlpha The alpha values of scattered points. Useful to better cope with overplotting. Default is 0.5
#' @param pointSize The size of scattered points.
#' @param pointColors The color(s) of scattered points. If \code{grp} is not \code{NULL}, groups are indicated
#'          by different colors, thus a vector with multiple color values has to be supplied. By default,
#'          the \code{Set1} palette of diverging palette type is chosen (see \url{http://colorbrewer2.org}).
#' @param legendPos The position of the legend, if a legend is drawn. Use \code{"bottom"}, \code{"top"}, \code{"left"}
#'          or \code{"right"} to position the legend above, below, on the left or right side of the diagram. Right
#'          positioning is default.
#' @param legendSize The text size of the legend. Default is 1. Relative size, so recommended values are from 0.3 to
#'          2.5
#' @param legendBorderColor Color of the legend's border. Default is \code{"white"}, so no visible border is drawn.
#' @param legendBackColor Fill color of the legend's background. Default is \code{"white"}, so no visible background is drawn.
#' @param showTickMarkLabels.x Whether x axis tick mark labels should be shown or not.
#' @param showTickMarkLabels.y Whether y axis tick mark labels  should be shown or not.
#' @param showTickMarks Whether tick marks of axes should be shown or not.
#' @param showGroupFitLine If \code{TRUE}, a fitted line for each group is drawn. See \code{fitmethod} to change the
#'          fit method of the fitted lines.
#' @param showTotalFitLine If \code{TRUE}, a fitted line for the overall scatterplot is drawn. See \code{fitmethod} to change the
#'          fit method of the fitted line.
#' @param showSE If \code{TRUE}, a shaded region indicating the standard error of the fitted lines will be added.
#' @param fitmethod By default, a linear method (\code{"lm"}) is used for fitting the fit lines. Possible values are
#'          for instance:
#'          \itemize{
#'            \item \code{"lm"}
#'            \item \code{"glm"}
#'            \item \code{"loess"}
#'            \item \code{"auto"}
#'          }
#'          (see \url{http://docs.ggplot2.org/current/stat_smooth.html} for more details).
#' @param useJitter If \code{TRUE}, points will be jittered (to avoid overplotting).
#' @param autojitter If \code{TRUE}, points will be jittered according to an overlap-estimation. A matrix of \code{x}
#'          and \code{y} values is created and the amount of cells (indicating a unique point position) is calculated.
#'          If more than 15\% (see \code{jitterRatio}) of the approximated amount of unique point coordinates seem to
#'          overlap, they are automatically jittered.
#' @param jitterRatio The ratio of tolerated overlapping (see \code{autojitter}). If approximated amount of overlapping 
#'          points exceed this ration, they are automatically jittered. Default is 0.15. Valid values range between 0 and 1.
#' @param showRug If \code{TRUE}, a marginal rug plot is displayed in the graph (see \url{http://docs.ggplot2.org/current/geom_rug.html}
#'          for more details).)
#' @param hideLegend Indicates whether legend (guide) should be shown or not.
#' @param borderColor User defined color of whole diagram border (panel border).
#' @param axisColor User defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border).
#' @param majorGridColor Specifies the color of the major grid lines of the diagram background.
#' @param minorGridColor Specifies the color of the minor grid lines of the diagram background.
#' @param hideGrid.x If \code{TRUE}, the x-axis-gridlines are hidden. Default is \code{FALSE}.
#' @param hideGrid.y If \code{TRUE}, the y-axis-gridlines are hidden. Default is \code{FALSE}.
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids.
#'          \itemize{
#'          \item Use \code{"bw"} for a white background with gray grids
#'          \item \code{"classic"} for a classic theme (black border, no grids)
#'          \item \code{"minimal"} for a minimalistic theme (no border,gray grids)
#'          \item \code{"none"} for no borders, grids and ticks or
#'          \item \code{"themr"} if you are using the \code{ggthemr} package (in such cases, you may use the \code{ggthemr::swatch} function to retrieve theme-colors for the \code{pointColors} parameter)
#'          }
#'          See \url{http://rpubs.com/sjPlot/custplot} for details and examples.
#' @param useFacetGrid \code{TRUE} when each scatter plot group should be plotted as single facet instead of 
#'          an integrated single graph. Only applies if \code{grp} is not \code{NULL}. Each category of
#'          \code{grp} will be plotted in an own facet.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#'
#' @examples
#' # load sample date
#' data(efc)
#' 
#' # simple scatter plot, auto-jittering
#' sjp.scatter(efc$e16sex,efc$neg_c_7)
#'
#' # simple scatter plot, no jittering needed
#' sjp.scatter(efc$c160age,efc$e17age)
#'
#' # grouped scatter plot
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep)
#'
#' # grouped and jittered scatter plot with marginal rug plot
#' sjp.scatter(efc$e16sex,efc$neg_c_7, efc$c172code, showRug=TRUE)
#' 
#' # grouped and labelled scatter plot
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep, title="Scatter Plot",
#'             legendTitle=sji.getVariableLabels(efc)['e42dep'],
#'             legendLabels=sji.getValueLabels(efc)[['e42dep']],
#'             axisTitle.x=sji.getVariableLabels(efc)['c160age'],
#'             axisTitle.y=sji.getVariableLabels(efc)['e17age'],
#'             showGroupFitLine=TRUE)
#' 
#' # grouped and labelled scatter plot as facets
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep, title="Scatter Plot",
#'             legendTitle=sji.getVariableLabels(efc)['e42dep'],
#'             legendLabels=sji.getValueLabels(efc)[['e42dep']],
#'             axisTitle.x=sji.getVariableLabels(efc)['c160age'],
#'             axisTitle.y=sji.getVariableLabels(efc)['e17age'],
#'             showGroupFitLine=TRUE, useFacetGrid=TRUE, showSE=TRUE)
#' 
#' # -------------------------------
#' # auto-detection of labels
#' # -------------------------------
#' efc <- sji.setVariableLabels(efc, sji.getVariableLabels(efc))
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep,
#'             title="auto", axisTitle.x="auto", axisTitle.y="auto")
#' 
#'   
#' @importFrom scales brewer_pal
#' @import ggplot2
#' @export
sjp.scatter <- function(x,
                        y,
                        grp=NULL,
                        title=NULL, 
                        titleSize=1.3,
                        titleColor="black",
                        legendTitle=NULL,
                        legendLabels=NULL,
                        axisTitle.x=NULL,
                        axisTitle.y=NULL,
                        axisTitleColor="black",
                        axisTitleSize=1.3,
                        axisTickMarkSize=1,
                        axisTickMarkColor="grey50",
                        breakTitleAt=50, 
                        breakLegendTitleAt=20, 
                        breakLegendLabelsAt=20,
                        pointAlpha=0.5,
                        pointSize=3,
                        pointColors=NULL,
                        legendPos="right",
                        legendSize=1,
                        legendBorderColor="white",
                        legendBackColor="white",
                        showTickMarkLabels.x=TRUE,
                        showTickMarkLabels.y=TRUE,
                        showTickMarks=TRUE,
                        majorGridColor=NULL,
                        minorGridColor=NULL,
                        hideGrid.x=FALSE,
                        hideGrid.y=FALSE,
                        borderColor=NULL, 
                        axisColor=NULL, 
                        showGroupFitLine=FALSE,
                        showTotalFitLine=FALSE,
                        showSE=FALSE,
                        fitmethod="lm",
                        useJitter=FALSE,
                        autojitter=TRUE,
                        jitterRatio=0.15,
                        showRug=FALSE,
                        hideLegend=FALSE,
                        theme=NULL,
                        useFacetGrid=FALSE,
                        printPlot=TRUE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(legendLabels) && !is.null(grp)) legendLabels <- autoSetValueLabels(grp)
  if (!is.null(axisTitle.x) && axisTitle.x=="auto") axisTitle.x <- autoSetVariableLabels(x)
  if (!is.null(axisTitle.y) && axisTitle.y=="auto") axisTitle.y <- autoSetVariableLabels(y)
  if (!is.null(title) && title=="auto") {
    t1 <- autoSetVariableLabels(x)
    t2 <- autoSetVariableLabels(y)
    if (!is.null(t1) && !is.null(t2)) {
      title <- paste0(t1, " by ", t2)
      if (!is.null(grp)) {
        t3 <- autoSetVariableLabels(grp)
        if (!is.null(t3)) title <- paste0(title, " (grouped by ", t3, ")")
      }
    }
  }
  # ------------------------------------------
  # check for auto-jittering
  # ------------------------------------------
  if (autojitter && !useJitter) {
    # check for valid range of jitter ratio
    if (jitterRatio<=0 || jitterRatio>=1) {
      # inform user
      cat("\njitterRatio out of valid bounds. Using 0.15 for jitterRatio...\n")
      jitterRatio <- 0.15
    }
    # retrieve the highest amount of points lying
    # on the same coordinate
    overlap <- nrow(table(x,y)) * ncol(table(x,y))
    # check ratio of overlapping points according to total points
    if (overlap < (length(x)*jitterRatio)) {
      # use jittering now
      useJitter <- TRUE
      cat("\nauto-jittering values...\n")
    }
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }
  # ------------------------------------------
  # create data frame
  # ------------------------------------------
  # check whether we have grouping variable
  if (is.null(grp)) {
    # if not, add a dummy grouping variable
    grp <- rep(1, length(x))
    # we don't need legend here
    hideLegend <- TRUE
  }
  # simple data frame
  df <- na.omit(data.frame(cbind(x=x, y=y, grp=grp)))
  # group as factor
  df$grp <- as.factor(df$grp)
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # Check whether we have any labels passed as parameter
  if (is.null(legendLabels)) {
    # if not, use category text of group variable as legend text
    legendLabels <- c(sort(unique(df$grp)))
  }
  # wrap legend text lines
  legendLabels <- sju.wordwrap(legendLabels, breakLegendLabelsAt)
  # check whether we have a title for the legend
  if (!is.null(legendTitle)) {
    # if yes, wrap legend title line
    legendTitle <- sju.wordwrap(legendTitle, breakLegendTitleAt)
  }
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) {
    axisTitle.x <- sju.wordwrap(axisTitle.x, breakTitleAt)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.y)) {
    axisTitle.y <- sju.wordwrap(axisTitle.y, breakTitleAt)
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
  else if (theme=="themr") {
    ggtheme <- NULL
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
  # Hide or show Tick Marks and Category Labels (x axis text) 
  # --------------------------------------------------------
  if (!showTickMarks && !is.null(ggtheme)) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  # --------------------------------------------------------
  # Prepare fill colors
  # --------------------------------------------------------
  if (is.null(pointColors)) {
    colen <- length(unique(na.omit(grp)))
    if (colen==1) {
      pointColors <- "#003399"
    }
    else {
      pointColors <- brewer_pal(palette="Set1")(colen+1)
    }
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
  # Plot scatter plot
  # --------------------------------------------------------
  scatter <- ggplot(df,aes(x, y, colour=grp)) +
    scale_color_manual(values=pointColors, labels=legendLabels)
  # --------------------------------------------------------
  # Add marginal rug
  # --------------------------------------------------------
  if (showRug) {
    if (useJitter) {
      scatter <- scatter + geom_rug(position="jitter")
    }
    else {
      scatter <- scatter + geom_rug()
    }
  }
  # --------------------------------------------------------
  # Use Jitter/Points
  # --------------------------------------------------------
  if (useJitter) {
    scatter <- scatter + geom_jitter(alpha=pointAlpha, size=pointSize)
  }
  else {
    scatter <- scatter + geom_point(alpha=pointAlpha, size=pointSize)
  }
  # --------------------------------------------------------
  # Show fitted lines
  # --------------------------------------------------------
  if (showGroupFitLine) {
    scatter <- scatter + stat_smooth(data=df, aes(colour=grp), method=fitmethod, se=showSE)
  }
  if (showTotalFitLine) {
    scatter <- scatter + stat_smooth(method=fitmethod, se=showSE, colour="black")
  }
  # --------------------------------------------------------
  # set font size for axes.
  # --------------------------------------------------------
  scatter <- scatter + 
    labs(title=title, x=axisTitle.x, y=axisTitle.y, colour=legendTitle)
  # --------------------------------------------------------
  # apply theme
  # --------------------------------------------------------
  if (!is.null(ggtheme)) {
    scatter <- scatter + 
      ggtheme +
      # do minor modifications to theme
      theme(axis.text = element_text(size=rel(axisTickMarkSize), colour=axisTickMarkColor), 
            axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
            plot.title = element_text(size=rel(titleSize), colour=titleColor))
  }
  # --------------------------------------------------------
  # Hide or show tick marks
  # --------------------------------------------------------
  if (!showTickMarkLabels.x) {
    scatter <- scatter + scale_x_continuous(labels=NULL)
  }
  if (!showTickMarkLabels.y) {
    scatter <- scatter + scale_y_continuous(labels=NULL)
  }
  # --------------------------------------------------------
  # Hide or show Legend
  # --------------------------------------------------------
  if (hideLegend) {
    # remove guide / legend
    scatter <- scatter + guides(colour=FALSE)
  }
  # --------------------------------------
  # set position and size of legend
  # --------------------------------------
  if (!hideLegend) {
    scatter <- scatter + 
      theme(legend.position = legendPos,
            legend.text = element_text(size=rel(legendSize)),
            legend.background = element_rect(colour=legendBorderColor, fill=legendBackColor))
  }
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      scatter <- scatter + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      cat("\nParameter 'borderColor' can only be applied to 'bw' theme.\n")
    }
  }
  if (!is.null(axisColor)) {
    scatter <- scatter + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    scatter <- scatter + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    scatter <- scatter + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideGrid.x) {
    scatter <- scatter + 
      theme(panel.grid.major.x = hidegrid,
            panel.grid.minor.x = hidegrid)
  }
  if (hideGrid.y) {
    scatter <- scatter + 
      theme(panel.grid.major.y = hidegrid,
            panel.grid.minor.y = hidegrid)
  }
  # --------------------------------------
  # facet plot
  # --------------------------------------
  if (useFacetGrid){
    scatter <- scatter + facet_wrap(~ grp)
  } 
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) plot(scatter)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpscatter",
                       list(plot = scatter,
                            df = df)))
}
                       