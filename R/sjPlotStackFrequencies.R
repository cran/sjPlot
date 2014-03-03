#' @title Plot stacked proportional bars
#' @name sjp.stackfrq
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2013/07/17/plotting-likert-scales-net-stacked-distributions-with-ggplot-rstats/}
#'              }
#'             
#' @seealso \code{\link{sjp.likert}} \cr
#'          \code{\link{sjt.stackfrq}}
#' 
#' @description Plot items (variables) of a scale as stacked proportional bars. This
#'                function is useful when several items with identical scale/categoroies
#'                should be plotted to compare the distribution of answers.
#' 
#' @note Thanks to Forrest Stevens (\url{http://www.clas.ufl.edu/users/forrest/}) for bug fixes
#' 
#' @param items A data frame with each column representing one likert-item.
#' @param legendLabels A list or vector of strings that indicate the likert-scale-categories and which
#'          appear as legend text.
#' @param orderBy Indicates whether the \code{items} should be ordered by highest count of first or last category of \code{items}.
#'          Use \code{"first"} to order ascending by lowest count of first category, 
#'          \code{"last"} to order ascending by lowest count of last category
#'          or \code{NULL} (default) for no sorting.
#'          In case you want to revers order (descending from highest count), use
#'          \code{reverseOrder} parameter.
#' @param weightBy A weight factor that will be applied to weight all cases from \code{items}.
#' @param weightByTitleString If a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
#'          may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
#'          cases are weighted. Use a string as parameter, e.g.: \code{weightByTitleString=" (weighted)"}.
#' @param hideLegend Indicates whether legend (guide) should be shown or not.
#' @param reverseOrder If \code{TRUE}, the item order on the x-axis is reversed.
#' @param title Title of the diagram, plotted above the whole diagram panel.
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param legendTitle Title of the diagram's legend.
#' @param includeN If \code{TRUE} (default), the N of each item is included into axis labels.
#' @param axisLabels.y Labels for the y-axis (the labels of the \code{items}). These parameters must
#'          be passed as list! Example: \code{axisLabels.y=list(c("Q1", "Q2", "Q3"))}
#' @param axisLabelSize The size of category labels at the axes. Default is 1.1, recommended values range
#'          between 0.5 and 3.0
#' @param axisLabelAngle.x Angle for axis-labels.
#' @param axisLabelColor User defined color for axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels.
#' @param valueLabelSize The size of value labels in the diagram. Default is 4, recommended values range
#'          between 2 and 8
#' @param valueLabelColor The color of value labels in the diagram. Default is black.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title.
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted.
#' @param breakLegendTitleAt Wordwrap for diagram legend title. Determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted.
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Valid values range from 0 to 1.
#' @param diagramMargins If \code{TRUE} (default), the diagram has margins, i.e. the y-axis is not exceeded
#'          to the diagram's boundaries.
#' @param barWidth Width of bars. Recommended values for this parameter are from 0.4 to 1.5
#' @param barColor User defined color for bars.
#'          If not specified (\code{NULL}), a default blue color palette will be used 
#'          for the bar charts. You can use pre-defined color-sets that are independent from the amount of categories:
#'          If barColor is \code{"brewer"}, use the \code{colorPalette} parameter to specify a palette of the color brewer
#'          Else specify your own color values as vector (e.g. \code{barColor=c("darkred", "red", "green", "darkgreen")})
#' @param colorPalette If \code{barColor} is \code{"brewer"}, specify a color palette from the color brewer here. All color brewer 
#'          palettes supported by ggplot are accepted here.
#' @param barAlpha Specify the transparancy (alpha value) of bars.
#' @param borderColor User defined color of whole diagram border (panel border).
#' @param barOutline If \code{TRUE}, each bar gets a colored outline. Default is \code{FALSE}.
#' @param outlineColor The color of the bar outline. Only applies, if \code{barOutline} is set to \code{TRUE}.
#' @param majorGridColor Specifies the color of the major grid lines of the diagram background.
#' @param minorGridColor Specifies the color of the minor grid lines of the diagram background.
#' @param hideGrid.x If \code{TRUE}, the x-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param hideGrid.y If \code{TRUE}, the y-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param axisColor User defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border).
#' @param axisTitle.x A label for the x axis. Useful when plotting histograms with metric scales where no category labels
#'          are assigned to the x axis.
#' @param axisTitle.y A label for the y axis. Useful when plotting histograms with metric scales where no category labels
#'          are assigned to the y axis.
#' @param axisTitleColor The color of the x and y axis labels. Refers to \code{axisTitle.x} and \code{axisTitle.y},
#'          not to the tick mark or category labels.
#' @param axisTitleSize The size of the x and y axis labels. Refers to \code{axisTitle.x} and \code{axisTitle.y},
#'          not to the tick mark or category labels.
#' @param showValueLabels Whether counts and percentage values should be plotted to each bar.
#' @param jitterValueLabels If \code{TRUE}, the value labels on the bars will be "jittered", i.e. they have
#'          alternating vertical positions to avoid overlapping of labels in case bars are
#'          very short. Default is \code{FALSE}.
#' @param showItemLabels Whether x axis text (category names) should be shown or not.
#' @param showTickMarks Whether tick marks of axes should be shown or not.
#' @param showSeparatorLine If \code{TRUE}, a line is drawn to visually "separate" each bar in the diagram.
#' @param separatorLineColor The color of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}.
#' @param separatorLineSize The size of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}.
#' @param legendPos The position of the legend. Default is \code{"right"}. Use one of the following values:
#'          \code{"right"}, \code{"left"}, \code{"bottom"}, \code{"top"}.
#' @param legendSize The size of the legend.
#' @param legendBorderColor The border color of the legend.
#' @param legendBackColor The background color of the legend.
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids.
#'          \itemize{
#'          \item Use \code{"bw"} for a white background with gray grids
#'          \item \code{"classic"} for a classic theme (black border, no grids)
#'          \item \code{"minimal"} for a minimalistic theme (no border,gray grids) or 
#'          \item \code{"none"} for no borders, grids and ticks.
#'          }
#' @param flipCoordinates If \code{TRUE}, the x and y axis are swapped.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @examples
#' # -------------------------------
#' # random sample
#' # -------------------------------
#' # prepare data for 4-category likert scale, 5 items
#' likert_4 <- data.frame(as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.2,0.3,0.1,0.4))),
#'                        as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.5,0.25,0.15,0.1))),
#'                        as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.25,0.1,0.4,0.25))),
#'                        as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.1,0.4,0.4,0.1))),
#'                        as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.35,0.25,0.15,0.25))))
#' # create labels
#' levels_4 <- list(c("Independent", "Slightly dependent", "Dependent", "Severely dependent"))
#' 
#' # create item labels
#' items <- list(c("Q1", "Q2", "Q3", "Q4", "Q5"))
#' 
#' # plot stacked frequencies of 5 (ordered) item-scales
#' sjp.stackfrq(likert_4, legendLabels=levels_4, axisLabels.y=items)
#' 
#' 
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' data(efc)
#' 
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc)=="c82cop1")
#' 
#' # recveive first item of COPE-index scale
#' end <- which(colnames(efc)=="c90cop9")
#' 
#' # retrieve variable and value labels
#' varlabs <- sji.getVariableLabels(efc)
#' vallabs <- sji.getValueLabels(efc)
#' 
#' # create value labels. We need just one variable of
#' # the COPE-index scale because they have all the same
#' # level / categorie / value labels
#' levels <- vallabs['c82cop1']
#' 
#' # create item labels
#' items <- list(varlabs[c(start:end)])
#' 
#' sjp.stackfrq(efc[,c(start:end)], legendLabels=levels,
#'              axisLabels.y=items, jitterValueLabels=TRUE)
#' 
#' @import ggplot2
#' @importFrom plyr ddply
#' @importFrom scales percent
#' @export
sjp.stackfrq <- function(items,
                        legendLabels,
                        orderBy=NULL,
                        weightBy=NULL,
                        weightByTitleString=NULL,
                        hideLegend=FALSE,
                        reverseOrder=TRUE,
                        title=NULL,
                        titleSize=1.3,
                        titleColor="black",
                        legendTitle=NULL,
                        includeN=TRUE,
                        axisLabels.y=NULL,
                        axisLabelSize=1.1,
                        axisLabelAngle.x=0, 
                        axisLabelColor="gray30", 
                        valueLabelSize=4,
                        valueLabelColor="black",
                        breakTitleAt=50, 
                        breakLabelsAt=30, 
                        breakLegendTitleAt=30, 
                        breakLegendLabelsAt=28,
                        gridBreaksAt=0.2,
                        diagramMargins=FALSE,
                        barWidth=0.5, 
                        barColor=NULL,
                        colorPalette="GnBu",
                        barAlpha=1,
                        borderColor=NULL, 
                        axisColor=NULL, 
                        barOutline=FALSE, 
                        outlineColor="black", 
                        majorGridColor=NULL,
                        minorGridColor=NULL,
                        hideGrid.x=FALSE,
                        hideGrid.y=FALSE,
                        axisTitle.x=NULL,
                        axisTitle.y=NULL,
                        axisTitleColor="black",
                        axisTitleSize=1.3,
                        theme=NULL,
                        showTickMarks=FALSE,
                        showValueLabels=TRUE,
                        jitterValueLabels=FALSE,
                        showItemLabels=TRUE,
                        showSeparatorLine=FALSE,
                        separatorLineColor="grey80",
                        separatorLineSize=0.3,
                        legendPos="right",
                        legendSize=1,
                        legendBorderColor="white",
                        legendBackColor="white",
                        flipCoordinates=TRUE,
                        printPlot=TRUE) {
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
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) {
    axisLabels.y <- unlistlabels(axisLabels.y)
  }
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }

  
  # --------------------------------------------------------
  # Check whether N of each item should be included into
  # axis labels
  # --------------------------------------------------------
  if (includeN && !is.null(axisLabels.y)) {
    for (i in 1:length(axisLabels.y)) {
      axisLabels.y[i] <- paste(axisLabels.y[i], sprintf(" (n=%i)", length(na.omit(items[,i]))), sep="")
    }
  }

  
  # -----------------------------------------------
  # if we have legend labels, we know the exact
  # amount of groups
  # -----------------------------------------------
  countlen <- length(legendLabels)
  
  
  # -----------------------------------------------
  # create cross table for stats, summary etc.
  # and weight variable. do this for each item that was
  # passed as parameter
  #---------------------------------------------------
  mydat <- c()
  # iterate item-list
  for (i in 1:ncol(items)) {
    # get each single items
    variable <- items[,i]
    # -----------------------------------------------
    # create proportional table so we have the percentage
    # values that should be used as y-value for the bar charts
    # We now have a data frame with categories, group-association
    # and percentage values (i.e. each cell as separate row in the
    # data frame)
    # -----------------------------------------------
    # check whether counts should be weighted or not
    if (is.null(weightBy)) {
      df <- as.data.frame(prop.table(table(variable)))
    }
    else {
      df <- as.data.frame(prop.table(round(xtabs(weightBy ~ variable),0)))
    }
    # give columns names
    names(df) <- c("var", "prc")

		##	FRS:  I can't suss out why we convert to numeric but rather than
		##		this I'm just going to number them:
    ## convert to numeric
    #df$var <- as.numeric(as.character(df$var))
    ## if categories start with zero, fix this here
    #if (min(df$var)==0) {
    #  df$var <- df$var+1
    #}
		df$var <- 1:length(df$var)

    # Create a vector of zeros 
    prc <- rep(0,countlen)
    # Replace the values in prc for those indices which equal df$var
    prc[df$var] <- df$prc
    # create new data frame. We now have a data frame with all
    # variable categories abd their related percentages, including
    # zero counts, but no(!) missings!
    mydf <- as.data.frame(cbind(grp=i, cat=1:countlen, prc))
    # now, append data frames
    mydat <- as.data.frame(rbind(mydat, mydf))
  }
  # ----------------------------
  # make sure group and count variable 
  # are factor values
  # ----------------------------
  mydat$grp <- as.factor(mydat$grp)
  mydat$cat <- as.factor(mydat$cat)
  # add half of Percentage values as new y-position for stacked bars
  mydat = ddply(mydat, "grp", transform, ypos = cumsum(prc) - 0.5*prc)
  # --------------------------------------------------------
  # Caculate vertical adjustment to avoid overlapping labels
  # --------------------------------------------------------
  jvert <- rep(c(1.1,-0.1), length.out=length(unique(mydat$cat)))
  jvert <- rep(jvert, length.out=nrow(mydat))
  
  
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
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
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) {
      title <- paste(title, weightByTitleString, sep="")
    }
    title <- sju.wordwrap(title, breakTitleAt)    
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- sju.wordwrap(axisLabels.y, breakLabelsAt)    
  }
  # If axisLabels.y were not defined, simply set numbers from 1 to
  # amount of items
  else {
    axisLabels.y <- c(1:length(items))
  }
  
  
  # ----------------------------
  # Check if ordering was requested
  # ----------------------------
  if (!is.null(orderBy)) {
    # order by first cat
    if (orderBy=="first") {
      facord <- order(mydat$prc[which(mydat$cat==1)])
    }
    # order by last cat
    else {
      facord <- order(mydat$prc[which(mydat$cat==countlen)])
    }
    # create dummy vectors from 1 to itemlength
    dummy1 <- dummy2 <- c(1:length(facord))
    # facords holds the ordered item indices! we now need to
    # change the original item-index with its ordered position index.
    # example:
    # we have 4 items, and they may be ordered like this:
    # 1 3 4 2
    # so the first item is the one with the lowest count , item 3 is on second postion, 
    # item 4 is on third position and item 2 is the last item (with highest count)
    # we now need their order as subsequent vector: 1 4 2 3
    # (i.e. item 1 is on first pos, item 2 is on fourth pos, item 3 is on
    # second pos and item 4 is on third pos in order)
    if (reverseOrder) {
      dummy2[rev(facord)] <- dummy1
    }
    else {
      dummy2[facord] <- dummy1
    }
    # now we have the order of either lowest to highest counts of first
    # or last category of "items". We now need to repeat these values as 
    # often as we have answer categories
    orderedrow <- unlist(tapply(dummy2, 1:length(dummy2), function (x) rep(x,countlen)))
    # replace old grp-order by new order
    mydat$grp <- as.factor(orderedrow)
    # reorder axis labels as well
    axisLabels.y <- axisLabels.y[order(dummy2)]
  }
  
  
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change category label order then
  # --------------------------------------------------------
  if (reverseOrder && is.null(orderBy)) {
    axisLabels.y <- rev(axisLabels.y)
  }

  
  # --------------------------------------------------------
  # define vertical position for labels
  # --------------------------------------------------------
  if (flipCoordinates) {
    # if we flip coordinates, we have to use other parameters
    # than for the default layout
    vert <- 0.35
  }
  else {
    vert <- waiver()
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
  # set diagram margins
  # --------------------------------------------------------
  if (diagramMargins) {
    expgrid <- waiver()
  }
  else {
    expgrid <- c(0,0)
  }
  # --------------------------------------------------------
  # Hide or show Tick Marks and Category Labels (x axis text) 
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showItemLabels) {
    axisLabels.y <- c("")
  }


  # --------------------------------------------------------
  # Prepare fill colors
  # --------------------------------------------------------
  if (is.null(barColor)) {
    scalecolors <- scale_fill_brewer(labels=legendLabels, palette="PuBu")
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
  # Set value labels
  # --------------------------------------------------------
  if (showValueLabels) {
    if (jitterValueLabels) {
      ggvaluelabels <-  geom_text(aes(y=ypos, label=sprintf("%.01f%%", 100*prc)),
                                  size=valueLabelSize,
                                  vjust=jvert,
                                  # hjust=hort,
                                  colour=valueLabelColor)
    }
    else {
      ggvaluelabels <-  geom_text(aes(y=ypos, label=sprintf("%.01f%%", 100*prc)),
                                  size=valueLabelSize,
                                  vjust=vert,
                                  # hjust=hort,
                                  colour=valueLabelColor)
    }
  }
  else {
    ggvaluelabels <-  geom_text(label="")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(gridBreaksAt)) {
    gridbreaks <- waiver()
  }
  else {
    gridbreaks <- c(seq(0, 1, by=gridBreaksAt))
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
  # check if category-oder on x-axis should be reversed
  # change x axis order then
  # --------------------------------------------------------
  if (reverseOrder && is.null(orderBy)) {
    baseplot <- ggplot(mydat, aes(x=rev(grp), y=prc, fill=cat))
  }
  else {
    baseplot <- ggplot(mydat, aes(x=grp, y=prc, fill=cat))
  }  
  baseplot <- baseplot +
    # plot bar chart
    geom_bar(stat="identity", position="stack", colour=outlineColor, width=barWidth, alpha=barAlpha)
  # --------------------------------------------------------
  # check whether bars should be visually separated by an 
  # additional separator line
  # --------------------------------------------------------
  if (showSeparatorLine) {
    baseplot <- baseplot +
      geom_vline(x=c(seq(1.5, length(items), by=1)), size=separatorLineSize, colour=separatorLineColor)
  }
  # --------------------------------------------------------
  # Hide or show Legend
  # --------------------------------------------------------
  if (hideLegend) {
    # remove guide / legend
    baseplot <- baseplot + guides(fill=FALSE)
  }
  baseplot <- baseplot +
    # show absolute and percentage value of each bar.
    ggvaluelabels +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title=title, x=axisTitle.x, y=axisTitle.y, fill=legendTitle) +
    # print value labels to the x-axis.
    # If parameter "axisLabels.y" is NULL, the category numbers (1 to ...) 
    # appear on the x-axis
    scale_x_discrete(labels=axisLabels.y) +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    scale_y_continuous(breaks=gridbreaks, limits=c(0, 1), expand=expgrid, labels=percent) +
    scalecolors  +
    ggtheme
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (flipCoordinates) {
    baseplot <- baseplot + coord_flip()
  }
  # set font size for axes.
  baseplot <- baseplot + 
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
          axis.text.x = element_text(angle=axisLabelAngle.x),
          plot.title = element_text(size=rel(titleSize), colour=titleColor))
  # --------------------------------------
  # set position and size of legend
  # --------------------------------------
  if (!hideLegend) {
    baseplot <- baseplot + 
      theme(legend.position = legendPos,
            legend.text = element_text(size=rel(legendSize)),
            legend.background = element_rect(colour=legendBorderColor, fill=legendBackColor))
  }
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      baseplot <- baseplot + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      cat("\nParameter 'borderColor' can only be applied to 'bw' theme.\n")
    }
  }
  if (!is.null(axisColor)) {
    baseplot <- baseplot + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    baseplot <- baseplot + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    baseplot <- baseplot + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideGrid.x) {
    baseplot <- baseplot + 
      theme(panel.grid.major.x = hidegrid,
            panel.grid.minor.x = hidegrid)
  }
  if (hideGrid.y) {
    baseplot <- baseplot + 
      theme(panel.grid.major.y = hidegrid,
            panel.grid.minor.y = hidegrid)
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) plot(baseplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpstackfrq",
                       list(plot = baseplot,
                            df = mydat)))
}
