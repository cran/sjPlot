# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("Freq", "ypos", "Question", "Response"))



#' @title Plot likert scales as centered stacked bars
#' @name sjp.likert
#' @references \url{http://strengejacke.wordpress.com/sjplot-r-package/} \cr \cr
#'             \url{http://strengejacke.wordpress.com/2013/07/17/plotting-likert-scales-net-stacked-distributions-with-ggplot-rstats/}
#' @seealso \code{\link{sjp.stackfrq}}
#' 
#' @description Plot likert scales as centered stacked bars. "Neutral" categories
#'                (odd-numbered categories) will be removed from the plot.
#' @note Transformation of data and ggplot-code taken from
#' \url{http://statisfactions.com/2012/improved-net-stacked-distribution-graphs-via-ggplot2-trickery/}
#' 
#' @param items A data frame with each column representing one likert-item.
#' @param legendLabels A list or vector of strings that indicate the likert-scale-categories and which
#'          appear as legend text.
#' @param orderBy Indicates whether the \code{items} should be ordered by total sum of positive or negative answers.
#'          Use \code{"pos"} to order descending by sum of positive answers, \code{"neg"} for sorting descending
#'          negative answers or \code{NULL} (default) for no sorting.
#' @param reverseOrder If \code{TRUE}, the item order (positive/negative) are reversed. Default is \code{FALSE}.
#' @param dropLevels Indicates specific factor levels that should be dropped from the items
#'          before the likert scale is plotted. Default is \code{NULL}, hence all factor levels
#'          are included. Exampe to drop first factor level: \code{dropLevels=c(1)}.
#' @param weightBy A weight factor that will be applied to weight all cases from \code{items}.
#' @param weightByTitleString If a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
#'          may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
#'          cases are weighted. Use a string as parameter, e.g.: \code{weightByTitleString=" (weighted)"}
#' @param hideLegend Indicates whether legend (guide) should be shown or not.
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
#' @param gridRange Sets the limit of the x-axis-range. Default is 1, so the x-scale ranges
#'          from zero to 100 percent on both sides from the center. Valid values 
#'          range from 0 (0 percent) to 1 (100 percent).
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Valid values range from 0 to 1.
#' @param diagramMargins If \code{TRUE} (default), the diagram has margins, i.e. the y-axis is not exceeded
#'          to the diagram's boundaries.
#' @param barWidth Width of bars. Recommended values for this parameter are from 0.4 to 1.5
#' @param barColor User defined color for bars.
#'          If not specified (\code{NULL}), a default red-green color palette for four(!) categories will be used 
#'          for the bar charts. You can use pre-defined color-sets that are independent from the amount of categories: \cr
#'          If barColor is \code{"brown"}, a brown-marine-palette will be used. \cr
#'          If barColor is \code{"violet"}, a violet-green palette will be used. \cr
#'          If barColor is \code{"pink"}, a pink-green palette will be used. \cr
#'          If barColor is \code{"brewer"}, use the \code{colorPalette} parameter to specify a palette of the color brewer \cr
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
#' @param axisTitleColor The color of the x and y axis labels. refers to \code{axisTitle.x} and \code{axisTitle.y},
#'          not to the tick mark or category labels.
#' @param axisTitleSize The size of the x and y axis labels. refers to \code{axisTitle.x} and \code{axisTitle.y},
#'          not to the tick mark or category labels.
#' @param showValueLabels Whether counts and percentage values should be plotted to each bar
#' @param showItemLabels Whether x axis text (category names) should be shown or not
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param showSeparatorLine If \code{TRUE}, a line is drawn to visually "separate" each bar in the diagram.
#' @param separatorLineColor The color of the separator line. Only applies, if \code{showSeparatorLine} is \code{TRUE}
#' @param separatorLineSize The size of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}
#' @param legendPos The position of the legend. Default is \code{"right"}. Use one of the following values:
#'          \code{"right"}, \code{"left"}, \code{"bottom"}, \code{"top"}.
#' @param legendSize The size of the legend.
#' @param legendBorderColor The border color of the legend.
#' @param legendBackColor The background color of the legend.
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{"bw"} for a white background with gray grids, \code{"classic"} for
#'          a classic theme (black border, no grids), \code{"minimal"} for a minimalistic theme (no border,
#'          gray grids) or \code{"none"} for no borders, grids and ticks.
#'          The ggplot-object can be returned with \code{returnPlot} set to \code{TRUE} in order to further
#'          modify the plot's theme.
#' @param flipCoordinates If \code{TRUE}, the x and y axis are swapped.
#' @param returnPlot If \code{TRUE}, the ggplot-object with the complete plot will be returned (and not plotted).
#'          Default is \code{FALSE}, hence the ggplot object will be plotted, not returned.
#' @return The ggplot-object with the complete plot in case \code{returnPlot} is \code{TRUE}.
#' 
#' @examples
#' # prepare data for dichotomous likert scale, 5 items
#' likert_2 <- data.frame(as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.3,0.7))),
#'                        as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.6,0.4))),
#'                        as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.25,0.75))),
#'                        as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.9,0.1))),
#'                        as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.35,0.65))))
#' # create labels
#' levels_2 <- list(c("Disagree", "Agree"))
#'                        
#' # prepare data for 4-category likert scale, 5 items
#' likert_4 <- data.frame(as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.2,0.3,0.1,0.4))),
#'                        as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.5,0.25,0.15,0.1))),
#'                        as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.25,0.1,0.4,0.25))),
#'                        as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.1,0.4,0.4,0.1))),
#'                        as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.35,0.25,0.15,0.25))))
#' # create labels
#' levels_4 <- list(c("Strongly disagree", "Disagree", "Agree", "Strongly Agree"))
#' 
#' # prepare data for 6-category likert scale, 5 items
#' likert_6 <- data.frame(
#'                as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.2,0.1,0.1,0.3,0.2,0.1))),
#'                as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.15,0.15,0.3,0.1,0.1,0.2))),
#'                as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.2,0.25,0.05,0.2,0.2,0.2))),
#'                as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.2,0.1,0.1,0.4,0.1,0.1))),
#'                as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.1,0.4,0.1,0.3,0.05,0.15))))
#' # create labels
#' levels_6 <- list(c("Very strongly disagree", "Strongly disagree", "Disagree",
#'                    "Agree", "Strongly Agree", "Very strongly agree"))
#' 
#' # create item labels
#' items <- list(c("Q1", "Q2", "Q3", "Q4", "Q5"))
#' 
#' # plot dichotomous likert scale, ordered by "negative" values
#' sjp.likert(likert_2, legendLabels=levels_2, axisLabels.y=items, orderBy="neg")
#' 
#' # plot 4-category-likert-scale, no order
#' sjp.likert(likert_4, legendLabels=levels_4, axisLabels.y=items)
#' 
#' # plot 4-category-likert-scale, ordered by positive values and in brown color scale
#' sjp.likert(likert_6, legendLabels=levels_6, barColor="brown",
#'            axisLabels.y=items, orderBy="pos")
#' 
#' @import ggplot2
#' @importFrom scales brewer_pal
#' @importFrom plyr ddply
#' @export
sjp.likert <- function(items,
                        legendLabels,
                        orderBy=NULL,
                        reverseOrder=FALSE,
                        dropLevels=NULL,
                        weightBy=NULL,
                        weightByTitleString=NULL,
                        hideLegend=FALSE,
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
                        gridRange=1,
                        gridBreaksAt=0.2,
                        diagramMargins=TRUE,
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
                        showItemLabels=TRUE,
                        showSeparatorLine=FALSE,
                        separatorLineColor="grey80",
                        separatorLineSize=0.3,
                        legendPos="right",
                        legendSize=1,
                        legendBorderColor="white",
                        legendBackColor="white",
                        flipCoordinates=TRUE,
                        returnPlot=FALSE) {
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
  # transform data frame content into factor
  # --------------------------------------------------------
  # check whether data in data frame are atomic, numeric etc.
  if (!is.factor(items[[1]])) {
    # iterate all data columns (items, variables)
    for (w in 1:ncol(items)) {
      # transform to factor
      items[,w] <- as.factor(items[,w])
      # check whether factor levels should be reversed
      if (reverseOrder) {
        # if yes, reverse levels. since not all variables/items may contain all possible factor
        # levels (i.e. some answer categories are missing), we determine the amount of factor
        # levels by the length of legend labels. The legend labels indicate the correct amount
        # of categories from the likert scale ("agree" to "disagree")
        items[,w] = factor(items[,w],levels(items[,w])[c(seq(from=length(legendLabels), to=1))])
      }
      # rename factor levels so they are quasi-numeric
      levels(items[,w]) <- c(paste(seq(1:length(legendLabels))))
    }
  }

  
  # --------------------------------------------------------
  # reverse legend labels, if factor levels should be reversed
  # --------------------------------------------------------
  if (!is.null(legendLabels) && reverseOrder) {
    legendLabels <- rev(legendLabels)
  }
  
  
  # --------------------------------------------------------
  # Drop factor levels, if requested
  # --------------------------------------------------------
  # check whether certain factor levels should be dropped
  if (!is.null(dropLevels)) {
    # iterate all data columns (items, variables)
    for (w in 1:ncol(items)) {
      # iterate all levels that should be dropped
      for (v in 1:length(dropLevels)) {
        # remove factor level
        items[,w][items[,w]==dropLevels[v]] <- NA
        items[,w] <- factor(items[,w])
      }
    }
    # now we have to remove the related legendLabels as well
    # therefor, order droplevels decreasing
    dropLevels <- dropLevels[order(dropLevels, decreasing=TRUE)]
    for (u in length(dropLevels):1) {
      legendLabels <- legendLabels[legendLabels!=legendLabels[dropLevels[u]]]
    }
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
  
  
  # ---------------------------------------------------------------------------------------------
  # The following part which does the transformation of factor levels into negative and positive
  # answers was taken from
  # http://statisfactions.com/2012/improved-net-stacked-distribution-graphs-via-ggplot2-trickery/
  #
  # slightly modifications were made by including a weight-factor and calculating the cumulative
  # sums of percentages for the value label positioninh
  # ---------------------------------------------------------------------------------------------
  # retrieve levels of items
  # --------------------------------------------------------
  all_levels <- levels(items[[1]])
  n <- length(all_levels)
  # --------------------------------------------------------
  # Reverse order of columns (to make ggplot2 output look right after coord_flip)
  # --------------------------------------------------------
  items <- items[length(items):1]
  # --------------------------------------------------------
  # Identify middle and "negative" levels
  # --------------------------------------------------------
  if(n %% 2 == 1) {
    neutral <- all_levels[ceiling(n/2)]
  }
  else {
    neutral <- NULL
  }
  # --------------------------------------------------------
  # split factor levels according to "agree" and "disagree"
  # --------------------------------------------------------
  negatives <- all_levels[1:floor(n/2)]
  positives <- setdiff(all_levels, c(negatives, neutral))

  
  # --------------------------------------------------------
  # remove neutral, summarize as proportion
  # --------------------------------------------------------
  listall <- lapply(names(items), function(y) {
    # column <- (na.omit(items[[y]]))
    column <- items[[y]]
    if (is.null(weightBy)) {
      out <- data.frame(Question = y, prop.table(table(column)))
    }
    else {
      out <- data.frame(Question = y, prop.table(round(xtabs(weightBy ~ column),0)))
    }
    names(out) <- c("Question", "Response", "Freq")
    
    if(!is.null(neutral)) {
      out <- out[out$Response != neutral,]
    }
    out
  })
  dfall <- do.call(rbind, listall)

  
  # --------------------------------------------------------
  # split by positive/negative, and check whether factor
  # levels should be reversed
  # --------------------------------------------------------
  pos <- dfall[dfall$Response %in% positives,]
  neg <- dfall[dfall$Response %in% negatives,]

  
  # --------------------------------------------------------
  # add half of Percentage values as new y-position for stacked bars
  # --------------------------------------------------------
  pos = ddply(pos, "Question", transform, ypos = cumsum(Freq) - 0.5*Freq)
  neg = ddply(neg, "Question", transform, ypos = rev(cumsum(rev(Freq)) - 0.5*rev(Freq)))

  
  # --------------------------------------------------------
  # Negate the frequencies of negative responses, reverse order
  # --------------------------------------------------------
  neg$Freq <- -neg$Freq
  neg$ypos <- -neg$ypos
  neg$Response <- ordered(neg$Response, levels = rev(levels(neg$Response)))
  # save numbers of items we have. needed later for calculating the
  # sorting of items
  questionCount <- nrow(pos)/(length(legendLabels)/2)

  # --------------------------------------------------------
  # order items according to sum of positive or negative answers
  # given.
  # --------------------------------------------------------
  if (!is.null(orderBy)) {
    if (orderBy=="pos") {
      pos <- ddply(pos, "Question", transform, ytotal = sum(Freq))
      orderGroupedItems <- order(pos$ytotal)
    }
    else {
      neg <- ddply(neg, "Question", transform, ytotal = sum(abs(Freq)))
      orderGroupedItems <- order(neg$ytotal)
    }
    # ------------
    # in "orderGroupedItems", we have now a "grouped" order. each "group" consists of data 
    # with equal count to the positive or negative amount of legend labels (i.e.
    # half of the amount of all legendLabels).
    # The amount of groups, however, is related to the amount of items we have.
    # Example: We have 5 items with 6 categories (legendLabels) each,
    # for instance "very strong disagree", "strong disagree", "disagree", "agree",
    # "strong agree", "very strong agree".
    # Now "orderGroupedItems" consists of five groups (= 5 items) and each "group" has three
    # data rows (6 categories divided by 2 (pos and neg)).
    # So we have 15 data rows.
    #
    # Example: "pos" after applying ddply-function:
    #     Question Response  Freq  ypos ytotal
    #     1        Q5        4 0.266 0.133  0.442
    #     2        Q5        5 0.050 0.291  0.442
    #     3        Q5        6 0.126 0.379  0.442
    #     4        Q4        4 0.406 0.203  0.588
    #     5        Q4        5 0.084 0.448  0.588
    #     6        Q4        6 0.098 0.539  0.588
    #     7        Q3        4 0.200 0.100  0.570
    #     8        Q3        5 0.196 0.298  0.570
    #     9        Q3        6 0.174 0.483  0.570
    #     10       Q2        4 0.094 0.047  0.376
    #     11       Q2        5 0.078 0.133  0.376
    #     12       Q2        6 0.204 0.274  0.376
    #     13       Q1        4 0.294 0.147  0.596
    #     14       Q1        5 0.216 0.402  0.596
    #     15       Q1        6 0.086 0.553  0.596
    #
    # Now "orderGroupedItems" looks like following:
    # [1] 10 11 12  1  2  3  7  8  9  4  5  6 13 14 15
    # 
    # So we have the order from lowest sum of positive or negative
    # answer frequencies to highest, but three times each. for ordering
    # the legend labels, we have to transform "orderGroupedItems", see below!
    # ------------
    pos$Freq <- pos$Freq[orderGroupedItems]
    neg$Freq <- neg$Freq[orderGroupedItems]
    pos$ypos <- pos$ypos[orderGroupedItems]
    neg$ypos <- neg$ypos[orderGroupedItems]
    
    # since "orderGroupedItems" has numbers from 1 to (items * legendLabels/2) - i.e. 1 to 15
    # in this example -, we need to know, which "group" belongs to which item. we do
    # this by dividing these numbers by "amount of positive / negative legendLabels",
    # i.e. "orderGroupedItems" will be divided by (length of legendLabels / 2).
    orderRelatedItems <- c(ceiling(orderGroupedItems/(length(legendLabels)/2)))
    
    # now we have the in "orderUniqueItems" the items assigned to each row of the data frame
    # pos resp. neg:
    # [1] 4 4 4 1 1 1 3 3 3 2 2 2 5 5 5
    # Next, we just need each item number once, so extract the unique values
    orderUniqueItems <- c(unique(orderRelatedItems))
    
    # now we have in "oderUniqueNumbers" the items with the lowest frequencies
    # to highest frequencies, with each number pointing the question position, beginng
    # from the end.
    # Example, how "oderUniqueNumbers" looks like:
    # [1] 4 1 3 2 5
    # That means, when we have 5 questions / items, the 4th question/item, counted 
    # from the end, is question/item 2.
    # Thus, question/item 2 has the lowest total frequencies (first position in
    # "oderUniqueNumbers", last position in order).
    # The second number in "oderUniqueNumbers" is "1", i.e. the first question from the
    # end is question 5, which appears at position 2 with the lowest total frequencies.
    # 
    # So we now have to switch index from (end to beginning) to (beginning to end)
    # and reverse the order to start with highest frequencies.
    orderUniqueItems <- rev(1+questionCount-orderUniqueItems)
    # If axisLabels.y were not defined, simply set numbers from 1 to
    # amount of items
    if (is.null(axisLabels.y)) {
      axisLabels.y <- c(1:length(items))
    }
    # The result in "orderUniqueItems" now is
    # [1] 1 4 3 5 2
    # with this we can order the axis labels (item/question labels)
    axisLabels.y <- axisLabels.y[orderUniqueItems]
  }

  
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # wrap legend text lines
  pattern <- c(paste('(.{1,', breakLegendLabelsAt, '})(\\s|$)', sep=""))
  for (n in 1:length(legendLabels)) {
    legendLabels[n] <- gsub(pattern, '\\1\n', legendLabels[n])
  }
  # check whether we have a title for the legend
  if (!is.null(legendTitle)) {
    # if yes, wrap legend title line
    pattern <- c(paste('(.{1,', breakLegendTitleAt, '})(\\s|$)', sep=""))
    legendTitle <- gsub(pattern, '\\1\n', legendTitle)
  }
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) {
      title <- paste(title, weightByTitleString, sep="")
    }
    pattern <- c(paste('(.{1,', breakTitleAt, '})(\\s|$)', sep=""))
    title <- gsub(pattern, '\\1\n', title)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) {
    pattern <- c(paste('(.{1,', breakLabelsAt, '})(\\s|$)', sep=""))
    for (n in 1:length(axisLabels.y)) {
      axisLabels.y[n] <- gsub(pattern, '\\1\n', axisLabels.y[n])
    }
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
  else {
    axisLabels.y <- rev(axisLabels.y)
  }
  # --------------------------------------------------------
  # Prepare fill colors
  # --------------------------------------------------------
  ln <- length(negatives)
  if (is.null(barColor)) {
    if (length(legendLabels)==2) {
      cols <- c('#AA1111', '#11AA11')
    }
    else if (length(legendLabels)==4) {
      cols <- c('#AA1111', '#BB6666', '#66BB66', '#11AA11')
    }
    else {
      cols <- c('#AA1111', '#BB6666', '#CC9999', '#99CC99', '#66BB66', '#11AA11')
    }
    scalecolors <- scale_fill_manual(labels=legendLabels, values=cols)
  }
  else if (barColor=="violet") {
    cp <- brewer_pal(palette="PRGn")(2*ln+1)
    cols <- cp[c(1:ln,(ln+2):((2*ln)+1))]
    scalecolors <- scale_fill_manual(labels=legendLabels, values=cols)
  }
  else if (barColor=="brown") {
    cp <- brewer_pal(palette="BrBG")(2*ln+1)
    cols <- cp[c(1:ln,(ln+2):((2*ln)+1))]
    scalecolors <- scale_fill_manual(labels=legendLabels, values=cols)
  }
  else if (barColor=="pink") {
    cp <- brewer_pal(palette="PiYG")(2*ln+1)
    cols <- cp[c(1:ln,(ln+2):((2*ln)+1))]
    scalecolors <- scale_fill_manual(labels=legendLabels, values=cols)
  }
  else if (barColor=="brewer") {
    # remember to specify the "colorPalette" if you use "brewer" as "barColor"
    scalecolors <- scale_fill_brewer(palette=colorPalette, labels=legendLabels)
  }
  else {
    scalecolors <- scale_fill_manual(values=barColor, labels=legendLabels)
  }
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  if (showValueLabels) {
    ggvaluelabels_lo <-  geom_text(data=neg, aes(y=ypos, label=sprintf("%.01f%%", -100*Freq)),
                                   size=valueLabelSize,
                                   vjust=vert,
                                   # hjust=hort,
                                   colour=valueLabelColor)
    ggvaluelabels_hi <-  geom_text(data=pos, aes(y=ypos, label=sprintf("%.01f%%", 100*Freq)),
                                   size=valueLabelSize,
                                   vjust=vert,
                                   # hjust=hort,
                                   colour=valueLabelColor)
  }
  else {
    ggvaluelabels_lo <-  geom_text(label="")
    ggvaluelabels_hi <-  geom_text(label="")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  gridbreaks <- c(seq(-gridRange, gridRange, by=gridBreaksAt))
  gridlabs <- paste0(c(abs(round(100*gridbreaks))),"%")
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
  # start plot
  # --------------------------------------------------------
  baseplot <- ggplot() +
    aes(Question, Freq, fill = Response, order = Response) + 
    geom_bar(data = neg, stat = "identity", colour=outlineColor, width=barWidth, alpha=barAlpha) +
    geom_bar(data = pos, stat = "identity", colour=outlineColor, width=barWidth, alpha=barAlpha) +
    geom_hline(yintercept=0, colour="white")
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
    ggvaluelabels_hi +
    ggvaluelabels_lo +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title=title, x=axisTitle.x, y=axisTitle.y, fill=legendTitle) +
    # print value labels to the x-axis.
    # If parameter "axisLabels.y" is NULL, the category numbers (1 to ...) 
    # appear on the x-axis
    scale_x_discrete(labels=axisLabels.y) +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    scale_y_continuous(breaks=gridbreaks, limits=c(-gridRange, gridRange), expand=expgrid, labels=gridlabs) +
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
          axis.text.x = element_text(angle=axisLabelAngle.x))
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
      print("Parameter 'borderColor' can only be applied to 'bw' theme.")
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
  if (returnPlot) {
    return(baseplot)
  }
  else {
    plot(baseplot)
  }
}
