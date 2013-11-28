#' @title Plot interaction terms of linear models
#' @name sjp.lm.int
#' @references \url{http://strengejacke.wordpress.com/sjplot-r-package/} \cr \cr
#'             \url{http://strengejacke.wordpress.com/2013/10/31/visual-interpretation-of-interaction-terms-in-linear-models-with-ggplot-rstats/}
#' 
#' @description Plot regression curves of significant interaction terms in linear models (lm).
#' @seealso \code{\link{sjp.lm}} \cr
#'          \code{\link{sjp.reglin}} \cr
#'          \code{\link{sjp.lm.ma}}
#' 
#' @param fit the fitted linear model (lm) object, including interaction terms
#' @param smooth smoothes the regression line in case it is not linear. Default is \code{none}, so no smoothing
#'          is applied. Use \code{loess} for loess-smoothing or \code{lm} to force linear regression lines.
#' @param diff if \code{FALSE} (default), the minimum and maximum interaction effects of predictor 2 on predictor 1
#'          are shown (one line each). if \code{TRUE}, only the difference between minimum and maximum interaction effect
#'          is shown (single line)
#' @param swapPredictors if \code{TRUE}, the predictor with less unique values is printed along the x-axis. Default is
#'          \code{FALSE}, so the predictor with more unique values is printed along the x-axis.
#' @param title a default title used for the plots. Default value is \code{NULL}, which means that each plot's title
#'          includes the dependent variable as well as the names of the interaction terms.
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param fillColor fill color of the shaded area between the minimum and maximum lines. Default is \code{grey}.
#'          Either set \code{fillColor} to \code{NULL} or use 0 for \code{fillAlpha} if you want to hide the shaded area.
#' @param fillAlpha alpha value (transparancy) of the shaded area between the minimum and maximum lines. Default is 0.4.
#'          Use either 0 or set \code{fillColor} to \code{NULL} if you want to hide the shaded area.
#' @param lowerBoundColor the color of the line indicating the lower bound of the interaction term.
#'          Default value is \code{"#3366cc"} (blue-like)
#' @param upperBoundColor the color of the line indicating the upper bound of the interaction term.
#'          Default value is \code{"#cc3300"} (red-like)
#' @param lineColor the color of the line indicating the upper difference between lower and upper
#'          bound of interaction terms. Only applies if \code{diff} is \code{TRUE}. 
#'          Default value is \code{"#33cc66"} (green-like)
#' @param axisTitle.x a default title used for the x-axis. Default value is \code{NULL}, 
#'          which means that each plot's x-axis uses the predictor's name as title.
#' @param axisTitle.y a default title used for the y-axis. Default value is \code{NULL}, 
#'          which means that each plot's y-axis uses the dependent variable's name as title.
#' @param axisLabelColor the color value for the axis labels at the tick marks. Default value
#'          is \code{"darkgray"}.
#' @param axisLabelSize The size of axis labels. Default is 1.1, recommended values range
#'          between 0.5 and 3.0
#' @param axisTitleColor the color value for the axis titles (both x and y). Default value
#'          is \code{"black"}.
#' @param axisTitleSize The size of axis titles (both x and y). Default is 1.3, recommended values range
#'          between 0.5 and 3.0
#' @param legendLabels Labels for the guide/legend. Default is \code{NULL}, so the name of the predictor with 
#'          min/max-effect is used as legend label.
#' @param legendLabelSize The size of legend labels. Default is 0.9, recommended values range
#'          between 0.5 and 3.0
#' @param legendLabelColor user defined color for legend labels. If not specified, black will be used for the labels
#' @param showValueLabels if \code{TRUE}, value labels are plotted along the lines. Default is \code{FALSE}.
#' @param valueLabelSize size of the value labels. Default is 4. Recommended Values range from
#'          2 to 8
#' @param valueLabelColor colour of the values inside the diagrams. Only applies, when parameter
#'          \code{showValueLabels} is set to \code{TRUE}. Use any valid colour value, e.g. \code{valueLabelColor="grey50"} or
#'          \code{valueLabelColor=c("#cc3366")}. Default is \code{"black"}.
#' @param valueLabelAlpha the alpha level (transparancy) of the value labels. Default is 0.8, use
#'          any value from 0 to 1.
#' @param breakTitleAt Wordwrap for diagram's title. Determines how many chars of the title are 
#'          displayed in one line and when a line break is inserted. Default is \code{50}.
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted. Default is \code{20}.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is \code{NULL}.
#' @param theme specifies the diagram's background theme. default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{bw} for a white background with gray grids, \code{classic} for
#'          a classic theme (black border, no grids), \code{minimal} for a minimalistic theme (no border,
#'          gray grids) or \code{none} for no borders, grids and ticks.
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#' @param majorGridColor specifies the color of the major grid lines of the diagram background
#' @param minorGridColor specifies the color of the minor grid lines of the diagram background
#' @param hideGrid.x If \code{TRUE}, the x-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param hideGrid.y If \code{TRUE}, the y-axis-gridlines are hidden. Default if \code{FALSE}.
#' 
#' @examples
#' # Note that the data sets used in this example may not be perfectly suitable for
#' # fitting linear models. I just used them because they are part of the R-software.
#' 
#' # fit "dummy" model.
#' fit <- lm(weight ~ Time * Diet, data=ChickWeight, x=TRUE)
#' 
#' # show summary to see significant interactions
#' summary(fit)
#' 
#' # plot regression line of interaction terms
#' sjp.lm.int(fit)
#' # plot regression line of interaction terms, including value labels
#' sjp.lm.int(fit, showValueLabels=TRUE)
#' 
#' 
#' # fit "dummy" model
#' fit <- lm(Fertility ~ .*., data=swiss, na.action=na.omit, x=TRUE)
#' 
#' # show summary to see significant interactions
#' summary(fit)
#' 
#' # plot regression line of interaction terms
#' sjp.lm.int(fit)
#' # plot smoothes regression line of interaction terms
#' sjp.lm.int(fit, smooth="loess")
#' # plot linear regression line of interaction terms
#' sjp.lm.int(fit, smooth="lm")
#' 
#' @import ggplot2
#' @export
sjp.lm.int <- function(fit,
                      smooth="none",
                      diff=FALSE,
                      swapPredictors=FALSE,
                      title=NULL,
                      titleSize=1.3,
                      titleColor="black",
                      fillColor="grey",
                      fillAlpha=0.4,
                      lowerBoundColor="#3366cc",
                      upperBoundColor="#cc3300",
                      lineColor="#33cc66",
                      axisTitle.x=NULL,
                      axisTitle.y=NULL,
                      axisLabelColor="gray30", 
                      axisLabelSize=1.1,
                      axisTitleColor="black",
                      axisTitleSize=1.3,
                      legendLabels=NULL,
                      legendLabelSize=0.9,
                      legendLabelColor="black",
                      showValueLabels=FALSE,
                      valueLabelSize=4,
                      valueLabelColor="black",
                      valueLabelAlpha=0.8,
                      breakTitleAt=50,
                      breakLegendLabelsAt=20,
                      gridBreaksAt=NULL,
                      theme=NULL,
                      showTickMarks=TRUE,
                      borderColor=NULL, 
                      axisColor=NULL, 
                      majorGridColor=NULL,
                      minorGridColor=NULL,
                      hideGrid.x=FALSE,
                      hideGrid.y=FALSE) {
  # -----------------------------------------------------------
  # parameter check
  # -----------------------------------------------------------
  if (is.null(fillColor)) {
    fillColor="white"
    fillAlpha=0
  }
  if (is.null(gridBreaksAt)) {
    gridbreaks.x <- gridbreaks.y <- waiver()
  }
  # -----------------------------------------------------------
  # retrieve amount of predictor variables
  # -----------------------------------------------------------
  listpv <- attr(fit$terms,"predvars")
  predvars <- c()
  # -----------------------------------------------------------
  # remove first two elements (including dependent variable)
  # -----------------------------------------------------------
  for (i in 3:length(listpv)) {
    predvars <- c(predvars, listpv[[i]])
  }
  # remember length of predictor variables
  predvars.length <- length(predvars)
  # -----------------------------------------------------------
  # retrieve p-values, without intercept
  # -----------------------------------------------------------
  pval <- summary(fit)$coefficients[-1,4]
  # -----------------------------------------------------------
  # retrieve estimates, without intercept
  # -----------------------------------------------------------
  estimates <- summary(fit)$coefficients[-1,1]
  estimates.names <- names(estimates)
  # -----------------------------------------------------------
  # retrieve estimate of intercept
  # -----------------------------------------------------------
  b0 <- estimates.intercept <- summary(fit)$coefficients[1,1]

  
  # -----------------------------------------------------------
  # find all significant interactions
  # we start looking for significant p-values beginning
  # with the first interaction, not the first single term!
  # thus, the starting point is first position after all single
  # predictor variables
  # -----------------------------------------------------------
  # save names of interaction predictor variables into this object
  intnames <- c()
  for (i in (predvars.length+1):length(pval)) {
    if (pval[i] < 0.05) {
      intnames <- c(intnames, names(pval[i]))
    }
  }
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames)) {
    stop("No significant interactions found...", call.=FALSE)
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
  # Hide or show Tick Marks
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
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
  
  
  # -----------------------------------------------------------
  # copy variable values to data frame
  # -----------------------------------------------------------
  fitdat <- as.data.frame(fit$x)
  # -----------------------------------------------------------
  # Now iterate all significant interaction terms
  # and manually calculate the linear regression by inserting
  # the estimates of each term and the associated interaction term,
  # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
  # -----------------------------------------------------------
  for (cnt in 1:length(intnames)) {
    # -----------------------------------------------------------
    # first, retrieve and split interaction term so we know 
    # the two predictor variables of the interaction term
    # -----------------------------------------------------------
    interactionterms <- strsplit(intnames[cnt], ":")
    labx <- c()
    # Label on y-axis is name of dependent variable
    laby <- listpv[[2]]
    # -----------------------------------------------------------
    # find estimates (beta values) for each single predictor of
    # the interaction as well as of the interaction term
    # -----------------------------------------------------------
    b1 <- as.numeric(estimates[match(interactionterms[[1]][1], estimates.names)])
    b2 <- as.numeric(estimates[match(interactionterms[[1]][2], estimates.names)])
    b3 <- as.numeric(estimates[match(intnames[cnt], estimates.names)])
    # -----------------------------------------------------------
    # retrieve number of unique values in each predictor variable.
    # depending on the amount of values the variable for the x-axis
    # is chosen. In this case, we use the predictor with the higher
    # number of unique values on the x-axis.
    # -----------------------------------------------------------
    # retrieve values as data frame
    df_pred1uniquevals <- unique(fitdat[interactionterms[[1]][1]])
    df_pred2uniquevals <- unique(fitdat[interactionterms[[1]][2]])
    # convert data frame to numeric vector
    pred1uniquevals <- pred2uniquevals <- as.numeric(c())
    pred1uniquevals <- sort(as.numeric(c(apply(df_pred1uniquevals, c(1), as.numeric ))))
    pred2uniquevals <- sort(as.numeric(c(apply(df_pred2uniquevals, c(1), as.numeric ))))
    # init data frame
    intdf <- c()
    # -----------------------------------------------------------
    # choose x-value according to higher number of unique values
    # choose minimum and maximum value from predictor that has
    # a "smaller range" (i.e. less unique values)
    # or swap predictors on axes if requested
    # -----------------------------------------------------------
    if (swapPredictors) {
      useFirstPredOnY <- ifelse(length(pred1uniquevals) > length(pred2uniquevals), F, T)
    }
    else {
      useFirstPredOnY <- ifelse(length(pred1uniquevals) > length(pred2uniquevals), T, F)
    }
    # -----------------------------------------------------------
    # calculate regression line
    # -----------------------------------------------------------
    if (useFirstPredOnY) {
      labx <- c(interactionterms[[1]][1])
      predy <- c(interactionterms[[1]][2])
      ymin <- min(pred2uniquevals)
      ymax <- max(pred2uniquevals)
      # -----------------------------------------------------------
      # Create data frame for plotting the interactions by
      # manually calculating the linear regression by inserting
      # the estimates of each term and the associated interaction term,
      # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
      # -----------------------------------------------------------
      for (j in 1:length(pred1uniquevals)) {
        # iterate x-values and calculate minimum y
        pr <- pred1uniquevals[j]
        # ------------------------------
        # We now calculate the effect of predictor 1 under absence (or lowest
        # impact) of predictor 2 on the dependent variable. Thus, the slope for
        # predictor 2 is not calculated. see
        # http://www.theanalysisfactor.com/interpreting-interactions-in-regression/
        # http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/
        # ------------------------------
        # miny = (b0 + (b1*pr) + (b2*ymin) + (b3*pr*ymin))
        miny = (b0 + (b1*pr) + (b3*pr*ymin))
        # ------------------------------
        # here we calculate the effect of predictor 1 under presence (or strongest
        # impact) of predictor 2 on the dependent variable. Thus, the slope for
        # predictor 2 only is not needed. see references above
        # ------------------------------
        # maxy = (b0 + (b1*pr) + (b2*ymax) + (b3*pr*ymax))
        maxy = (b0 + (b1*pr) + (b3*pr*ymax))
        # store in df
        tmp <- as.data.frame(cbind(x=j, y=miny, ymin=miny, ymax=maxy, grp="min"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        tmp <- as.data.frame(cbind(x=j, y=maxy, ymin=miny, ymax=maxy, grp="max"))
        intdf <- as.data.frame(rbind(intdf, tmp))
      }
    }
    else {
      labx <- c(interactionterms[[1]][2])
      predy <- c(interactionterms[[1]][1])
      ymin <- min(pred1uniquevals)
      ymax <- max(pred1uniquevals)
      # -----------------------------------------------------------
      # Create data frame for plotting the interactions by
      # manually calculating the linear regression by inserting
      # the estimates of each term and the associated interaction term,
      # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
      # -----------------------------------------------------------
      # compute for minimum value
      for (j in 1:length(pred2uniquevals)) {
        # iterate x-values and calculate minimum y
        pr <- pred2uniquevals[j]
        # ------------------------------
        # We now calculate the effect of predictor 2 under absence (or lowest
        # impact) of predictor 1 on the dependent variable. Thus, the slope for
        # predictor 1 is not calculated. see
        # http://www.theanalysisfactor.com/interpreting-interactions-in-regression/
        # http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/
        # ------------------------------
        # miny = (b0 + (b1*ymin) + (b2*pr) + (b3*pr*ymin))
        miny = (b0 + (b2*pr) + (b3*pr*ymin))
        # ------------------------------
        # here we calculate the effect of predictor 2 under presence (or strongest
        # impact) of predictor 1 on the dependent variable. Thus, the slope for
        # predictor 1 only is not needed. see references above
        # ------------------------------
        # maxy = (b0 + (b1*ymax) + (b2*pr) + (b3*pr*ymax))
        maxy = (b0 + (b2*pr) + (b3*pr*ymax))
        # store in df
        tmp <- as.data.frame(cbind(x=j, y=miny, ymin=miny, ymax=maxy, grp="min"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        tmp <- as.data.frame(cbind(x=j, y=maxy, ymin=miny, ymax=maxy, grp="max"))
        intdf <- as.data.frame(rbind(intdf, tmp))
      }
    }
    # -----------------------------------------------------------
    # convert df-values to numeric
    # -----------------------------------------------------------
    intdf$x <- as.numeric(as.character(intdf$x))
    intdf$y <- as.numeric(as.character(intdf$y))
    intdf$ymin <- as.numeric(as.character(intdf$ymin))
    intdf$ymax <- as.numeric(as.character(intdf$ymax))
    intdf$ydiff <- intdf$ymax-intdf$ymin
    # -----------------------------------------------------------
    # retrieve lowest and highest x and y position to determine
    # the scale limits
    # -----------------------------------------------------------
    lowerLim.x <- floor(min(intdf$x))
    upperLim.x <- ceiling(max(intdf$x))
    lowerLim.y <- floor(min(intdf$y))
    upperLim.y <- ceiling(max(intdf$y))
    # -----------------------------------------------------------
    # check whether user defined grid breaks / tick marks are used
    # -----------------------------------------------------------
    if (!is.null(gridBreaksAt)) {
      gridbreaks.x <- c(seq(lowerLim.x, upperLim.x, by=gridBreaksAt))
      gridbreaks.y <- c(seq(lowerLim.y, upperLim.y, by=gridBreaksAt))
    }
    # -----------------------------------------------------------
    # prepare plot title and axis titles
    # -----------------------------------------------------------
    if (is.null(title)) {
#       labtitle <- paste0("Effect of ", interactionterms[[1]][ifelse(useFirstPredOnY==TRUE,1,2)], 
#                          " on ", listpv[[2]], 
#                          " under minimum and maximum interaction with ", interactionterms[[1]][ifelse(useFirstPredOnY==TRUE,2,1)])
      labtitle <- paste0("Interaction of ", interactionterms[[1]][ifelse(useFirstPredOnY==TRUE,1,2)], 
                         " and ", interactionterms[[1]][ifelse(useFirstPredOnY==TRUE,2,1)],
                         " on ", listpv[[2]])
    }
    else {
      labtitle <- title
    }
    if (is.null(legendLabels)) {
      lLabels <- c(paste0("lower bound of ", predy), paste0("upper bound of ", predy))
    }
    else {
      lLabels <- legendLabels
    }
    if (!is.null(axisTitle.x)) {
      labx <- axisTitle.x
    }
    if (!is.null(axisTitle.y)) {
      laby <- axisTitle.y
    }
    # wrap title
    pattern <- c(paste('(.{1,', breakTitleAt, '})(\\s|$)', sep=""))
    for (n in 1:length(labtitle)) {
      labtitle[n] <- gsub(pattern, '\\1\n', labtitle[n])
    }
    # wrap legend labels
    pattern <- c(paste('(.{1,', breakLegendLabelsAt, '})(\\s|$)', sep=""))
    for (n in 1:length(lLabels)) {
      lLabels[n] <- gsub(pattern, '\\1\n', lLabels[n])
    }
    
    
    # -----------------------------------------------------------
    # prepare base plot of interactions
    # -----------------------------------------------------------
    if (smooth=="none") {
      if (diff) {
        baseplot <- ggplot(intdf, aes(x=x, y=ydiff)) + 
          # add a shaded region between minimun and maximum curve of interactions
          geom_ribbon(aes(x=x, ymin=0, ymax=ydiff), fill=fillColor, alpha=fillAlpha) +
          geom_line(colour=lineColor)
        if (showValueLabels) {
          baseplot <- baseplot +
            geom_text(aes(label=round(ydiff,1), x=x, y=ydiff), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
        }
      }
      else {
        baseplot <- ggplot(intdf) + 
          geom_point(aes(x=x, y=y, colour=grp)) +
          # add a shaded region between minimun and maximum curve of interactions
          geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax), fill=fillColor, alpha=fillAlpha) +
          geom_line(aes(x=x, y=y, colour=grp))
        if (showValueLabels) {
          baseplot <- baseplot +
            geom_text(aes(label=round(y,1), x=x, y=y), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
        }
      }
    }
    else {
      if (diff) {
        baseplot <- ggplot(intdf, aes(x=x, y=ydiff)) + 
          stat_smooth(colour=lineColor, method=smooth, se=FALSE)
        # ------------------------------------------------------------
        # Thanks to Stackoverflow user Henrik for the following solution
        # (http://stackoverflow.com/q/19643234/2094622)
        # ------------------------------------------------------------
        # build plot object for rendering 
        ggloess <- ggplot_build(baseplot)
        # extract data for the loess lines from the 'data' slot
        loessdf <- data.frame(x = ggloess$data[[1]]$x, ydiff = ggloess$data[[1]]$y)
        # use the loess data to add the 'ribbon' to plot
        # add a shaded region between minimun and maximum curve of interactions
        baseplot <- baseplot + geom_ribbon(data = loessdf, aes(x = x, ymin = 0, ymax = ydiff), fill = fillColor, alpha = fillAlpha)
        if (showValueLabels) {
          baseplot <- baseplot +
            geom_text(data = loessdf, aes(label=round(ydiff,1), x=x, y=ydiff), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
        }
      }
      else {
        baseplot <- ggplot(intdf) + 
          stat_smooth(aes(x=x, y=ymin, colour="max"), method=smooth, se=FALSE) +
          stat_smooth(aes(x=x, y=ymax, colour="min"), method=smooth, se=FALSE)
        # ------------------------------------------------------------
        # Thanks to Stackoverflow user Henrik for the following solution
        # (http://stackoverflow.com/q/19643234/2094622)
        # ------------------------------------------------------------
        # build plot object for rendering 
        ggloess <- ggplot_build(baseplot)
        # extract data for the loess lines from the 'data' slot
        loessdf <- data.frame(x = ggloess$data[[1]]$x, ymin = ggloess$data[[1]]$y, ymax = ggloess$data[[2]]$y)
        # use the loess data to add the 'ribbon' to plot
        # add a shaded region between minimun and maximum curve of interactions
        baseplot <- baseplot + geom_ribbon(data = loessdf, aes(x = x, ymin = ymin, ymax = ymax), fill = fillColor, alpha = fillAlpha)
        if (showValueLabels) {
          baseplot <- baseplot +
            geom_text(data = loessdf, aes(label=round(ymin,1), x=x, y=ymin), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE) +
            geom_text(data = loessdf, aes(label=round(ymax,1), x=x, y=ymax), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
        }
      }
    }
    # ------------------------------------------------------------------------------------
    # build plot object with theme and labels
    # ------------------------------------------------------------------------------------
    baseplot <- baseplot + 
      # set plot and axis titles
      labs(title=labtitle, x=labx, y=laby) +
      # set axis scale breaks
      scale_x_continuous(limits=c(lowerLim.x, upperLim.x), breaks=gridbreaks.x) +
      scale_y_continuous(limits=c(lowerLim.y, upperLim.y), breaks=gridbreaks.y) +
      # apply theme
      ggtheme  + 
      # do minor modifications to theme
      theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
            axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor),
            legend.text = element_text(size=rel(legendLabelSize), colour=legendLabelColor),
            plot.title = element_text(size=rel(titleSize), colour=titleColor))
    # ------------------------------------------------------------------------------------
    # check whether only diff-line is shown or upper and lower boundaries. in the latter
    # case, show legend, else hide legend
    # ------------------------------------------------------------------------------------
    if (diff) {
      baseplot <- baseplot +
        guides(fill=FALSE)
    }
    else {
      baseplot <- baseplot +
        scale_colour_manual(values=c(lowerBoundColor, upperBoundColor), name="", labels=lLabels)
    }
    # ------------------------------------------------------------------------------------
    # apply specific border/theme properties
    # ------------------------------------------------------------------------------------
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
    # ------------------------------------------------------------------------------------
    # plot final object
    # ------------------------------------------------------------------------------------
    print(baseplot)
  }
}