# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("xn", "vld"))

#' @title Plot adjusted (estimated marginal) means of interaction (moderation) in linear models
#' @name sjp.emm.int
#' @references \itemize{
#'              \item \url{http://rpubs.com/sjPlot/sjpemmint}
#'              \item \url{http://strengejacke.wordpress.com/2014/08/19/visualize-pre-post-comparison-of-intervention-rstats/}
#'              \item \url{http://www.theanalysisfactor.com/using-adjusted-means-to-interpret-moderators-in-analysis-of-covariance/}
#'              }
#'             
#' @description Plot estimated marginal means of (significant) interaction terms in linear models (lm). This function may
#'                be used to plot differences in interventions between control and treatment groups over multiple
#'                time points.
#' 
#' @note Please note that all interaction terms have to be of type \code{\link{factor}}!
#'         Furthermore, predictors of interactions that are introduced first into the model
#'         are used as grouping variable, while the latter predictor is printed along the x-axis
#'         (i.e. lm(y~a+b+a:b) means that "a" is used as grouping variable and "b" is plotted along the x-axis).
#' 
#' @seealso \code{\link{sjp.lm.int}} \cr
#'          \code{\link{sjp.reglin}} \cr
#'          \code{\link{sjp.aov1}} \cr
#'          \code{\link{sjp.lm.ma}}
#' 
#' @param fit the fitted linear model (lm) object, including interaction terms
#' @param swapPredictors if \code{TRUE}, the grouping variable and predictor on
#'          the x-axis are swapped.
#' @param plevel Indicates at which p-value an interaction term is considered as significant. Default is
#'          0.05 (5 percent).
#' @param title a default title used for the plots. Default value is \code{NULL}, which means that each plot's title
#'          includes the dependent variable as well as the names of the interaction terms.
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param lowerBoundColor the color of the line indicating the lower bound of the interaction term (moderator value).
#'          Default value is \code{"#3366cc"} (blue-like)
#' @param upperBoundColor the color of the line indicating the upper bound of the interaction term (moderator value).
#'          Default value is \code{"#cc3300"} (red-like)
#' @param colorPalette If the grouping variable has more than two levels, more than two colors are
#'          needed for plotting the lines. In this case, specify a color palette from the \url{http://colorbrewer2.org} here. 
#'          All color brewer palettes supported by ggplot are accepted here. Alternatively, you can provide
#'          a vector of colors, i.e. \code{c("blue", "red", "gren")}.
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
#' @param valueLabel.digits the amount of digits of the displayed value labels. Defaults to 2.
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
#' @param breakAnnotationLabelsAt Wordwrap for diagram annotation labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted. Default is \code{50}.
#'          Only applies if \code{showInterceptLine} is \code{TRUE}.
#' @param axisLimits.y A vector with two values, defining the lower and upper limit from the y-axis.
#'          By default, this value is \code{NULL}, i.e. axis limits will be calculated upon the
#'          range of y-values.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is \code{NULL}.
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids.
#'          \itemize{
#'          \item Use \code{"bw"} for a white background with gray grids
#'          \item \code{"classic"} for a classic theme (black border, no grids)
#'          \item \code{"minimal"} for a minimalistic theme (no border,gray grids)
#'          \item \code{"none"} for no borders, grids and ticks or
#'          \item \code{"themr"} if you are using the \code{ggthemr} package (in such cases, you may use the \code{ggthemr::swatch} function to retrieve theme-colors for the \code{lowerBoundColor} parameter)
#'          }
#'          See \url{http://rpubs.com/sjPlot/custplot} for details and examples.
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param borderColor user defined color of whole diagram border (panel border)
#' @param axisColor user defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border).
#' @param majorGridColor specifies the color of the major grid lines of the diagram background
#' @param minorGridColor specifies the color of the minor grid lines of the diagram background
#' @param hideGrid.x If \code{TRUE}, the x-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param hideGrid.y If \code{TRUE}, the y-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-objects with the complete plot-list (\code{plot.list}) 
#'           as well as the data frame that were used for setting up the ggplot-objects (\code{df.list}).
#' 
#' @examples
#' \dontrun{
#' # Note that the data sets used in this example may not be perfectly suitable for
#' # fitting linear models. I just used them because they are part of the R-software.
#' 
#' # prepare data frame
#' df <- data.frame(mpg=mtcars$mpg,vs=factor(mtcars$vs),am=factor(mtcars$am))
#' # fit "dummy" model.
#' fit <- lm(mpg~vs+am+vs:am, data=df)
#' # show summary to see significant interactions
#' summary(fit)
#' 
#' # plot marginal means of interaction terms
#' # note we have to adjust plevel, because no interaction
#' # is significant
#' sjp.emm.int(fit, plevel=1)
#' # plot marginal means of interaction terms, including value labels
#' sjp.emm.int(fit, plevel=1, showValueLabels=TRUE)
#' 
#' 
#' # load sample data set
#' data(efc)
#' # create data frame with variables that should be included
#' # in the model
#' df <- as.data.frame(cbind(burden=efc$neg_c_7,
#'                           sex=efc$c161sex, 
#'                           education=efc$c172code))
#' # convert gender predictor to factor                         
#' df$sex <- factor(df$sex)
#' df$education <- factor(df$education)
#' # name factor levels and dependent variable
#' levels(df$sex) <- c("female", "male")
#' levels(df$education) <- c("low", "mid", "high")
#' df$burden <- sji.setVariableLabels(df$burden, "care burden")
#' # fit "dummy" model
#' fit <- lm(burden ~ .*., data=df, na.action=na.omit)
#' summary(fit)
#' 
#' # plot marginal means of interactions, no interaction found
#' sjp.emm.int(fit)
#' # plot marginal means of interactions, including those with p-value up to 1
#' sjp.emm.int(fit, plevel=1)
#' # swap predictors
#' sjp.emm.int(fit, plevel=1, swapPredictors=TRUE)}
#' 
#' 
#' @import ggplot2
#' @export
sjp.emm.int <- function(fit,
                       swapPredictors=FALSE,
                       plevel=0.05,
                       title=NULL,
                       titleSize=1.3,
                       titleColor="black",
                       lowerBoundColor="#3366cc",
                       upperBoundColor="#cc3300",
                       colorPalette="Set2",
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
                       valueLabel.digits=2,
                       valueLabelSize=4,
                       valueLabelColor="black",
                       valueLabelAlpha=0.8,
                       breakTitleAt=50,
                       breakLegendLabelsAt=20,
                       breakAnnotationLabelsAt=50,
                       axisLimits.y=NULL,
                       gridBreaksAt=NULL,
                       theme=NULL,
                       showTickMarks=TRUE,
                       borderColor=NULL, 
                       axisColor=NULL, 
                       majorGridColor=NULL,
                       minorGridColor=NULL,
                       hideGrid.x=FALSE,
                       hideGrid.y=FALSE,
                       printPlot=TRUE) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("lsmeans", quietly = TRUE)) {
    stop("Package 'lsmeans' needed for this function to work. Please install it.", call. = FALSE)
  }
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # parameter check
  # -----------------------------------------------------------
  if (is.null(gridBreaksAt)) {
    gridbreaks.x <- gridbreaks.y <- waiver()
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }
  # -----------------------------------------------------------
  # retrieve p-values, without intercept
  # -----------------------------------------------------------
  pval <- summary(fit)$coefficients[-1,4]
  # -----------------------------------------------------------
  # find all significant interactions
  # we start looking for significant p-values beginning
  # with the first interaction, not the first single term!
  # thus, the starting point is first position after all single
  # predictor variables
  # -----------------------------------------------------------
  # save all term labels
  it <- attr(fit$terms, "term.labels")
  # save coefficients
  cf <- names(fit$coefficients[-1])
  # init counter
  it.nr <- 0
  it.pos <- c()
  it.names <- c()
  # loop all term labels
  for (i in 1:length(it)) {
    # check whether current term name contains a ":",
    # thus if it is an interaction term
    pos <- grep(":", it[i])
    # if yes...
    if (length(pos)>0) {
      it.names <- c(it.names, it[i])
    }
  }
  # loop all coefficients
  for (i in 1:length(cf)) {
    # check whether current coefficient contains a ":",
    # thus if it is an interaction term
    pos <- grep(":", cf[i])
    # if yes...
    if (length(pos)>0) {
      # ... increase counter of interactions
      it.nr <- it.nr+1
      # ... and save position of coefficient in model
      it.pos <- c(it.pos, i)
    }
  }
  # check whether we have any interaction terms included at all
  if(it.nr==0) {
    stop("No interaction term found in fitted model...", call.=FALSE)
  }
  # save names of interaction predictor variables into this object
  # but only those with a specific p-level
  intnames <- c()
  for (i in 1:length(it.pos)) {
    if (pval[it.pos[i]] < plevel) {
      intnames <- c(intnames, cf[it.pos[i]])
    }
  }
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames) || 0==length(intnames)) {
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
  # Hide or show Tick Marks
  # --------------------------------------------------------
  if (!showTickMarks && !is.null(ggtheme)) {
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
  # Now iterate all interaction terms from model
  # -----------------------------------------------------------
  interactionterms <- c()
  for (i in 1:length(it.names)) {
    # -----------------------------------------------------------
    # retrieve interaction terms
    # -----------------------------------------------------------
    terms <- unlist(strsplit(it.names[i], ":"))
    # -----------------------------------------------------------
    # Iterate all interactions on factor-level-basis from model
    # -----------------------------------------------------------
    for (cnt in 1:length(intnames)) {
      # -----------------------------------------------------------
      # first, retrieve and split interaction term so we know 
      # the two predictor variables, or factor levels of the 
      # interaction term
      # -----------------------------------------------------------
      lvls <- unlist(strsplit(intnames[cnt], ":"))
      # -----------------------------------------------------------
      # since we may have factors with more levels, the original
      # term labels differ from what we have as coefficient-
      # e.g., "ChickWeight$Diet", becomes "Diet1", "Diet2", etc.
      # to calculate marginal means, we only need "Diet". So here
      # we have to find, which terms match the significant coefficients
      # found, and use the term labels for ls means...
      # -----------------------------------------------------------
      if (grep(terms[1], lvls[1])>0 && grep(terms[2], lvls[2])>0) {
        # we found a match        
        interactionterms <- rbind(interactionterms, terms)
        # leave loop
        break
      }
    }
  }
  for (cnt in 1:nrow(interactionterms)) {
    # -----------------------------------------------------------
    # retrieve each pair of interaction terms
    # -----------------------------------------------------------
    term.pairs <- interactionterms[cnt,]
    if (swapPredictors) term.pairs <- rev(term.pairs)
    # -----------------------------------------------------------
    # retrieve estiamted marginal means
    # -----------------------------------------------------------
    emm <- summary(lsmeans::lsmeans(fit, term.pairs))
    # create data frame from lsmeans
    intdf <- data.frame(emm[2], emm[3], emm[1], emm[6], emm[7], rep(valueLabel.digits, times=nrow(emm[1])))
    colnames(intdf) <- c("x", "y", "grp", "l.ci", "u.ci", "vld")
    # -----------------------------------------------------------
    # convert df-values to numeric
    # -----------------------------------------------------------
    intdf$y <- as.numeric(as.character(intdf$y))
    # add numeric x for geom_line
    intdf$xn <- as.numeric(intdf$x)
    # order data frame
    intdf <- intdf[order(intdf$grp),]
    # -----------------------------------------------------------
    # retrieve lowest and highest x and y position to determine
    # the scale limits
    # -----------------------------------------------------------
    if (is.null(axisLimits.y)) {
      lowerLim.y <- floor(min(intdf$y))
      upperLim.y <- ceiling(max(intdf$y))
    }
    else {
      lowerLim.y <- axisLimits.y[1]
      upperLim.y <- axisLimits.y[2]
    }
    # -----------------------------------------------------------
    # check whether user defined grid breaks / tick marks are used
    # -----------------------------------------------------------
    if (!is.null(gridBreaksAt)) {
      gridbreaks.y <- c(seq(lowerLim.y, upperLim.y, by=gridBreaksAt))
    }
    # -----------------------------------------------------------
    # prepare label and name from depend variable
    # -----------------------------------------------------------
    response.name <- attr(fit$model[[1]],"variable.label")
    response.label <- unname(attr(fit$model[[1]],"variable.label"))    
    # -----------------------------------------------------------
    # prepare plot title and axis titles
    # -----------------------------------------------------------
    if (is.null(title)) {
      labtitle <- paste0("Interaction of ", term.pairs[1], 
                         " and ", term.pairs[2],
                         " on ", response.label)
    }
    else {
      labtitle <- title
    }
    if (is.null(legendLabels)) {
      lLabels <- levels(fit$model[term.pairs[1]][,1])
    }
    else {
      lLabels <- legendLabels
    }
    if (!is.null(axisTitle.x)) {
      labx <- axisTitle.x
    }
    else {
      labx <- term.pairs[2]
    }
    if (!is.null(axisTitle.y)) {
      laby <- axisTitle.y
    }
    else {
      laby <- response.name
    }
    # -----------------------------------------------------------
    # prepare annotation labels
    # -----------------------------------------------------------
    # wrap title
    labtitle <- sju.wordwrap(labtitle, breakTitleAt)
    # wrap legend labels
    lLabels <- sju.wordwrap(lLabels, breakLegendLabelsAt)
    # -----------------------------------------------------------
    # prepare base plot of interactions
    # -----------------------------------------------------------
    baseplot <- ggplot(intdf) + 
      geom_point(aes(x=x, y=y, colour=grp)) +
      geom_line(aes(x=xn, y=y, colour=grp))
    # ------------------------------------------------------------
    # plot value labels
    # ------------------------------------------------------------
    if (showValueLabels) {
      baseplot <- baseplot +
        geom_text(aes(label=round(y,vld), x=x, y=y), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
    }
    # ------------------------------------------------------------------------------------
    # build plot object with theme and labels
    # ------------------------------------------------------------------------------------
    baseplot <- baseplot + 
      # set plot and axis titles
      labs(title=labtitle, x=labx, y=laby) +
      # set axis scale breaks
      scale_y_continuous(limits=c(lowerLim.y, upperLim.y), breaks=gridbreaks.y)
    # apply theme
    if (!is.null(ggtheme)) {
      baseplot <- baseplot + 
        ggtheme +
        # do minor modifications to theme
        theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
              axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor),
              legend.text = element_text(size=rel(legendLabelSize), colour=legendLabelColor),
              plot.title = element_text(size=rel(titleSize), colour=titleColor))
    }
    # ------------------------------------------------------------------------------------
    # check whether only diff-line is shown or upper and lower boundaries. in the latter
    # case, show legend, else hide legend
    # ------------------------------------------------------------------------------------
    if (length(lLabels)==2) {
      scalecolorsline <- scale_colour_manual(values=c(lowerBoundColor, upperBoundColor), name=term.pairs[1], labels=lLabels)
    }
    else {
      if (length(colorPalette)==1) {
        scalecolorsline <- scale_colour_brewer(palette=colorPalette, name=term.pairs[1], labels=lLabels)
      }
      else {
        scalecolorsline <- scale_colour_manual(values=colorPalette, name=term.pairs[1], labels=lLabels)
      }
    }
    baseplot <- baseplot + scalecolorsline
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
    if (printPlot) print(baseplot)
    # concatenate plot object
    plotlist[[length(plotlist)+1]] <- baseplot
    dflist[[length(dflist)+1]] <- intdf
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpaocint",
                       list(plot.list = plotlist,
                            df.list = dflist)))
}