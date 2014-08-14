#' @title Plot interaction terms (moderation) of linear models
#' @name sjp.lm.int
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2013/10/31/visual-interpretation-of-interaction-terms-in-linear-models-with-ggplot-rstats/}
#'              \item \url{http://www.theanalysisfactor.com/interpreting-interactions-in-regression/}
#'              \item \url{http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/}
#'              \item \url{http://www.theanalysisfactor.com/3-tips-interpreting-moderation/}
#'              \item Aiken and West (1991). Multiple Regression: Testing and Interpreting Interactions.
#'              }
#'             
#' @description Plot regression curves of significant interaction terms (moderation) in linear models (lm). Note that beside interaction
#'                terms, also the single predictors of each interaction must be included in the fitted model as well.
#'                Thus, \code{lm(dep~pred1*pred2)} will work, but \code{lm(dep~pred1:pred2)} won't!
#' 
#' @note Beside interaction terms, also the single predictors of each interaction must be included in the fitted model as well.
#'         Thus, \code{lm(dep~pred1*pred2)} will work, but \code{lm(dep~pred1:pred2)} won't!
#' 
#' @seealso \code{\link{sjp.lm}} \cr
#'          \code{\link{sjp.emm.int}} \cr
#'          \code{\link{sjp.reglin}} \cr
#'          \code{\link{sjp.lm.ma}}
#' 
#' @param fit the fitted linear model (lm) object, including interaction terms
#' @param smooth smoothes the regression line in case it is not linear. Default is \code{"none"}, so no smoothing
#'          is applied. Use \code{"loess"} for loess-smoothing or \code{"lm"} to force linear regression lines.
#' @param diff if \code{FALSE} (default), the minimum and maximum interaction effects of predictor 2 on predictor 1
#'          are shown (one line each). if \code{TRUE}, only the difference between minimum and maximum interaction effect
#'          is shown (single line)
#' @param moderatorValues indicates which values of the moderator variable should be used when plotting the effects of the 
#'          independent variable on the dependent variable. By default, \code{"minmax"} is used, i.e. the minimum and maximum values
#'          (lower and upper bounds) of the moderator are used to plot the interaction between independent variable and moderator.
#'          Use \code{"meansd"} to use the mean value of the moderator as well as one standard deviation below and above mean value
#'          to plot the effect of the moderator on the independent variable (following
#'          the convention suggested by Cohen and Cohen and popularized by Aiken and West, 
#'          i.e. using the mean, the value one standard deviation above, and the value one standard deviation below the mean
#'          as values of the moderator, see \url{http://www.theanalysisfactor.com/3-tips-interpreting-moderation/}).
#' @param swapPredictors if \code{TRUE}, the predictor with less unique values is printed along the x-axis. Default is
#'          \code{FALSE}, so the predictor with more unique values is printed along the x-axis.
#' @param plevel Indicates at which p-value an interaction term is considered as significant. Default is
#'          0.05 (5 percent).
#' @param title a default title used for the plots. Default value is \code{NULL}, which means that each plot's title
#'          includes the dependent variable as well as the names of the interaction terms.
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param fillColor fill color of the shaded area between the minimum and maximum lines. Default is \code{"grey"}.
#'          Either set \code{fillColor} to \code{NULL} or use 0 for \code{fillAlpha} if you want to hide the shaded area.
#' @param fillAlpha alpha value (transparancy) of the shaded area between the minimum and maximum lines. Default is 0.4.
#'          Use either 0 or set \code{fillColor} to \code{NULL} if you want to hide the shaded area.
#' @param lowerBoundColor the color of the line indicating the lower bound of the interaction term (moderator value).
#'          Default value is \code{"#3366cc"} (blue-like)
#' @param upperBoundColor the color of the line indicating the upper bound of the interaction term (moderator value).
#'          Default value is \code{"#cc3300"} (red-like)
#' @param meanColor the color of the line indicating the mean value of the interaction term (moderator value).
#'          Only applies, when \code{moderatorValues} is \code{"meansd"}.
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
#' @param breakAnnotationLabelsAt Wordwrap for diagram annotation labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted. Default is \code{50}.
#'          Only applies if \code{showInterceptLine} is \code{TRUE}.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is \code{NULL}.
#' @param theme specifies the diagram's background theme. default (parameter \code{NULL}) is a gray 
#'          background with white grids. Use \code{"bw"} for a white background with gray grids, \code{"classic"} for
#'          a classic theme (black border, no grids), \code{"minimal"} for a minimalistic theme (no border,
#'          gray grids) or \code{"none"} for no borders, grids and ticks.
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param showInterceptLines If \code{TRUE}, the intercept and the estimate of the predictor
#'          (reference category of predictor in case interaction is not present) are plotted.
#' @param showInterceptLabels If \code{TRUE} (default), the intercept lines are labelled. Only
#'          applies if \code{showInterceptLines} is \code{TRUE}.
#' @param interceptLineColor The line color of the model's intercept line. Only applies, if
#'          \code{showInterceptLines} is \code{TRUE}.
#' @param estLineColor The line color of the model's predictor's estimate line. Only applies, if
#'          \code{showInterceptLines} is \code{TRUE}.
#' @param lineLabelSize The size of the intercept line annotations inside the plot. Only applies
#'          if \code{showInterceptLines} is \code{TRUE}. Default is 3.7.
#' @param lineLabelColor The color of the intercept line annotations inside the plot. Only applies
#'          if \code{showInterceptLines} is \code{TRUE}. Default is \code{"black"}.
#' @param lineLabelString Default string for the intercept lines that is appended to the predictor
#'          variable name. By default, this string is \code{"(no interaction)"}.
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
#' # load sample data set
#' data(efc)
#' # create data frame with variables that should be included
#' # in the model
#' df <- as.data.frame(cbind(usage=efc$tot_sc_e,
#'                           sex=efc$c161sex, 
#'                           education=efc$c172code, 
#'                           burden=efc$neg_c_7, 
#'                           dependency=efc$e42dep))
#' # convert gender predictor to factor                         
#' df$sex <- relevel(factor(df$sex), ref="2")
#' # fit "dummy" model
#' fit <- lm(usage ~ .*., data=df, x=TRUE)
#' summary(fit)
#' 
#' # plot interactions
#' sjp.lm.int(fit)
#' # plot interactions, including those with p-value up to 0.1
#' sjp.lm.int(fit, plevel=0.1, showInterceptLines=TRUE)
#' 
#' 
#' @import ggplot2
#' @export
sjp.lm.int <- function(fit,
                      smooth="none",
                      diff=FALSE,
                      moderatorValues="minmax",
                      swapPredictors=FALSE,
                      plevel=0.05,
                      title=NULL,
                      titleSize=1.3,
                      titleColor="black",
                      fillColor="grey",
                      fillAlpha=0.4,
                      lowerBoundColor="#3366cc",
                      upperBoundColor="#cc3300",
                      meanColor="#00cc33",
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
                      breakAnnotationLabelsAt=50,
                      gridBreaksAt=NULL,
                      theme=NULL,
                      showTickMarks=TRUE,
                      showInterceptLines=FALSE,
                      showInterceptLabels=TRUE,
                      interceptLineColor="#3366cc",
                      estLineColor="#cc3300",
                      lineLabelSize=3.7,
                      lineLabelColor="black",
                      lineLabelString="(no interaction)",
                      borderColor=NULL, 
                      axisColor=NULL, 
                      majorGridColor=NULL,
                      minorGridColor=NULL,
                      hideGrid.x=FALSE,
                      hideGrid.y=FALSE,
                      printPlot=TRUE) {
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
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
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
  # retrieve position of interaction terms
  it <- rownames(summary(fit)$coefficients)[-1]
  # init indicator for first term
  firstit <- 0
  # iterate all rownames. interaction terms contain a colon...
  for (i in 1:length(it)) {
    # check whether current interactio term name contains a ":",
    # and firstit is not already set
    pos <- grep(":", it[i], fixed=FALSE)
    if (length(pos)>0) {
      # set position to first interaction term in model
      firstit <- i
      break;
    }
  }
  # check whether we have any interaction terms included at all
  if(firstit==0) {
    stop("No interaction term found in fitted model...", call.=FALSE)
  }
  # save names of interaction predictor variables into this object
  intnames <- c()
  for (i in firstit:length(pval)) {
    if (pval[i] < plevel) {
      intnames <- c(intnames, it[i])
    }
  }
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames)) {
    stop("No significant interactions found...", call.=FALSE)
  }
  # --------------------------------------------------------
  # Check whether we have any estimate names. this variable is
  # null in case we only have one interaction in the fitted
  # model.
  # --------------------------------------------------------
  if (is.null(estimates.names)) {
    estimates.names <- it
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
  # check whether parameter X=TRUE was set when fitting the linear
  # model. if not, we cannot procede here
  # -----------------------------------------------------------
  if(class(fit$x)!="matrix") {
    stop("The model matrix is not available! Please use \"x=TRUE\" in your lm-command...", call.=FALSE)
  }
  # -----------------------------------------------------------
  # copy variable values to data frame
  # -----------------------------------------------------------
  fitdat <- as.data.frame(fit$x)
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
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
    # check whether each predictor was included in the model 
    # as single term as well
    # -----------------------------------------------------------
    if(is.na(b1) || is.na(b2) || is.na(b3)) {
      stop("Predictors of interaction terms must be included as single term as well. See Note in ?sjp.lm.int", call.=FALSE)
    }
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
      # -----------------------------------------------------------
      # check which values of moderator should be plotted, i.e. if
      # lower/upper bound (min-max) or mean and standard-deviation 
      # should be used as valus for the moderator.
      # see http://www.theanalysisfactor.com/3-tips-interpreting-moderation/
      # -----------------------------------------------------------
      if (moderatorValues=="minmax") {
        mw <- NA
        ymin <- min(pred2uniquevals)
        ymax <- max(pred2uniquevals)
      }
      else {
        mw <- mean(pred2uniquevals, na.rm=T)
        ymin <- mw-sd(pred2uniquevals, na.rm=T)
        ymax <- mw+sd(pred2uniquevals, na.rm=T)
      }
      # intercept of predictor's reference category
      est_b <- b2+b0
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
        tmp <- as.data.frame(cbind(x=pr, y=miny, ymin=miny, ymax=maxy, grp="min"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        tmp <- as.data.frame(cbind(x=pr, y=maxy, ymin=miny, ymax=maxy, grp="max"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        if (moderatorValues!="minmax") {
          # ------------------------------
          # here we calculate the effect of predictor 1 under presence 
          # of mean of predictor 2 on the dependent variable. Thus, the slope for
          # predictor 2 only is not needed. see references above
          # ------------------------------
          mittelwert <- (b0 + (b1*pr) + (b3*pr*mw))
          tmp <- as.data.frame(cbind(x=pr, y=mittelwert, ymin=miny, ymax=maxy, grp="mean"))
          intdf <- as.data.frame(rbind(intdf, tmp))
        }
      }
    }
    else {
      labx <- c(interactionterms[[1]][2])
      predy <- c(interactionterms[[1]][1])
      # -----------------------------------------------------------
      # check which values of moderator should be plotted, i.e. if
      # lower/upper bound (min-max) or mean and standard-deviation 
      # should be used as valus for the moderator.
      # see http://www.theanalysisfactor.com/3-tips-interpreting-moderation/
      # -----------------------------------------------------------
      if (moderatorValues=="minmax") {
        mw <- NA
        ymin <- min(pred1uniquevals)
        ymax <- max(pred1uniquevals)
      }
      else {
        mw <- mean(pred1uniquevals, na.rm=T)
        ymin <- mw-sd(pred1uniquevals, na.rm=T)
        ymax <- mw+sd(pred1uniquevals, na.rm=T)
      }
      # intercept of predictor's reference category
      est_b <- b1+b0
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
        tmp <- as.data.frame(cbind(x=pr, y=miny, ymin=miny, ymax=maxy, grp="min"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        tmp <- as.data.frame(cbind(x=pr, y=maxy, ymin=miny, ymax=maxy, grp="max"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        if (moderatorValues!="minmax") {
          # ------------------------------
          # here we calculate the effect of predictor 2 under presence 
          # of mean of predictor 1 on the dependent variable. Thus, the slope for
          # predictor 1 only is not needed. see references above
          # ------------------------------
          mittelwert <- (b0 + (b2*pr) + (b3*pr*mw))
          tmp <- as.data.frame(cbind(x=pr, y=mittelwert, ymin=miny, ymax=maxy, grp="mean"))
          intdf <- as.data.frame(rbind(intdf, tmp))
        }
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
    # check whether we have to modify axis limits in case intercept
    # lines are also plotted
    # -----------------------------------------------------------
    if (showInterceptLines) {
      # retrieve intercept bounds
      ilmin <- min(b0, est_b)
      ilmax <- min(b0, est_b)
      # adjust lower lim if necessary
      if (ilmin < lowerLim.y) {
        lowerLim.y <- floor(ilmin)
      }
      # adjust upper lim if necessary
      if (ilmax > upperLim.y) {
        upperLim.y <- ceiling(max(ilmax))
      }
    }
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
      if (moderatorValues=="minmax") {
        lLabels <- c(paste0("lower bound of ", predy), paste0("upper bound of ", predy))
      }
      else {
        lLabels <- c(paste0("lower sd of ", predy), paste0("upper sd of ", predy), paste0("mean of ", predy))
      }
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
    # -----------------------------------------------------------
    # prepare annotation labels
    # -----------------------------------------------------------
    annoLabels <- paste(lLabels[1], lineLabelString)
    annoLabels <- c(annoLabels, paste(lLabels[2], lineLabelString))
    # wrap title
    labtitle <- sju.wordwrap(labtitle, breakTitleAt)
    # wrap legend labels
    lLabels <- sju.wordwrap(lLabels, breakLegendLabelsAt)
    # wrap annotation labels
    annoLabels <- sju.wordwrap(annoLabels, breakAnnotationLabelsAt)
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
        # ------------------------------------------------------------
        # plot value labels
        # ------------------------------------------------------------
        if (showValueLabels) {
          baseplot <- baseplot +
            geom_text(aes(label=round(y,1), x=x, y=y), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
        }
        # ------------------------------------------------------------
        # plot intercept line and estimate line (i.e. reference category
        # of predictor, in case interaction is not present)
        # ------------------------------------------------------------
        if (showInterceptLines) {
          baseplot <- baseplot +
            geom_abline(intercept=b0, slope=0, colour=interceptLineColor) +
            geom_abline(intercept=est_b, slope=0, colour=estLineColor)
          if (showInterceptLabels) {
            baseplot <- baseplot +
              annotate("text", label=annoLabels[1], x=-Inf, hjust=-0.05, vjust=-0.5, colour=lineLabelColor, size=lineLabelSize, y=b0) +
              annotate("text", label=annoLabels[2], x=-Inf, hjust=-0.05, vjust=-0.5, colour=lineLabelColor, size=lineLabelSize, y=est_b)
          }
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
        # ------------------------------------------------------------
        # plot value labels
        # ------------------------------------------------------------
        if (showValueLabels) {
          baseplot <- baseplot +
            geom_text(data = loessdf, aes(label=round(ymin,1), x=x, y=ymin), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE) +
            geom_text(data = loessdf, aes(label=round(ymax,1), x=x, y=ymax), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
        }
        # ------------------------------------------------------------
        # plot intercept line and estimate line (i.e. reference category
        # of predictor, in case interaction is not present)
        # ------------------------------------------------------------
        if (showInterceptLines) {
          baseplot <- baseplot +
            geom_abline(intercept=b0, slope=0, colour=interceptLineColor) +
            geom_abline(intercept=est_b, slope=0, colour=estLineColor)
          if (showInterceptLabels) {
            baseplot <- baseplot +
              annotate("text", label=annoLabels[1], x=-Inf, hjust=-0.05, vjust=-0.5, colour=lineLabelColor, size=lineLabelSize, y=b0) +
              annotate("text", label=annoLabels[2], x=-Inf, hjust=-0.05, vjust=-0.5, colour=lineLabelColor, size=lineLabelSize, y=est_b)
          }
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
      if (moderatorValues=="minmax") {
        baseplot <- baseplot +
          scale_colour_manual(values=c(lowerBoundColor, upperBoundColor), name="", labels=lLabels)
      }
      else {
        baseplot <- baseplot +
          scale_colour_manual(values=c(lowerBoundColor, upperBoundColor, meanColor), name="", labels=lLabels)
      }
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
  invisible (structure(class = "sjplmint",
                       list(plot.list = plotlist,
                            df.list = dflist)))
}