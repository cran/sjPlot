# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("vars", "Beta", "xv", "lower", "upper", "stdbeta", "p", "x", "ydiff", "y", "grp", ".stdresid", ".resid", ".fitted", "V1", "V2"))


#' @title Plot beta coefficients of lm
#' @name sjp.lm
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2013/03/22/plotting-lm-and-glm-models-with-ggplot-rstats/}
#'             }
#' 
#' @description Plot beta coefficients of linear regressions with confidence intervalls as dot plot
#'                (forest plot). Additionally, the standardized beta values are plotted
#'                as red dots.
#'                
#' @seealso \code{\link{sjp.lm.ma}} \cr
#'          \code{\link{sjp.reglin}} \cr
#'          \code{\link{sjp.lm.int}} \cr
#'          \code{\link{sjp.scatter}} \cr
#'          \code{\link{sju.betaCoef}}
#' 
#' @note Based on an an idea from surefoss:
#' \url{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/}
#'
#' @param fit The model of the linear regression (lm-Object).
#' @param title Diagram's title as string.
#'          Example: \code{title=c("my title")}
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param sort Determines whether the predictors are sorted by beta-values (default, or use \code{"beta"} as
#'          parameter) or by standardized beta values (use \code{"std"}).
#' @param axisLabels.y Labels of the predictor variables (independent vars) that are used for labelling the
#'          axis. Passed as vector of strings.
#'          Example: \code{axisLabels.y=c("Label1", "Label2", "Label3")}.
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getValueLabels}} function, you receive a
#'          list object with label string. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param showAxisLabels.y Whether x axis text (category names, predictor labels) should be shown (use \code{TRUE})
#'          or not. Default is \code{TRUE}
#' @param axisLabelSize The size of value labels in the diagram. Default is 4, recommended values range
#'          between 2 and 8.
#' @param axisLabelColor The color of the category labels (predictor labels). Default is a dark grey (grey30).
#' @param axisTitle.x A label for the x axis. Default is \code{"Estimates"}.
#' @param axisTitleColor The color of the x axis label. Default is a dark grey.
#' @param axisTitleSize The size of the x axis label. Default is 1.4.
#' @param axisLimits Defines the range of the axis where the beta coefficients and their confidence intervalls
#'          are drawn. By default, the limits range from the lowest confidence interval to the highest one, so
#'          the diagram has maximum zoom. Use your own values as 2-value-vector, for instance: \code{limits=c(-0.8,0.8)}.
#' @param valueLabelColor Colour of the values (significant beta coefficients) inside the diagrams. Only applies, when parameter
#'          \code{showValueLabels} is set to \code{TRUE}. Use any valid colour value, e.g. \code{valueLabelColor="grey50"} or
#'          \code{valueLabelColor=c("#cc3366")}.
#' @param valueLabelColorNS Colour of the non significant values (non significant beta coefficients) inside the diagrams.
#'          Only applies, when parameter \code{showValueLabels} is set to \code{TRUE}. Use any valid colour value, e.g. 
#'          \code{valueLabelColor="grey50"} or \code{valueLabelColor=c("#cc3366")}.
#' @param valueLabelSize Size of the value labels. Default is 4.5. Recommended Values range from
#'          2 to 8
#' @param valueLabelAlpha The alpha level (transparancy) of the value labels. Default is 0.8, use
#'          any value from 0 to 1.
#' @param axisLabelAngle.x Angle for axis-labels where the estimates are printed. Note
#'          that due to the coordinate flip, the acutal y-axis with estimates labels are appearing on the x-axis.
#' @param axisLabelAngle.y Angle for axis-labels, passed as numeric value.
#' @param errorBarColor The color of the error bars that indicate the confidence intervalls
#'          of the beta-coefficients
#' @param errorBarWidth The width of the error bar ends. Default is 0
#' @param errorBarSize The size of the error bar. Default is 0.8
#' @param pointColor The colour of the points that indicate the beta-value.
#' @param pointSize The size of the points that indicate the beta-value. Default is 3.
#' @param pointColorStdBeta The colour of the points that indicate the standardized 
#'          beta-value.
#' @param pointSizeStdBeta The size of the points that indicate the 
#'          standardized beta-value. Default is 3.
#' @param stdBetaLineType The standardized beta-value dots are connected by a thin line
#'          for a better overview. With this parameter you can specify the line type.
#' @param stdBetaLineAlpha The alpha-value for the line that connects the
#'          standardized beta-value dots.
#' @param interceptLineType The linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor The color of the intercept line. Default value is \code{"grey70"}.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is \code{NULL}, so \code{\link{pretty}} gridbeaks will be used.
#' @param borderColor User defined color of whole diagram border (panel border).
#' @param axisColor User defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border).
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids.
#'          \itemize{
#'          \item Use \code{"bw"} for a white background with gray grids
#'          \item \code{"classic"} for a classic theme (black border, no grids)
#'          \item \code{"minimal"} for a minimalistic theme (no border,gray grids) or 
#'          \item \code{"none"} for no borders, grids and ticks.
#'          }
#' @param flipCoordinates If \code{TRUE} (default), predictors are plotted on the left y-axis and estimate
#'          values are plotted on the x-axis.
#' @param majorGridColor Specifies the color of the major grid lines of the diagram background.
#' @param minorGridColor Specifies the color of the minor grid lines of the diagram background.
#' @param hideGrid.x If \code{TRUE}, the x-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param hideGrid.y If \code{TRUE}, the y-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param showValueLabels Whether the beta and standardized beta values should be plotted 
#'          to each dot or not.
#' @param labelDigits The amount of digits for rounding the estimations (see \code{showValueLabels}).
#'          Default is 2, i.e. estimators have 2 digits after decimal point.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not
#' @param showModelSummary If \code{TRUE} (default), a summary of the regression model with 
#'          Intercept, R-square, F-Test and AIC-value is printed to the lower right corner
#'          of the diagram.
#' @param showStandardBeta Whether or not the dots for the standardized beta values 
#'          should be plotted to the diagram.
#' @param showStandardBetaLine Whether or not the connecting line for the standardized beta values 
#'          should be plotted to the diagram. Default is \code{FALSE}.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @examples
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#' 
#' # plot estimates with CI and standardized beta-values
#' sjp.lm(fit, gridBreaksAt=2)
#' 
#' # plot estimates with CI without standardized beta-values
#' # and with narrower tick marks (because "gridBreaksAt" was not specified)
#' sjp.lm(fit, showStandardBeta=FALSE)
#' 
#' @import ggplot2
#' @export
sjp.lm <- function(fit,
                    sort="beta",
                    title=NULL,
                    titleSize=1.3,
                    titleColor="black",
                    axisLabels.y=NULL, 
                    showAxisLabels.y=TRUE,
                    axisLabelSize=1.1,
                    axisLabelColor="gray30",
                    axisTitle.x="Estimates",
                    axisTitleSize=1.4,
                    axisTitleColor=c("#444444"),
                    axisLimits=NULL,
                    valueLabelColor="grey20",
                    valueLabelColorNS="grey50",
                    valueLabelSize=4.5,
                    valueLabelAlpha=0.8,
                    axisLabelAngle.x=0,
                    axisLabelAngle.y=0, 
                    errorBarColor="#3366a0",
                    errorBarWidth=0,
                    errorBarSize=0.8,
                    pointColor="#3366a0",
                    pointSize=3,
                    pointColorStdBeta="#cc5533",
                    pointSizeStdBeta=3,
                    stdBetaLineType=2,
                    stdBetaLineAlpha=0.3,
                    interceptLineType=2,
                    interceptLineColor="grey70",
                    breakTitleAt=50, 
                    breakLabelsAt=12, 
                    gridBreaksAt=NULL,
                    borderColor=NULL, 
                    axisColor=NULL, 
                    theme=NULL,
                    flipCoordinates=TRUE,
                    majorGridColor=NULL,
                    minorGridColor=NULL,
                    hideGrid.x=FALSE,
                    hideGrid.y=FALSE,
                    showTickMarks=TRUE,
                    showValueLabels=TRUE, 
                    labelDigits=2,
                    showPValueLabels=TRUE,
                    showModelSummary=TRUE,
                    showStandardBeta=FALSE,
                    showStandardBetaLine=FALSE,
                    printPlot=TRUE) {
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) {
    axisLabels.y <- unlistlabels(axisLabels.y)
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
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- sju.wordwrap(axisLabels.y, breakLabelsAt)
  }
  
  
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    modsum <- sju.modsum.lm(fit)
  }
  

  # ----------------------------
  # print beta- and p-values in bar charts
  # ----------------------------
  # retrieve sigificance level of independent variables (p-values)
  pv <- coef(summary(fit))[-1,4]
  # for better readability, convert p-values to asterisks
  # with:
  # p < 0.001 = ***
  # p < 0.01 = **
  # p < 0.05 = *
  # retrieve betas, leave out intercept ([-1])
  bv <- coef(fit)[-1]
  # retrieve standardized betas
  stdbv <- sju.betaCoef(fit)
  # init data column for p-values
  ps <- sprintf("%.*f", labelDigits, bv)
  pstdbv <- sprintf("%.*f", labelDigits, stdbv)
  # if no values should be shown, clear
  # vector now
  if (!showValueLabels) {
    ps <- rep(c(""), length(ps))
    pstdbv <- rep(c(""), length(pstdbv))
  }
  
  # --------------------------------------------------------
  # copy p-values into data column
  # --------------------------------------------------------
  if (showPValueLabels) {
    for (i in 1:length(pv)) {
      if (pv[i]>=0.05) {
      }
      else if (pv[i]>=0.01 && pv[i]<0.05) {
        ps[i] <- paste(ps[i], "*")
      }
      else if (pv[i]>=0.001 && pv[i]<0.01) {
        ps[i] <- paste(ps[i], "**")
      }
      else {
        ps[i] <- paste(ps[i], "***")
      }
    }  
  }
  
  # --------------------------------------------------------
  # create new data.frame, since ggplot requires data.frame as parameter
  # The data frame contains betas, CI and p-values
  # --------------------------------------------------------
  tmp<-data.frame(cbind(
    # Append beta coefficients, [-1] means that the first
    # row (Intercept) will be removed / ignored
    coefficients(fit)[-1],
    # append CI
    confint(fit, level=0.95)[-1,]))
  # append p-values and standardized beta coefficients
  # further more, we take the stand. beta as string, because in
  # case no values are drawn, we simply use an empty string.
  # finally, we need the p-values of the coefficients, because the value
  # labels may have different colours according to their significance level
  betas <- cbind(tmp, c(ps), sju.betaCoef(fit), c(pstdbv), pv)
  
  # --------------------------------------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) {
    axisLabels.y <- row.names(betas)
  }
  
  # --------------------------------------------------------
  # define sorting critaria. the values on the x-axis are being sorted
  # either by beta-values (sort="beta") or by standardized
  # beta values (sort = anything else)
  # --------------------------------------------------------
  
  # --------------------------------------------------------
  # sort labels descending in order of (std.) beta values
  # --------------------------------------------------------
  if (sort=="beta") {
    axisLabels.y <- axisLabels.y[order(bv)]
  }
  else {
    axisLabels.y <- axisLabels.y[order(stdbv)]
  }
  
  # --------------------------------------------------------
  # sort rows of data frame descending in order of (std.) beta values
  # --------------------------------------------------------
  if (sort=="beta") {
    betas <- betas[order(bv),]
  }
  else {
    betas <- betas[order(stdbv),]
  }
  betas <- cbind(c(seq(1:nrow(betas))), betas)
  betas$p <- as.character(betas$p)
  # give columns names
  names(betas)<-c("xv", "Beta", "lower", "upper", "p", "stdbeta", "pstdbv", "pv")
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user-defined range (if "axisLimits"
  # is not NULL)
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    upper_lim <- (ceiling(10*max(betas$upper))) / 10
    lower_lim <- (floor(10*min(betas$lower))) / 10
  }
  else {
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # determine gridbreaks
  if (is.null(gridBreaksAt)) {
    ticks <- pretty(c(lower_lim, upper_lim))
  }
  else {
    ticks <- c(seq(lower_lim, upper_lim, by=gridBreaksAt))
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
  # Set up visibility of tick marks
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showAxisLabels.y) {
    axisLabels.y <- c("")
  }
  
  
  # --------------------------------------------------------
  # Start plot here!
  # --------------------------------------------------------
  betaplot <- ggplot(betas, aes(y=Beta, x=xv)) +
    # print point
    geom_point(size=pointSize, colour=pointColor) +
    # and error bar
    geom_errorbar(aes(ymin=lower, ymax=upper), size=errorBarSize, width=errorBarWidth, colour=errorBarColor)
  # show points for standard beta values
  if (showStandardBeta) {
    # first, add points that indicate standardized beta values to get an impression of
    # the effect strength of predictors
    betaplot <- betaplot +
      geom_point(aes(y=stdbeta, x=xv), colour=pointColorStdBeta, size=pointSizeStdBeta) +
      # Print std.beta-values. With vertical adjustment, so they don't overlap with the errorbars
      geom_text(aes(label=pstdbv, y=stdbeta, colour=pv>0.05), vjust=1.8, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
      # to better distinguish betas and stand. betas, the stand. beta points can be connected with a line
    if (showStandardBetaLine) {
      # print line for standardized beta values
      betaplot <- betaplot +
        geom_line(aes(y=stdbeta, x=xv), colour=pointColorStdBeta, linetype=stdBetaLineType, alpha=stdBetaLineAlpha)
    }
  }
  betaplot <- betaplot +
    # Intercept-line
    geom_hline(yintercept=0, linetype=interceptLineType, color=interceptLineColor) +
    # Print p-values. With vertical adjustment, so they don't overlap with the errorbars
    geom_text(aes(label=p, y=Beta, colour=pv>0.05), vjust=-0.8, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE) +
    # give value labels different colours depending on significance level
    scale_colour_manual(values=c(valueLabelColor, valueLabelColorNS)) +
    # set y-scale-limits, breaks and tick labels
    scale_y_continuous(limits=c(lower_lim,upper_lim), breaks=ticks, labels=ticks) +
    # set value labels to x-axis
    scale_x_discrete(labels=axisLabels.y, limits=c(1:nrow(betas))) +
    labs(title=title, x=NULL, y=axisTitle.x) +
    ggtheme +
    # set axes text and 
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
          axis.text.x = element_text(angle=axisLabelAngle.x),
          axis.text.y = element_text(angle=axisLabelAngle.y),
          plot.title = element_text(size=rel(titleSize), colour=titleColor))
  # --------------------------------------------------------
  # flip coordinates?
  # --------------------------------------------------------
  if (flipCoordinates)  {
    betaplot <- betaplot +
      coord_flip()
  }
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      betaplot <- betaplot + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      cat("\nParameter 'borderColor' can only be applied to 'bw' theme.\n")
    }
  }
  if (!is.null(axisColor)) {
    betaplot <- betaplot + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    betaplot <- betaplot + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    betaplot <- betaplot + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideGrid.x) {
    betaplot <- betaplot + 
      theme(panel.grid.major.x = hidegrid,
            panel.grid.minor.x = hidegrid)
  }
  if (hideGrid.y) {
    betaplot <- betaplot + 
      theme(panel.grid.major.y = hidegrid,
            panel.grid.minor.y = hidegrid)
  }
  # check whether modelsummary should be printed
  if (showModelSummary) {
    # add annotations with model summary
    # annotations include intercept-value and model's r-square
    betaplot <- betaplot + annotate("text", label=modsum, parse=TRUE, x=-Inf, y=Inf, colour=valueLabelColor, size=valueLabelSize, alpha=valueLabelAlpha, vjust=-0.5, hjust=1.1)
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(betaplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjplm",
                       list(plot = betaplot,
                            df = betas)))
}


#' @title Plot regression lines for each predictor
#' @name sjp.reglin
#' 
#' @description Plot regression lines with confidence intervals for each single predictor of
#'                a fitted model. This method extracts all predictors of a fitted model and fits 
#'                each of them against the response variable. \cr \cr
#'                This function plots two lines: The resulting linear regression line
#'                including confidence interval (in blue) and a loess-smoothed line without
#'                confidence interval (in red). The better the linear relationship
#'                of predictor and response is, the more both lines should overlap
#'                (i.e. the red loess-smoothed line is almost linear). \cr \cr
#'                Furthermore, a scatter plot of response and predictor values
#'                is plotted.
#'                
#' @seealso \code{\link{sjp.lm}} \cr
#'          \code{\link{sjp.scatter}} \cr
#'          \code{\link{sjp.lm.ma}} \cr
#'          \code{\link{sjp.lm.int}}
#'          
#' @param fit The model of the linear regression (lm-Object).
#' @param data The data/dataset/dataframe used in the fitted model.
#' @param lineColor The color of the regression line. Default is \code{"blue"}.
#' @param showCI If \code{TRUE} (default), a confidence region for the regression line
#'          will be plotted. Use \code{ciLevel} to specifiy the confidence level.
#' @param ciLevel The confidence level of the confidence region. Only applies when
#'          \code{showCI} is \code{TRUE}. Default is 0.95.
#' @param pointAlpha The alpha values of the scatter plot's point-geoms.
#'          Default is 0.2.
#' @param pointColor The color of the scatter plot's point-geoms. Only applies when \code{showScatterPlot}
#'          is \code{TRUE}. Default is \code{"black"}.
#' @param showScatterPlot If \code{TRUE} (default), a scatter plot of response and predictor values
#'          for each predictor of the fitted model \code{fit} is plotted.
#' @param showLoess If \code{TRUE} (default), an additional loess-smoothed line is plotted.
#' @param loessLineColor The color of the loess-smoothed line. Default is \code{"red"}. Only applies, if
#'          \code{showLoess} is \code{TRUE}.
#' @param showLoessCI If \code{TRUE}, a confidence region for the loess-smoothed line
#'          will be plotted. Default is \code{FALSE}. Use \code{loessCiLevel} to specifiy the confidence level.
#'          Only applies, if \code{showLoess} is \code{TRUE}.
#' @param loessCiLevel The confidence level of the loess-line's confidence region.
#'          Only applies, if \code{showLoessCI} is \code{TRUE}. Default is 0.95.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-objects with the complete plot-list (\code{plot.list}) 
#'           as well as the data frame that were used for setting up the ggplot-objects (\code{df.list}).
#' 
#' @examples
#' data(efc)
#' fit <- lm(tot_sc_e ~ c12hour + e17age + e42dep, data=efc)
#' 
#' # reression line and scatter plot
#' sjp.reglin(fit, efc)
#'            
#' # reression line w/o scatter plot
#' sjp.reglin(fit, efc, showScatterPlot=FALSE)
#' 
#' # reression line w/o CI
#' sjp.reglin(fit, efc, showCI=FALSE)
#' 
#' @import ggplot2
#' @export
sjp.reglin <- function(fit,
                       data,
                       lineColor="blue",
                       showCI=TRUE,
                       ciLevel=0.95,
                       pointAlpha=0.2,
                       pointColor="black",
                       showScatterPlot=TRUE,
                       showLoess=TRUE,
                       loessLineColor="red",
                       showLoessCI=FALSE,
                       loessCiLevel=0.95,
                       printPlot=TRUE) {
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
  # retrieve name of dependent variable
  # -----------------------------------------------------------
  response <- as.character(attr(fit$terms, "predvars")[[2]])
  # -----------------------------------------------------------
  # retrieve column names of dataset so we can identify in which
  # column the data for each predictor is.
  # -----------------------------------------------------------
  cn <- colnames(data)
  # init return var
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # iterate all predictors
  # -----------------------------------------------------------
  for (j in 1:predvars.length) {
    # -----------------------------------------------------------
    # retrieve each single predictor
    # -----------------------------------------------------------
    xval <- predvars[j]
    # -----------------------------------------------------------
    # create dummy-data frame with response and predictor
    # as data columns, used for the ggplot
    # -----------------------------------------------------------
    mydat <- as.data.frame(cbind(data[,which(cn==xval)],
                                 data[,which(cn==response)]))
    # -----------------------------------------------------------
    # plot regression line and confidence intervall
    # -----------------------------------------------------------
    reglinplot <- ggplot(mydat, aes(x=V1, y=V2))
    # -----------------------------------------------------------
    # plot jittered values if requested
    # -----------------------------------------------------------
    if (showScatterPlot) {
      reglinplot <- reglinplot + geom_jitter(alpha=pointAlpha, colour=pointColor)
    }
    # -----------------------------------------------------------
    # check whether confidence region should be plotted
    # -----------------------------------------------------------
    if (showCI) {
      reglinplot <- reglinplot + 
        stat_smooth(method="lm", level=ciLevel, colour=lineColor)
    }
    else {
      reglinplot <- reglinplot + 
        stat_smooth(method="lm", se=FALSE, colour=lineColor)
    }
    # -----------------------------------------------------------
    # check whether additional loess-line should be plotted
    # -----------------------------------------------------------
    if (showLoess) {
      # plot loess with CI
      if (showLoessCI) {
        reglinplot <- reglinplot + 
          stat_smooth(method="loess", level=loessCiLevel, colour=loessLineColor)
      }
      # plot loess w/o CI
      else {
        reglinplot <- reglinplot + 
          stat_smooth(method="loess", se=FALSE, colour=loessLineColor)
      }
    }
    # -----------------------------------------------------------
    # set plot labs
    # -----------------------------------------------------------
    reglinplot <- reglinplot + 
      labs(x=colnames(data)[which(cn==xval)],
      y=colnames(data)[which(cn==response)])
    # ---------------------------------------------------------
    # Check whether ggplot object should be returned or plotted
    # ---------------------------------------------------------
    # concatenate plot object
    plotlist[[length(plotlist)+1]] <- reglinplot
    dflist[[length(dflist)+1]] <- mydat
    # print plot
    if (printPlot) print(reglinplot)
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpreglin",
                       list(plot.list = plotlist,
                            df.list = dflist)))
}


#' @title Plot model assumptions of lm's
#' @name sjp.lm.ma
#' 
#' @description Plots model assumptions of linear models to verify if linear regression is applicable
#' 
#' @seealso \code{\link{sjp.lm}} \cr
#'          \code{\link{sjp.reglin}} \cr
#'          \code{\link{sjp.lm.int}}
#'          
#' @param linreg a fitted lm-model
#' @param showOriginalModelOnly if \code{TRUE} (default), only the model assumptions of the fitted model
#'   \code{linreg} are plotted. if \code{FALSE}, the model assumptions of an updated model where outliers
#'   are automatically excluded are also plotted.
#' @param completeDiagnostic if \code{TRUE}, additional tests are performed. Default is \code{FALSE}
#' @return an updated fitted linear model where outliers are dropped out.
#' 
#' @examples
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#' fit.updated <- sjp.lm.ma(fit)
#' 
#' @importFrom lmtest bptest
#' @importFrom car outlierTest crPlots durbinWatsonTest leveragePlots ncvTest spreadLevelPlot
#' @export
sjp.lm.ma <- function(linreg, showOriginalModelOnly=TRUE, completeDiagnostic=FALSE) {
  # ---------------------------------
  # remove outliers
  # ---------------------------------
  # copy current model
  model <- linreg
  # get r2
  rs <- summary(model)$r.squared
  # maximum loops
  maxloops <- 10
  maxcnt <- maxloops
  # remember how many cases have been removed
  removedcases <- 0
  loop <- TRUE
  # start loop
  while(loop==TRUE) {
    # get outliers of model
    ol <- outlierTest(model)
    # retrieve variable numbers of outliers
    vars <- as.numeric(attr(ol$p, "names"))
    # update model by removing outliers
    dummymodel <- update(model, subset=-c(vars))
    # retrieve new r2
    dummyrs <- summary(dummymodel)$r.squared
    # decrease maximum loops
    maxcnt <- maxcnt -1
    # check whether r2 of updated model is lower
    # than previous r2 or if we have already all loop-steps done,
    # stop loop
    if(dummyrs<rs || maxcnt<1) {
      loop <- FALSE
    }
    else {
      # else copy new model, which is the better one (according to r2)
      model <- dummymodel
      # and get new r2
      rs <- dummyrs
      # count removed cases
      removedcases <- removedcases + length(vars)
    }
  }
  # ---------------------------------
  # print steps from original to updated model
  # ---------------------------------
  cat(sprintf(("\nRemoved %i cases during %i step(s).\nR-square/adj. R-square of original model: %f / %f\nR-square/adj. R-square of updated model: %f / %f\n\n"), 
              removedcases,
              maxloops-(maxcnt+1), 
              summary(linreg)$r.squared, 
              summary(linreg)$adj.r.squared,
              summary(model)$r.squared, 
              summary(model)$adj.r.squared))
  modelOptmized <- ifelse(removedcases>0, TRUE, FALSE)
  if (showOriginalModelOnly) modelOptmized <- FALSE
  # ---------------------------------
  # show VIF-Values
  # ---------------------------------
  sjp.vif(linreg)
  if (modelOptmized) sjp.vif(model)
  # ---------------------------------
  # Print non-normality of residuals and outliers both of original and updated model
  # dots should be plotted along the line, this the dots should follow a linear direction
  # ---------------------------------
  y <- quantile(linreg$resid[!is.na(linreg$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  interc <- y[1L] - slope * x[1L]
  print(ggplot(linreg, aes(sample=.stdresid)) + 
          stat_qq() + 
          geom_abline(slope=slope, intercept=interc, color="blue") +
          ggtitle("Non-normality of residuals and outliers (original model)\n(Dots should be plotted along the line)"))
  if (modelOptmized) {
    y <- quantile(model$resid[!is.na(model$resid)], c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    interc <- y[1L] - slope * x[1L]
    print(ggplot(model, aes(sample=.stdresid)) + 
            stat_qq() + 
            geom_abline(slope=slope, intercept=interc, color="blue") +
            ggtitle("Non-normality of residuals and outliers (updated model)\n(Dots should be plotted along the line)"))
  }
  # ---------------------------------
  # Print non-normality of residuals both of original and updated model
  # Distribution should look like normal curve
  # ---------------------------------
  print(ggplot(linreg, aes(x=.resid)) + 
          geom_histogram(aes(y=..density..), binwidth=0.2, fill="grey60", colour="grey30") +
          geom_density(aes(y=..density..), fill="#4080cc", alpha=0.2) +
          stat_function(fun=dnorm, args=list(mean=mean(unname(linreg$residuals), na.rm=TRUE), sd=sd(unname(linreg$residuals), na.rm=TRUE)), colour="FireBrick", size=0.8) +
          ggtitle("Non-normality of residuals (original model)\n(Distribution should look like normal curve)"))
  if (modelOptmized) {
    print(ggplot(model, aes(x=.resid)) + 
            geom_histogram(aes(y=..density..), binwidth=0.2, fill="grey60", colour="grey30") +
            geom_density(aes(y=..density..), fill="#4080cc", alpha=0.2) +
            stat_function(fun=dnorm, args=list(mean=mean(unname(model$residuals), na.rm=TRUE), sd=sd(unname(model$residuals), na.rm=TRUE)), colour="FireBrick", size=0.8) +
            ggtitle("Non-normality of residuals (updated model)\n(Distribution should look like normal curve)"))
  }
  # ---------------------------------
  # Non-constant residuals
  # ---------------------------------
  # Frage: Können hohe Werte auf der X-Achse genauso gut hervorgesagt
  # werden wie niedrige Werte auf der X-Achse? Das Muster muss sich ähneln
  # über den Verlauf der X-Achse
  #
  # The linearity assumption is supported to the extent that the amount 
  # of points scattered above and below the line is equal.
  #
  # A linear trend would mean that the error of the model (the difference between observed and fitted values) 
  # is in some way systematic. If, for instance, lower fitted values have residuals that are more towards the 0 line. 
  # Higher fitted values are consistently more off, so the model is more wrong with larger values. So, ideally what 
  # you want is something that is akin towards a horizontal line. In such case, the data is somehow not homogenous 
  # maybe because one part of the data is more variable than another. If that is the case, you might need to transform 
  # the data in order to make it meet the assumptions that are necessary for linear models.  
  print(ggplot(linreg, aes(x=.fitted, y=.resid)) +
          geom_hline(yintercept=0, alpha=0.7) +
          geom_point() +
          geom_smooth(se=FALSE) +
          ggtitle("Homoscedasticity (homogeneity of variance,\nrandomly distributed residuals, original model)\n(Amount and distance of points scattered above/below line is equal)"))
  
  if (modelOptmized) {
    print(ggplot(model, aes(x=.fitted, y=.resid)) +
            geom_hline(yintercept=0, alpha=0.7) +
            geom_point() +
            geom_smooth(se=FALSE) +
            ggtitle("Homoscedasticity (homogeneity of variance,\nrandomly distributed residuals, updated model)\n(Amount and distance of points scattered above/below line is equal)"))
  }
  # ---------------------------------
  # summarize old and new model
  # ---------------------------------
  sjp.lm(linreg, title="Original model")
  if (modelOptmized) sjp.lm(model, title="Updated model")
  if (completeDiagnostic) {
    # ---------------------------------
    # Non-linearity
    # ---------------------------------
    plot(crPlots(linreg))
    # ---------------------------------
    # non-independence of residuals
    # ---------------------------------
    print(durbinWatsonTest(linreg))
    # ---------------------------------
    # Print leverage plots
    # ---------------------------------
    plot(leveragePlots(linreg))
    # ---------------------------------
    # Non-constant residuals
    # ---------------------------------
    print(ncvTest(linreg))
    print(bptest(linreg))
    print(spreadLevelPlot(linreg))
  }
  # return updated model
  return(model)
}


#' @title Plot regression line of fitted lm
#' @name sjp.lm1
#' 
#' @description Plot a regression line with confidence interval for a fitted model with only
#'                one predictor (i.e. \code{lm(y~y)}).
#'                This function may plot two lines: The resulting linear regression line
#'                including confidence interval (in blue) by default, and a loess-smoothed line without
#'                confidence interval (in red) if parameter \code{showLoess} is \code{TRUE}.
#'                The better the linear relationship of predictor and response is, the more both lines should overlap
#'                (i.e. the red loess-smoothed line is almost linear). \cr \cr
#'                Furthermore, a scatter plot of response and predictor values
#'                is plotted.
#'                
#' @seealso \code{\link{sjp.lm}} \cr
#'          \code{\link{sjp.reglin}} \cr
#'          \code{\link{sjp.scatter}}
#'          
#' @param fit The model of the linear regression (lm-Object).
#' @param data The data/dataset/dataframe used to fit the model
#' @param title Diagram's title as string.
#'          Example: \code{title=c("my title")}
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param axisLabel.x Labels of the predictor (independent variable) that is used for labelling the
#'          axis. Passed as string.
#'          Example: \code{axisLabel.x=c("My Predictor Var")}.
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getVariableLabels}} function, you receive a
#'          character vector with variable label strings. You can use it like so:
#'          \code{axisLabel.x=sji.getVariableLabels(efc)['quol_5']}
#' @param axisLabel.y Labels of the response (dependent variable) that is used for labelling the
#'          axis. Passed as string.
#'          Example: \code{axisLabel.y=c("My Depdendent Var")}.
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getVariableLabels}} function, you receive a
#'          character vector with variable label strings. You can use it like so:
#'          \code{axisLabel.y=sji.getVariableLabels(efc)['neg_c_7']}
#' @param breakLabelsAt Wordwrap for axis labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param axisLabelSize The size of axis tick marks Default is 1.1.
#' @param axisLabelColor The color of the axis tick marks. Default is a dark grey (grey30).
#' @param axisTitleColor The color of the axis labels (response and predictor label). Default is a dark grey.
#' @param axisTitleSize The size of the axis label (response and predictor label). Default is 1.4.
#' @param lineColor The color of the regression line. Default is \code{"blue"}.
#' @param modsumLabelColor Colour of the model summary inside the diagrams. Only applies, when parameter
#'          \code{showModelSummary} is set to \code{TRUE}. Use any valid colour value, e.g. \code{modsumLabelColor="grey50"} or
#'          \code{valueLabelColor=c("#cc3366")}.
#' @param modsumLabelSize Size of the model summary text. Default is 4.5. Recommended Values range from
#'          2 to 8
#' @param showCI If \code{TRUE} (default), a confidence region for the regression line
#'          will be plotted. Use \code{ciLevel} to specifiy the confidence level.
#' @param ciLevel The confidence level of the confidence region. Only applies when
#'          \code{showCI} is \code{TRUE}. Default is 0.95.
#' @param pointAlpha The alpha values of the scatter plot's point-geoms.
#'          Default is 0.2.
#' @param pointColor The color of the scatter plot's point-geoms. Only applies when \code{showScatterPlot}
#'          is \code{TRUE}. Default is \code{"black"}.
#' @param showScatterPlot If \code{TRUE} (default), a scatter plot of response and predictor values
#'          for each predictor of the fitted model \code{fit} is plotted.
#' @param showLoess If \code{TRUE}, an additional loess-smoothed line is plotted.
#' @param loessLineColor The color of the loess-smoothed line. Default is \code{"red"}. Only applies, if
#'          \code{showLoess} is \code{TRUE}.
#' @param showLoessCI If \code{TRUE}, a confidence region for the loess-smoothed line
#'          will be plotted. Default is \code{FALSE}. Use \code{loessCiLevel} to specifiy the confidence level.
#'          Only applies, if \code{showLoess} is \code{TRUE}.
#' @param loessCiLevel The confidence level of the loess-line's confidence region.
#'          Only applies, if \code{showLoessCI} is \code{TRUE}. Default is 0.95.
#' @param showModelSummary If \code{TRUE} (default), a summary of the regression model with 
#'          Intercept, R-square, F-Test and AIC-value is printed to the lower right corner
#'          of the diagram.
#' @param borderColor User defined color of whole diagram border (panel border).
#' @param axisColor User defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border).
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids.
#'          \itemize{
#'          \item Use \code{"bw"} for a white background with gray grids
#'          \item \code{"classic"} for a classic theme (black border, no grids)
#'          \item \code{"minimal"} for a minimalistic theme (no border,gray grids) or 
#'          \item \code{"none"} for no borders, grids and ticks.
#'          }
#' @param majorGridColor Specifies the color of the major grid lines of the diagram background.
#' @param minorGridColor Specifies the color of the minor grid lines of the diagram background.
#' @param hideGrid.x If \code{TRUE}, the x-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param hideGrid.y If \code{TRUE}, the y-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @examples
#' # load sample data
#' data(efc)
#' # fit model
#' fit <- lm(neg_c_7 ~ quol_5, data=efc, na.action=na.omit)
#' # plot regression line
#' sjp.lm1(fit, efc)
#' # plot regression line with label strings
#' sjp.lm1(fit,
#'         efc, 
#'         axisLabel.x=sji.getVariableLabels(efc)['quol_5'],
#'         axisLabel.y=sji.getVariableLabels(efc)['neg_c_7'],
#'         showLoess=TRUE)
#' 
#' @import ggplot2
#' @export
sjp.lm1 <- function(fit,
                   data,
                   title=NULL,
                   titleSize=1.3,
                   titleColor="black",
                   breakTitleAt=50, 
                   axisLabel.x=NULL,
                   axisLabel.y=NULL,
                   breakLabelsAt=12,
                   axisLabelSize=1.1,
                   axisLabelColor="gray30",
                   axisTitleSize=1.4,
                   axisTitleColor=c("#444444"),
                   lineColor="blue",
                   modsumLabelColor="grey20",
                   modsumLabelSize=4.5,
                   showCI=TRUE,
                   ciLevel=0.95,
                   pointAlpha=0.2,
                   pointColor="black",
                   showScatterPlot=TRUE,
                   showLoess=FALSE,
                   loessLineColor="red",
                   showLoessCI=FALSE,
                   loessCiLevel=0.95,
                   showModelSummary=TRUE,
                   borderColor=NULL, 
                   axisColor=NULL, 
                   theme=NULL,
                   majorGridColor=NULL,
                   minorGridColor=NULL,
                   hideGrid.x=FALSE,
                   hideGrid.y=FALSE,
                   showTickMarks=TRUE,
                   printPlot=TRUE) {
  # -----------------------------------------------------------
  # retrieve amount of predictor variables
  # -----------------------------------------------------------
  listpv <- attr(fit$terms,"predvars")
  predvars <- c()
  # -----------------------------------------------------------
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  # -----------------------------------------------------------
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)    
  }
  # -----------------------------------------------------------
  # remove first two elements (including dependent variable)
  # -----------------------------------------------------------
  for (i in 3:length(listpv)) {
    predvars <- c(predvars, listpv[[i]])
  }
  # -----------------------------------------------------------
  # remember length of predictor variables
  # -----------------------------------------------------------
  predvars.length <- length(predvars)
  # -----------------------------------------------------------
  # this function requires a fitted model with only one predictor,
  # so check whether only one predictor was used
  # -----------------------------------------------------------
  if (predvars.length>1) {
    stop("Only one predictor is allowed in fitted model. Formula y=b*x is plotted.", call.=FALSE)
  }
  # -----------------------------------------------------------
  # retrieve name of dependent variable
  # -----------------------------------------------------------
  response <- as.character(attr(fit$terms, "predvars")[[2]])
  # -----------------------------------------------------------
  # retrieve column names of dataset so we can identify in which
  # column the data for each predictor is.
  # -----------------------------------------------------------
  cn <- colnames(data)
  # -----------------------------------------------------------
  # retrieve each single predictor
  # -----------------------------------------------------------
  xval <- predvars[[1]]
  # -----------------------------------------------------------
  # create dummy-data frame with response and predictor
  # as data columns, used for the ggplot
  # -----------------------------------------------------------
  mydat <- as.data.frame(cbind(data[,which(cn==xval)],
                               data[,which(cn==response)]))
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    modsum <- sju.modsum.lm(fit)
  }
  # ----------------------------
  # prepare axis labels
  # ----------------------------
  if (is.null(axisLabel.x)) {
    axisLabel.x=colnames(data)[which(cn==xval)]
  }
  if (is.null(axisLabel.y)) {
    axisLabel.y=colnames(data)[which(cn==response)]
  }
  # check length of axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  axisLabel.x <- sju.wordwrap(axisLabel.x, breakLabelsAt)    
  axisLabel.y <- sju.wordwrap(axisLabel.y, breakLabelsAt)    
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
  # Set up visibility of tick marks
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  # -----------------------------------------------------------
  # plot regression line and confidence intervall
  # -----------------------------------------------------------
  reglinplot <- ggplot(mydat, aes(x=V1, y=V2))
  # -----------------------------------------------------------
  # plot jittered values if requested
  # -----------------------------------------------------------
  if (showScatterPlot) {
    reglinplot <- reglinplot + geom_jitter(alpha=pointAlpha, colour=pointColor)
  }
  # -----------------------------------------------------------
  # check whether confidence region should be plotted
  # -----------------------------------------------------------
  if (showCI) {
    reglinplot <- reglinplot + 
      stat_smooth(method="lm", level=ciLevel, colour=lineColor)
  }
  else {
    reglinplot <- reglinplot + 
      stat_smooth(method="lm", se=FALSE, colour=lineColor)
  }
  # -----------------------------------------------------------
  # check whether additional loess-line should be plotted
  # -----------------------------------------------------------
  if (showLoess) {
    # plot loess with CI
    if (showLoessCI) {
      reglinplot <- reglinplot + 
        stat_smooth(method="loess", level=loessCiLevel, colour=loessLineColor)
    }
    # plot loess w/o CI
    else {
      reglinplot <- reglinplot + 
        stat_smooth(method="loess", se=FALSE, colour=loessLineColor)
    }
  }
  # -----------------------------------------------------------
  # set plot labs
  # -----------------------------------------------------------
  reglinplot <- reglinplot + 
    labs(title=title,
         x=axisLabel.x,
         y=axisLabel.y) + 
    ggtheme +
    # set axes text and title
    theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
          axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
          plot.title = element_text(size=rel(titleSize), colour=titleColor))
  # -----------------------------------------------------------
  # prepare border and grid colors
  # -----------------------------------------------------------
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      reglinplot <- reglinplot + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      cat("\nParameter 'borderColor' can only be applied to 'bw' theme.\n")
    }
  }
  if (!is.null(axisColor)) {
    reglinplot <- reglinplot + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    reglinplot <- reglinplot + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    reglinplot <- reglinplot + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideGrid.x) {
    reglinplot <- reglinplot + 
      theme(panel.grid.major.x = hidegrid,
            panel.grid.minor.x = hidegrid)
  }
  if (hideGrid.y) {
    reglinplot <- reglinplot + 
      theme(panel.grid.major.y = hidegrid,
            panel.grid.minor.y = hidegrid)
  }
  # -----------------------------------------------------------
  # check whether modelsummary should be printed
  # -----------------------------------------------------------
  if (showModelSummary) {
    # add annotations with model summary
    # annotations include intercept-value and model's r-square
    reglinplot <- reglinplot + annotate("text", label=modsum, parse=TRUE, x=-Inf, y=Inf, colour=modsumLabelColor, size=modsumLabelSize, hjust=-0.05, vjust=1.5)
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(reglinplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjplm1",
                       list(plot = reglinplot,
                            df = mydat)))
}
