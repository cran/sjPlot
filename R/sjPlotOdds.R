# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("OR", "lower", "upper", "p"))



#' @title Plot odds ratios (forest plots)
#' @name sjp.glm
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2013/03/22/plotting-lm-and-glm-models-with-ggplot-rstats/}
#'              \item \url{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/}
#'              }
#' 
#' @description Plot odds ratios (exponentiated coefficients) with confidence intervalls as bar chart or dot plot
#' @seealso \code{\link{sjp.glm.ma}}
#' 
#' @note Based on the script from surefoss:
#' \url{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/}
#'
#' @param fit The fitted model of a logistic regression (or any other \code{\link{glm}}-object).
#' @param sortOdds If \code{TRUE} (default), the odds ratios are ordered according their OR value from highest first
#'          to lowest last. Use \code{FALSE} if you don't want to change the order of the predictors.
#' @param title Diagram's title as string.
#'          Example: \code{title=c("my title")}
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param axisLabels.y Labels of the predictor variables (independent vars, odds) that are used for labelling the
#'          axis. Passed as vector of strings.
#'          Example: \code{axisLabels.y=c("Label1", "Label2", "Label3")}
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getValueLabels}} function, you receive a
#'          \code{list} object with label string. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param axisLabelSize The size of value labels in the diagram. Default is 1.1, recommended values range
#'          between 0.7 and 3.0
#' @param showAxisLabels.y Whether odds names (predictor labels) should be shown or not.
#' @param showTickMarks Whether tick marks of axes should be shown or not.
#' @param axisTitle.x A label ("title") for the x axis.
#' @param axisTitleColor The color of the x axis label.
#' @param axisTitleSize The size of the x axis label.
#' @param axisLimits Defines the range of the axis where the beta coefficients and their confidence intervalls
#'          are drawn. By default, the limits range from the lowest confidence interval to the highest one, so
#'          the diagram has maximum zoom. Use your own values as 2-value-vector, for instance: \code{limits=c(-0.8,0.8)}.
#' @param axisLabelAngle.x Angle for axis-labels where the odds ratios are printed. Note
#'          that due to the coordinate flip, the acutal y-axis with odds ratio labels are appearing on the x-axis.
#' @param axisLabelAngle.y Angle for axis-labels where the predictor labels (\code{axisLabels.y}) are printed. Note
#'          that due to the coordinate flip, the acutal x-axis with predictor labels are appearing on the y-axis.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is 0.5
#' @param transformTicks if \code{TRUE}, the grid bars have exponential distances, i.e. they
#'          visually have the same distance from one grid bar to the next. Default is \code{FALSE} which
#'          means that grids are plotted on every \code{gridBreaksAt}'s position, thus the grid bars
#'          become narrower with higher odds ratio values.
#' @param type Indicates Whether the Odds Ratios should be plotted as \code{"dots"} (aka forest plots, default)
#'          or as \code{"bars"}.
#' @param hideErrorBars If \code{TRUE}, the error bars that indicate the confidence intervals of the odds ratios are not
#'          shown. Only applies if parameter \code{type} is \code{bars}. Default value is \code{FALSE}.
#' @param pointSize The size of the points that indicate the beta-value. Default is 3.
#' @param barColor A vector with colors for representing the odds values (i.e. points and error bars in case the
#'          parameter \code{type} is \code{"dots"} or the bar charts in case of \code{"bars"}). The first color value indicates
#'          odds ratio values larger than 1, the second color value indicates odds ratio values lower or equal to 1.
#'          Default colors is a blue/red-scheme. You can also use:
#'          \itemize{
#'            \item \code{"bw"} or \code{"black"} for only one colouring in almost black
#'            \item \code{"gray"}, \code{"grey"} or \code{"gs"} for a grayscale
#'            \item \code{"brewer"} for colours from the color brewer palette.
#'            }
#'          If barColors is \code{"brewer"}, use the \code{colorPalette} parameter to specify a palette of the \url{http://colorbrewer2.org}
#'          Else specify your own color values as vector (e.g. \code{barColors=c("#f00000", "#00ff00")}).
#' @param colorPalette If parameter \code{barColor} is \code{brewer}, specify a color palette from the \url{http://colorbrewer2.org} here.
#'          All color brewer palettes supported by ggplot are accepted here.
#' @param barWidth The width of the bars in bar charts. only applies if parameter \code{type} is \code{bars}. Default is 0.5
#' @param barAlpha The alpha value of the bars in bar charts. only applies if parameter \code{type} is \code{bars}. Default is 1
#' @param axisLabelColor Colour of the tick labels at the axis (variable names, odds names).
#' @param valueLabelColor The colour of the odds values. These values are printed above the plots respectively beside the
#'          bar charts. default color is \code{"black"}.
#' @param valueLabelSize Size of the value labels. Drfault is 4.5. Recommended Values range from
#'          2 to 8
#' @param valueLabelAlpha The alpha level (transparancy) of the value labels. Default is 1, use
#'          any value from 0 to 1.
#' @param axisColor User defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border).
#' @param borderColor User defined color of whole diagram border (panel border).
#' @param barOutline If \code{TRUE}, each bar gets a colored outline. only applies if parameter \code{type} is \code{bars}.
#'          Default is \code{FALSE}.
#' @param barOutlineColor The color of the bar outline. Only applies, if \code{barOutline} is set to \code{TRUE}.
#'          Default is black.
#' @param interceptLineType The linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor The color of the intercept line. Default value is \code{"grey70"}.
#' @param errorBarWidth The width of the error bar ends. Default is \code{0}
#' @param errorBarSize The size (thickness) of the error bars. Default is \code{0.8}
#' @param errorBarLineType The linetype of error bars. Default is \code{1} (solid line).
#' @param majorGridColor Specifies the color of the major grid lines of the diagram background.
#' @param minorGridColor Specifies the color of the minor grid lines of the diagram background.
#' @param hideGrid.x If \code{TRUE}, the x-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param hideGrid.y If \code{TRUE}, the y-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids.
#'          \itemize{
#'          \item Use \code{"bw"} for a white background with gray grids
#'          \item \code{"classic"} for a classic theme (black border, no grids)
#'          \item \code{"minimal"} for a minimalistic theme (no border,gray grids)
#'          \item \code{"none"} for no borders, grids and ticks or
#'          \item \code{"themr"} if you are using the \code{ggthemr} package (in such cases, you may use the \code{ggthemr::swatch} function to retrieve theme-colors for the \code{barColor} parameter)
#'          }
#'          See \url{http://rpubs.com/sjPlot/custplot} for details and examples.
#' @param flipCoordinates If \code{TRUE} (default), predictors are plotted on the left y-axis and estimate
#'          values are plotted on the x-axis.
#' @param showIntercept If \code{TRUE}, the intercept of the fitted model is also plotted.
#'          Default is \code{FALSE}. Please note that due to exp-transformation of
#'          estimates, the intercept in some cases can not be calculated, thus the
#'          function call is interrupted and no plot printed.
#' @param showValueLabels Whether the beta and standardized beta values should be plotted 
#'          to each dot or not.
#' @param labelDigits The amount of digits for rounding the estimations (see \code{showValueLabels}).
#'          Default is 2, i.e. estimators have 2 digits after decimal point.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not.
#' @param showModelSummary If \code{TRUE} (default), a summary of the regression model with 
#'          Intercept, R-square, F-Test and AIC-value is printed to the lower right corner
#'          of the diagram.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#'          
#' @examples
#' # prepare dichotomous dependent variable
#' y <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
#' 
#' # fit model
#' fitOR <- glm(y ~ swiss$Education + swiss$Examination + swiss$Infant.Mortality + swiss$Catholic,
#'              family=binomial(link="logit"))
#' 
#' # print Odds Ratios as dots
#' sjp.glm(fitOR)
#' 
#' # print Odds Ratios as bars
#' sjp.glm(fitOR, type="bars")
#' 
#' 
#' # -------------------------------
#' # Predictors for negative impact
#' # of care. Data from the EUROFAMCARE
#' # sample dataset
#' # -------------------------------
#' data(efc)
#' # retrieve predictor variable labels
#' labs <- sji.getVariableLabels(efc)
#' predlab <- c(labs[['c161sex']],
#'              labs[['e42dep']],
#'              paste0(labs[['c172code']], " (mid)"),
#'              paste0(labs[['c172code']], " (high)"))
#' # create binary response
#' y <- ifelse(efc$neg_c_7<median(na.omit(efc$neg_c_7)), 0, 1)
#' # create dummy variables for educational status
#' edu.mid <- ifelse(efc$c172code==2, 1, 0)
#' edu.high <- ifelse(efc$c172code==3, 1, 0)
#' # create data frame for fitted model
#' df <- na.omit(as.data.frame(cbind(y,
#'                 as.factor(efc$c161sex),
#'                 as.factor(efc$e42dep),
#'                 as.factor(edu.mid),
#'                 as.factor(edu.high))))
#' # fit model
#' fit <- glm(y ~., data=df, family=binomial(link="logit"))
#' # plot odds
#' sjp.glm(fit, title=labs[['neg_c_7']], axisLabels.y=predlab)
#' 
#' @import ggplot2
#' @export
sjp.glm <- function(fit, 
                    sortOdds=TRUE,
                    title=NULL,
                    titleSize=1.3,
                    titleColor="black",
                    axisLabels.y=NULL, 
                    axisLabelSize=1.1,
                    axisLabelAngle.x=0, 
                    axisLabelAngle.y=0, 
                    axisLabelColor="gray30", 
                    axisTitle.x="Odds Ratios",
                    axisTitleSize=1.2,
                    axisTitleColor=c("#444444"),
                    axisLimits=NULL,
                    breakTitleAt=50, 
                    breakLabelsAt=12, 
                    gridBreaksAt=0.5,
                    transformTicks=FALSE,
                    type="dots",
                    hideErrorBars=FALSE,
                    errorBarWidth=0,
                    errorBarSize=0.8,
                    errorBarLineType=1,
                    pointSize=3,
                    colorPalette="Paired",
                    barColor=NULL,
                    barWidth=0.3,
                    barAlpha=1,
                    valueLabelColor="black",
                    valueLabelSize=4.5,
                    valueLabelAlpha=1,
                    axisColor=NULL, 
                    borderColor=NULL, 
                    barOutline=FALSE, 
                    barOutlineColor="black", 
                    interceptLineType=2,
                    interceptLineColor="grey70",
                    majorGridColor=NULL,
                    minorGridColor=NULL,
                    hideGrid.x=FALSE,
                    hideGrid.y=FALSE,
                    theme=NULL,
                    flipCoordinates=TRUE,
                    showIntercept=FALSE,
                    showAxisLabels.y=TRUE,
                    showTickMarks=TRUE,
                    showValueLabels=TRUE, 
                    labelDigits=2,
                    showPValueLabels=TRUE,
                    showModelSummary=TRUE,
                    printPlot=TRUE) {
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) {
    axisLabels.y <- unlistlabels(axisLabels.y)
  }
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)    
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) {
    axisTitle.x <- sju.wordwrap(axisTitle.x, breakTitleAt)    
  }
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- sju.wordwrap(axisLabels.y, breakLabelsAt)    
  }
  # create data frame for ggplot
  tmp <- data.frame(cbind(exp(coef(fit)), exp(confint(fit))))
  # ----------------------------
  # print p-values in bar charts
  # ----------------------------
  # retrieve sigificance level of independent variables (p-values)
  pv <- coef(summary(fit))[,4]
  # for better readability, convert p-values to asterisks
  # with:
  # p < 0.001 = ***
  # p < 0.01 = **
  # p < 0.05 = *
  # retrieve odds ratios
  ov <- exp(coef(fit))
  # init data column for p-values
  ps <- NULL
  for (i in 1:length(pv)) {
    ps[i] <- c("")
  }
  # ----------------------------
  # copy OR-values into data column
  # ----------------------------
  if (showValueLabels) {
    for (i in 1:length(pv)) {
      ps[i] <- sprintf("%.*f", labelDigits, ov[i])
    }
  }
  # ----------------------------
  # copy p-values into data column
  # ----------------------------
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
  # ----------------------------
  # remove intercept
  # ----------------------------
  odds <- cbind(tmp[-1,])
  # ----------------------------
  # retrieve odds ratios, without intercept. now we can order
  # the predictors according to their OR value, while the intercept
  # is always shown on top
  # ----------------------------
  ov <- exp(coef(fit))[-1]
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(axisLabels.y)) {
    axisLabels.y <- row.names(odds)
  }
  # ----------------------------
  # sort labels descending in order of
  # odds ratio values
  # This is necessary because the OR-values are reorderd by size
  # in the ggplot function below
  # ----------------------------
  if (sortOdds) {
    axisLabels.y <- axisLabels.y[order(ov)]
  }
  # ----------------------------
  # bind p-values to data frame
  # ----------------------------
  odds <- cbind(odds, ps[-1])
  # we repeat the whole procedure for our
  # tmp-data frame as well, since this data frame
  # contains the intercepts. We than later just copy the
  # intercept row to our odds-data frame, if needed. The intercept
  # is not included from the beginning, because when sorting the OR values,
  # the intercept should not be sorted, but alway placed on top
  tmp <- cbind(tmp, ps)
  # set column names
  names(odds) <- c("OR", "lower", "upper", "p")
  names(tmp) <- c("OR", "lower", "upper", "p")
  lhj <- ifelse(odds$OR>1, 1.3, -0.3)
  odds <- cbind(odds, labhjust=lhj)
  lhj <- ifelse(tmp$OR>1, 1.3, -0.3)
  tmp <- cbind(tmp, labhjust=lhj)
  # ----------------------------
  # Create new variable. Needed for sorting the variables / OR
  # in the graph (see reorder in ggplot-function)
  # ----------------------------
  tmp$vars <- as.factor(c(nrow(tmp)))
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user defined range
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    # if intercept is shown, we have to adjuste the axis limits to max/min
    # values of odds ratios AND intercept
    if (showIntercept) {
      rdf <- tmp
    }
    # else, we have to adjuste the axis limits to max/min
    # values just of odds ratios
    else {
      rdf <- odds
    }
    # check whether we have bar chart and error bars hidden
    # in this case, the upper limit does not correspond to the
    # upper CI, but to the highest OR value
    if (type=="bars" && hideErrorBars) {
      maxval <- max(rdf$OR)
      minval <- min(rdf$OR)
    }
    else {
      # else we have confindence intervals displayed, so
      # the range corresponds to the boundaries given by
      # the CI's
      maxval <- max(rdf$upper)
      minval <- min(rdf$lower)
    }
    upper_lim <- (ceiling(10*maxval)) / 10
    lower_lim <- (floor(10*minval)) / 10
    # give warnings when auto-limits are very low/high
    if ((minval < 0.1) || (maxval > 100)) {
      warning("Exp. coefficients and/or exp. confidence intervals may be out of printable bounds. Consider using \"axisLimits\" parameter!")
    }
  }
  else {
    # Here we have user defind axis range
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # --------------------------------------------------------
  # Define axis ticks, i.e. at which position we have grid
  # bars.
  # --------------------------------------------------------
  ticks<-c(seq(lower_lim, upper_lim, by=gridBreaksAt))
  # since the odds are plotted on a log-scale, the grid bars'
  # distance shrinks with higher odds values. to provide a visual
  # proportional distance of the grid bars, we can apply the
  # exponential-function on the tick marks
  if (transformTicks) {
    ticks <- exp(ticks)-1
    ticks <- round(ticks[which(ticks<=upper_lim)],1)
  }
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    psr <- PseudoR2(fit)
    modsum <- as.character(as.expression(
      substitute("(Intercept)" == ic * "," ~~ italic(R)[CS]^2 == r2cs * "," ~~ italic(R)[N]^2 == r2n * "," ~~ -2 * lambda == la * "," ~~ chi^2 == c2 * "," ~~ "AIC" == aic,
                 list(ic=sprintf("%.2f", exp(coef(fit)[1])),
                      r2cs=sprintf("%.3f", psr[2]),
                      r2n=sprintf("%.3f", psr[3]),
                      la=sprintf("%.2f", -2*logLik(fit)),
                      c2=sprintf("%.2f", Chisquare.glm(fit)),
                      aic=sprintf("%.2f", fit$aic)))))
    cat(sprintf("Intercept = %.2f\nR2[cs] = %.3f\nR2[n] = %.3f\nLambda = %.2f\nChi2 = %.2f\nAIC = %.2f",
            exp(coef(fit)[1]),
            psr[2],
            psr[3],
            -2*logLik(fit),
            Chisquare.glm(fit),
            fit$aic))
  }
  # --------------------------------------------------------
  # define bar / line colors
  # --------------------------------------------------------
  # if we have no odds lower than one, swicth fill colours
  # so we have the correct colour for odds > 1
  switchcolors <- ifelse (length(which(ov<1))==0, TRUE, FALSE)
  # check whether barColor is defined
  if (is.null(barColor)) {
    # define default colours
    if (switchcolors) barcols <- c("#3399cc", "#cc5544") else barcols <- c("#cc5544", "#3399cc")
  }
  else {
    # if we have b/w colors, i.e. no differentiation between odds > 1 and < 1,
    # we simply set both colors for ORs lower and greater than 1 to the same color-value
    if (barColor=="bw" || barColor=="black") {
      barcols <- c("#333333", "#333333")
    }
    # grey-scale colors
    else if (barColor=="gray" || barColor=="grey" || barColor=="gs") {
      if (switchcolors) barcols <- c("#555555", "#999999") else barcols <- c("#999999", "#555555")
    }
    else {
      # else, use user-colors
      barcols <- barColor
    }
  }
  # check whether we have brewer color scale
  if (!is.null(barColor) && barColor=="brewer") {
    # remember to specify the "colorPalette" if you use "brewer" as "oddsColorss"
    if (type=="dots") {
      # plots need scale_colour
      scalecolors <- scale_colour_brewer(palette=colorPalette, guide=FALSE)
    }
    else {
      # bars need scale_fill
      scalecolors <- scale_fill_brewer(palette=colorPalette, guide=FALSE)
    }
  }
  else {
    if (type=="dots") {
      scalecolors <- scale_colour_manual(values=barcols, guide=FALSE)
    }
    else {
      scalecolors <- scale_fill_manual(values=barcols, guide=FALSE)
    }
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
  # Set up visibility oftick marks
  # --------------------------------------------------------
  if (!showTickMarks && !is.null(ggtheme)) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showAxisLabels.y) {
    axisLabels.y <- c("")
  }
  # --------------------------------------------------------
  # check whether bars should have an outline
  # --------------------------------------------------------
  if (!barOutline) {
    barOutlineColor <- waiver()
  }
  # --------------------------------------------------------
  # Order odds according to beta-coefficients
  # --------------------------------------------------------
  if (sortOdds) {
    odds <- odds[order(ov),]
  }
  odds$vars <- cbind(c(1:nrow(odds)))
  odds$vars <- as.factor(odds$vars)
  # --------------------------------------------------------
  # check whether intercept should be shown
  # --------------------------------------------------------
  if (showIntercept) {
    odds <- data.frame(rbind(tmp[1,], odds))
    axisLabels.y <- c("Intercept", axisLabels.y)
  }
  # --------------------------------------------------------
  # body of plot, i.e. this is the same in both bar and dot plots
  # --------------------------------------------------------
  if (type=="dots") {
    # plot as dots
    plotHeader <- ggplot(odds, aes(y=OR, x=vars))
  }
  else {
    # plot as bars, fill bars according to
    # OR-value greater / lower than 1
    plotHeader <- ggplot(odds, aes(y=OR, x=vars))
  }
  # --------------------------------------------------------
  # start with dot-plotting here
  # --------------------------------------------------------
  if (type=="dots") {
    plotHeader <- plotHeader +
      # Order odds according to beta-coefficients, colour points and lines according to
      # OR-value greater / lower than 1
      geom_point(size=pointSize, aes(colour=(OR>1))) +
      # print confidence intervalls (error bars)
      geom_errorbar(aes(ymin=lower, ymax=upper, colour=(OR>1)), width=errorBarWidth, size=errorBarSize, linetype=errorBarLineType) +
      # print value labels and p-values
      geom_text(aes(label=p, y=OR), vjust=-0.7, colour=valueLabelColor, size=valueLabelSize, alpha=valueLabelAlpha)
  }
  # --------------------------------------------------------
  # start with bar plots here
  # --------------------------------------------------------
  else if (type=="bars") {
    # Order odds according to beta-coefficients, colour points and lines according to
    # OR-value greater / lower than 1
    plotHeader <- plotHeader +
      # stat-parameter indicates statistics
      # stat="bin": y-axis relates to count of variable
      # stat="identity": y-axis relates to value of variable
      geom_bar(aes(fill=(OR>1)), stat="identity", position="identity", width=barWidth, colour=barOutlineColor, alpha=barAlpha) +
      # print value labels and p-values
      geom_text(aes(label=p, y=1), vjust=-1, hjust=odds$labhjust, colour=valueLabelColor, size=valueLabelSize, alpha=valueLabelAlpha)
    if (hideErrorBars==FALSE) {
      plotHeader <- plotHeader +
        # print confidence intervalls (error bars)
      geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=errorBarWidth, size=errorBarSize, linetype=errorBarLineType)
    }
  }
  # check whether modelsummary should be printed
  if (showModelSummary) {
    # add annotations with model summary
    # here we print out the log-lik-ratio "lambda" and the chi-square significance of the model
    # compared to the null-model
    plotHeader <- plotHeader + annotate("text", label=modsum, parse=TRUE, x=-Inf, y=Inf, colour=valueLabelColor, size=valueLabelSize, alpha=valueLabelAlpha, vjust=-0.5, hjust=1.1)
  }
  plotHeader <- plotHeader +
    # Intercept-line
    geom_hline(yintercept=1, linetype=interceptLineType, color=interceptLineColor) +
    labs(title=title, x=NULL, y=axisTitle.x) +
    scale_x_discrete(labels=axisLabels.y) +
    # logarithmic scale for odds
    scale_y_log10(limits=c(lower_lim, upper_lim), breaks=ticks, labels=ticks) +
    scalecolors
  # --------------------------------------------------------
  # apply theme
  # --------------------------------------------------------
  if (!is.null(ggtheme)) {
    plotHeader <- plotHeader +
      ggtheme +
      # set axes text and 
      theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
            axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
            axis.text.x = element_text(angle=axisLabelAngle.x),
            axis.text.y = element_text(angle=axisLabelAngle.y),
            plot.title = element_text(size=rel(titleSize), colour=titleColor))
  }
  # --------------------------------------------------------
  # flip coordinates?
  # --------------------------------------------------------
  if (flipCoordinates)  {
    plotHeader <- plotHeader +
      coord_flip()
  }
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      plotHeader <- plotHeader + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      cat("\nParameter 'borderColor' can only be applied to 'bw' theme.\n")
    }
  }
  if (!is.null(axisColor)) {
    plotHeader <- plotHeader + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    plotHeader <- plotHeader + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    plotHeader <- plotHeader + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideGrid.x) {
    plotHeader <- plotHeader + 
      theme(panel.grid.major.x = hidegrid,
            panel.grid.minor.x = hidegrid)
  }
  if (hideGrid.y) {
    plotHeader <- plotHeader + 
      theme(panel.grid.major.y = hidegrid,
            panel.grid.minor.y = hidegrid)
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(plotHeader)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpglm",
                       list(plot = plotHeader,
                            df = odds)))
}


#' @title Plot model assumptions of glm's
#' @name sjp.glm.ma
#' 
#' @description Plots model assumptions of generalized linear models
#'              to verify if generalized linear regression is applicable
#' 
#' @seealso \code{\link{sjp.glm}}

#' @param logreg a fitted \code{\link{glm}}-model
#' @param showOriginalModelOnly if \code{TRUE} (default), only the model assumptions of the fitted model
#'   \code{logreg} are plotted. if \code{FALSE}, the model assumptions of an updated model where outliers
#'   are automatically excluded are also plotted.
#' @return an updated fitted generalized linear model where outliers are dropped out.
#' 
#' @examples
#' # prepare dichotomous dependent variable
#' y <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
#' 
#' # fit model
#' fitOR <- glm(y ~ swiss$Education + swiss$Examination + swiss$Infant.Mortality + swiss$Catholic,
#'              family=binomial(link="logit"))
#' 
#' # plot model assumptions
#' sjp.glm.ma(fitOR)
#' 
#' @importFrom car outlierTest influencePlot
#' @export
sjp.glm.ma <- function(logreg, showOriginalModelOnly=TRUE) {
  # ---------------------------------
  # remove outliers
  # ---------------------------------
  # copy current model
  model <- logreg
  # get AIC-Value
  aic <- logreg$aic
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
    # retrieve new AIC-value
    dummyaic <- dummymodel$aic
    # decrease maximum loops
    maxcnt <- maxcnt -1
    # check whether AIC-value of updated model is larger
    # than previous AIC-value or if we have already all loop-steps done,
    # stop loop
    if(dummyaic >= aic || maxcnt<1) {
      loop <- FALSE
    }
    else {
      # else copy new model, which is the better one (according to AIC-value)
      model <- dummymodel
      # and get new AIC-value
      aic <- dummyaic
      # count removed cases
      removedcases <- removedcases + length(vars)
    }
  }
  # ---------------------------------
  # print steps from original to updated model
  # ---------------------------------
  cat(sprintf(("\nRemoved %i cases during %i step(s).\nAIC-value of original model: %.2f\nAIC-value of updated model: %.2f\n\n"), 
              removedcases,
              maxloops-(maxcnt+1), 
              logreg$aic, 
              model$aic))
  
  modelOptmized <- ifelse(removedcases>0, TRUE, FALSE)
  if (showOriginalModelOnly) modelOptmized <- FALSE
  # ---------------------------------
  # show VIF-Values
  # ---------------------------------
  sjp.vif(logreg)
  if (modelOptmized) sjp.vif(model)
  # ------------------------------------------------------
  # Overdispersion
  # Sometimes we can get a deviance that is much larger than expected 
  # if the model was correct. It can be due to the presence of outliers, 
  # sparse data or clustering of data. A half-normal plot of the residuals 
  # can help checking for outliers:
  # ------------------------------------------------------
  halfnorm <- function (x, nlab=2, labs=as.character(1:length(x)), ylab="Sorted Data", ...) {
    x <- abs(x)
    labord <- order(x)
    x <- sort(x)
    i <- order(x)
    n <- length(x)
    ui <- qnorm((n + 1:n)/(2 * n + 1))
    plot(ui, x[i], xlab="Half-normal quantiles", ylab=ylab, ylim=c(0,max(x)), type="n", ...)
    if(nlab < n) {
      points(ui[1:(n - nlab)], x[i][1:(n - nlab)])
    }
    text(ui[(n - nlab + 1):n], x[i][(n - nlab + 1):n], labs[labord][(n - nlab + 1):n])
  }
  # show half-normal quantiles for original model
  halfnorm(residuals(logreg), main="Original model (over-/underdispersion)")
  if (!showOriginalModelOnly) {
    # show half-normal quantiles for updated model
    halfnorm(residuals(model), main="Updated model (over-/underdispersion)")
  }
  # ------------------------------------------------------
  # Influential and leverage points
  # ------------------------------------------------------
  influencePlot(logreg)
  if (!showOriginalModelOnly) {
    influencePlot(model)
  }
  # ------------------------------------------------------
  # Residual plot
  # ------------------------------------------------------
  res <- residuals(logreg, type="deviance")
  plot(log(abs(predict(logreg))), res, main="Residual plot (original model)", xlab="Log-predicted values", ylab="Deviance residuals")
  abline(h=0, lty=2)
  qqnorm(res)
  qqline(res)
  if (!showOriginalModelOnly) {
    res <- residuals(model, type="deviance")
    plot(log(abs(predict(model))), res, main="Residual plot (updated model)", xlab="Log-predicted values", ylab="Deviance residuals")
    abline(h=0, lty=2)
    qqnorm(res)
    qqline(res)
  }
  # -------------------------------------
  # Anova-Test
  # We can see that all terms were highly significant when they were 
  # introduced into the model.
  # -------------------------------------
  cat(paste("\n--------------------\nCheck significance of terms when they entered the model...\n"))
  cat(paste("\nAnova original model:\n"))
  print(anova(logreg,test="Chisq"))
  if (!showOriginalModelOnly) {
    cat(paste("\n\n\nAnova updated model:\n"))
    print(anova(model,test="Chisq"))
  }
  # -------------------------------------
  sjp.glm(logreg, title="Original model")
  if (!showOriginalModelOnly) {
    sjp.glm(model, title="Updated model")
  }
  # return updated model
  return(model)
}