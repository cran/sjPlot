#' @title Show (and compare) generalized linear models as HTML table
#' @name sjt.glm
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2013/08/20/print-glm-output-to-html-table-rstats/}
#'              }
#' 
#' @description Shows (and compares multiple) generalized linear models (Odds Ratios)
#'                as HTML table, or saves them as file. The fitted glm's should have the same predictor variables and
#'                either
#'                \itemize{
#'                \item differ only in their response (dependent variable), to see the effect of a specific set of predictors on different responses, or
#'                \item all have the same reponse variables, but differ in their \code{\link{family}} objects and link function in order to see which model fits best to the data.
#'                }
#'                See parameter \code{showFamily} for details and section \code{examples}.
#' 
#' @seealso \code{\link{sjt.lm}} \cr
#'          \code{\link{sjp.glm}}
#' 
#' @param ... One or more fitted glm-objects.
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
#' @param labelPredictors Labels of the predictor variables, provided as char vector.
#' @param labelDependentVariables Labels of the dependent variables of all fitted models
#'          which have been used as first parameter(s), provided as char vector.
#' @param stringPredictors String constant used as headline for the predictor column.
#'          Default is \code{"Predictors"}.
#' @param stringDependentVariables String constant used as headline for the 
#'          dependent variable columns. Default is \code{"Dependent Variables"}.
#' @param showHeaderStrings If \code{TRUE}, the header strings \code{stringPredictors}
#'          and \code{stringDependentVariables} are shown. By default, they're hidden.
#' @param stringModel String constant used as headline for the model names in case no 
#'          labels for the dependent variables are provided (see labelDependentVariables).
#'          Default is \code{"Model"}.
#' @param stringIntercept String constant used as headline for the Intercept row
#'          default is \code{"Intercept"}.
#' @param stringObservations String constant used in the summary row for the count of observation
#'          (cases). Default is \code{"Observations"}.
#' @param pvaluesAsNumbers If \code{TRUE}, p-values are shown as numbers. If \code{FALSE} (default),
#'          p-values are indicated by asterisks.
#' @param boldpvalues If \code{TRUE} (default), significant p-values are shown bold faced.
#' @param showConfInt If \code{TRUE} (default), the confidence intervall is also printed to the table. Use
#'          \code{FALSE} to omit the CI in the table.
#' @param showStdError If \code{TRUE}, the standard errors are also printed.
#'          Default is \code{FALSE}.
#' @param separateConfColumn if \code{TRUE}, the CI values are shown in a separate table column.
#'          Default is \code{FALSE}.
#' @param showAbbrHeadline If \code{TRUE} (default), the table data columns have a headline with 
#'          abbreviations for odds ratios, confidence interval and p-values.
#' @param showPseudoR If \code{TRUE} (default), the pseudo R2 values for each model are printed
#'          in the model summary. R2cs is the Cox-Snell-pseudo R-square value, R2n is Nagelkerke's 
#'          pseudo R-square value.
#' @param showLogLik If \code{TRUE}, the Log-Likelihood for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param showAIC If \code{TRUE}, the \code{\link{AIC}} value for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param showChi2 If \code{TRUE}, the chi-square value for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param showFamily If \code{TRUE}, the family object and link function for each fitted model
#'          are printed. Can be used in case you want to compare models with different link functions
#'          and same predictors and response, to decide which model fits best. See \code{\link{family}}
#'          for more details. It is recommended to inspect the model \code{\link{AIC}} (see \code{showAIC}) to get a
#'          decision help for which model to choose.
#' @param encoding The charset encoding used for variable and value labels. Default is \code{"UTF-8"}. Change
#'          encoding if specific chars are not properly displayed (e.g.) German umlauts).
#' @param CSS A \code{\link{list}} with user-defined style-sheet-definitions, according to the official CSS syntax (see
#'          \url{http://www.w3.org/Style/CSS/}). See return value \code{page.style} for details
#'          of all style-sheet-classnames that are used in this function. Parameters for this list need:
#'          \enumerate{
#'            \item the class-names with \code{"css."}-prefix as parameter name and
#'            \item each style-definition must end with a semicolon
#'          } 
#'          Examples:
#'          \itemize{
#'            \item \code{css.table='border:2px solid red;'} for a solid 2-pixel table border in red.
#'            \item \code{css.summary='font-weight:bold;'} for a bold fontweight in the summary row.
#'            \item \code{css.lasttablerow='border-bottom: 1px dotted blue;'} for a blue dotted border of the last table row.
#'          }
#'          See further examples below.
#' @param useViewer If \code{TRUE}, the function tries to show the HTML table in the IDE's viewer pane. If
#'          \code{FALSE} or no viewer available, the HTML table is opened in a web browser.
#' @param no.output If \code{TRUE}, the html-output is neither opened in a browser nor shown in
#'          the viewer pane and not even saved to file. This option is useful when the html output
#'          should be used in \code{knitr} documents. The html output can be accessed via the return
#'          value.
#' @return Invisibly returns a \code{\link{structure}} with
#'          \itemize{
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{output.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'
#' @note The HTML tables can either be saved as file and manually opened (specify parameter \code{file}) or
#'         they can be saved as temporary files and will be displayed in the RStudio Viewer pane (if working with RStudio)
#'         or opened with the default web browser. Displaying resp. opening a temporary file is the
#'         default behaviour (i.e. \code{file=NULL}).
#'         
#' @examples
#' # prepare dummy variables for binary logistic regression
#' y1 <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
#' y2 <- ifelse(swiss$Infant.Mortality<median(swiss$Infant.Mortality), 0, 1)
#' y3 <- ifelse(swiss$Agriculture<median(swiss$Agriculture), 0, 1)
#' 
#' # Now fit the models. Note that both models share the same predictors
#' # and only differ in their dependent variable (y1, y2 and y3)
#' fitOR1 <- glm(y1 ~ swiss$Education+swiss$Examination+swiss$Catholic,
#'               family=binomial(link="logit"))
#' fitOR2 <- glm(y2 ~ swiss$Education+swiss$Examination+swiss$Catholic,
#'               family=binomial(link="logit"))
#' fitOR3 <- glm(y3 ~ swiss$Education+swiss$Examination+swiss$Catholic,
#'               family=binomial(link="logit"))
#'
#' # open HTML-table in RStudio Viewer Pane or web browser
#' \dontrun{
#' sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Infant Mortality"),
#'         labelPredictors=c("Education", "Examination", "Catholic"))}
#' 
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # table indicating p-values as numbers
#' \dontrun{
#' sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Infant Mortality"),
#'         labelPredictors=c("Education", "Examination", "Catholic"),
#'         pvaluesAsNumbers=TRUE)}
#' 
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # printing CI in a separate column
#' \dontrun{
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         labelDependentVariables=c("Fertility", "Infant Mortality", "Agriculture"),
#'         labelPredictors=c("Education", "Examination", "Catholic"),
#'         separateConfColumn=TRUE)}
#' 
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # indicating p-values as numbers and printing CI in a separate column
#' \dontrun{
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         labelDependentVariables=c("Fertility", "Infant Mortality", "Agriculture"),
#'         labelPredictors=c("Education", "Examination", "Catholic"),
#'         pvaluesAsNumbers=TRUE, separateConfColumn=TRUE)}
#' 
#' # ---------------------------------------------------------------- 
#' # User defined style sheet
#' # ---------------------------------------------------------------- 
#' \dontrun{
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         labelDependentVariables=c("Fertility", "Infant Mortality", "Agriculture"),
#'         labelPredictors=c("Education", "Examination", "Catholic"),
#'         CSS=list(css.table="border: 2px solid;",
#'                  css.tdata="border: 1px solid;",
#'                  css.depvarhead="color:#003399;"))}
#' 
#' # ---------------------------------------------------------------- 
#' # Compare models with different link functions, but same
#' # predictors and response
#' # ---------------------------------------------------------------- 
#' # load efc sample data
#' data(efc)
#' # dichtomozize service usage by "service usage yes/no"
#' efc$services <- sju.dicho(efc$tot_sc_e, "v", 0)
#' # fit 3 models with different link-functions
#' fit1 <- glm(services ~ neg_c_7 + c161sex + e42dep, data=efc, family=binomial(link="logit"))
#' fit2 <- glm(services ~ neg_c_7 + c161sex + e42dep, data=efc, family=binomial(link="probit"))
#' fit3 <- glm(services ~ neg_c_7 + c161sex + e42dep, data=efc, family=poisson(link="log"))
#' # compare models
#' \dontrun{
#' sjt.glm(fit1, fit2, fit3, showAIC=TRUE, showFamily=TRUE, showPseudoR=FALSE)}
#' 
#' @export
sjt.glm <- function (..., 
                     file=NULL, 
                     labelPredictors=NULL, 
                     labelDependentVariables=NULL, 
                     stringPredictors="Predictors", 
                     stringDependentVariables="Dependent Variables", 
                     showHeaderStrings=FALSE,
                     stringModel="Model",
                     stringIntercept="(Intercept)",
                     stringObservations="Observations",
                     pvaluesAsNumbers=FALSE,
                     boldpvalues=TRUE,
                     showConfInt=TRUE,
                     showStdError=FALSE,
                     separateConfColumn=FALSE,
                     showAbbrHeadline=TRUE,
                     showPseudoR=TRUE,
                     showLogLik=FALSE,
                     showAIC=FALSE,
                     showChi2=FALSE,
                     showFamily=FALSE,
                     encoding="UTF-8",
                     CSS=NULL,
                     useViewer=TRUE,
                     no.output=FALSE) {
  # -------------------------------------
  # init header
  # -------------------------------------
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.summary <- "summary"
  tag.colnames <- "colnames"
  tag.firstsumrow <- "firstsumrow"
  tag.labelcellborder <- "labelcellborder"
  tag.lasttablerow <- "lasttablerow"
  tag.depvarhead <- "depvarhead"
  tag.topborder <- "topborder"
  tag.topcontentborder <- "topcontentborder"
  tag.annorow <- "annorow"
  tag.noannorow <- "noannorow"
  tag.annostyle <- "annostyle"
  tag.leftalign <- "leftalign"
  tag.centeralign <- "centeralign"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "border-bottom: 1px solid; padding:0.2cm;"
  css.tdata <- "padding:0.2cm;"
  css.summary <- "padding-top:0.1cm; padding-bottom:0.1cm;"
  css.colnames <- "font-style:italic;"
  css.firstsumrow <- "border-top:1px solid;"
  css.labelcellborder <- "border-bottom:1px solid;"
  css.lasttablerow <- "border-bottom: double;"
  css.topborder <- "border-top:double;"
  css.depvarhead <- "text-align:center; border-bottom:1px solid;"
  css.topcontentborder <- "border-top:2px solid;"
  css.annorow <- "border-top:2px solid;"
  css.noannorow <- "border-bottom:double;"
  css.annostyle <- "text-align:right;"
  css.leftalign <- "text-align:left;"
  css.centeralign <- "text-align:center;"
  # change table style if we have pvalues as numbers
  if (pvaluesAsNumbers) css.table <- sprintf("%s%s", css.table, css.noannorow)
  if (showHeaderStrings) css.labelcellborder <- ""
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- CSS[['css.table']]
    if (!is.null(CSS[['css.thead']])) css.thead <- CSS[['css.thead']]
    if (!is.null(CSS[['css.tdata']])) css.tdata <- CSS[['css.tdata']]
    if (!is.null(CSS[['css.leftalign']])) css.leftalign <- CSS[['css.leftalign']]
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- CSS[['css.centeralign']]
    if (!is.null(CSS[['css.summary']])) css.summary <- CSS[['css.summary']]
    if (!is.null(CSS[['css.labelcellborder']])) css.labelcellborder <- CSS[['css.labelcellborder']]
    if (!is.null(CSS[['css.colnames']])) css.colnames <- CSS[['css.colnames']]
    if (!is.null(CSS[['css.firstsumrow']])) css.firstsumrow <- CSS[['css.firstsumrow']]
    if (!is.null(CSS[['css.lasttablerow']])) css.lasttablerow <- CSS[['css.lasttablerow']]
    if (!is.null(CSS[['css.topborder']])) css.topborder <- CSS[['css.topborder']]
    if (!is.null(CSS[['css.depvarhead']])) css.depvarhead <- CSS[['css.depvarhead']]
    if (!is.null(CSS[['css.topcontentborder']])) css.topcontentborder <- CSS[['css.topcontentborder']]
    if (!is.null(CSS[['css.annorow']])) css.annorow <- CSS[['css.annorow']]
    if (!is.null(CSS[['css.noannorow']])) css.noannorow <- CSS[['css.noannorow']]
    if (!is.null(CSS[['css.annostyle']])) css.annostyle <- CSS[['css.annostyle']]
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                         tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata,
                         tag.summary, css.summary, tag.colnames, css.colnames,
                         tag.firstsumrow, css.firstsumrow, tag.lasttablerow, css.lasttablerow,
                         tag.topborder, css.topborder, tag.depvarhead, css.depvarhead,
                         tag.topcontentborder, css.topcontentborder, tag.annorow, css.annorow, 
                         tag.noannorow, css.noannorow, tag.annostyle, css.annostyle,
                         tag.labelcellborder, css.labelcellborder,
                         tag.centeralign, css.centeralign, tag.leftalign, css.leftalign)
  # ------------------------
  # start content
  # ------------------------
  toWrite <- paste0(toWrite, page.style)
  toWrite = paste(toWrite, "\n</head>\n<body>", "\n")
  # -------------------------------------
  # retrieve fitted models
  # -------------------------------------
  input_list <- list(...)
  # -------------------------------------
  # if confidence interval should be omitted,
  # don't use separate column for CI!
  # -------------------------------------
  if (!showConfInt) {
    separateConfColumn <- FALSE
    showCIString <- c("OR")
  }
  else {
    showCIString <- c("OR (CI)")
  }
  # -------------------------------------
  # table headline
  # -------------------------------------
  headerColSpan <- length(input_list)
  headerColSpanFactor <- 1
  if (pvaluesAsNumbers) headerColSpanFactor <- headerColSpanFactor+1
  if (separateConfColumn) headerColSpanFactor <- headerColSpanFactor+1
  if (showStdError) headerColSpanFactor <- headerColSpanFactor+1
  
  headerColSpan <- headerColSpanFactor * headerColSpan
  # -------------------------------------
  # start table
  # -------------------------------------
  page.content <- "<table>"
  # -------------------------------------
  # check if we want to see header strings
  # -------------------------------------
  if (showHeaderStrings) {
    page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata topborder\" rowspan=\"2\"><em>%s</em></td>", stringPredictors))
    page.content <- paste0(page.content, sprintf("\n    <td colspan=\"%i\" class=\"tdata topborder depvarhead\"><em>%s</em></td>", headerColSpan, stringDependentVariables))
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # table headline: label for dependent variables (model outcomes)
  # -------------------------------------
  page.content <- paste0(page.content, "<tr>")
  # -------------------------------------
  # If we don't show header strings, a rowspan-attribute is missing,
  # so we need to insert an empty cell here
  # -------------------------------------
  tcp <- ""
  if (!showHeaderStrings) {
    page.content <- paste0(page.content, "\n    <td class=\"tdata topborder\"></td>")
    tcp <- " topborder"
  }
  # -------------------------------------
  # continue with labels
  # -------------------------------------
  if (!is.null(labelDependentVariables)) {
    for (i in 1:length(labelDependentVariables)) {
      if (headerColSpanFactor>1) {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\" colspan=\"%i\">%s</td>", tcp, headerColSpanFactor, labelDependentVariables[i]))
      }
      else {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\">%s</td>", tcp, labelDependentVariables[i]))
      }
    }
    page.content <- paste0(page.content, "\n  </tr>")
  }
  else {
    for (i in 1:length(input_list)) {
      if (headerColSpanFactor>1) {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\" colspan=\"%i\">%s %i</td>", tcp, headerColSpanFactor, stringModel, i))
      }
      else {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\">%s %i</td>", tcp, stringModel, i))
      }
    }
    page.content <- paste0(page.content, "\n  </tr>")
  }
  # -------------------------------------
  # calculate coefficients and confidence intervalls
  # for all models
  # -------------------------------------
  coeffs <- c()
  confi_lower <- c()
  confi_higher <- c()
  pv <- c()
  se <- c()
  # -------------------------------------
  # retrieve data from fitted models
  # -------------------------------------
  for (i in 1:length(input_list)) {
    fit <- input_list[[i]]
    coeffs <- rbind(coeffs, exp(coef(fit)))
    confi_lower <- cbind(confi_lower, exp(confint(fit))[,1])
    confi_higher <- cbind(confi_higher, exp(confint(fit))[,2])
    pv <- cbind(pv, round(summary(fit)$coefficients[,4],3))
    # standard error
    se <- cbind(se, round(summary(fit)$coefficients[,2],2))
  }
  # -------------------------------------
  # rotate coefficients
  # -------------------------------------
  coeffs <- t(coeffs)
  # -------------------------------------
  # set default predictor labels
  # -------------------------------------
  if (is.null(labelPredictors)) {
    labelPredictors <- row.names(coeffs)[-1]
  }
  # -------------------------------------
  # prepare p-values, either as * or as numbers
  # -------------------------------------
  if (!pvaluesAsNumbers) {
    pv <- apply(pv, c(1,2), function(x) {
      if (x>=0.05) x <- c("")
      else if (x>=0.01 && x<0.05) x <- c("*")
      else if (x>=0.001 && x<0.01) x <- c("**")
      else if (x<0.001) x <- c("***")
    })
  }
  else {
    pv <- apply(pv, c(1,2), function(x) {
      if (x <0.05 && boldpvalues) {
        x <- sprintf("<b>%.3f</b>", x)      }
      else {
        x <- sprintf("%.3f", x) 
      }
    })
  }
  # -------------------------------------
  # table header: or/ci and p-labels
  # -------------------------------------
  if (showAbbrHeadline) {
    page.content <- paste0(page.content, "\n  <tr>\n    <td class=\"tdata colnames\">&nbsp;</td>")
    colnr <- ifelse(is.null(labelDependentVariables), length(input_list), length(labelDependentVariables))
    for (i in 1:colnr) {
      # confidence interval in separate column
      if (separateConfColumn) {
        page.content <- paste0(page.content, "\n    <td class=\"tdata centeralign colnames\">OR</td>")
        if (showConfInt) page.content <- paste0(page.content, "<td class=\"tdata centeralign colnames\">CI</td>")
      }
      else {
        # confidence interval in Beta-column
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames\">%s</td>", showCIString))
      }
      # show std. error
      if (showStdError) page.content <- paste0(page.content, "<td class=\"tdata centeralign colnames\">std. Error</td>")
      # show p-values as numbers in separate column
      if (pvaluesAsNumbers) page.content <- paste0(page.content, "<td class=\"tdata centeralign colnames\">p</td>")
    }
    page.content <- paste(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # close table headline
  # -------------------------------------
  page.content <- paste0(page.content, "  <tr>\n")
  # -------------------------------------
  # 1. row: intercept
  # -------------------------------------
  page.content <- paste0(page.content, sprintf("    <td class=\"tdata leftalign topcontentborder\">%s</td>", stringIntercept))
  for (i in 1:ncol(coeffs)) {
    # confidence interval in separate column
    if (separateConfColumn) {
      # open table cell for Beta-coefficient
      page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign topcontentborder\">%.2f", coeffs[1,i]))
      # if p-values are not shown as numbers, insert them after beta-value
      if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf(" %s", pv[1,i]))
      # if we have CI, start new table cell (CI in separate column)
      if (showConfInt) {
        page.content <- paste0(page.content, sprintf("</td><td class=\"tdata centeralign topcontentborder\">%.2f-%.2f</td>", confi_lower[1,i], confi_higher[1,i]))
      }
      else {
        page.content <- paste0(page.content, "</td>")
      }
    }
    else {
      # open table cell for Beta-coefficient
      page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign topcontentborder\">%.2f", coeffs[1,i]))
      # confidence interval in Beta-column
      if (showConfInt) page.content <- paste0(page.content, sprintf(" (%.2f-%.2f)", confi_lower[1,i], confi_higher[1,i]))
      # if p-values are not shown as numbers, insert them after beta-value
      if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf(" %s", pv[1,i]))
      page.content <- paste0(page.content, "</td>")
    }
    # show std. error
    if (showStdError) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign topcontentborder\">%s</td>", se[1,i]))
    # show p-values as numbers in separate column
    if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign topcontentborder\">%s</td>", pv[1,i]))
  }
  page.content <- paste0(page.content, "\n  </tr>")  
  # -------------------------------------
  # subsequent rows: pedictors
  # -------------------------------------
  predlen <- length(labelPredictors)
  for (i in 1:predlen) {
    page.content <- paste0(page.content, "\n  <tr>\n", sprintf("    <td class=\"tdata leftalign\">%s</td>", labelPredictors[i]))
    for (j in 1:ncol(coeffs)) {
      # confidence interval in separate column
      if (separateConfColumn) {
        # open table cell for Beta-coefficient
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign\">%.2f", coeffs[i+1,j]))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf(" %s", pv[i+1,j]))
        # if we have CI, start new table cell (CI in separate column)
        if (showConfInt) {
          page.content <- paste0(page.content, sprintf("</td><td class=\"tdata centeralign\">%.2f-%.2f</td>", confi_lower[i+1,j], confi_higher[i+1,j]))
        }
        else {
          page.content <- paste0(page.content, "</td>")
        }
      }
      else {
        # open table cell for Beta-coefficient
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign\">%.2f", coeffs[i+1,j]))
        # confidence interval in Beta-column
        if (showConfInt) page.content <- paste0(page.content, sprintf(" (%.2f-%.2f)", confi_lower[i+1,j], confi_higher[i+1,j]))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf(" %s", pv[i+1,j]))
        page.content <- paste0(page.content, "</td>")
      }
      # show std. error
      if (showStdError) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign\">%s</td>", se[i+1,j]))
      # show p-values as numbers in separate column
      if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata\">%s</td>", pv[i+1,j]))
    }
    page.content <- paste0(page.content, "\n  </tr>")
  }
  # -------------------------------------
  # Model-Summary: N
  # -------------------------------------
  if (headerColSpanFactor>1) {
    colspanstring <- sprintf("<td class=\"tdata centeralign summary\" colspan=\"%i\">", headerColSpanFactor)
    colspanstringfirstrow <- sprintf("<td class=\"tdata summary centeralign firstsumrow\" colspan=\"%i\">", headerColSpanFactor)
  }
  else {
    colspanstring <- c("<td class=\"tdata centeralign summary\">")
    colspanstringfirstrow <- c("<td class=\"tdata summary centeralign firstsumrow\">")
  }
  page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign firstsumrow\">%s</td>\n", stringObservations))
  for (i in 1:length(input_list)) {
    psr <- PseudoR2(input_list[[i]])
    page.content <- paste(page.content, sprintf("   %s%i</td>\n", colspanstringfirstrow, psr[1]))
  }
  page.content <- paste0(page.content, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: pseudo r2
  # -------------------------------------
  if (showPseudoR) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">Pseudo-R<sup>2</sup></td>\n")
    for (i in 1:length(input_list)) {
      psr <- PseudoR2(input_list[[i]])
      page.content <- paste0(page.content, sprintf("    %sR<sup>2</sup><sub>CS</sub> = %.3f<br>R<sup>2</sup><sub>N</sub> = %.3f</td>\n", colspanstring, psr[2], psr[3]))
    }
    page.content <- paste(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: log likelihood
  # -------------------------------------
  if (showLogLik) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">-2 Log-Likelihood</td>\n")
    for (i in 1:length(input_list)) {
      psr <- PseudoR2(input_list[[i]])
      page.content <- paste0(page.content, sprintf("    %s%.3f</td>\n", colspanstring, -2*logLik(input_list[[i]])))
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: AIC
  # -------------------------------------
  if (showAIC) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">AIC</td>\n")
    for (i in 1:length(input_list)) {
      page.content <- paste0(page.content, sprintf("    %s%.2f</td>\n", colspanstring, AIC(input_list[[i]])))
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: Chi2
  # -------------------------------------
  if (showChi2) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">&Chi;<sup>2</sup></td>\n")
    for (i in 1:length(input_list)) {
      page.content <- paste0(page.content, sprintf("    %s%.3f</td>\n", colspanstring, Chisquare.glm(input_list[[i]])))
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: Family
  # -------------------------------------
  if (showFamily) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">Family</td>\n")
    for (i in 1:length(input_list)) {
      fam <- input_list[[i]]$family
      page.content <- paste0(page.content, sprintf("    %s%s (%s)</td>\n", colspanstring, fam$family, fam$link))
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # table footnote
  # -------------------------------------
  if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata annorow\">Notes</td><td class=\"tdata annorow annostyle\" colspan=\"%i\"><em>* p&lt;0.05&nbsp;&nbsp;&nbsp;** p&lt;0.01&nbsp;&nbsp;&nbsp;*** p&lt;0.001</em></td>\n  </tr>\n", headerColSpan))
  page.content <- paste0(page.content, "</table>\n")
  # -------------------------------------
  # finish table
  # -------------------------------------
  toWrite <- paste0(toWrite, page.content)
  toWrite <- paste0(toWrite, "</body></html>")
  # -------------------------------------
  # replace class attributes with inline style,
  # useful for knitr
  # -------------------------------------
  # copy page content
  # -------------------------------------
  knitr <- page.content
  # -------------------------------------
  # set style attributes for main table tags
  # -------------------------------------
  knitr <- gsub("class=", "style=", knitr)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), knitr)
  # -------------------------------------
  # replace class-attributes with inline-style-definitions
  # -------------------------------------
  knitr <- gsub(tag.tdata, css.tdata, knitr)
  knitr <- gsub(tag.thead, css.thead, knitr)
  knitr <- gsub(tag.summary, css.summary, knitr)  
  knitr <- gsub(tag.colnames, css.colnames, knitr)
  knitr <- gsub(tag.leftalign, css.leftalign, knitr)
  knitr <- gsub(tag.centeralign, css.centeralign, knitr)
  knitr <- gsub(tag.firstsumrow, css.firstsumrow, knitr)
  knitr <- gsub(tag.lasttablerow, css.lasttablerow, knitr)  
  knitr <- gsub(tag.labelcellborder, css.labelcellborder, knitr)  
  knitr <- gsub(tag.topborder, css.topborder, knitr)  
  knitr <- gsub(tag.depvarhead, css.depvarhead, knitr)  
  knitr <- gsub(tag.topcontentborder, css.topcontentborder, knitr)  
  knitr <- gsub(tag.noannorow, css.noannorow, knitr)
  knitr <- gsub(tag.annorow, css.annorow, knitr)  
  knitr <- gsub(tag.annostyle, css.annostyle, knitr)  
  # -------------------------------------
  # check if html-content should be outputted
  # -------------------------------------
  if (!no.output) {
    # -------------------------------------
    # check if we have filename specified
    # -------------------------------------
    if (!is.null(file)) {
      # write file
      write(knitr, file=file)
    }
    # -------------------------------------
    # else open in viewer pane
    # -------------------------------------
    else {
      # else create and browse temporary file
      htmlFile <- tempfile(fileext=".html")
      write(toWrite, file=htmlFile)
      # check whether we have RStudio Viewer
      viewer <- getOption("viewer")
      if (useViewer && !is.null(viewer)) {
        viewer(htmlFile)
      }
      else {
        utils::browseURL(htmlFile)    
      }
      # delete temp file
      # unlink(htmlFile)
    }
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjtglm",
                       list(page.style = page.style,
                            page.content = page.content,
                            output.complete = toWrite,
                            knitr = knitr)))
}
