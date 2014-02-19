#' @title Save odds Ratios as HTML-Table
#' @name sjt.glm
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2013/08/20/print-glm-output-to-html-table-rstats/}
#'              }
#' 
#' @description Save (multiple) generalized linear models (Odds Ratios)
#'                as HTML-Table. The fitted glm's should have the same predictor variables and
#'                differ only in their response (dependent variable).
#' 
#' @seealso \link{sjt.lm}
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
#' @param separateConfColumn if \code{TRUE}, the CI values are shown in a separate table column.
#'          Default is \code{FALSE}.
#' @param showAbbrHeadline If \code{TRUE} (default), the table data columns have a headline with 
#'          abbreviations for odds ratios, confidence interval and p-values.
#' @param showPseudoR If \code{TRUE} (default), the pseudo R2 values for each model are printed
#'          in the model summary. R2cs is the Cox-Snell-pseudo R-square value, R2n is Nagelkerke's 
#'          pseudo R-square value.
#' @param showLogLik If \code{TRUE}, the Log-Likelihood for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param showAIC If \code{TRUE}, the AIC value for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param encoding The charset encoding used for variable and value labels. Default is \code{"UTF-8"}. Change
#'          encoding if specific chars are not properly displayed (e.g.) German umlauts).
#' @return Invisibly returns a \link{structure} with the web page style sheet (\code{page.style}) and the
#'          web page content (\code{page.content}) for further use.
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
#' # save HTML-tables to "or_table3.html", printing CI in a separate column
#' \dontrun{
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         labelDependentVariables=c("Fertility", "Infant Mortality", "Agriculture"),
#'         labelPredictors=c("Education", "Examination", "Catholic"),
#'         file="or_table3.html", separateConfColumn=TRUE)}
#' 
#' # save HTML-tables to "or_table4.html", indicating p-values as numbers 
#' # and printing CI in a separate column
#' \dontrun{
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         labelDependentVariables=c("Fertility", "Infant Mortality", "Agriculture"),
#'         labelPredictors=c("Education", "Examination", "Catholic"),
#'         file="or_table4.html", pvaluesAsNumbers=TRUE, separateConfColumn=TRUE)}
#' 
#' @export
sjt.glm <- function (..., 
                     file=NULL, 
                     labelPredictors=NULL, 
                     labelDependentVariables=NULL, 
                     stringPredictors="Predictors", 
                     stringDependentVariables="Dependent Variables", 
                     stringModel="Model",
                     stringIntercept="(Intercept)",
                     stringObservations="Observations",
                     pvaluesAsNumbers=FALSE,
                     boldpvalues=TRUE,
                     showConfInt=TRUE,
                     separateConfColumn=FALSE,
                     showAbbrHeadline=TRUE,
                     showPseudoR=TRUE,
                     showLogLik=FALSE,
                     showAIC=FALSE,
                     encoding="UTF-8") {
  # -------------------------------------
  # init header
  # -------------------------------------
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  # -------------------------------------
  # init style sheet
  # -------------------------------------
  page.style <- "<style>table { border-collapse:collapse; border:none; }\nth { border-bottom: 1px solid; }\ntable td { padding:0.2cm; }\n.summary td { padding-top:0.1cm; padding-bottom:0.1cm }\n.colnames td { font-style:italic }\n.firstsumrow { border-top:1px solid }\n.lasttablerow { border-bottom: double; }\n.topborder { border-top:2px solid }\n.depvarhead { text-align:center; border-bottom:1px solid; border-top:1px solid }\n.topcontentborder { border-top:double }\n.annorow { border-top:2px solid }\n.annostyle { text-align:right }\n</style>"
  toWrite = paste(toWrite, page.style)
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
  
  headerColSpan <- headerColSpanFactor * headerColSpan

  page.content <- "<table>\n"
  page.content <- paste(page.content, sprintf("  <tr class=\"topborder\">\n    <td rowspan=\"2\"><em>%s</em></td>", stringPredictors), "\n")
  page.content <- paste(page.content, sprintf("    <td colspan=\"%i\" class=\"depvarhead\"><em>%s</em></td>", headerColSpan, stringDependentVariables), "\n")
  page.content <- paste(page.content, "  </tr>\n  <tr>", "\n")
  
  # -------------------------------------
  # table headline: label for dependent variables (model outcomes)
  # -------------------------------------
  if (!is.null(labelDependentVariables)) {
    for (i in 1:length(labelDependentVariables)) {
      if (headerColSpanFactor>1) {
        page.content <- paste(page.content, sprintf("    <td colspan=\"%i\">%s</td>", headerColSpanFactor, labelDependentVariables[i]), "\n")
      }
      else {
        page.content <- paste(page.content, sprintf("    <td>%s</td>", labelDependentVariables[i]), "\n")
      }
    }
    page.content <- paste(page.content, "  </tr>", "\n")
  }
  else {
    for (i in 1:length(input_list)) {
      if (headerColSpanFactor>1) {
        page.content <- paste(page.content, sprintf("    <td colspan=\"%i\">%s %i</td>", headerColSpanFactor, stringModel, i), "\n")
      }
      else {
        page.content <- paste(page.content, sprintf("    <td>%s %i</td>", stringModel, i), "\n")
      }
    }
    page.content <- paste(page.content, "  </tr>", "\n")
  }
  
  # -------------------------------------
  # calculate coefficients and confidence intervalls
  # for all models
  # -------------------------------------
  coeffs <- c()
  confi_lower <- c()
  confi_higher <- c()
  pv <- c()
  
  for (i in 1:length(input_list)) {
    fit <- input_list[[i]]
    coeffs <- rbind(coeffs, exp(coef(fit)))
    confi_lower <- cbind(confi_lower, exp(confint(fit))[,1])
    confi_higher <- cbind(confi_higher, exp(confint(fit))[,2])
    pv <- cbind(pv, round(summary(fit)$coefficients[,4],3))
  }
  
  coeffs <- t(coeffs)
  
  if (is.null(labelPredictors)) {
    labelPredictors <- row.names(coeffs)[-1]
  }
  
  
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
    page.content <- paste(page.content, "  <tr class=\"colnames\">\n    <td>&nbsp;</td>\n")
    colnr <- ifelse(is.null(labelDependentVariables), length(input_list), length(labelDependentVariables))
    for (i in 1:colnr) {
      if (pvaluesAsNumbers) {
        if (separateConfColumn) {
          page.content <- paste(page.content, "    <td>OR</td><td>CI</td><td>p</td>\n")
        }
        else {
          page.content <- paste(page.content, sprintf("    <td>%s</td><td>p</td>\n", showCIString))
        }
      }
      else {
        if (separateConfColumn) {
          page.content <- paste(page.content, "    <td>OR</td><td>CI</td>\n")
        }
        else {
          page.content <- paste(page.content, sprintf("    <td>%s</td>\n", showCIString))
        }
      }
    }
    page.content <- paste(page.content, "  </tr>\n")
  }
  
  
  # -------------------------------------
  # close table headline
  # -------------------------------------
  page.content <- paste(page.content, "  <tr class=\"topcontentborder\">", "\n")
  
  
  # -------------------------------------
  # 1. row: intercept
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <td>%s</td>", stringIntercept), "\n")
  for (i in 1:ncol(coeffs)) {
    if (pvaluesAsNumbers) {
      if (separateConfColumn) {
        page.content <- paste(page.content, sprintf("    <td>%.2f</td><td>%.2f-%.2f</td><td>%s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
      }
      else {
        if (showConfInt) {
          page.content <- paste(page.content, sprintf("    <td>%.2f (%.2f-%.2f)</td><td>%s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
        }
        else {
          page.content <- paste(page.content, sprintf("    <td>%.2f</td><td>%s</td>", coeffs[1,i], pv[1,i]), "\n")
        }
      }
    }
    else {
      if (separateConfColumn) {
        page.content <- paste(page.content, sprintf("    <td>%.2f</td><td>%.2f-%.2f %s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
      }
      else {
        if (showConfInt) {
          page.content <- paste(page.content, sprintf("    <td>%.2f (%.2f-%.2f) %s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
        }
        else {
          page.content <- paste(page.content, sprintf("    <td>%.2f %s</td>", coeffs[1,i], pv[1,i]), "\n")
        }
      }
    }
  }
  page.content <- paste(page.content, "  </tr>", "\n")
  
  
  # -------------------------------------
  # subsequent rows: pedictors
  # -------------------------------------
  predlen <- length(labelPredictors)
  for (i in 1:predlen) {
    page.content <- paste(page.content, "  <tr>\n", sprintf("    <td>%s</td>", labelPredictors[i]), "\n")
    for (j in 1:ncol(coeffs)) {
      if (pvaluesAsNumbers) {
        if (separateConfColumn) {
          page.content <- paste(page.content, sprintf("    <td>%.2f</td><td>%.2f-%.2f</td><td>%s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
        }
        else {
          if (showConfInt) {
            page.content <- paste(page.content, sprintf("    <td>%.2f (%.2f-%.2f)</td><td>%s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
          }
          else {
            page.content <- paste(page.content, sprintf("    <td>%.2f</td><td>%s</td>", coeffs[i+1,j], pv[i+1,j]), "\n")
          }
        }
      }
      else {
        if (separateConfColumn) {
          page.content <- paste(page.content, sprintf("    <td>%.2f</td><td>%.2f-%.2f %s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
        }
        else {
          if (showConfInt) {
            page.content <- paste(page.content, sprintf("    <td>%.2f (%.2f-%.2f) %s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
          }
          else {
            page.content <- paste(page.content, sprintf("    <td>%.2f %s</td>", coeffs[i+1,j], pv[i+1,j]), "\n")
          }
        }
      }
    }
    page.content <- paste(page.content, "  </tr>", "\n")
  }
  

  PseudoR2 <- function(rr) { # rr must be the result of lm/glm
    n <- nrow(rr$model)
    COX <- (1-exp((rr$deviance-rr$null)/n))
    NR <- COX/(1-exp(-rr$null/n))
    RVAL <- c(N=n, CoxSnell=COX, Nagelkerke=NR)
    return(RVAL)
  }
  Chisquare <- function(rr) {
    return (with(rr, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE), digits=3))
  }
  # -------------------------------------
  # Model-Summary: N
  # -------------------------------------
  if (headerColSpanFactor>1) {
    colspanstring <- sprintf("<td  colspan=\"%i\">", headerColSpanFactor)
  }
  else {
    colspanstring <- c("<td>")
  }
  page.content <- paste(page.content, sprintf("  <tr class=\"summary firstsumrow\">\n    <td>%s</td>\n", stringObservations))
  for (i in 1:length(input_list)) {
    psr <- PseudoR2(input_list[[i]])
    page.content <- paste(page.content, sprintf("    %s%i</td>\n", colspanstring, psr[1]))
  }
  page.content <- paste(page.content, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: pseudo r2
  # -------------------------------------
  if (showPseudoR) {
    page.content <- paste(page.content, "  <tr class=\"summary\">\n    <td>Pseudo-R<sup>2</sup></td>\n")
    for (i in 1:length(input_list)) {
      psr <- PseudoR2(input_list[[i]])
      page.content <- paste(page.content, sprintf("    %sR<sup>2</sup><sub>CS</sub> = %.3f<br>R<sup>2</sup><sub>N</sub> = %.3f</td>\n", colspanstring, psr[2], psr[3]))
    }
    page.content <- paste(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: chisquare
  # -------------------------------------
#   page.content <- paste(page.content, "  <tr class=\"summary\">\n    <td><em>p</em>(&Chi;<sup>2</sup>)</td>\n")
#   for (i in 1:length(input_list)) {
#     psr <- PseudoR2(input_list[[i]])
#     page.content <- paste(page.content, sprintf("    %s%.3f</td>\n", colspanstring, Chisquare(input_list[[i]])))
#   }
#   page.content <- paste(page.content, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: log likelihood
  # -------------------------------------
  if (showLogLik) {
    page.content <- paste(page.content, "  <tr class=\"summary\">\n    <td>-2 Log-Likelihood</td>\n")
    for (i in 1:length(input_list)) {
      psr <- PseudoR2(input_list[[i]])
      page.content <- paste(page.content, sprintf("    %s%.3f</td>\n", colspanstring, -2*logLik(input_list[[i]])))
    }
    page.content <- paste(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: AIC
  # -------------------------------------
  if (showAIC) {
    page.content <- paste(page.content, "  <tr class=\"summary\">\n    <td>AIC</td>\n")
    for (i in 1:length(input_list)) {
      page.content <- paste(page.content, sprintf("    %s%.2f</td>\n", colspanstring, AIC(input_list[[i]])))
    }
    page.content <- paste(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # table footnote
  # -------------------------------------
  page.content <- paste(page.content, sprintf("  <tr class=\"annorow\">\n    <td>Notes</td><td class=\"annostyle\" colspan=\"%i\"><em>* p&lt;0.005&nbsp;&nbsp;&nbsp;** p&lt;0.01&nbsp;&nbsp;&nbsp;*** p&lt;0.001</em></td>\n  </tr>\n</table>", headerColSpan), "\n")
  # -------------------------------------
  # finish table
  # -------------------------------------
  toWrite <- paste(toWrite, page.content)
  toWrite <- paste(toWrite, "\n</body></html>", "\n")
  # -------------------------------------
  # check if we have filename specified
  # -------------------------------------
  if (!is.null(file)) {
    # write file
    write(toWrite, file=file)
  }
  else {
    # else create and browse temporary file
    htmlFile <- tempfile(fileext=".html")
    write(toWrite, file=htmlFile)
    # check whether we have RStudio Viewer
    viewer <- getOption("viewer")
    if (!is.null(viewer)) {
      viewer(htmlFile)
    }
    else {
      utils::browseURL(htmlFile)    
    }
    # delete temp file
    # unlink(htmlFile)
  }
  invisible (structure(class = "sjtglm",
                       list(page.style = page.style,
                            page.content = page.content)))
}
