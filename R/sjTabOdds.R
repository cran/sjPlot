#' @title Save odds Ratios as HTML-Table
#' @name sjt.glm
#' @references \url{http://strengejacke.wordpress.com/sjplot-r-package/} \cr \cr
#'             \url{http://strengejacke.wordpress.com/2013/08/20/print-glm-output-to-html-table-rstats/}
#' 
#' @description Save (multiple) generalized linear models (Odds Ratios)
#'                as HTML-Table. The fitted glm's should have the same predictor variables and
#'                differ only in their response (dependent variable).
#' 
#' @seealso \link{sjt.lm}
#' 
#' @param ... One or more fitted glm-objects.
#' @param file The destination file, which will be in html-format.
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
#' @param pvaluesAsNumbers If \code{TRUE}, p-values are shown as numbers. If \code{FALSE} (default),
#'          p-values are indicated by asterisks.
#' @param boldpvalues If \code{TRUE} (default), significant p-values are shown bold faced.
#' @param showConfInt If \code{TRUE} (default), the confidence intervall is also printed to the table. Use
#'          \code{FALSE} to omit the CI in the table.
#' @param separateConfColumn if \code{TRUE}, the CI values are shown in a separate table column.
#'          Default is \code{FALSE}.
#' @param showAbbrHeadline If \code{TRUE} (default), the table data columns have a headline with 
#'          abbreviations for odds ratios, confidence interval and p-values.
#'
#' @examples
#' # prepare dummy variable for binary logistic regression
#' y1 <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
#' # prepare dummy variable for binary logistic regression
#' y2 <- ifelse(swiss$Agriculture<median(swiss$Agriculture), 0, 1)
#' 
#' # Now fit the models. Note that both models share the same predictors
#' # and only differ in their dependent variable (y1 and y2)
#' 
#' # fit first model
#' fitOR1 <- glm(y1 ~ swiss$Education+swiss$Examination+swiss$Infant.Mortality+swiss$Catholic,
#'               family=binomial(link="logit"))
#' # fit second model
#' fitOR2 <- glm(y2 ~ swiss$Education+swiss$Examination+swiss$Infant.Mortality+swiss$Catholic,
#'               family=binomial(link="logit"))
#' 
#' # save HTML-tables to "or_table1.html"
#' sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Agriculture"),
#'         labelPredictors=c("Education", "Examination", "Infant Mortality", "Catholic"),
#'         file="or_table1.html")
#' 
#' # save HTML-tables to "or_table2.html", indicating p-values as numbers
#' sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Agriculture"),
#'         labelPredictors=c("Education", "Examination", "Infant Mortality", "Catholic"),
#'         file="or_table2.html", pvaluesAsNumbers=TRUE)
#' 
#' # save HTML-tables to "or_table3.html", printing CI in a separate column
#' sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Agriculture"),
#'         labelPredictors=c("Education", "Examination", "Infant Mortality", "Catholic"),
#'         file="or_table3.html", separateConfColumn=TRUE)
#' 
#' # save HTML-tables to "or_table4.html", indicating p-values as numbers 
#' # and printing CI in a separate column
#' sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Agriculture"),
#'         labelPredictors=c("Education", "Examination", "Infant Mortality", "Catholic"),
#'         file="or_table4.html", pvaluesAsNumbers=TRUE, separateConfColumn=TRUE)
#' 
#' @export
sjt.glm <- function (..., 
                     file, 
                     labelPredictors=NULL, 
                     labelDependentVariables=NULL, 
                     stringPredictors="Predictors", 
                     stringDependentVariables="Dependent Variables", 
                     stringModel="Model",
                     stringIntercept="(Intercept)",
                     pvaluesAsNumbers=FALSE,
                     boldpvalues=TRUE,
                     showConfInt=TRUE,
                     separateConfColumn=FALSE,
                     showAbbrHeadline=TRUE) {
  
  toWrite = '<html>\n<head>\n<style>table { border-collapse:collapse; border:none; }\nth { border-bottom: 1px solid; }\ntable td { padding:0.2cm; }\ntd.summary { padding-top:0.1cm; padding-bottom:0.1cm }\n.lasttablerow { border-bottom: double; }\n</style>\n</head>\n<body>\n'
  toWrite = paste(toWrite, "<table>", "\n")
  
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

  toWrite = paste(toWrite, sprintf("  <tr style=\"border-top:2px solid\">\n    <td rowspan=\"2\"><em>%s</em></td>", stringPredictors), "\n")
  toWrite = paste(toWrite, sprintf("    <td colspan=\"%i\" style=\"text-align:center;border-bottom:1px solid;border-top:1px solid\"><em>%s</em></td>", headerColSpan, stringDependentVariables), "\n")
  toWrite = paste(toWrite, "  </tr>\n  <tr>", "\n")
  
  # -------------------------------------
  # table headline: label for dependent variables (model outcomes)
  # -------------------------------------
  if (!is.null(labelDependentVariables)) {
    for (i in 1:length(labelDependentVariables)) {
      if (headerColSpanFactor>1) {
        toWrite = paste(toWrite, sprintf("    <td colspan=\"%i\">%s</td>", headerColSpanFactor, labelDependentVariables[i]), "\n")
      }
      else {
        toWrite = paste(toWrite, sprintf("    <td>%s</td>", labelDependentVariables[i]), "\n")
      }
    }
    toWrite = paste(toWrite, "  </tr>", "\n")
  }
  else {
    for (i in 1:length(input_list)) {
      if (headerColSpanFactor>1) {
        toWrite = paste(toWrite, sprintf("    <td colspan=\"%i\">%s %i</td>", headerColSpanFactor, stringModel, i), "\n")
      }
      else {
        toWrite = paste(toWrite, sprintf("    <td>%s %i</td>", stringModel, i), "\n")
      }
    }
    toWrite = paste(toWrite, "  </tr>", "\n")
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
    toWrite = paste(toWrite, "  <tr>\n    <td>&nbsp;</td>\n")
    colnr <- ifelse(is.null(labelDependentVariables), length(input_list), length(labelDependentVariables))
    for (i in 1:colnr) {
      if (pvaluesAsNumbers) {
        if (separateConfColumn) {
          toWrite = paste(toWrite, "    <td><em>OR</em></td><td><em>CI</em></td><td><em>p</em></td>\n")
        }
        else {
          toWrite = paste(toWrite, sprintf("    <td><em>%s</em></td><td><em>p</em></td>\n", showCIString))
        }
      }
      else {
        if (separateConfColumn) {
          toWrite = paste(toWrite, "    <td><em>OR</em></td><td><em>CI</em></td>\n")
        }
        else {
          toWrite = paste(toWrite, sprintf("    <td><em>%s</em></td>\n", showCIString))
        }
      }
    }
    toWrite = paste(toWrite, "  </tr>\n")
  }
  
  
  # -------------------------------------
  # close table headline
  # -------------------------------------
  toWrite = paste(toWrite, "  <tr style=\"border-top:double\">", "\n")
  
  
  # -------------------------------------
  # 1. row: intercept
  # -------------------------------------
  toWrite = paste(toWrite, sprintf("    <td>%s</td>", stringIntercept), "\n")
  for (i in 1:ncol(coeffs)) {
    if (pvaluesAsNumbers) {
      if (separateConfColumn) {
        toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f</td><td>%s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
      }
      else {
        if (showConfInt) {
          toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f)</td><td>%s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
        }
        else {
          toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%s</td>", coeffs[1,i], pv[1,i]), "\n")
        }
      }
    }
    else {
      if (separateConfColumn) {
        toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f %s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
      }
      else {
        if (showConfInt) {
          toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f) %s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
        }
        else {
          toWrite = paste(toWrite, sprintf("    <td>%.2f %s</td>", coeffs[1,i], pv[1,i]), "\n")
        }
      }
    }
  }
  toWrite = paste(toWrite, "  </tr>", "\n")
  
  
  # -------------------------------------
  # subsequent rows: pedictors
  # -------------------------------------
  predlen <- length(labelPredictors)
  for (i in 1:predlen) {
    toWrite = paste(toWrite, "  <tr>\n", sprintf("    <td>%s</td>", labelPredictors[i]), "\n")
    for (j in 1:ncol(coeffs)) {
      if (pvaluesAsNumbers) {
        if (separateConfColumn) {
          toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f</td><td>%s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
        }
        else {
          if (showConfInt) {
            toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f)</td><td>%s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
          }
          else {
            toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%s</td>", coeffs[i+1,j], pv[i+1,j]), "\n")
          }
        }
      }
      else {
        if (separateConfColumn) {
          toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f %s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
        }
        else {
          if (showConfInt) {
            toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f) %s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
          }
          else {
            toWrite = paste(toWrite, sprintf("    <td>%.2f %s</td>", coeffs[i+1,j], pv[i+1,j]), "\n")
          }
        }
      }
    }
    toWrite = paste(toWrite, "  </tr>", "\n")
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
    colspanstring <- sprintf("<td  class=\"summary\" colspan=\"%i\">", headerColSpanFactor)
  }
  else {
    colspanstring <- c("<td class=\"summary\">")
  }
  toWrite = paste(toWrite, "  <tr style=\"border-top:1px solid\">\n    <td class=\"summary\">Observations</td>\n")
  for (i in 1:length(input_list)) {
    psr <- PseudoR2(input_list[[i]])
    toWrite = paste(toWrite, sprintf("    %s%i</td>\n", colspanstring, psr[1]))
  }
  toWrite = paste(toWrite, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: pseudo r2
  # -------------------------------------
  toWrite = paste(toWrite, "  <tr>\n    <td class=\"summary\">Pseudo-R<sup>2</sup></td>\n")
  for (i in 1:length(input_list)) {
    psr <- PseudoR2(input_list[[i]])
    toWrite = paste(toWrite, sprintf("    %sR<sup>2</sup><sub>CS</sub> = %.3f<br>R<sup>2</sup><sub>N</sub> = %.3f</td>\n", colspanstring, psr[2], psr[3]))
  }
  toWrite = paste(toWrite, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: chisquare
  # -------------------------------------
#   toWrite = paste(toWrite, "  <tr>\n    <td class=\"summary\"><em>p</em>(&Chi;<sup>2</sup>)</td>\n")
#   for (i in 1:length(input_list)) {
#     psr <- PseudoR2(input_list[[i]])
#     toWrite = paste(toWrite, sprintf("    %s%.3f</td>\n", colspanstring, Chisquare(input_list[[i]])))
#   }
#   toWrite = paste(toWrite, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: log likelihood
  # -------------------------------------
  toWrite = paste(toWrite, "  <tr>\n    <td class=\"summary\">-2 Log-Likelihood</td>\n")
  for (i in 1:length(input_list)) {
    psr <- PseudoR2(input_list[[i]])
    toWrite = paste(toWrite, sprintf("    %s%.3f</td>\n", colspanstring, -2*logLik(input_list[[i]])))
  }
  toWrite = paste(toWrite, "  </tr>\n")
  
  
    
  # -------------------------------------
  # table footnote
  # -------------------------------------
  toWrite = paste(toWrite, sprintf("  <tr style=\"border-top:2px solid\">\n    <td>Notes</td><td style=\"text-align:right\" colspan=\"%i\"><em>* p<0.005&nbsp;&nbsp;&nbsp;** p<0.01&nbsp;&nbsp;&nbsp;*** p <0.001</em></td>\n  </tr>", headerColSpan), "\n")
  

  # -------------------------------------
  # finish table
  # -------------------------------------
  toWrite = paste(toWrite, "</table>\n</body></html>", "\n")
  write(toWrite, file=file)  
}
