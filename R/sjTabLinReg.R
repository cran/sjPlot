#' @title Save linear regression as HTML-Table
#' @name sjt.lm
#' @references \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#' 
#' @description Save (multiple) fitted linear models (beta coefficients, std. beta values etc.)
#'                as HTML-Table. The fitted lm's should have the same predictor variables and
#'                differ only in their response (dependent variable).
#'                
#' @seealso \link{sjt.glm}
#' 
#' @param ... One or more fitted lm-objects.
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
#' @param showConfInt If \code{TRUE} (default), the confidence intervall is also printed to the table. Use
#'          \code{FALSE} to omit the CI in the table.
#' @param showStdBeta If \code{TRUE}, the standardized beta-coefficients are also printed.
#'          Default is \code{FALSE}.
#' @param pvaluesAsNumbers If \code{TRUE}, p-values are shown as numbers. If \code{FALSE} (default),
#'          p-values are indicated by asterisks.
#' @param boldpvalues If \code{TRUE} (default), significant p-values are shown bold faced.
#' @param separateConfColumn if \code{TRUE}, the CI values are shown in a separate table column.
#'          Default is \code{FALSE}.
#' @param showAbbrHeadline If \code{TRUE} (default), the table data columns have a headline with 
#'          abbreviations for beta- and std. beta-values, confidence interval and p-values.
#'
#' @examples
#' # Now fit the models. Note that both models share the same predictors
#' # and only differ in their dependent variable
#' data(efc)
#' 
#' # fit first model
#' fit1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data=efc)
#' # fit second model
#' fit2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + c172code, data=efc)
#' 
#' # save HTML-tables to "lm_table1.html"
#' sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
#'        labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
#'        file="lm_table1.html")
#' 
#' # save HTML-tables to "lm_table2.html", indicating p-values as numbers
#' sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
#'        labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
#'        file="lm_table2.html", showStdBeta=TRUE, pvaluesAsNumbers=TRUE)
#' 
#' # save HTML-tables to "lm_table3.html", printing CI in a separate column
#' sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
#'        labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
#'        file="lm_table3.html", separateConfColumn=TRUE)
#' 
#' # save HTML-tables to "lm_table4.html", indicating p-values as numbers
#' # and printing CI in a separate column
#' sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
#'        labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
#'        file="lm_table4.html", showStdBeta=TRUE, pvaluesAsNumbers=TRUE, separateConfColumn=TRUE)
#' 
#' @export
sjt.lm <- function (..., 
                     file, 
                     labelPredictors=NULL, 
                     labelDependentVariables=NULL, 
                     stringPredictors="Predictors", 
                     stringDependentVariables="Dependent Variables", 
                     stringModel="Model",
                     stringIntercept="(Intercept)",
                     showConfInt=TRUE,
                     showStdBeta=FALSE,
                     pvaluesAsNumbers=FALSE,
                     boldpvalues=TRUE,
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
    showCIString <- c("B")
  }
  else {
    showCIString <- c("B (CI)")
  }
  
  # -------------------------------------
  # table headline
  # -------------------------------------
  headerColSpan <- length(input_list)
  headerColSpanFactor <- 1
  if (pvaluesAsNumbers) headerColSpanFactor <- headerColSpanFactor+1
  if (separateConfColumn) headerColSpanFactor <- headerColSpanFactor+1
  if (showStdBeta) headerColSpanFactor <- headerColSpanFactor+1
  
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
  stdbv <- c()
  
  for (i in 1:length(input_list)) {
    fit <- input_list[[i]]
    coeffs <- rbind(coeffs, coef(fit))
    confi_lower <- cbind(confi_lower, confint(fit)[,1])
    confi_higher <- cbind(confi_higher, confint(fit)[,2])
    pv <- cbind(pv, round(summary(fit)$coefficients[,4],3))
    # retrieve standardized betas
    stdbv <- cbind(stdbv, sprintf("%.2f", round(sju.betaCoef(fit),2)))
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
          if (showStdBeta) {
            toWrite = paste(toWrite, "    <td><em>B</em></td><td><em>CI</em></td><td><em>std. Beta</em></td><td><em>p</em></td>\n")            
          }
          else {
            toWrite = paste(toWrite, "    <td><em>B</em></td><td><em>CI</em></td><td><em>p</em></td>\n")            
          }
        }
        else {
          if (showStdBeta) {
            toWrite = paste(toWrite, sprintf("    <td><em>%s</em></td><td><em>std. Beta</em></td><td><em>p</em></td>\n", showCIString))
          }
          else {
            toWrite = paste(toWrite, sprintf("    <td><em>%s</em></td><td><em>p</em></td>\n", showCIString))
          }
        }
      }
      else {
        if (separateConfColumn) {
          if (showStdBeta) {
            toWrite = paste(toWrite, "    <td><em>B</em></td><td><em>CI</em></td><td><em>std. Beta</em></td>\n")
          }
          else {
            toWrite = paste(toWrite, "    <td><em>B</em></td><td><em>CI</em></td>\n")
          }
        }
        else {
          if (showStdBeta) {
            toWrite = paste(toWrite, sprintf("    <td><em>%s</em></td><td><em>std. Beta</em></td>\n", showCIString))
          }
          else {
            toWrite = paste(toWrite, sprintf("    <td><em>%s</em></td>\n", showCIString))
          }
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
        if (showStdBeta) {
          toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f</td><td></td><td>%s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
        }
        else {
          toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f</td><td>%s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
        }
      }
      else {
        if (showConfInt) {
          toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f)</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i]))
        }
        else {
          toWrite = paste(toWrite, sprintf("    <td>%.2f</td>", coeffs[1,i]))
        }
        if (showStdBeta) {
          toWrite = paste(toWrite, sprintf("<td></td><td>%s</td>", pv[1,i]), "\n")
        }
        else {
          toWrite = paste(toWrite, sprintf("<td>%s</td>", pv[1,i]), "\n")
        }
      }
    }
    else {
      if (separateConfColumn) {
        if (showStdBeta) {
          toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f %s</td><td></td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
        }
        else {
          toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f %s</td>", coeffs[1,i], confi_lower[1,i], confi_higher[1,i], pv[1,i]), "\n")
        }
      }
      else {
        if (showConfInt) {
          toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f)", coeffs[1,i], confi_lower[1,i], confi_higher[1,i]))
        }
        else {
          toWrite = paste(toWrite, sprintf("    <td>%.2f", coeffs[1,i]))
        }
        if (showStdBeta) {
          toWrite = paste(toWrite, sprintf("%s</td><td></td>", pv[1,i]), "\n")
        }
        else {
          toWrite = paste(toWrite, sprintf("%s</td>", pv[1,i]), "\n")
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
          if (showStdBeta) {
            toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f</td><td>%s</td><td>%s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], stdbv[i,j], pv[i+1,j]), "\n")
          }
          else {
            toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f</td><td>%s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
          }
        }
        else {
          if (showConfInt) {
            toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f)</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j]))
          }
          else {
            toWrite = paste(toWrite, sprintf("    <td>%.2f</td>", coeffs[i+1,j]))
          }
          if (showStdBeta) {
            toWrite = paste(toWrite, sprintf("<td>%s</td><td>%s</td>", stdbv[i,j], pv[i+1,j]), "\n")
          }
          else {
            toWrite = paste(toWrite, sprintf("<td>%s</td>", pv[i+1,j]), "\n")
          }
        }
      }
      else {
        if (separateConfColumn) {
          if (showStdBeta) {
            toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f %s</td><td>%s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j], stdbv[i,j]), "\n")
          }
          else {
            toWrite = paste(toWrite, sprintf("    <td>%.2f</td><td>%.2f-%.2f %s</td>", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j], pv[i+1,j]), "\n")
          }
        }
        else {
          if (showConfInt) {
            toWrite = paste(toWrite, sprintf("    <td>%.2f (%.2f-%.2f)", coeffs[i+1,j], confi_lower[i+1,j], confi_higher[i+1,j]))
          }
          else {
            toWrite = paste(toWrite, sprintf("    <td>%.2f", coeffs[i+1,j]))
          }
          if (showStdBeta) {
            toWrite = paste(toWrite, sprintf("%s</td><td>%s</td>", pv[i+1,j], stdbv[i,j]), "\n")
          }
          else {
            toWrite = paste(toWrite, sprintf("%s</td>", pv[i+1,j]), "\n")
          }
        }
      }
    }
    toWrite = paste(toWrite, "  </tr>", "\n")
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
    toWrite = paste(toWrite, sprintf("    %s%i</td>\n", colspanstring, summary(input_list[[i]])$df[2]))
  }
  toWrite = paste(toWrite, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: r2 and sdj. r2
  # -------------------------------------
  toWrite = paste(toWrite, "  <tr>\n     <td class=\"summary\">R<sup>2</sup> / adj. R<sup>2</sup></td>\n")
  for (i in 1:length(input_list)) {
    rsqu <- summary(input_list[[i]])$r.squared
    adjrsqu <- summary(input_list[[i]])$adj.r.squared
    toWrite = paste(toWrite, sprintf("    %s%.3f / %.3f</td>\n", colspanstring, rsqu, adjrsqu))
  }
  toWrite = paste(toWrite, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: F-statistics
  # -------------------------------------
  toWrite = paste(toWrite, "  <tr>\n     <td class=\"summary\">F-statistics</td>\n")
  for (i in 1:length(input_list)) {
    fstat <- summary(input_list[[i]])$fstatistic
    # Calculate p-value for F-test
    pval <- pf(fstat[1], fstat[2], fstat[3],lower.tail = FALSE)
    # indicate significance level by stars
    pan <- c("")
    if (pval<=0.005) {
      pan <- c("***")
    }
    else  if (pval<=0.01) {
      pan <- c("**")
    }
    else  if (pval<=0.05) {
      pan <- c("*")
    }
    toWrite = paste(toWrite, sprintf("    %s%.2f%s</td>\n", colspanstring, fstat[1], pan))
  }
  toWrite = paste(toWrite, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: AIC
  # -------------------------------------
  toWrite = paste(toWrite, "  <tr>\n     <td class=\"summary\">AIC</td>\n")
  for (i in 1:length(input_list)) {
    toWrite = paste(toWrite, sprintf("    %s%.2f</td>\n", colspanstring, AIC(input_list[[i]])))
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
