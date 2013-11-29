\name{sjt.lm}
\alias{sjt.lm}
\title{Save linear regression as HTML-Table}
\usage{
  sjt.lm(..., file, labelPredictors = NULL,
    labelDependentVariables = NULL,
    stringPredictors = "Predictors",
    stringDependentVariables = "Dependent Variables",
    stringModel = "Model", stringIntercept = "(Intercept)",
    showConfInt = TRUE, showStdBeta = FALSE,
    pvaluesAsNumbers = FALSE, boldpvalues = TRUE,
    separateConfColumn = FALSE, showAbbrHeadline = TRUE)
}
\arguments{
  \item{...}{One or more fitted lm-objects.}

  \item{file}{The destination file, which will be in
  html-format.}

  \item{labelPredictors}{Labels of the predictor variables,
  provided as char vector.}

  \item{labelDependentVariables}{Labels of the dependent
  variables of all fitted models which have been used as
  first parameter(s), provided as char vector.}

  \item{stringPredictors}{String constant used as headline
  for the predictor column.  Default is
  \code{"Predictors"}.}

  \item{stringDependentVariables}{String constant used as
  headline for the dependent variable columns. Default is
  \code{"Dependent Variables"}.}

  \item{stringModel}{String constant used as headline for
  the model names in case no labels for the dependent
  variables are provided (see labelDependentVariables).
  Default is \code{"Model"}.}

  \item{stringIntercept}{String constant used as headline
  for the Intercept row default is \code{"Intercept"}.}

  \item{showConfInt}{If \code{TRUE} (default), the
  confidence intervall is also printed to the table. Use
  \code{FALSE} to omit the CI in the table.}

  \item{showStdBeta}{If \code{TRUE}, the standardized
  beta-coefficients are also printed.  Default is
  \code{FALSE}.}

  \item{pvaluesAsNumbers}{If \code{TRUE}, p-values are
  shown as numbers. If \code{FALSE} (default), p-values are
  indicated by asterisks.}

  \item{boldpvalues}{If \code{TRUE} (default), significant
  p-values are shown bold faced.}

  \item{separateConfColumn}{if \code{TRUE}, the CI values
  are shown in a separate table column.  Default is
  \code{FALSE}.}

  \item{showAbbrHeadline}{If \code{TRUE} (default), the
  table data columns have a headline with abbreviations for
  beta- and std. beta-values, confidence interval and
  p-values.}
}
\description{
  Save (multiple) fitted linear models (beta coefficients,
  std. beta values etc.)  as HTML-Table. The fitted lm's
  should have the same predictor variables and differ only
  in their response (dependent variable).
}
\examples{
# Now fit the models. Note that both models share the same predictors
# and only differ in their dependent variable
data(efc)

# fit first model
fit1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data=efc)
# fit second model
fit2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + c172code, data=efc)

# save HTML-tables to "lm_table1.html"
sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
       labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
       file="lm_table1.html")

# save HTML-tables to "lm_table2.html", indicating p-values as numbers
sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
       labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
       file="lm_table2.html", showStdBeta=TRUE, pvaluesAsNumbers=TRUE)

# save HTML-tables to "lm_table3.html", printing CI in a separate column
sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
       labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
       file="lm_table3.html", separateConfColumn=TRUE)

# save HTML-tables to "lm_table4.html", indicating p-values as numbers
# and printing CI in a separate column
sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
       labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
       file="lm_table4.html", showStdBeta=TRUE, pvaluesAsNumbers=TRUE, separateConfColumn=TRUE)
}
\references{
  \url{http://strengejacke.wordpress.com/sjplot-r-package/}
}
\seealso{
  \link{sjt.glm}
}

