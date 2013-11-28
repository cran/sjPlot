\name{sjt.glm}
\alias{sjt.glm}
\title{Save odds Ratios as HTML-Table}
\usage{
  sjt.glm(..., file, labelPredictors = NULL,
    labelDependentVariables = NULL,
    stringPredictors = "Predictors",
    stringDependentVariables = "Dependent Variables",
    stringModel = "Model", stringIntercept = "(Intercept)",
    pvaluesAsNumbers = FALSE, boldpvalues = TRUE,
    showConfInt = TRUE, separateConfColumn = FALSE,
    showAbbrHeadline = TRUE)
}
\arguments{
  \item{...}{One or more fitted glm-objects.}

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

  \item{pvaluesAsNumbers}{If \code{TRUE}, p-values are
  shown as numbers. If \code{FALSE} (default), p-values are
  indicated by asterisks.}

  \item{boldpvalues}{If \code{TRUE} (default), significant
  p-values are shown bold faced.}

  \item{showConfInt}{If \code{TRUE} (default), the
  confidence intervall is also printed to the table. Use
  \code{FALSE} to omit the CI in the table.}

  \item{separateConfColumn}{if \code{TRUE}, the CI values
  are shown in a separate table column.  Default is
  \code{FALSE}.}

  \item{showAbbrHeadline}{If \code{TRUE} (default), the
  table data columns have a headline with abbreviations for
  odds ratios, confidence interval and p-values.}
}
\description{
  Save (multiple) generalized linear models (Odds Ratios)
  as HTML-Table. The fitted glm's should have the same
  predictor variables and differ only in their response
  (dependent variable).
}
\examples{
# prepare dummy variable for binary logistic regression
y1 <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
# prepare dummy variable for binary logistic regression
y2 <- ifelse(swiss$Agriculture<median(swiss$Agriculture), 0, 1)

# Now fit the models. Note that both models share the same predictors
# and only differ in their dependent variable (y1 and y2)

# fit first model
fitOR1 <- glm(y1 ~ swiss$Education+swiss$Examination+swiss$Infant.Mortality+swiss$Catholic,
              family=binomial(link="logit"))
# fit second model
fitOR2 <- glm(y2 ~ swiss$Education+swiss$Examination+swiss$Infant.Mortality+swiss$Catholic,
              family=binomial(link="logit"))

# save HTML-tables to "or_table1.html"
sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Agriculture"),
        labelPredictors=c("Education", "Examination", "Infant Mortality", "Catholic"),
        file="or_table1.html")

# save HTML-tables to "or_table2.html", indicating p-values as numbers
sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Agriculture"),
        labelPredictors=c("Education", "Examination", "Infant Mortality", "Catholic"),
        file="or_table2.html", pvaluesAsNumbers=TRUE)

# save HTML-tables to "or_table3.html", printing CI in a separate column
sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Agriculture"),
        labelPredictors=c("Education", "Examination", "Infant Mortality", "Catholic"),
        file="or_table3.html", separateConfColumn=TRUE)

# save HTML-tables to "or_table4.html", indicating p-values as numbers
# and printing CI in a separate column
sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Agriculture"),
        labelPredictors=c("Education", "Examination", "Infant Mortality", "Catholic"),
        file="or_table4.html", pvaluesAsNumbers=TRUE, separateConfColumn=TRUE)
}
\references{
  \url{http://strengejacke.wordpress.com/sjplot-r-package/}
  \cr \cr
  \url{http://strengejacke.wordpress.com/2013/08/20/print-glm-output-to-html-table-rstats/}
}
\seealso{
  \link{sjt.lm}
}

