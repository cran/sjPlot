\name{sjt.glm}
\alias{sjt.glm}
\title{Save odds Ratios as HTML-Table}
\usage{
sjt.glm(..., file = NULL, labelPredictors = NULL,
  labelDependentVariables = NULL, stringPredictors = "Predictors",
  stringDependentVariables = "Dependent Variables", stringModel = "Model",
  stringIntercept = "(Intercept)", stringObservations = "Observations",
  pvaluesAsNumbers = FALSE, boldpvalues = TRUE, showConfInt = TRUE,
  separateConfColumn = FALSE, showAbbrHeadline = TRUE, showPseudoR = TRUE,
  showLogLik = FALSE, showAIC = FALSE)
}
\arguments{
  \item{...}{One or more fitted glm-objects.}

  \item{file}{The destination file, which will be in
  html-format. If no filepath is specified, the file will
  be saved as temporary file and openend either in the
  RStudio View pane or in the default web browser.}

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

  \item{stringObservations}{String constant used in the
  summary row for the count of observation (cases). Default
  is \code{"Observations"}.}

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

  \item{showPseudoR}{If \code{TRUE} (default), the pseudo
  R2 values for each model are printed in the model
  summary. R2cs is the Cox-Snell-pseudo R-square value, R2n
  is Nagelkerke's pseudo R-square value.}

  \item{showLogLik}{If \code{TRUE}, the Log-Likelihood for
  each model is printed in the model summary. Default is
  \code{FALSE}.}

  \item{showAIC}{If \code{TRUE}, the AIC value for each
  model is printed in the model summary. Default is
  \code{FALSE}.}
}
\description{
Save (multiple) generalized linear models (Odds Ratios) as
HTML-Table. The fitted glm's should have the same predictor
variables and differ only in their response (dependent
variable).
}
\note{
The HTML tables can either be saved as file and manually
opened (specify parameter \code{file}) or they can be saved
as temporary files and will be displayed in the RStudio
Viewer pane (if working with RStudio) or opened with the
default web browser. Displaying resp. opening a temporary
file is the default behaviour (i.e. \code{file=NULL}).
}
\examples{
# prepare dummy variables for binary logistic regression
y1 <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
y2 <- ifelse(swiss$Infant.Mortality<median(swiss$Infant.Mortality), 0, 1)
y3 <- ifelse(swiss$Agriculture<median(swiss$Agriculture), 0, 1)

# Now fit the models. Note that both models share the same predictors
# and only differ in their dependent variable (y1, y2 and y3)
fitOR1 <- glm(y1 ~ swiss$Education+swiss$Examination+swiss$Catholic,
              family=binomial(link="logit"))
fitOR2 <- glm(y2 ~ swiss$Education+swiss$Examination+swiss$Catholic,
              family=binomial(link="logit"))
fitOR3 <- glm(y3 ~ swiss$Education+swiss$Examination+swiss$Catholic,
              family=binomial(link="logit"))

# open HTML-table in RStudio Viewer Pane or web browser
## Note that example may open browser, thus it is outcommented
# sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Infant Mortality"),
#         labelPredictors=c("Education", "Examination", "Catholic"))

# open HTML-table in RStudio Viewer Pane or web browser,
# table indicating p-values as numbers
## Note that example may open browser, thus it is outcommented
# sjt.glm(fitOR1, fitOR2, labelDependentVariables=c("Fertility", "Infant Mortality"),
#         labelPredictors=c("Education", "Examination", "Catholic"),
#         pvaluesAsNumbers=TRUE)

# save HTML-tables to "or_table3.html", printing CI in a separate column
sjt.glm(fitOR1, fitOR2, fitOR3,
        labelDependentVariables=c("Fertility", "Infant Mortality", "Agriculture"),
        labelPredictors=c("Education", "Examination", "Catholic"),
        file="or_table3.html", separateConfColumn=TRUE)

# save HTML-tables to "or_table4.html", indicating p-values as numbers
# and printing CI in a separate column
sjt.glm(fitOR1, fitOR2, fitOR3,
        labelDependentVariables=c("Fertility", "Infant Mortality", "Agriculture"),
        labelPredictors=c("Education", "Examination", "Catholic"),
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

