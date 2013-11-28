\name{sji.convertToValue}
\alias{sji.convertToValue}
\title{Converts factors to numeric variables}
\usage{
  sji.convertToValue(fac, startAt = 1)
}
\arguments{
  \item{fac}{A (factor) variable.}

  \item{startAt}{the starting index, i.e. numeric value of
  the variable.}
}
\value{
  A numeric variable with values ranging from
  \code{startAt} to \code{startAt} + length of factor
  levels.
}
\description{
  This function converts (replaces) factor values with the
  related factor level index number, thus the factor is
  converted to a numeric variable.
}
\examples{
data(efc)
test <- sji.convertToLabel(efc$e42dep)
table(test)

table(sji.convertToValue(test))
hist(sji.convertToValue(test,0))
}
\seealso{
  \link{sji.convertToLabel} \cr \link{sji.getValueLabels}
  \cr \link{sji.getVariableLabels} \cr \link{sji.SPSS}
}

