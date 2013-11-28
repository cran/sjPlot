\name{sji.SPSS}
\alias{sji.SPSS}
\title{Import SPSS dataset as data frame into R}
\usage{
  sji.SPSS(path, enc = NA)
}
\arguments{
  \item{path}{The file path to the SPSS dataset.}

  \item{enc}{The file encoding of the SPSS dataset.}
}
\value{
  A data frame containing the SPSS data. retrieve value
  labels with \code{\link{sji.getValueLabels}} and variable
  labels with \code{\link{sji.getVariableLabels}}.
}
\description{
  Import data from SPSS, including NA's, value and variable
  labels.
}
\examples{
# import SPSS data set
# mydat <- sji.SPSS("my_spss_data.sav", enc="UTF-8")

# retrieve variable labels
# mydat.var <- sji.getVariableLabels(mydat)

# retrieve value labels
# mydat.val <- sji.getValueLabels(mydat)
}
\references{
  \url{http://strengejacke.wordpress.com/sjplot-r-package/}
  \cr \cr
  \url{http://strengejacke.wordpress.com/2013/02/24/simplify-your-r-workflow-with-functions-rstats/}
}
\seealso{
  \link{sji.getValueLabels} \cr
  \link{sji.getVariableLabels} \cr
  \link{sji.convertToLabel} \cr \link{sji.convertToValue}
}

