\name{sji.getVariableLabels}
\alias{sji.getVariableLabels}
\title{Retrieve variable labels of an SPSS-imported data frame}
\usage{
sji.getVariableLabels(dat)
}
\arguments{
  \item{dat}{A data frame containing imported SPSS data.}
}
\value{
A list with all variable labels from the SPSS dataset.
}
\description{
This function retrieves the variable labels of an imported
SPSS data set and returns the result as list.
}
\examples{
# import SPSS data set
# mydat <- sji.SPSS("my_spss_data.sav", enc="UTF-8")

# retrieve variable labels
# mydat.var <- sji.getVariableLabels(mydat)

# retrieve value labels
# mydat.val <- sji.getValueLabels(mydat)
}
\seealso{
\link{sji.getValueLabels} \cr \link{sji.SPSS} \cr
\link{sji.convertToLabel} \cr \link{sji.convertToValue}
}

