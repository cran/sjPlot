\docType{data}
\name{efc}
\alias{efc}
\title{Sample dataset from the EUROFAMCARE project}
\description{
  Sample dataset from the EUROFAMCARE project
}
\examples{
# Attach EFC-data
data(efc)

# Show structure
str(efc)

# show first rows
head(efc)

# show variable labels
sji.getVariableLabels(efc)
}
\references{
  \url{http://www.uke.de/eurofamcare/}
}
\seealso{
  \link{sji.SPSS} \cr \link{sji.getValueLabels} \cr
  \link{sji.getVariableLabels} \cr
  \link{sji.convertToLabel} \cr \link{sjp.frq}
}
\keyword{data}
\keyword{datasets}

