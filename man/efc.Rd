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

# show variables
\dontrun{
sji.viewSPSS(efc)}

# show variable labels
sji.getVariableLabels(efc)

# plot efc-data frame summary via "describe" of the "psysch package"
\dontrun{
require(psych)
sjt.df(round(describe(efc),2), showType=F, alternateRowColors=T, stringObservation="Variable")}
}
\references{
\url{http://www.uke.de/eurofamcare/}
}
\seealso{
\code{\link{sji.SPSS}} \cr \code{\link{sji.viewSPSS}} \cr
\code{\link{sjt.df}} \cr \code{\link{sji.getValueLabels}}
\cr \code{\link{sji.getVariableLabels}} \cr
\code{\link{sji.convertToLabel}} \cr \code{\link{sjp.frq}}
}
\keyword{data}

