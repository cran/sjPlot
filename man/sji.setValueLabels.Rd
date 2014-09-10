\name{sji.setValueLabels}
\alias{sji.setValueLabels}
\title{Attach value labels to a variable or vector}
\usage{
sji.setValueLabels(x, labels)
}
\arguments{
  \item{x}{a variable (vector) or a data frame where labels
  should be attached. Replaces former value labels.}

  \item{labels}{a character vector of labels that will be
  attached to \code{"x"} by setting the
  \code{"value.labels"} attribute. The length of this
  character vector must equal the value range of
  \code{"x"}, i.e. if \code{"x"} has values from 1 to 3,
  \code{"labels"} should have a length of 3.  If \code{"x"}
  is a data frame, \code{labels} must be a
  \code{\link{list}} of character vectors.}
}
\value{
\code{"x"} with attached value labels.
}
\description{
This function attaches character labels as
\code{"value.labels"} attribute to a variable or vector
\code{"x"}, resp. to all variables of a data frame if
\code{"x"} is a \code{\link{data.frame}}. These value
labels will be accessed by most of this package's
functions, in order to automatically set values or legend
labels.
}
\examples{
dummy <- sample(1:4, 40, replace=TRUE)
sjp.frq(dummy)

dummy <- sji.setValueLabels(dummy, c("very low", "low", "mid", "hi"))
sjp.frq(dummy)
}
\references{
\url{http://rpubs.com/sjPlot/datainit}
}
\seealso{
\link{sji.SPSS} \cr \link{sji.getVariableLabels} \cr
\link{sji.convertToLabel} \cr \link{sji.convertToValue} \cr
\link{sji.getValueLabels}
}

