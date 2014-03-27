\name{sji.setValueLabels}
\alias{sji.setValueLabels}
\title{Attach value labels to a variable or vector}
\usage{
sji.setValueLabels(var, labels)
}
\arguments{
  \item{var}{a variable (vector) where labels should be
  attached. Replaces former value labels.}

  \item{labels}{a character vector of labels that will be
  attached to \code{"var"} by setting the
  \code{"value.labels"} attribute. The length of this
  character vector must equal the value range of
  \code{"var"}, i.e. if \code{"var"} has values from 1 to
  3, \code{"labels"} should have a length of 3.}
}
\value{
the variable \code{"var"} with attached value labels.
}
\description{
This function attaches character labels as
\code{"value.labels"} attribute to a variable or vector
\code{"var"}. These value labels will be accessed by most
of this package's functions, in order to automatically set
values or legend labels.
}
\examples{
dummy <- sample(1:4, 40, replace=TRUE)
sjp.frq(dummy)

dummy <- sji.setValueLabels(dummy, c("very low", "low", "mid", "hi"))
sjp.frq(dummy)
}
\seealso{
\link{sji.SPSS} \cr \link{sji.getVariableLabels} \cr
\link{sji.convertToLabel} \cr \link{sji.convertToValue} \cr
\link{sji.getValueLabels}
}

