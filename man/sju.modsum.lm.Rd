\name{sju.modsum.lm}
\alias{sju.modsum.lm}
\title{Retrieve model summary of lm}
\usage{
sju.modsum.lm(fit)
}
\arguments{
  \item{fit}{A fitted linear model.}
}
\value{
The model summary of the fitted linear model as
\link{expression} string.
}
\description{
Returns a model summary of a fitted linear model as
expression.  The summary includes intercept, r2,
F-statistics and AIC.  It is returned as \link{expression}
string to be used in plots like ggplot.
}
\examples{
# fit linear model
fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
# print model summary
sju.modsum.lm(fit)
}
\seealso{
\code{\link{sjp.lm}} \cr \code{\link{sjp.lm1}} \cr
\code{\link{sjp.lm.ma}}
}

