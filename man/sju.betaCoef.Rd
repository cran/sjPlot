\name{sju.betaCoef}
\alias{sju.betaCoef}
\title{Retrieve std. beta coefficients of lm}
\usage{
sju.betaCoef(fit)
}
\arguments{
  \item{fit}{A fitted linear model.}
}
\value{
The standardiized beta coefficients of the fitted linear
model.
}
\description{
Returns the standardized beta coefficients of a fitted
linear model.
}
\examples{
# fit linear model
fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
# print std. beta coefficients
sju.betaCoef(fit)
}
\seealso{
\code{\link{sjp.lm}}
}

