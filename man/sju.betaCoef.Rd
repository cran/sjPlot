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
\note{
"Standardized coefficients refer to how many standard
deviations a dependent variable will change, per standard
deviation increase in the predictor variable.
Standardization of the coefficient is usually done to
answer the question of which of the independent variables
have a greater effect on the dependent variable in a
multiple regression analysis, when the variables are
measured in different units of measurement (for example,
income measured in dollars and family size measured in
number of individuals)." (Source: Wikipedia)
}
\examples{
# fit linear model
fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
# print std. beta coefficients
sju.betaCoef(fit)
}
\references{
\url{http://en.wikipedia.org/wiki/Standardized_coefficient}
}
\seealso{
\code{\link{sjp.lm}} \cr \code{\link{sjt.lm}}
}

