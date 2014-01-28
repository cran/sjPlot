\name{sjp.vif}
\alias{sjp.vif}
\title{Plot multicollinearity of linear models}
\usage{
sjp.vif(fit, printnumbers = TRUE)
}
\arguments{
  \item{fit}{The fitted (generalized) linear model which
  should be checked for multicollinearity.}

  \item{printnumbers}{If \code{TRUE} (default), the VIF
  value are also printed to console.}
}
\description{
Plots the Variance Inflation Factors (check for
multicollinearity) of (generalized) linear models. Values
below 5 are good and indicating no multicollinearity,
values between 5 and 10 may be tolerable. Values greater
than 10 are not acceptable and indicate multicollinearity
between model's predictors.
}
\examples{
# fit linear model
fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
# plot VIF values
sjp.vif(fit)
}

