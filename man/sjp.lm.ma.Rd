\name{sjp.lm.ma}
\alias{sjp.lm.ma}
\title{Plot model assumptions of lm's}
\usage{
sjp.lm.ma(linreg, showOriginalModelOnly = TRUE, completeDiagnostic = FALSE)
}
\arguments{
  \item{linreg}{a fitted lm-model}

  \item{showOriginalModelOnly}{if \code{TRUE} (default),
  only the model assumptions of the fitted model
  \code{linreg} are plotted. if \code{FALSE}, the model
  assumptions of an updated model where outliers are
  automatically excluded are also plotted.}

  \item{completeDiagnostic}{if \code{TRUE}, additional
  tests are performed. Default is \code{FALSE}}
}
\value{
an updated fitted linear model where outliers are dropped
out.
}
\description{
Plots model assumptions of linear models to verify if
linear regression is applicable
}
\examples{
\dontrun{
# fit linear model
fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
fit.updated <- sjp.lm.ma(fit)}
}
\references{
\url{http://rpubs.com/sjPlot/sjplm}
}
\seealso{
\code{\link{sjp.lm}} \cr \code{\link{sjp.reglin}} \cr
\code{\link{sjp.lm.int}}
}

