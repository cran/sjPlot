\name{sju.dicho}
\alias{sju.dicho}
\title{Dichotomize variables}
\usage{
sju.dicho(var, dichBy = "median", dichVal = -1)
}
\arguments{
  \item{var}{The variable that should be dichotomized.}

  \item{dichBy}{Indicates the split criterion where the
  variable is dichotomized. By default, \code{var} is split
  into two groups at the median (\code{dichBy="median"}).
  Further values for \code{dichBy} are \code{"mean"} (split
  into groups at the mean of \code{var})), and
  \code{"value"}. In the latter case, you have to specifiy
  \code{dichVal}.}

  \item{dichVal}{Indicates a value where \code{var} is
  dichotomized when \code{dichBy="value"}.  Note that
  \code{dichVal} is inclusive, i.e. \code{dichVal=10} will
  split \code{var} into one group with values from lowest
  to 10 and another group with values greater than 10.}
}
\value{
A dichotomized variable (0/1-coded).
}
\description{
Dichotomizes variables into dummy variables (0/1).
Dichotomization is either done by median or mean (see
\code{dichBy}).
}
\examples{
data(efc)
summary(efc$c12hour)
table(sju.dicho(efc$c12hour))
table(sju.dicho(efc$c12hour, "mean"))
table(sju.dicho(efc$c12hour, "value", 30))
}

