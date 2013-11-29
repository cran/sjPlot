\name{sjc.elbow}
\alias{sjc.elbow}
\title{Plot elbow values of a k-means cluster analysis}
\usage{
  sjc.elbow(data, steps = 15)
}
\arguments{
  \item{data}{The data frame containing all variables that
  should be used for determining the elbow criteria.}

  \item{steps}{The maximum group-count for the k-means
  cluster analysis for which the elbow-criterion should be
  displayed. Default is \code{15}.}
}
\description{
  Plot elbow values of a k-means cluster analysis. This
  function computes a k-means cluster analysis on the
  provided data frame and produces two plots: one with the
  different elbow values and a second plot that maps the
  differences between each "step" (i.e. between elbow
  values) on the y-axis. An increase in the second plot may
  indicate the elbow criterion.
}
\examples{
# plot elbow values of mtcars dataset
sjc.elbow(mtcars)
}
\seealso{
  \code{\link{sjc.dend}} \cr \code{\link{sjc.cluster}} \cr
  \code{\link{sjc.grpdisc}}
}

