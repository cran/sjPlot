\name{sjc.grpdisc}
\alias{sjc.grpdisc}
\title{Compute a linear discriminant analysis on classified cluster groups}
\usage{
  sjc.grpdisc(data, groups, groupcount,
    showTotalCorrect = TRUE)
}
\arguments{
  \item{data}{The data frame containing all variables that
  should be used for the check for goodness of
  classification of a cluster analysis.}

  \item{groups}{The group classification of the cluster
  analysis that was returned from the
  \code{\link{sjc.cluster}}-function.}

  \item{groupcount}{The amount of groups (clusters) that
  should be used. Use the \code{\link{sjc.elbow}}-function
  to determine the group-count depending on the
  elbow-criterion.}

  \item{showTotalCorrect}{If \code{TRUE} (default), a
  vertical line indicating the overall goodness of
  classification is added to the plot, so one can see
  whether a certain group is below or above the average
  classification goodness.}
}
\description{
  Computes linear discriminant analysis on classified
  cluster groups.  This function plots a bar graph
  indicating the goodness of classification for each group.
}
\examples{
# retrieve group classification from hierarchical cluster analysis
# on the mtcars data set (5 groups)
groups <- sjc.cluster(mtcars, 5)

# plot goodness of group classificatoin
sjc.grpdisc(mtcars, groups, 5)
}
\seealso{
  \code{\link{sjc.dend}} \cr \code{\link{sjc.cluster}} \cr
  \code{\link{sjc.elbow}}
}

