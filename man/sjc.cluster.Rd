\name{sjc.cluster}
\alias{sjc.cluster}
\title{Compute hierarchical or kmeans cluster analysis}
\usage{
  sjc.cluster(data, groupcount, method = "h")
}
\arguments{
  \item{data}{The data frame containing all variables that
  should be used for the cluster analysis.}

  \item{groupcount}{The amount of groups (clusters) that
  should be retrieved. Use the
  \code{\link{sjc.elbow}}-function to determine the
  group-count depending on the elbow-criterion. Use
  \code{\link{sjc.grpdisc}}-function to inspect the
  goodness of grouping.}

  \item{method}{Indicates the clustering method. If
  \code{"h"} (default), a hierachical clustering using the
  ward method is computed. Use any other parameter to
  compute a k-means clustering.}
}
\value{
  The group classification for each observation as vector.
  This group classification is needed for
  \code{\link{sjc.grpdisc}}-function to check the goodness
  of classification.
}
\description{
  Compute hierarchical or kmeans cluster analysis and
  returns the group association for each observation as
  vector.
}
\examples{
# Hierarchical clustering of mtcars-dataset
groups <- sjc.cluster(mtcars, 5)

# K-means clustering of mtcars-dataset
groups <- sjc.cluster(mtcars, 5, method="k")
}
\seealso{
  \code{\link{sjc.dend}} \cr \code{\link{sjc.grpdisc}} \cr
  \code{\link{sjc.elbow}}
}

