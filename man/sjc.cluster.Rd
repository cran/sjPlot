\name{sjc.cluster}
\alias{sjc.cluster}
\title{Compute hierarchical or kmeans cluster analysis}
\usage{
sjc.cluster(data, groupcount, method = "h", distance = "euclidean",
  agglomeration = "ward")
}
\arguments{
  \item{data}{The data frame containing all variables that
  should be used for the cluster analysis.}

  \item{groupcount}{The amount of groups (clusters) that
  should be retrieved. Following functions may be helpful
  for estimating the amount of clusters: \itemize{ \item
  Use \code{\link{sjc.elbow}}-function to determine the
  group-count depending on the elbow-criterion.  \item If
  using kmeans as \code{method}, use
  \code{\link{sjc.kgap}}-function to determine the
  group-count according to the gap-statistic.  \item If
  using hierarchical as \code{method} (default), use
  \code{\link{sjc.dend}}-function to inspect different
  cluster group solutions.  \item Use
  \code{\link{sjc.grpdisc}}-function to inspect the
  goodness of grouping (accuracy of classification).  }}

  \item{method}{Indicates the clustering method. If
  \code{"h"} (default), a hierachical clustering using the
  ward method is computed. Use any other parameter to
  compute a k-means clustering.}

  \item{distance}{The distance measure to be used when
  \code{"method"} is \code{"h"} (for hierarchical
  clustering). This must be one of \code{"euclidean"}
  (default), \code{"maximum"}, \code{"manhattan"},
  \code{"canberra"}, \code{"binary"} or \code{"minkowski"}.
  See \link{dist}.}

  \item{agglomeration}{The agglomeration method to be used
  when \code{"method"} is \code{"h"} (for hierarchical
  clustering). This should be one of \code{"ward"},
  \code{"single"}, \code{"complete"}, \code{"average"},
  \code{"mcquitty"}, \code{"median"} or \code{"centroid"}.
  Default is \code{"ward"}. See \link{hclust}.}
}
\value{
The group classification for each observation as vector.
This group classification is needed for
\code{\link{sjc.grpdisc}}-function to check the goodness of
classification.
}
\description{
Compute hierarchical or kmeans cluster analysis and returns
the group association for each observation as vector.
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

