\name{sjc.dend}
\alias{sjc.dend}
\title{Compute hierarchical cluster analysis and visualize group classification}
\usage{
  sjc.dend(data, groupcount)
}
\arguments{
  \item{data}{The data frame containing all variables that
  should be used for the cluster analysis.}

  \item{groupcount}{The amount of groups (clusters) that
  should be used. Use the \code{\link{sjc.elbow}}-function
  to determine the group-count depending on the
  elbow-criterion. Use \code{\link{sjc.grpdisc}}-function
  to inspect the goodness of grouping.}
}
\description{
  Computes a hierarchical cluster analysis and plots a
  hierarchical dendogram with highlighting rectangles
  around the classified groups.  Can be used, for instance,
  as visual tool to verify the elbow-criterion (see
  \code{\link{sjc.elbow}}).
}
\examples{
# Plot dendogram of hierarchical clustering of mtcars-dataset
# and show group classification
sjc.dend(mtcars, 5)
}
\seealso{
  \code{\link{sjc.cluster}} \cr \code{\link{sjc.grpdisc}}
  \cr \code{\link{sjc.elbow}}
}

