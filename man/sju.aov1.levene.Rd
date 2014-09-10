\name{sju.aov1.levene}
\alias{sju.aov1.levene}
\title{Plot Levene-Test for One-Way-Anova}
\usage{
sju.aov1.levene(depVar, grpVar)
}
\arguments{
  \item{depVar}{The dependent variable. Will be used with
  following formular: \code{aov(depVar ~ grpVar)}}

  \item{grpVar}{The grouping variable, as unordered factor.
  Will be used with following formular: \code{aov(depVar ~
  grpVar)}}
}
\description{
Plot results of Levene's Test for Equality of Variances for
One-Way-Anova.
}
\examples{
data(efc)
sju.aov1.levene(efc$c12hour, efc$e42dep)
}
\seealso{
\code{\link{sjp.aov1}}, \code{\link{sjs.chi2.gof}},
\code{\link{sjs.mwu}} and \code{\link{wilcox.test}},
\code{\link{ks.test}}, \code{\link{kruskal.test}},
\code{\link{t.test}}, \code{\link{chisq.test}},
\code{\link{fisher.test}}
}

