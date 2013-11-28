\name{sju.groupVar}
\alias{sju.groupVar}
\title{Recode scales into grouped factors}
\usage{
  sju.groupVar(var, groupsize = 5, asNumeric = TRUE,
    rightInterval = FALSE)
}
\arguments{
  \item{var}{The scale variable, which should recoded into
  groups.}

  \item{groupsize}{The group-size, i.e. the range for
  grouping. By default, for each 5 categories new group is
  built.}

  \item{asNumeric}{If \code{TRUE} (default), the recoded
  variable will be returned as numeric vector.  If
  \code{FALSE}, a factor is returned.}

  \item{rightInterval}{If \code{TRUE}, grouping starts with
  the lower bound of \code{groupsize}. In this case, groups
  cover the ranges from 50-54, 55-59, 60-64 etc. \cr If
  \code{FALSE} (default), grouping starts with the upper
  bound of \code{groupsize}. In this case, groups cover the
  ranges from 51-55, 56-60, 61-65 etc.}
}
\value{
  A grouped variables, either as numeric or as factor (see
  paramter \code{asNumeric}).
}
\description{
  Recode scales into grouped factors.
}
\examples{
age <- abs(round(rnorm(100, 65, 20)))
age.grp <- sju.groupVar(age, 10)
hist(age)
hist(age.grp)

# histogram with EUROFAMCARE sample dataset
# variable not grouped
data(efc)
efc.val <- sji.getValueLabels(efc)
efc.var <- sji.getVariableLabels(efc)
sjp.frq(efc$e17age,
        title=efc.var[['e17age']],
        type="h",
        showValueLabels=FALSE)

# bar plot with EUROFAMCARE sample dataset
# grouped variable
data(efc)
efc.val <- sji.getValueLabels(efc)
efc.var <- sji.getVariableLabels(efc)
ageGrp <- sju.groupVar(efc$e17age)
ageGrpLab <- sju.groupVarLabels(efc$e17age)
sjp.frq(ageGrp,
        title=efc.var[['e17age']],
        axisLabels.x=ageGrpLab,
        maxYlim=FALSE)
}
\seealso{
  \code{\link{sju.groupVarLabels}}
}

