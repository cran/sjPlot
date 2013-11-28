#' @title Recode scales into grouped factors
#' @name sju.groupVar
#' @description Recode scales into grouped factors.
#' @seealso \code{\link{sju.groupVarLabels}}
#'
#' @param var The scale variable, which should recoded into groups.
#' @param groupsize The group-size, i.e. the range for grouping. By default, for each 5 categories 
#'          new group is built.
#' @param asNumeric If \code{TRUE} (default), the recoded variable will be returned as numeric vector.
#'          If \code{FALSE}, a factor is returned.
#' @param rightInterval If \code{TRUE}, grouping starts with the lower bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 50-54, 55-59, 60-64 etc. \cr
#'          If \code{FALSE} (default), grouping starts with the upper bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 51-55, 56-60, 61-65 etc.
#' @return A grouped variables, either as numeric or as factor (see paramter \code{asNumeric}).
#' 
#' @examples
#' age <- abs(round(rnorm(100, 65, 20)))
#' age.grp <- sju.groupVar(age, 10)
#' hist(age)
#' hist(age.grp)
#' 
#' # histogram with EUROFAMCARE sample dataset
#' # variable not grouped
#' data(efc)
#' efc.val <- sji.getValueLabels(efc)
#' efc.var <- sji.getVariableLabels(efc)
#' sjp.frq(efc$e17age,
#'         title=efc.var[['e17age']],
#'         type="h",
#'         showValueLabels=FALSE)
#' 
#' # bar plot with EUROFAMCARE sample dataset
#' # grouped variable
#' data(efc)
#' efc.val <- sji.getValueLabels(efc)
#' efc.var <- sji.getVariableLabels(efc)
#' ageGrp <- sju.groupVar(efc$e17age)
#' ageGrpLab <- sju.groupVarLabels(efc$e17age)
#' sjp.frq(ageGrp,
#'         title=efc.var[['e17age']],
#'         axisLabels.x=ageGrpLab,
#'         maxYlim=FALSE)
#'  
#' @export
sju.groupVar <- function(var, groupsize=5, asNumeric=TRUE, rightInterval=FALSE) {
  # Einteilung der Variablen in Gruppen. Dabei werden unbenutzte Faktoren gleich entfernt
  var <- droplevels(cut(var,breaks=c(seq(0,max(na.omit(var))+2*groupsize,by=groupsize)), right=rightInterval))
  # Die Level der Gruppierung wird neu erstellt
  levels(var) <- c(1:length(levels(var)))
  # in numerisch umwandeln
  if (asNumeric) {
    var <- as.numeric(as.character(var))  
  }
  return (var)
}

#' @title Create labels for recoded groups
#' @name sju.groupVarLabels
#' @description Creates the related labels for the grouped variable created by
#'                the \code{\link{sju.groupVar}} function.
#' @seealso \code{\link{sju.groupVar}}
#' @note Usually you should use the same values for \code{groupsize} and
#'         \code{rightInterval} as used in the \code{\link{sju.groupVar}} function
#'         if you want to create labels for the related recoded variable.
#'         
#' @param var The scale variable, which should recoded into groups.
#' @param groupsize The group-size, i.e. the range for grouping. By default, for each 5 categories 
#'          new group is built.
#' @param rightInterval If \code{TRUE}, grouping starts with the lower bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 50-54, 55-59, 60-64 etc. \cr
#'          If \code{FALSE} (default), grouping starts with the upper bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 51-55, 56-60, 61-65 etc.
#' @return A string vector containing labels based on the grouped counts of \code{var},
#'           formatted as "from lower bound to upper bound", e.g. \code{"10-19"  "20-29"  "30-39"} etc.
#'           See example below.
#' 
#' @examples
#' age <- abs(round(rnorm(100, 65, 20)))
#' age.grp <- sju.groupVar(age, 10)
#' hist(age)
#' hist(age.grp)
#' 
#' age.grpvar <- sju.groupVarLabels(age, 10)
#' table(age.grp)
#' print(age.grpvar)
#' 
#' # histogram with EUROFAMCARE sample dataset
#' # variable not grouped
#' data(efc)
#' efc.val <- sji.getValueLabels(efc)
#' efc.var <- sji.getVariableLabels(efc)
#' sjp.frq(efc$e17age,
#'         title=efc.var[['e17age']],
#'         type="h",
#'         showValueLabels=FALSE)
#' 
#' # bar plot with EUROFAMCARE sample dataset
#' # grouped variable
#' data(efc)
#' efc.val <- sji.getValueLabels(efc)
#' efc.var <- sji.getVariableLabels(efc)
#' ageGrp <- sju.groupVar(efc$e17age)
#' ageGrpLab <- sju.groupVarLabels(efc$e17age)
#' sjp.frq(ageGrp,
#'         title=efc.var[['e17age']],
#'         axisLabels.x=ageGrpLab,
#'         maxYlim=FALSE)
#' 
#' @export
sju.groupVarLabels <- function(var, groupsize=5, rightInterval=FALSE) {
  # Einteilung der Variablen in Gruppen. Dabei werden unbenutzte Faktoren gleich entfernt
  var <- droplevels(cut(var,breaks=c(seq(0,max(na.omit(var))+2*groupsize,by=groupsize)), right=rightInterval))
  # Gruppen holen
  lvl <- levels(var) 
  # rückgabewert init
  retval <- rep(c(""), length(lvl))
  # alle Gruppierungen durchgehen
  for (i in 1:length(lvl)) {
    # Länge jedes Labels der Gruppeneinteilungen auslesen
    sublength <- nchar(lvl[i])
    # "(" und "]", das bei "cut"-Funktion automatisch erstellt wird, aus dem Label entfernen
    lvlstr <- substr(lvl[i], 2, sublength-1)
    # Unter- und Obergrenze in jeweils einem string
    subs <- strsplit(lvlstr, ",")
    # Untergrenze als Zahlenwert
    lower <- as.numeric(subs[[1]][1])
    # Obergrenze als Zahlenwert
    upper <- as.numeric(subs[[1]][2])
    # Prüfen, welche Intervallgrenze ein- und welche ausgeschlossen werden soll
    if(rightInterval) {
      lower <- lower+1
    }
    else {
      upper <- upper-1
    }
    # Rückgabe des Strings
    retval[i] <- c(paste(c(lower), "-", c(upper), sep=""))
  }
  return (c(retval))
}



#' @title Retrieve std. beta coefficients of lm
#' @name sju.betaCoef
#' @description Returns the standardized beta coefficients of a fitted linear model.
#' @seealso \code{\link{sjp.lm}}
#'         
#' @param fit A fitted linear model.
#' @return The standardiized beta coefficients of the fitted linear model.
#' 
#' @examples
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#' # print std. beta coefficients
#' sju.betaCoef(fit)
#' 
#' @export
sju.betaCoef <- function(fit) {
  b <- summary(fit)$coef[-1, 1]
  sx <- sapply(fit$model[-1], sd)
  sy <- sapply(fit$model[1], sd)
  beta <- b * sx/sy
  return(beta)
}