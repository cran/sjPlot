# -------------------------------------
# Help-functions
# -------------------------------------


# -------------------------------------
# Calculate statistics of cross tabs
# -------------------------------------
crosstabsum <- function(ftab) {
  # calculate chi square value
  chsq <- chisq.test(ftab)
  tab <- sjs.table.values(ftab)
  fish <- NULL
  # check whether variables are dichotome or if they have more
  # than two categories. if they have more, use Cramer's V to calculate
  # the contingency coefficient
  if (nrow(ftab)>2 || ncol(ftab)>2) {
    # if minimum expected values below 5, compute fisher's exact test
    if(min(tab$expected)<5 || (min(tab$expected)<10 && chsq$parameter==1)) fish <- fisher.test(ftab, simulate.p.value=TRUE)
    # check whether fisher's test or chi-squared should be printed
    if (is.null(fish)) {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "p" == pva,
                   list(tn=summary(ftab)$n.cases,
                        c2=sprintf("%.2f", chsq$statistic),
                        dft=c(chsq$parameter),
                        kook=sprintf("%.2f", sjs.cramer(ftab)),
                        pva=sprintf("%.3f", chsq$p.value)))))
    }
    else {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "Fisher's p" == pva,
                   list(tn=summary(ftab)$n.cases,
                        dft=c(chsq$parameter),
                        kook=sprintf("%.2f", sjs.cramer(ftab)),
                        pva=sprintf("%.3f", fish$p.value)))))
    }
  }
  # if variables have two categories (2x2 table), use phi to calculate
  # the degree of association
  else {
    # if minimum expected values below 5, compute fisher's exact test
    if(min(tab$expected)<5 || (min(tab$expected)<10 && chsq$parameter==1)) fish <- fisher.test(ftab)
    # check whether fisher's test or chi-squared should be printed
    if (is.null(fish)) {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "p" == pva,
                   list(tn=summary(ftab)$n.cases,
                        c2=sprintf("%.2f", chsq$statistic),
                        dft=c(chsq$parameter),
                        kook=sprintf("%.2f", sjs.phi(ftab)),
                        pva=sprintf("%.3f", chsq$p.value)))))
    }
    else {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "Fisher's p" == pva,
                   list(tn=summary(ftab)$n.cases,
                        dft=c(chsq$parameter),
                        kook=sprintf("%.2f", sjs.phi(ftab)),
                        pva=sprintf("%.3f", fish$p.value)))))
    }
  }  
  return (modsum)
}


# -------------------------------------
# automatically set labels of values,
# if attributes are present
# -------------------------------------
autoSetValueLabels <- function(x) {
  # check if we have value label attribut
  vl <- attr(x, "value.labels")
  lv <- levels(x)
  label <- NULL
  # check  if we have value labels
  if (!is.null(vl) && length(vl)>0) {
    label <- rev(names(vl))
  }
  # check  if we have factor levels
  else if (!is.null(lv)) {
    label <- lv
  }
  # if we have string values, copy them as labels
  else if (is.character(x)) {
    label <- unique(x)
  }
  return(label)
}


# -------------------------------------
# automatically set labels of variables,
# if attributes are present
# -------------------------------------
autoSetVariableLabels <- function(x) {
  # check if we have variable label attribut
  vl <- as.vector(attr(x, "variable.label"))
  label <- NULL
  # check if we have variable labels
  if (!is.null(vl) && length(vl)>0) {
    label <- vl
  }
  return(label)
}


# -------------------------------------
# compute pseudo r-square for glm
# -------------------------------------
PseudoR2 <- function(rr) { # rr must be the result of lm/glm
  n <- nrow(rr$model)
  COX <- (1-exp((rr$deviance-rr$null)/n))
  NR <- COX/(1-exp(-rr$null/n))
  RVAL <- c(N=n, CoxSnell=COX, Nagelkerke=NR)
  return(RVAL)
}


# -------------------------------------
# compute chi-square for glm
# -------------------------------------
Chisquare.glm <- function(rr, digits=3) {
  return (with(rr, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE), digits=digits))
}


# -------------------------------------
# compute model statistics for lm
# -------------------------------------
sju.modsum.lm <- function(fit) {
  # get F-statistics
  fstat <- summary(fit)$fstatistic
  # Calculate p-value for F-test
  pval <- pf(fstat[1], fstat[2], fstat[3],lower.tail = FALSE)
  # indicate significance level by stars
  pan <- c("")
  if (pval<=0.001) {
    pan <- c("***")
  }
  else  if (pval<=0.01) {
    pan <- c("**")
  }
  else  if (pval<=0.05) {
    pan <- c("*")
  }
  # create mathematical term
  modsum <- as.character(as.expression(
    substitute(italic(b[0]) == a * "," ~~ R^2 == r2 * "," ~~ "adj. " * R^2 == ar2 * "," ~~ "F" == f*panval * "," ~~ "AIC" == aic,
               list(a=format(coef(fit)[1], digits=3),
                    r2=format(summary(fit)$r.squared, digits=3),
                    ar2=format(summary(fit)$adj.r.squared, digits=3),
                    f=sprintf("%.2f", fstat[1]),
                    panval=pan,
                    aic=sprintf("%.2f", AIC(fit))))))
  return(modsum)
}
# --------------------------------------------------------
# Erzeugt eine rotierte Faktorladungen einer Hauptkomponentenanalyse
# (Paramter "data") mit einer bestimmten Anzahl an Faktoren (Parameter "factors")
# auf Grundlage der Varimax-Rotation
#
# Parameter:
# - data: the results (object) from a principal component analysis
#         (prcomp(myData...))
# - factors: the amount of factors. can be calculated from the
#            below function "factorcount"
# --------------------------------------------------------
varimaxrota <- function(data, factors) {
  # Faktorladungen berechnen
  # Die Faktorladungen erhält man durch Multiplikation der Eigenvektoren
  # mit der Diagonalmatrix der ausgewiesenen Standardabweichungen
  ladungen <- data$rotation%*%diag(data$sdev)
  # Zur Durchführung der VARIMAX-Rotation erzeugen wir eine Matrix
  # mit den Faktorladungen der ausgewählten Faktoren (Anzahl = Parameter "factors")
  ladb <- c()
  for (i in 1:factors) {
    ladb <- cbind(ladb, ladungen[,i])
  }
  # Varimax Rotation durchführen
  varib <- varimax(ladb)
  return (varib)
}


# --------------------------------------------------------
# unlist labels
# Help function that unlists a list into a vector
# --------------------------------------------------------
unlistlabels <- function(lab) {
  dummy <- unlist(lab)
  labels <- c()
  for (i in 1:length(dummy)) {
    labels <- c(labels, as.character(dummy[i]))
  }
  return (labels)
}
