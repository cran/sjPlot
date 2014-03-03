# -------------------------------------
# Help-functions
# -------------------------------------
PseudoR2 <- function(rr) { # rr must be the result of lm/glm
  n <- nrow(rr$model)
  COX <- (1-exp((rr$deviance-rr$null)/n))
  NR <- COX/(1-exp(-rr$null/n))
  RVAL <- c(N=n, CoxSnell=COX, Nagelkerke=NR)
  return(RVAL)
}
Chisquare.glm <- function(rr, digits=3) {
  return (with(rr, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE), digits=digits))
}
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
    substitute(italic(b[0]) == a * "," ~~ R^2 == r2 * "," ~~ "F" == f*panval * "," ~~ "AIC" == aic,
               list(a=format(coef(fit)[1], digits=3),
                    r2=format(summary(fit)$r.squared, digits=3),
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
# ---------------------------------------------
# Source taken from "psych" package, "KMO"-function
# http://cran.r-project.org/web/packages/psych/index.html
# ---------------------------------------------
pca.kmo <- function (r) {
  cl <- match.call()
  if (nrow(r) > ncol(r)) 
    r <- cor(r, use = "pairwise")
  Q <- try(solve(r))
  if (class(Q) == as.character("try-error")) {
    message("matrix is not invertible, image not found")
    Q <- r
  }
  S2 <- diag(1/diag(Q))
  IC <- S2 %*% Q %*% S2
  Q <- Image <- cov2cor(Q)
  diag(Q) <- 0
  diag(r) <- 0
  sumQ2 <- sum(Q^2)
  sumr2 <- sum(r^2)
  MSA <- sumr2/(sumr2 + sumQ2)
  MSAi <- colSums(r^2)/(colSums(r^2) + colSums(Q^2))
  results <- list(MSA = MSA, MSAi = MSAi, Image = Image, ImCov = IC, 
                  Call = cl)
  class(results) <- c("psych", "KMO")
  return(results)
}
# ---------------------------------------------
# Source taken from "psych" package, "cortest.bartlett"-function
# http://cran.r-project.org/web/packages/psych/index.html
# ---------------------------------------------
cor.bart <- function (R, n = NULL) {
  if (dim(R)[1] != dim(R)[2]) {
    n <- dim(R)[1]
    message("R was not square, finding R from data")
    R <- cor(R, use = "pairwise")
  }
  p <- dim(R)[2]
  if (!is.matrix(R)) 
    R <- as.matrix(R)
  if (is.null(n)) {
    n <- 100
    warning("n not specified, 100 used")
  }
  detR <- det(R)
  statistic <- -log(detR) * (n - 1 - (2 * p + 5)/6)
  df <- p * (p - 1)/2
  pval <- pchisq(statistic, df, lower.tail = FALSE)
  bartlett <- list(chisq = statistic, p.value = pval, df = df)
  return(bartlett)
}
# -----------------------------------------------------------------------------
# Source code of the following function was taken from the cluster-package
#
# Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., Hornik, K.(2013). 
# cluster: Cluster Analysis Basics and Extensions. R package version 1.14.4.
# 
# http://cran.r-project.org/web/packages/cluster/index.html
# -----------------------------------------------------------------------------
#### Originally from orphaned package SLmisc
#### (Version: 1.4.1, 2007-04-12, Maintainer: Matthias Kohl <kohl@sirs-lab.com>)
#### License: GPL (version 2 or later)
####
#### which said
####  "function corresponds to function gap in package SAGx"
## MM: SAGx is now in Bioconductor --- 1.10.1{devel} or 1.11.1{release}
##     had gap() *corrected* to re-cluster using FUNcluster --> see ./gap-SAGx.R.~orig~
##
## MM: Package 'lga' -- has gap() and lga and robust lga [-> UBC]
##    - it uses  boot() nicely  [2012-01: ORPHANED because  Justin Harrington is amiss]
## MM: renamed arguments, and changed almost everything
clusGap <- function (x, FUNcluster, K.max, B = 100, verbose = interactive(), ...)
{
  stopifnot(is.function(FUNcluster), length(dim(x)) == 2, K.max >= 2,
            (n <- nrow(x)) >= 1, (p <- ncol(x)) >= 1)
  if(B != (B. <- as.integer(B)) || (B <- B.) <= 0)
    stop("'B' has to be a positive integer")
  
  if(is.data.frame(x))
    x <- as.matrix(x)
  ii <- seq_len(n)
  W.k <- function(X, kk) {
    clus <- if(kk > 1) FUNcluster(X, kk, ...)$cluster else rep.int(1L, nrow(X))
    ##                 ---------- =  =       -------- kmeans() has 'cluster'; pam() 'clustering'
    0.5* sum(vapply(split(ii, clus),
                    function(I) { xs <- X[I,, drop=FALSE]
                                  sum(dist(xs)/nrow(xs)) }, 0.))
  }
  logW <- E.logW <- SE.sim <- numeric(K.max)
  if(verbose) cat("Clustering k = 1,2,..., K.max (= ",K.max,"): .. ", sep='')
  for(k in 1:K.max)
    logW[k] <- log(W.k(x, k))
  if(verbose) cat("done\n")
  
  ## Scale 'x' into "hypercube" -- we later fill with H0-generated data
  xs <- scale(x, center=TRUE, scale=FALSE)
  m.x <- rep(attr(xs,"scaled:center"), each = n)# for back transforming
  V.sx <- svd(xs)$v
  rng.x1 <- apply(xs %*% V.sx, # = transformed(x)
                  2, range)
  
  logWks <- matrix(0., B, K.max)
  if(verbose) cat("Bootstrapping, b = 1,2,..., B (= ", B,
                  ")  [one \".\" per sample]:\n", sep="")
  for (b in 1:B) {
    ## Generate "H0"-data as "parametric bootstrap sample" :
    z1 <- apply(rng.x1, 2,
                function(M, nn) runif(nn, min=M[1], max=M[2]),
                nn=n)
    z <- tcrossprod(z1, V.sx) + m.x # back transformed
    for(k in 1:K.max) {
      logWks[b,k] <- log(W.k(z, k))
    }
    if(verbose) cat(".", if(b %% 50 == 0) paste(b,"\n"))
  }
  if(verbose && (B %% 50 != 0)) cat("",B,"\n")
  E.logW <- colMeans(logWks)
  SE.sim <- sqrt((1 + 1/B) * apply(logWks, 2, var))
  structure(class = "clusGap",
            list(Tab = cbind(logW, E.logW, gap = E.logW - logW, SE.sim),
                 ## K.max == nrow(T)
                 n = n, B = B, FUNcluster=FUNcluster))
}
## lga/R/gap.R   --- has for Tibshirani et al (2001):
## ElogWks[k,] <- c(mean(BootOutput), sqrt(var(BootOutput)*(1+1/B)))
## GAP[k] <- ElogWks[k,1] - logWks[k]
## if (k > 1)
##     if(GAP[k-1] >= GAP[k]-ElogWks[k,2] & !doall)
##         finished <- TRUE
##  so they effectively only look for the *first* (local) maximum which ..
## MM: <==> diff(GAP) = GAP[k] - GAP[k-1] <= +SE.sim[k]
## criteria.DandF() -- Dudoit and Fridlyand (2002)
## ---------------- looks at the *global* maximum and then to the left..
## y <- x$data
## crit <- diff(y[which.max(y[,"Gap"]), c("Sks", "Gap")])
## nclust <- min(which(y[,"Gap"] > crit))
## return(ifelse(nclust == nrow(y), NA, nclust))
maxSE <- function(f, SE.f,
                  method = c("firstSEmax", "Tibs2001SEmax",
                             "globalSEmax", "firstmax", "globalmax"),
                  SE.factor = 1)
{
  method <- match.arg(method)
  stopifnot((K <- length(f)) >= 1, K == length(SE.f), SE.f >= 0, SE.factor >= 0)
  fSE <- SE.factor * SE.f
  switch(method,
         "firstmax" = { ## the first local maximum  (== firstSEmax with SE.factor == 0)
           decr <- (dg <- diff(f)) <= 0 # length K-1
           if(any(decr)) which.max(decr) else K # the first TRUE, or K
         },
         "globalmax" = {
           which.max(f)
         },
         "Tibs2001SEmax" = { ## The one Tibshirani et al (2001) proposed:
           ## "the smallest k such that f(k) >= f(k+1) - s_{k+1}"
           g.s <- f - fSE
           if(any(mp <- f[-K] >= g.s[-1])) which.max(mp) else K
         },
         "firstSEmax" = { ## M.Maechler(2012): rather ..
           ## look at the first *local* maximum and then to the left ..:
           decr <- (dg <- diff(f)) <= 0 # length K-1
           nc <- if(any(decr)) which.max(decr) else K # the first TRUE, or K
           if(any(mp <- f[seq_len(nc - 1)] >= f[nc] - fSE[nc]))
             which(mp)[1]
           else nc
         },
         "globalSEmax" = { ## Dudoit and Fridlyand (2002) *thought* Tibshirani proposed..
           ## in 'lga', see criteria.DandF():
           ## looks at the *global* maximum and then to the left..
           nc <- which.max(f)
           if(any(mp <- f[seq_len(nc - 1)] >= f[nc] - fSE[nc]))
             which(mp)[1]
           else nc
         })
}
