#' @title Dichotomize variables
#' @name sju.dicho
#' @description Dichotomizes variables into dummy variables (0/1). Dichotomization is
#'                either done by median or mean (see \code{dichBy}).
#'
#' @param var The variable that should be dichotomized.
#' @param dichBy Indicates the split criterion where the variable is dichotomized. By default,
#'          \code{var} is split into two groups at the median (\code{dichBy="median"}). Further
#'          values for \code{dichBy} are \code{"mean"} (split into groups at the mean of \code{var})),
#'          and \code{"value"}. In the latter case, you have to specifiy \code{dichVal}.
#' @param dichVal Indicates a value where \code{var} is dichotomized when \code{dichBy="value"}.
#'          Note that \code{dichVal} is inclusive, i.e. \code{dichVal=10} will split \code{var}
#'          into one group with values from lowest to 10 and another group with values greater
#'          than 10.
#' @return A dichotomized variable (0/1-coded).
#' 
#' @examples
#' data(efc)
#' summary(efc$c12hour)
#' table(sju.dicho(efc$c12hour))
#' table(sju.dicho(efc$c12hour, "mean"))
#' table(sju.dicho(efc$c12hour, "value", 30))
#'  
#' @export
sju.dicho <- function(var, dichBy="median", dichVal=-1) {
  # check if factor
  if (is.factor(var)) {
    # try to convert to numeric
    var <- as.numeric(as.character(var))
  }
  # check for correct dichotome types
  if (dichBy!="median" && dichBy!="mean" && dichBy!="value") {
    stop("Parameter \"dichBy\" must either be \"median\", \"mean\" or \"value\"..." , call.=FALSE)
  }
  if (dichBy=="median") {
    var <- ifelse(var<=median(var, na.rm=T),0,1)
  }
  else if (dichBy=="mean") {
    var <- ifelse(var<=mean(var, na.rm=T),0,1)
  }
  else {
    var <- ifelse(var<=dichVal,0,1)
  }
  return(var)
}


#' @title Recode scales into grouped factors
#' @name sju.groupVar
#' @description Recode scales into grouped factors.
#' @seealso \code{\link{sju.groupVarLabels}}
#'
#' @param var The scale variable, which should recoded into groups.
#' @param groupsize The group-size, i.e. the range for grouping. By default, for each 5 categories 
#'          new group is built, i.e. \code{groupsize=5}. Use \code{groupsize="auto"} to automatically
#'          resize a variable into a maximum of 30 groups (which is the ggplot-default grouping when
#'          plotting histograms). Use \code{autoGroupCount} to determin the amount of groups.
#' @param asNumeric If \code{TRUE} (default), the recoded variable will be returned as numeric vector.
#'          If \code{FALSE}, a factor is returned.
#' @param rightInterval If \code{TRUE}, grouping starts with the lower bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 50-54, 55-59, 60-64 etc. \cr
#'          If \code{FALSE} (default), grouping starts with the upper bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 51-55, 56-60, 61-65 etc.
#' @param autoGroupCount Sets the maximum number of groups that are built when auto-grouping is on
#'          (\code{groupsize="auto"}). Default is 30. If \code{groupsize} is not set to \code{"auto"},
#'          this parameter will be ignored.

#' @return A grouped variable, either as numeric or as factor (see paramter \code{asNumeric}).
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
sju.groupVar <- function(var, groupsize=5, asNumeric=TRUE, rightInterval=FALSE, autoGroupCount=30) {
  # minimum range. will be changed when autogrouping
  minval <- 0
  multip <- 2
  # check for auto-grouping
  if (groupsize=="auto") {
    # determine groupsize, which is 1/30 of range
    size <- ceiling((max(na.omit(var)-min(na.omit(var))))/autoGroupCount)
    # reset groupsize var
    groupsize <- as.numeric(size)
    # change minvalue
    minval <- min(na.omit(var))
    multip <- 1
  }
  # Einteilung der Variablen in Gruppen. Dabei werden unbenutzte Faktoren gleich entfernt
  var <- droplevels(cut(var, breaks=c(seq(minval, max(na.omit(var))+multip*groupsize, by=groupsize)), right=rightInterval))
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
#'          new group is built, i.e. \code{groupsize=5}. Use \code{groupsize="auto"} to automatically
#'          resize a variable into a maximum of 30 groups (which is the ggplot-default grouping when
#'          plotting histograms). Use parameter \code{autoGroupCount} to define the amount of groups.
#' @param rightInterval If \code{TRUE}, grouping starts with the lower bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 50-54, 55-59, 60-64 etc. \cr
#'          If \code{FALSE} (default), grouping starts with the upper bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 51-55, 56-60, 61-65 etc.
#' @param autoGroupCount Sets the maximum number of groups that are built when auto-grouping is on
#'          (\code{groupsize="auto"}). Default is 30. If \code{groupsize} is not set to \code{"auto"},
#'          this parameter will be ignored.
#' 
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
sju.groupVarLabels <- function(var, groupsize=5, rightInterval=FALSE, autoGroupCount=30) {
  # minimum range. will be changed when autogrouping
  minval <- 0
  multip <- 2
  # check for auto-grouping
  if (groupsize=="auto") {
    # determine groupsize, which is 1/30 of range
    size <- ceiling((max(na.omit(var)-min(na.omit(var))))/autoGroupCount)
    # reset groupsize var
    groupsize <- as.numeric(size)
    # change minvalue
    minval <- min(na.omit(var))
    multip <- 1
  }
  # Einteilung der Variablen in Gruppen. Dabei werden unbenutzte Faktoren gleich entfernt
  var <- droplevels(cut(var,breaks=c(seq(minval, max(na.omit(var))+multip*groupsize, by=groupsize)), right=rightInterval))
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


#' @title Retrieve model summary of lm
#' @name sju.modsum.lm
#' @description Returns a model summary of a fitted linear model as expression.
#'                The summary includes intercept, r2, F-statistics and AIC.
#'                It is returned as \link{expression} string to be used in plots
#'                like ggplot.
#' @seealso \code{\link{sjp.lm}} \cr
#'          \code{\link{sjp.lm1}} \cr
#'          \code{\link{sjp.lm.ma}}
#'         
#' @param fit A fitted linear model.
#' @return The model summary of the fitted linear model as \link{expression} string.
#' 
#' @examples
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#' # print model summary
#' sju.modsum.lm(fit)
#'
#' @export
sju.modsum.lm <- function(fit) {
  # get F-statistics
  fstat <- summary(fit)$fstatistic
  # Calculate p-value for F-test
  pval <- pf(fstat[1], fstat[2], fstat[3],lower.tail = FALSE)
  # indicate significance level by stars
  pan <- c("")
  if (pval<=0.005) {
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


#' @title Adjust y range of ggplot-objects
#' @name sju.adjustPlotRange.y
#' @description This method adjusts the y-range of a ggplot-object, which is useful when
#'                value labels are outside of the plot region. A modified ggplot-object will
#'                be returned with adjusted y-range so everything should be visible.
#'                Note that this function only works on \code{scale_y_continuous}.
#'
#' @note Note that this function only works on \code{scale_y_continuous}.
#' 
#' @references \url{http://www.r-bloggers.com/setting-axis-limits-on-ggplot-charts/}
#' 
#' @param gp A ggplot-object. Usually, this will be returned by most of this
#'          package's plotting functions via the \code{returnPlot} parameter.
#' @param upperMargin Defines the margin of the upper y bound of the plot. This value will
#'          be multiplied with the total y range. Default is 1.05, which means that the upper
#'          margin of the plot is about 5 percent of the "visible" plot area (i.e. the y-range
#'          is 105 percent of the actual needed range to make all object visible).
#' @return The same ggplot-object, with adjusted y-range, so all graphics and labels
#'          should be visible.
#' 
#' @examples
#' # sample data set
#' data(efc)
#' # show frequencies of relationship-variable and
#' # retrieve plot object
#' gp <- sjp.frq(efc$e15relat, returnPlot=TRUE)
#' # show current plot
#' plot(gp)
#' # show adjusted plot
#' sju.adjustPlotRange.y(gp)
#' 
#' @export
sju.adjustPlotRange.y <- function(gp, upperMargin=1.05) {
  # retrieve y-range of original plot
  gp <- gp + scale_y_continuous(limits=NULL)
  # build ggplot object
  gy <- ggplot_build(gp)
  # calculate new limit
  ylo <- abs(gy$panel$ranges[[1]]$y.range[1])
  yhi <- abs(gy$panel$ranges[[1]]$y.range[2]*upperMargin)
  # change y scale
  gp <- gp + scale_y_continuous(expand=c(0,0), 
                                limits=c(0,ylo+yhi))
  # return plot
  return(gp)
}


#' @title Insert line breaks in long labels
#' @name sju.wordwrap
#' @description Insert line breaks in long character strings. Useful if you want to wordwrap
#'                plot labels.
#'
#' @param labels The label(s) (i.e. character string). You can also pass several strings as vector
#'          (e.g. \code{labels=c("first long string", "second long string")})
#' @param wrap The amount of chars per line (i.e. line length)
#' @return New label(s) with line breaks inserted at every \code{wrap}'s position.
#' 
#' @examples
#' sju.wordwrap(c("A very long string", "And another even longer string!"), 10)
#' 
#' @export
sju.wordwrap <- function(labels, wrap) {
  # create regex pattern for line break
  pattern <- c(paste('(.{1,', wrap, '})(\\s|$)', sep=""))
  # iterate all labels
  for (n in 1:length(labels)) {
    # insert line breaks
    labels[n] <- gsub(pattern, '\\1\n', labels[n])
    # -----------------------
    # in case label was short enough, we still have a line break
    # at the end of the label. here we remove any trailing line breaks
    # -----------------------
    # get length of label
    l <- nchar(labels[n])
    # get last char
    lc <- substr(labels[n], l, l)
    # check if line breaj
    if (lc=='\n') {
      # if yes, remove it
      labels[n] <- substr(labels[n], 0, l-1)
    }
  }
  return(labels)
}


#' @title Recode variable categories into new values.
#' @name sju.recodeTo
#' @description Recoded the categories of a variables \code{var} into new category values, beginning
#'                with the lowest value specified by parameter \code{lowest}. Useful if you want
#'                to recode dummy variables with 1/2 coding to 0/1 coding, or recoding scales from
#'                1-4 to 0-3 etc.
#'
#' @param var The variable (vector) that should be recoded.
#' @param lowest Indicating the lowest category value after recoding. Default is 0, so the new
#'          variable starts with the category value 0.
#' @param highest If specified and larger that \code{lowest}, all category values larger than
#'          \code{highest} will be set to \code{NA}. Default is -1, i.e. this parameter is ignored
#'          and no NA's will be produced.
#' @return A new variable with recoded category values, where \code{lowest} indicates the lowest
#'           value.
#' 
#' @examples
#' # recode 1-4 to 0-3
#' dummy <- sample(1:4, 10, replace=TRUE)
#' sju.recodeTo(dummy)
#' 
#' # recode 3-6 to 0-3
#' # note that numeric type is returned
#' dummy <- as.factor(3:6)
#' sju.recodeTo(dummy) 
#' 
#' # lowest value starting with 1
#' dummy <- sample(11:15, 10, replace=TRUE)
#' sju.recodeTo(dummy, 1) 
#'
#' # lowest value starting with 1, highest with 3
#' # all others set to NA
#' dummy <- sample(11:15, 10, replace=TRUE)
#' sju.recodeTo(dummy, 1, 3) 
#' 
#' @export
sju.recodeTo <- function(var, lowest=0, highest=-1) {
  # check if factor
  if (is.factor(var)) {
    # try to convert to numeric
    var <- as.numeric(as.character(var))
  }
  # retrieve lowest category
  minval <- min(na.omit(var))
  # check substraction difference between current lowest value
  # and requested lowest value
  downsize <- minval-lowest
  var <- sapply(var, function(x) x-downsize)
  # check for highest range
  if (highest>lowest) {
    # set NA to all values out of range
    var[var>highest] <- NA
  }
  # return recoded var
  return(var)
}


#' @title Plot multicollinearity of linear models
#' @name sjp.vif
#' @description Plots the Variance Inflation Factors (check for multicollinearity) of 
#'                (generalized) linear models. Values below 5 are good and indicating no
#'                multicollinearity, values between 5 and 10 may be tolerable. Values 
#'                greater than 10 are not acceptable and indicate multicollinearity
#'                between model's predictors.
#'
#' @param fit The fitted (generalized) linear model which should be checked for
#'          multicollinearity.
#' @param printnumbers If \code{TRUE} (default), the VIF value are also printed to console.
#' 
#' @examples
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#' # plot VIF values
#' sjp.vif(fit)
#' 
#' @importFrom car vif
#' @export
sjp.vif <- function(fit, printnumbers=TRUE) {
  # check if we have more than 1 term
  if (length(coef(fit))>2) {
    # variance inflation factor
    # claculate VIF
    vifval <- vif(fit)
    # Wenn VIF-Werte an Konsole ausgegeben werden sollen, diese nun ausdrucken
    if (printnumbers) {
      print(vifval)
    }
    if (is.matrix(vifval)) {
      val <- vifval[,1]
    }
    else {
      val <- vifval
    }
    # retrieve highest VIF-value to determine y-axis range
    maxval <- val[which.max(val)]
    # determine upper limit of y-axis
    upperLimit <-10
    # check whether maxval exceeds the critical VIF-Limit
    # of 10. If so, set upper limit to max. value
    if (maxval >= upperLimit) {
      upperLimit <- ceiling(maxval)
    }
    mydat <- data.frame(cbind(round(val,2)))
    # Neue Variable erstellen, damit die Ergebnisse sortiert werden
    # können (siehe reorder in ggplot-Funktion)
    mydat$vars<-row.names(mydat)
    # die variablenlabel sollen noch mal sortiert werden, nach 
    # VIF-Werten aufsteigend. Dies ist für die X-Achsenbeschriftung
    # nötig, da diese sonst nicht mehr mit den sortierten VIF-Werten
    # (Balkenreihenfolge auf X-Achse) übereinstimmt
    mydat <- cbind(mydat, mydat[order(val),2])
    # Spalten sollen Namen kriegen
    names(mydat)<-c("vif", "vars", "label")
    # grafik ausgeben, dabei die variablen der X-Achse nach aufsteigenden
    # VIF-Werten ordnen
    plot(ggplot(mydat, aes(x=reorder(vars, vif), y=vif)) +
            # Balken zeichnen. Stat=identity heißt, dass nicht die counts, sondern
            # die tatsächlichen Zahlenwerte (VIF-Werte) abgebildet werden sollen
            geom_bar(stat="identity", width=0.7, fill="#80acc8") +
            # grüne Linie zeichnen, die den guten Bereich anzeigt (VIF < 5)
            geom_hline(yintercept=5, linetype=2, colour="darkgreen", alpha=0.7) +
            # rote  Linie zeichnen, die den tolerablen Bereich anzeigt (VIF < 10)
            geom_hline(yintercept=10, linetype=2, colour="darkred", alpha=0.7) +
            # grüne und rote Line beschriften
            annotate("text", x=1, y=4.7, label="good", size=4, colour="darkgreen") +
            annotate("text", x=1, y=9.7, label="tolerable", size=4, colour="darkred") +
            # als X-Achsenbeschriftung die Variablennamen setzen
            scale_x_discrete(labels=mydat$label) +
            # Keine weiteren Titel an X- und Y-Achse angeben
            labs(title="Variance Inflation Factors (multicollinearity)", x=NULL, y=NULL) +
            # maximale Obergrenze der Y-Achse setzen
            scale_y_continuous(limits=c(0, upperLimit), expand=c(0,0)) +
            # Beschriftung der X-Achse (Variablenlabel) in 45-Grad-Winkel setzen
            theme(axis.text.x=element_text(angle=45, vjust=0.5, size=rel(1.2))))
  }
}
