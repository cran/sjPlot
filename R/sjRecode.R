#' @title Dichotomize variables
#' @name sju.dicho
#' @description Dichotomizes variables into dummy variables (0/1). Dichotomization is
#'                either done by median or mean (see \code{dichBy}).
#'
#' @param var The variable that should be dichotomized.
#' @param dichBy Indicates the split criterion where the variable is dichotomized. By default,
#'          \code{var} is split into two groups at the median (\code{dichBy="median"} or
#'          \code{dichBy="md"}). Further values for \code{dichBy} are \code{"mean"} (or \code{"m"}),
#'          which splits into groups at the mean of \code{var}; and \code{"value"} (or \code{"v"}).
#'          In the latter case, you have to specifiy \code{dichVal}.
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
  # check abbreviations
  if (dichBy=="md") dichBy <- "median"
  if (dichBy=="m") dichBy <- "mean"
  if (dichBy=="v") dichBy <- "value"
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
#'                
#' @seealso \code{\link{sju.groupVar}}
#' 
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
#' 
#' @seealso \code{\link{sjp.lm}} \cr
#'          \code{\link{sjt.lm}}
#'         
#' @param fit A fitted linear model.
#' @return The standardiized beta coefficients of the fitted linear model.
#' 
#' @note "Standardized coefficients refer to how many standard deviations a dependent variable will change, 
#'         per standard deviation increase in the predictor variable. Standardization of the coefficient is 
#'         usually done to answer the question of which of the independent variables have a greater effect 
#'         on the dependent variable in a multiple regression analysis, when the variables are measured 
#'         in different units of measurement (for example, income measured in dollars and family size 
#'         measured in number of individuals)." (Source: Wikipedia)
#' 
#' @references \url{http://en.wikipedia.org/wiki/Standardized_coefficient}
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
#'          package's plotting functions.
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
#' gp <- sjp.frq(efc$e15relat, printPlot=FALSE)
#' # show current plot
#' plot(gp$plot)
#' # show adjusted plot
#' sju.adjustPlotRange.y(gp$plot)
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
#' @param linesep By default, this parameter is \code{NULL} and a regular new line
#'          string is used. For HTML-needs, for instance, \code{linesep} could be \code{"<br>"}.
#' @return New label(s) with line breaks inserted at every \code{wrap}'s position.
#' 
#' @examples
#' sju.wordwrap(c("A very long string", "And another even longer string!"), 10)
#' 
#' @export
sju.wordwrap <- function(labels, wrap, linesep=NULL) {
  # check for valid value
  if (is.null(labels)) {
    return(NULL)
  }
  # default line separator is \n
  if (is.null(linesep)) {
    linesep <- '\\1\n'
  }
  else {
    # however, for html-function we can use "<br>"
    # as parameter
    linesep <- sprintf("\\1%s", linesep)
  }
  # create regex pattern for line break
  pattern <- c(paste('(.{1,', wrap, '})(\\s|$)', sep=""))
  # iterate all labels
  for (n in 1:length(labels)) {
    # insert line breaks
    labels[n] <- gsub(pattern, linesep, labels[n])
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
#' @description Recodes the categories of a variables \code{var} into new category values, beginning
#'                with the lowest value specified by parameter \code{lowest}. Useful if you want
#'                to recode dummy variables with 1/2 coding to 0/1 coding, or recoding scales from
#'                1-4 to 0-3 etc.
#'
#' @param var The variable (vector) that should be recoded.
#' @param lowest Indicating the lowest category value after recoding. Default is 0, so the new
#'          variable starts with the category value 0.
#' @param highest If specified and larger than \code{lowest}, all category values larger than
#'          \code{highest} will be set to \code{NA}. Default is \code{-1}, i.e. this parameter is ignored
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


#' @title Recode variable values.
#' @name sju.recode
#' @description Recodes the categories of a variables. Wrapper function that calls
#'                the \code{\link{recode}} function from the \code{car} package.
#'
#' @param ... parameters, see \code{\link{recode}} function from the \code{car} package.
#' @return A variable with recoded values.
#' 
#' @examples
#' data(efc)
#' table(efc$e42dep)
#' table(sju.recode(efc$e42dep, "1:2=1;3:4=2"))
#'
#' @importFrom car recode
#' @export
sju.recode <- function(...) {
  # return recoded var
  return(recode(...))
}


#' @title Plot Variance Inflation Factors of linear models
#' @name sjp.vif
#' @description Plots the Variance Inflation Factors (check for multicollinearity) of 
#'                (generalized) linear models. Values below 5 are good and indicating no
#'                multicollinearity, values between 5 and 10 may be tolerable. Values 
#'                greater than 10 are not acceptable and indicate multicollinearity
#'                between model's predictors.
#'
#' @param fit The fitted (generalized) linear model which should be checked for
#'          multicollinearity.
#' @return (invisibly) returns the VIF values.
#' 
#' @examples
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#' # plot VIF values
#' sjp.vif(fit)
#' 
#' @importFrom car vif
#' @export
sjp.vif <- function(fit) {
  # check if we have more than 1 term
  if (length(coef(fit))>2) {
    # variance inflation factor
    # claculate VIF
    vifval <- vif(fit)
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
  invisible(vifval)
}


#' @title Set NA for specific variable values
#' @name sju.setNA
#' @description This function sets specific values of a variable \code{var}
#'                as missings (\code{NA}).
#'
#' @param var The variable where new missing values should be defined.
#' @param values The values that should be replaced with \code{\link{NA}}'s.
#' 
#' @return The \code{var} with each values of \code{values} replaced by an \code{NA}.
#' 
#' @examples
#' # create random variable
#' dummy <- sample(1:8, 100, replace=TRUE)
#' # show value distribution
#' table(dummy)
#' # set value 1 and 8 as missings
#' dummy <- sju.setNA(dummy, c(1,8))
#' # show value distribution, including missings
#' table(dummy, exclude=NULL)
#' 
#' @export
sju.setNA <- function(var, values) {
  # iterate all values that should be 
  # replaced by NA's
  for (i in seq_along(values)) {
    # find associated values in var
    # and set them to NA
    var[var==values[i]] <- NA
  }
  return(var)
}


#' @title Weight a variable
#' @name sju.weight2
#' @description This function weights the variable \code{var} by
#'                a specific vector of \code{weights}. It's an 
#'                alternative weight calculation to \code{\link{sju.weight}},
#'                where \code{\link{sju.weight}} usage is recommended.
#'                This function sums up all \code{weights} values of the associated
#'                categories of \code{var}, whereas the \code{\link{sju.weight}} function
#'                uses a \code{\link{xtabs}} formula to weight cases. Thus, this function
#'                may return a value with a different length than that from \code{var}.
#'
#' @seealso \code{\link{sju.weight}}
#'
#' @param var The (unweighted) variable 
#' @param weights A vector with same length as \code{var}, which
#'          contains weight factors. Each value of \code{var} has a
#'          specific assigned weight in \code{weights}.
#' 
#' @return The weighted \code{var}.
#' 
#' @note The values of the returned vector are in sorted order, whereas the categories
#'        of the original \code{var} may be spread randomly. Hence, \code{var} can't be
#'        used, for instance, for further cross tabulation. In case you want to have
#'        weighted contingency tables or (grouped) box plots etc., use the \code{weightBy}
#'        parameter of most functions (like in \code{\link{sjt.xtab}} or \code{\link{sjp.grpfrq}}).
#' 
#' @examples
#' v <- sample(1:4, 20, TRUE)
#' table(v)
#' w <- abs(rnorm(20))
#' table(sju.weight2(v,w))
#' 
#' @export
sju.weight2 <- function(var, weights) {
  items <- unique(var)
  newvar <- c()
  for (i in 1:length(items)) {
    newcount = round(sum(weights[which(var==items[i])]))
    newvar <- c(newvar, rep(items[i], newcount))
  }
  return (newvar)
}


#' @title Weight a variable
#' @name sju.weight
#' @description This function weights the variable \code{var} by
#'                a specific vector of \code{weights}.
#'
#' @seealso \code{\link{sju.weight2}}
#' 
#' @param var The (unweighted) variable 
#' @param weights A vector with same length as \code{var}, which
#'          contains weight factors. Each value of \code{var} has a
#'          specific assigned weight in \code{weights}.
#' 
#' @return The weighted \code{var}.
#' 
#' @note The values of the returned vector are in sorted order, whereas the categories
#'        of the original \code{var} may be spread randomly. Hence, \code{var} can't be
#'        used, for instance, for further cross tabulation. In case you want to have
#'        weighted contingency tables or (grouped) box plots etc., use the \code{weightBy}
#'        parameter of most functions (like in \code{\link{sjt.xtab}} or \code{\link{sjp.grpfrq}}).
#' 
#' @examples
#' v <- sample(1:4, 20, TRUE)
#' table(v)
#' w <- abs(rnorm(20))
#' table(sju.weight(v,w))
#' 
#' @export
sju.weight <- function(var, weights) {
  # init values
  weightedvar <- c()
  wtab <- round(xtabs(weights ~ var, data=data.frame(cbind(weights=weights,var=var))))
  # iterate all table values
  for (w in 1:length(wtab)) {
    # retrieve count of each table cell
    w_count <- wtab[[w]]
    # retrieve "cell name" which is identical to the variable value
    w_value <- as.numeric(names(wtab[w]))
    # append variable value, repeating it "w_count" times.
    weightedvar <- c(weightedvar, rep(w_value, w_count))
  }
  return(weightedvar)
}



#' @title Performs a Mann-Whitney-U-Test
#' @name sju.mwu
#' @description This function performs a Mann-Whitney-U-Test (or \code{Wilcoxon rank sum test},
#'                see \code{\link{wilcox.test}}) for the variable \code{var}, which is
#'                divided into groups indicated by \code{grp} (so the formula \code{var ~ grp}
#'                is used). If \code{grp} has more than two categories, a comparison between each 
#'                two groups is performed.
#'
#' @param var A numeric vector / variable, where the Mann-Whitney-U-Test should be applied to.
#' @param grp The grouping variable indicating the groups that should be used for comparison.
#' @param alternative a character string specifying the alternative hypothesis, must be one 
#'          of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can 
#'          specify just the initial letter.
#' @return (Invisibly) returns a data frame with p-values for each group-comparison.
#' 
#' @note This function calls the \code{\link{wilcox.test}} with formula. If \code{grp}
#'         has more than two groups, additionally a Kruskal-Wallis-Test (see \code{\link{kruskal.test}})
#'         is performed.
#' 
#' @examples
#' data(efc)
#' # Mann-Whitney-U-Tests for elder's age by elder's dependency.
#' sju.mwu(efc$e17age, efc$e42dep)
#' 
#' @export
sju.mwu <- function(var, grp, alternative="two.sided") {
  if (min(na.omit(grp))==0) {
    grp <- grp+1
  }
  cnt <- length(unique(na.omit(grp)))
  cat("\nPerforming Mann-Whitney-U-Test...\n")
  cat("---------------------------------\n")
  cat("(showing p-levels between groups (x|y)\n")
  df <- data.frame()
  for (i in 1:cnt) {
    for (j in i:cnt) {
      if (i!=j) {
        xsub <- var[which(grp==i | grp==j)]
        ysub <- grp[which(grp==i | grp==j)]
        ysub <- ysub[which(!is.na(xsub))]
        xsub <- as.numeric(na.omit(xsub))
        ysub <- as.numeric(na.omit(ysub))
        wt <- wilcox.test(xsub ~ ysub, paired=FALSE, alternative=alternative)
        cat(sprintf("p(%i|%i)=%.3f\n", i, j, wt$p.value))
        df <- rbind(df, cbind(grp1=i, grp2=j, p=wt$p.value))
      }
    }
  }
  # if we have more than 2 groups, also perfom kruskal-wallis-test
  if (cnt>2) {
    cat("\nPerforming Kruskal-Wallis-Test...\n")
    cat("---------------------------------\n")
    kw <- kruskal.test(var, grp)
    cat(sprintf("chi-squared=%.3f\n",kw$statistic ))
    cat(sprintf("df=%i\n",kw$parameter ))
    cat(sprintf("p=%.3f\n",kw$p.value ))
  }
  invisible(df)
}


#' @title Calculates Cronbach's Alpha for a matrix
#' @name sju.cronbach
#' @description This function calcates the Cronbach's alpha value for each column
#'                of a data frame or matrix.
#'
#' @param df A data frame or matrix with more than 2 columns.
#' @return The Cronbach's alpha value for \code{df}.
#' 
#' @note For use case, see \code{\link{sjp.pca}} and \code{\link{sjt.pca}}.
#' 
#' @export
sju.cronbach <- function(df) { # df must be matrix or data.frame with more than 2 columns
  df <- na.omit(df)
  if (is.null(ncol(df)) || ncol(df)<2) {
    cat("\nToo less columns in this factor to calculate alpha value!\n")
    return(0)
  }
  return (dim(df)[2]/(dim(df)[2]-1)*(1-sum(apply(df,2,var))/var(rowSums(df))))
}    


#' @title Performs a reliability test on an item scale.
#' @name sju.reliability
#' @description This function calcates the corrected item-total correlations for each item of \code{df}
#'                with the remaining items; furthermore, the Cronbach's alpha for each item, if it was
#'                deleted from the scale, is calculated.
#'
#' @param df A data frame with 
#' @param scaleItems If \code{TRUE}, the data frame's vectors will be scaled. Recommended,
#'          when the variables have different measures / scales.
#' @param digits Amount of digits for Cronbach's Alpha and correlation values in
#'          returned data frame.
#' @return A data frame with the corrected item-total correlations and Cronbach's alpha (if item deleted)
#'           for each item of the scale.
#' 
#' @note This function is similar to a basic reliability test in SPSS. The correlations in
#'         the Item-Total-Statistic are a computed correlation of each item against the sum
#'         of the remaining items (which are thus treated as one item).
#' 
#' @examples
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' data(efc)
#' 
#' # retrieve variable and value labels
#' varlabs <- sji.getVariableLabels(efc)
#' 
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc)=="c82cop1")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc)=="c90cop9")
#'  
#' # create data frame with COPE-index scale
#' df <- as.data.frame(efc[,c(start:end)])
#' colnames(df) <- varlabs[c(start:end)]
#' 
#' \dontrun{
#' sjt.df(sju.reliability(df), 
#'        describe=FALSE,
#'        showCommentRow=TRUE, 
#'        commentString=sprintf("Cronbach's &alpha;=%.2f", sju.cronbach(df)))}
#' 
#' # ---------------------------------------
#' # Compute PCA on Cope-Index, and perform a
#' # reliability check on each extracted factor.
#' # ---------------------------------------
#' \dontrun{
#' factors <- sjt.pca(df)$factor.index
#' findex <- sort(unique(factors))
#' for (i in 1:length(findex)) {
#'  rel.df <- subset(df, select=which(factors==findex[i]))
#'  if (ncol(rel.df)>=3) {
#'    sjt.df(sju.reliability(rel.df),
#'           describe=FALSE,
#'           showCommentRow=TRUE,
#'           useViewer=FALSE,
#'           title="Item-Total-Statistic",
#'           commentString=sprintf("Scale's overall Cronbach's &alpha;=%.2f", 
#'                                 sju.cronbach(rel.df)))
#'    }
#'  }}
#'  
#' @export
sju.reliability <- function(df, scaleItems=FALSE, digits=3) {
  # -----------------------------------
  # check for minimum amount of columns
  # can't be less than 3, because the reliability
  # test checks for Cronbach's alpha if a specific
  # item is deleted. If data frame has only two columns
  # and one is deleted, Cronbach's alpha cannot be calculated.
  # -----------------------------------
  if (ncol(df)<3) {
    stop("Data frame needs at least three columns!", call.=FALSE)
  }
  # -----------------------------------
  # remove missings, so correlation works
  # -----------------------------------
  df <- na.omit(df)
  # -----------------------------------
  # remember item (column) names for return value
  # return value gets column names of initial data frame
  # -----------------------------------
  df.names <- colnames(df)
  # -----------------------------------
  # Check whether items should be scaled. Needed,
  # when items have different measures / scales
  # -----------------------------------
  if (scaleItems) {
    df <- data.frame(scale(df, center=TRUE, scale=TRUE))
  }
  # -----------------------------------
  # init vars
  # -----------------------------------
  totalCorr <- c()
  cronbachDeleted <- c()
  # -----------------------------------
  # iterate all items
  # -----------------------------------
  for (i in 1:ncol(df)) {
    # -----------------------------------
    # create subset with all items except current one
    # (current item "deleted")
    # -----------------------------------
    sub.df <- subset(df, select=c(-i))
    # -----------------------------------
    # calculate cronbach-if-deleted
    # -----------------------------------
    cronbachDeleted <- c(cronbachDeleted, sju.cronbach(sub.df))
    # -----------------------------------
    # calculate corrected total-item correlation
    # -----------------------------------
    totalCorr <- c(totalCorr, cor(df[,i], apply(sub.df, 1, sum), use="pairwise.complete.obs"))
  }
  # -----------------------------------
  # create return value
  # -----------------------------------
  ret.df <- data.frame(cbind(round(cronbachDeleted,digits), round(totalCorr,digits)))
  # -----------------------------------
  # set names of data frame
  # -----------------------------------
  colnames(ret.df) <- c("Cronbach's &alpha; if item deleted", "Corrected item-total correlation")
  rownames(ret.df) <- df.names
  # -----------------------------------
  return(ret.df)
}
