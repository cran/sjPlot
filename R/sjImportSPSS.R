#' @title Import SPSS dataset as data frame into R
#' @name sji.SPSS
#' @references \url{http://strengejacke.wordpress.com/sjplot-r-package/} \cr \cr
#'             \url{http://strengejacke.wordpress.com/2013/02/24/simplify-your-r-workflow-with-functions-rstats/}
#' 
#' @description Import data from SPSS, including NA's, value and variable labels.
#' 
#' @seealso \code{\link{sji.getValueLabels}} \cr
#'          \code{\link{sji.getVariableLabels}} \cr
#'          \code{\link{sji.convertToLabel}} \cr
#'          \code{\link{sji.convertToValue}} \cr
#'          \code{\link{sji.viewSPSS}}
#'          
#' @param path The file path to the SPSS dataset.
#' @param enc The file encoding of the SPSS dataset.
#' @param autoAttachVarLabels if \code{TRUE}, variable labels will automatically be
#'          attached to each variable as \code{"variable.label"} attribute.
#'          See \code{\link{sji.setVariableLabels}} for details.
#' @return A data frame containing the SPSS data. retrieve value labels with \code{\link{sji.getValueLabels}}
#'   and variable labels with \code{\link{sji.getVariableLabels}}.
#'   
#' @note This is a wrapper function for \code{\link{read.spss}} of the
#'         \code{foreign} package, using convenient parameter default
#'         settings.
#' 
#' @examples
#' # import SPSS data set
#' # mydat <- sji.SPSS("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- sji.getVariableLabels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- sji.getValueLabels(mydat)
#' 
#' @importFrom foreign read.spss
#' @export
sji.SPSS <- function(path, enc=NA, autoAttachVarLabels=FALSE) {
  # import data as data frame
  data.spss <- read.spss(path, to.data.frame=TRUE, use.value.labels=FALSE, reencode=enc)
  # auto attach labels
  if (autoAttachVarLabels) {
    cat("Attaching variable labels. Please wait...\n")
    data.spss <- sji.setVariableLabels(data.spss, sji.getVariableLabels(data.spss))
  }
  # return data frame
  return(data.spss)
}


#' @title Retrieve value labels of an SPSS-imported data frame
#' @name sji.getValueLabels
#' @description This function retrieves the value labels of an imported
#' SPSS data set and returns the result as list.
#' 
#' @seealso \link{sji.SPSS} \cr
#'          \link{sji.getVariableLabels} \cr
#'          \link{sji.convertToLabel} \cr
#'          \link{sji.convertToValue} \cr
#'          \link{sji.setValueLabels}
#'
#' @param dat a data frame containing imported SPSS data
#' @return a list with all value labels from the SPSS dataset
#' 
#' @examples
#' # import SPSS data set
#' # mydat <- sji.SPSS("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- sji.getVariableLabels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- sji.getValueLabels(mydat)
#' 
#' @export
sji.getValueLabels <- function(dat) {
  a <- lapply(dat, FUN = getValLabels)
  return (a)
}
getValLabels <- function(x){
  rev(names(attr(x, "value.labels")))
}


#' @title Attach value labels to a variable or vector
#' @name sji.setValueLabels
#' @description This function attaches character labels as \code{"value.labels"} attribute
#'                to a variable or vector \code{"var"}. These value labels will be accessed
#'                by most of this package's functions, in order to automatically set values
#'                or legend labels.
#' 
#' @seealso \link{sji.SPSS} \cr
#'          \link{sji.getVariableLabels} \cr
#'          \link{sji.convertToLabel} \cr
#'          \link{sji.convertToValue} \cr
#'          \link{sji.getValueLabels}
#'
#' @param var a variable (vector) where labels should be attached. Replaces former value labels.
#' @param labels a character vector of labels that will be attached to \code{"var"} by setting
#'          the \code{"value.labels"} attribute. The length of this character vector must equal
#'          the value range of \code{"var"}, i.e. if \code{"var"} has values from 1 to 3,
#'          \code{"labels"} should have a length of 3. 
#' @return the variable \code{"var"} with attached value labels.
#' 
#' @examples
#' dummy <- sample(1:4, 40, replace=TRUE)
#' sjp.frq(dummy)
#' 
#' dummy <- sji.setValueLabels(dummy, c("very low", "low", "mid", "hi"))
#' sjp.frq(dummy)
#' 
#' @export
sji.setValueLabels <- function(var, labels) {
  # check for null
  if (!is.null(labels)) {
    if (is.character(var) || is.null(var)) {
      cat("Can't attach labels to string or NULL vectors.\n")
    }
    else {
      # checl if var ist a factor
      if (is.factor(var)) {
        # retrieve levels
        minval <- 1
        maxval <- length(levels(var))
      }
      else {
        # retrieve values
        minval <- min(var, na.rm=TRUE)
        maxval <- max(var, na.rm=TRUE)
      }
      # check for unlisting
      if (is.list(labels)) {
        labels <- as.vector(unlist(labels))
      }
      lablen <- length(labels)
      valrange <- maxval-minval+1
      if (is.infinite(valrange)) {
        cat("Can't set value labels. Infinite value range.\n")
      }
      # check for valid length of labels
      else if (valrange<lablen) {
        cat(sprintf("More labels than values of \"var\". Using first %i labels.\n", valrange))
        attr(var, "value.labels") <- c(as.character(c(minval:maxval)))
        names(attr(var, "value.labels")) <- rev(labels[1:valrange])
      }
      else if (valrange>lablen) {
        cat("Can't set value labels. Value range of \"var\" is longer than length of \"labels\".\n")
      }
      else {
        attr(var, "value.labels") <- c(as.character(c(minval:maxval)))
        names(attr(var, "value.labels")) <- rev(labels)
      }
    }
  }
  return (var)
}


#' @title Retrieve variable labels of an SPSS-imported data frame
#' @name sji.getVariableLabels
#' @description This function retrieves the variable labels of an imported
#' SPSS data set and returns the result as list.
#' 
#' @seealso \link{sji.getValueLabels} \cr
#'          \link{sji.setValueLabels} \cr
#'          \link{sji.setVariableLabels} \cr
#'          \link{sji.SPSS} \cr
#'          \link{sji.convertToLabel} \cr
#'          \link{sji.convertToValue}
#' 
#' @param dat A data frame containing imported SPSS data.
#' @return A list with all variable labels from the SPSS dataset.
#' 
#' @examples
#' # import SPSS data set
#' # mydat <- sji.SPSS("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- sji.getVariableLabels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- sji.getValueLabels(mydat)
#' 
#' @export
sji.getVariableLabels <- function(dat) {
  return(attr(dat, "variable.labels"))
}


#' @title Set variable label(s) to a single variable or data frame
#' @name sji.setVariableLabels
#' @description This function sets variable labels to a single variable or to
#'                a set of variables in a data frame. To each variable, the
#'                attribute \code{"variable.label"} with the related variable
#'                name is attached. Most of this package's function can automatically
#'                retrieve the variable name to use it as axis labels or plot title.
#' 
#' @seealso \link{sji.getValueLabels} \cr
#'          \link{sji.setValueLabels} \cr
#'          \link{sji.getVariableLabels} \cr
#'          \link{sji.SPSS} \cr
#'          \link{sji.convertToLabel} \cr
#'          \link{sji.convertToValue}
#' 
#' @param x A single variable (vector) or data frame with variables.
#' @param lab If \code{x} is a vector (single variable), use a single character string with 
#'          the variable label for \code{x}. If \code{x} is a \code{\link{data.frame}}, use a
#'          vector with character labels of same length as \code{ncol(x)}.
#' @return \code{x}, with attached \code{"variable.label"} attribute(s), which contains the
#'           variable name(s).
#' 
#' @examples
#' # sample data set, imported from SPSS. Variable labels are attached
#' # as attribute to the data frame (so variables currently don't have this attribute)
#' data(efc)
#' # get variable labels
#' variable.labels <- sji.getVariableLabels(efc)
#' # set variable labels as attribute to each single variable of data frame
#' efc <- sji.setVariableLabels(efc, variable.labels)
#' 
#' \dontrun{
#' sjt.frq(efc$e42dep)
#' sjt.frq(data.frame(efc$e42dep, efc$e16sex))}
#' 
#' # ---------------------------------------------
#' # manually set value and variable labels
#' # ---------------------------------------------
#' dummy <- sample(1:4, 40, replace=TRUE)
#' dummy <- sji.setValueLabels(dummy, c("very low", "low", "mid", "hi"))
#' dummy <- sji.setVariableLabels(dummy, "Dummy-variable")
#' # auto-detection of value labels by default, auto-detection of
#' # variable labels if parameter "title" set to "auto"
#' sjp.frq(dummy, title="auto")
#' 
#' @export
sji.setVariableLabels <- function(x, lab) {
  if (!is.null(lab) && !is.null(x)) {
    if (is.data.frame(x)) {
      if (ncol(x)!=length(lab)) {
        cat("Parameter \"lab\" must be of same length as numbers of columns in \"x\".\n")
      }
      else {
        # -------------------------------------
        # create progress bar
        # -------------------------------------
        pb <- txtProgressBar(min=0, max=ncol(x), style=3)
        for (i in 1:ncol(x)) {
          attr(x[,i], "variable.label") <- lab[i]
          # update progress bar
          setTxtProgressBar(pb, i)
        }
        close(pb)
      }
    }
    else {
      attr(x, "variable.label") <- lab
    }
  }
  return (x)
}

#' @title Replaces variable values with their associated value labels
#' @name sji.convertToLabel
#' @description This function converts (replaces) variable values (of factors) with their
#' associated value labels. Might be helpful for factor variables.
#' For instance, if you have a Gender variable with 0/1, and associated
#' labels are male/female, this function would convert all 0 to male and
#' all 1 to female in the data frame.
#' 
#' @seealso \link{sji.convertToValue} \cr
#'          \link{sji.getValueLabels} \cr
#'          \link{sji.getVariableLabels} \cr
#'          \link{sji.SPSS}
#' 
#' @param variable A (factor) variable.
#' @return A factor variable containing with the replaced value labels.
#' 
#' @examples
#' data(efc)
#' print(sji.getValueLabels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(sji.convertToLabel(efc$c161sex))
#' 
#' print(sji.getValueLabels(efc)['e42dep'])
#' table(efc$e42dep)
#' table(sji.convertToLabel(efc$e42dep))
#' 
#' @export
sji.convertToLabel <- function(variable) {
  vl <- rev(names(attr(variable, "value.labels")))
  vn <- sort(unique(na.omit(variable)))
  
  for (i in 1:length(vl)) {
    variable[variable==vn[i]] <- vl[i]
  }
  return (as.factor(variable))
}


#' @title Converts factors to numeric variables
#' @name sji.convertToValue
#' @description This function converts (replaces) factor values with the
#' related factor level index number, thus the factor is converted to 
#' a numeric variable.
#' 
#' @seealso \link{sji.convertToLabel} \cr
#'          \link{sji.getValueLabels} \cr
#'          \link{sji.getVariableLabels} \cr
#'          \link{sji.SPSS}
#' 
#' @param fac A (factor) variable.
#' @param startAt the starting index, i.e. numeric value of the variable.
#' @return A numeric variable with values ranging from \code{startAt} to
#'           \code{startAt} + length of factor levels.
#' 
#' @examples
#' data(efc)
#' test <- sji.convertToLabel(efc$e42dep)
#' table(test)
#' 
#' table(sji.convertToValue(test))
#' hist(sji.convertToValue(test,0))
#' 
#' @export
sji.convertToValue <- function(fac, startAt=1) {
  l <- length(levels(fac))
  end <- startAt+l-1
  levels(fac) <- c(startAt:end)
  return (as.numeric(as.character(fac)))
}