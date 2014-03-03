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
#' @return A data frame containing the SPSS data. retrieve value labels with \code{\link{sji.getValueLabels}}
#'   and variable labels with \code{\link{sji.getVariableLabels}}.
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
sji.SPSS <- function(path, enc=NA) {
  # import data as data frame
  data.spss <- read.spss(path, to.data.frame=TRUE, use.value.labels=FALSE, reencode=enc)
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
#'          \link{sji.convertToValue}
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



#' @title Retrieve variable labels of an SPSS-imported data frame
#' @name sji.getVariableLabels
#' @description This function retrieves the variable labels of an imported
#' SPSS data set and returns the result as list.
#' 
#' @seealso \link{sji.getValueLabels} \cr
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