#' @title Save frequencies as HTML table
#' @name sjt.frq
#' 
#' @description Save (multiple) frequency tables as HTML file.
#' 
#' @seealso \link{sjp.frq}
#' 
#' @param data The variables which frequencies should be printed as table. Either use a single variable
#'          (vector) or a data frame where each column represents a variable (see examples.
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
#' @param weightBy A weight factor that will be applied to weight all cases from \code{data}.
#'          default is \code{NULL}, so no weights are used.
#' @param variableLabels A single character vector or a list of character vectors that indicate
#'          the variable names of those variables from \code{data}. Note that if multiple variables
#'          are supplied (as data frame), the variable labels must be supplied as \code{list} object
#'          (see examples).
#' @param valueLabels A list of character vectors that indicate the value labels of those variables 
#'          from \code{data}. Note that if multiple variables are supplied (as data frame), the 
#'          value labels must be supplied as nested \code{list} object (see examples).
#' @param autoGroupAt A value indicating at which length of unique values a variable from \code{data}
#'          is automatically grouped into smaller units (see \link{sju.groupVar}). Variables with large 
#'          numbers of unique values may be too time consuming when a HTML table is created and R would
#'          not respond any longer. Hence it's recommended to group such variables. Default value is 50,
#'          i.e. variables with 50 and more unique values will be grouped using \link{sju.groupVar} with
#'          \code{groupsize="auto"} parameter. By default, the maximum group count is 30. However, if
#'          \code{autoGroupAt} is less than 30, \code{autoGroupAt} groups are built.
#' @param stringCount String label for the first table column containing the counts. Default is \code{"N"}.
#' @param stringPerc String label for the second table column containing the percentages, where the
#'          count percentages include missing values.
#' @param stringValidPerc String label for the third table column containing the valid percentages, i.e. the
#'          count percentage value exluding possible missing values.
#' @param stringCumPerc String label for the last table column containing the cumulative percentages.
#' @param stringMissingValue String label for the last table data row containing missing values.
#' @param highlightMedian If \code{TRUE}, the table row indicating the median value will
#'          be highlighted.
#' @param highlightQuartiles If \code{TRUE}, the table row indicating the lower and upper quartiles will
#'          be highlighted.
#' @param skipZeroRows If \code{TRUE}, rows with only zero-values are not printed. Default is \code{FALSE}.
#' @param showSummary If \code{TRUE} (default), a summary row with total and valid N as well as mean and
#'          standard deviation is shown.
#' 
#' @note The HTML tables can either be saved as file and manually opened (specify parameter \code{file}) or
#'         they can be saved as temporary files and will be displayed in the RStudio Viewer pane (if working with RStudio)
#'         or opened with the default web browser. Displaying resp. opening a temporary file is the
#'         default behaviour (i.e. \code{file=NULL}).
#' 
#' @examples
#' # load sample data
#' data(efc)
#' 
#' # retrieve value and variable labels
#' variables <- sji.getVariableLabels(efc)
#' values <- sji.getValueLabels(efc)
#' 
#' # show frequencies of "e42dep" in RStudio Viewer Pane
#' # or default web browser
#' ## Note that example may open browser, thus it is outcommented
#' # sjt.frq(efc$e42dep)
#' 
#' # plot and save frequency table of "e42dep" with labels
#' sjt.frq(efc$e42dep,
#'         file="dependency_labels.html",
#'         variableLabels=variables['e42dep'],
#'         valueLabels=values[['e42dep']])
#' 
#' # plot frequencies of e42dep, e16sex and c172code in one HTML file
#' # and show table in RStudio Viewer Pane or default web browser
#' ## Note that example may open browser, thus it is outcommented
#' # sjt.frq(as.data.frame(cbind(efc$e42dep, efc$e16sex, efc$c172code)),
#' #         variableLabels=list(variables['e42dep'], variables['e16sex'], variables['c172code']),
#' #         valueLabels=list(values[['e42dep']], values[['e16sex']], values[['c172code']]))
#' 
#' # plot larger scale including zero-counts
#' # and save to file, indicating median and quartiles
#' sjt.frq(efc$neg_c_7,
#'         file="negativeimpact.html",
#'         variableLabels=variables['neg_c_7'],
#'         valueLabels=values[['neg_c_7']],
#'         highlightMedian=TRUE,
#'         highlightQuartiles=TRUE)
#' 
#' @export
sjt.frq <- function (data,
                     file=NULL,
                     weightBy=NULL,
                     variableLabels=NULL,
                     valueLabels=NULL,
                     autoGroupAt=50,
                     stringCount="N",
                     stringPerc="raw %",
                     stringValidPerc="valid %",
                     stringCumPerc="cumulative %",
                     stringMissingValue="missings",
                     highlightMedian=FALSE,
                     highlightQuartiles=FALSE,
                     skipZeroRows=FALSE,
                     showSummary=TRUE) {
  # -------------------------------------
  # table init
  # -------------------------------------
  toWrite <- c("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\">\n<style>\n.qrow { border-bottom: 1px solid #cc3333 }\n.mdrow { font-weight:bolder; font-style:italic; color:#993333 }\n.abstand { margin-bottom: 2em }\ntable { border-collapse:collapse; border:none }\nth { border-top: double; text-align:center; font-style:italic; font-weight:normal }\ntable td { padding:0.2cm }\ntd.summary { text-align:right; font-style:italic; font-size:0.9em; padding-top:0.1cm; padding-bottom:0.1cm }\n.lasttablerow { border-top:1px solid; border-bottom: double }\n.firsttablerow { border-bottom:1px solid }\n.leftalign { text-align:left }\n.centeralign { text-align:center }\ncaption { font-weight: bold; text-align:left  }\n</style>\n</head>\n<body>\n")
  # -------------------------------------
  # make data frame of single variable, so we have
  # unique handling for the data
  # -------------------------------------
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  # -------------------------------------
  # determine number of variables
  # -------------------------------------
  nvar <- ncol(data)
  # -------------------------------------
  # transform variable and value labels 
  # to list object
  # -------------------------------------
  if (!is.null(variableLabels) && !is.list(variableLabels)) {
    # if we have variable labels as vector, convert them to list
    variableLabels <- as.list(variableLabels)
  }
  else if (is.null(variableLabels)) {
    # if we have no variable labels, use column names
    # of data frame
    variableLabels <- as.list(colnames(data))
  }
  if (!is.null(valueLabels) && !is.list(valueLabels)) {
    # if we have value labels as vector, convert them to list
    valueLabels <- list(valueLabels)
  }
  else if (is.null(valueLabels)) {
    # create list
    valueLabels <- list()
    # iterate all variables
    for (i in 1:nvar) {
      # retrieve variable
      dummy <- data[,i]
      # and add label range to value labels list
      valueLabels <- c(valueLabels, list(min(dummy, na.rm=TRUE):max(dummy, na.rm=TRUE)))
    }
  }
  # -------------------------------------
  # header row of table
  # -------------------------------------
  headerRow <- sprintf("   <tr class=\"firsttablerow\">\n     <th></th>\n     <th>%s</th>\n     <th>%s</th>\n     <th>%s</th>\n     <th>%s</th>\n   </tr>\n\n", stringCount, stringPerc, stringValidPerc, stringCumPerc)
  # -------------------------------------
  # start iterating all variables
  # -------------------------------------
  for (cnt in 1:nvar) {
    # -----------------------------------------------
    # prepare data: create frequencies and weight them,
    # if requested. put data into a data frame
    #---------------------------------------------------
    # get variable
    orivar <- var <- data[,cnt]
    # -----------------------------------------------
    # check for length of unique values and skip if too long
    # -----------------------------------------------
    if (length(unique(var))>=autoGroupAt) {
      cat(sprintf("Variable %s with %i unique values was grouped...\n", colnames(data)[cnt], length(unique(var))))
      varsum <- var
      agcnt <- ifelse (autoGroupAt<30, autoGroupAt, 30)
      valueLabels[[cnt]] <- sju.groupVarLabels(var, groupsize="auto", autoGroupCount=agcnt)
      var <- sju.groupVar(var, groupsize="auto", asNumeric=TRUE, autoGroupCount=agcnt)
    }
    # create frequency table
    if (is.null(weightBy)) {
      # unweighted, including NA
      ftab <- table(var)
      ftab.NA <- table(var, useNA="always")
      ftab.perc <- 100*round(prop.table(ftab.NA),6)
      ftab.valid <- 100*round(prop.table(ftab),6)
      # unweihted
      weightedvar <- NULL
      varsum <- var
    }
    else {
      # weighted, including NA
      ftab <- round(xtabs(weightBy ~ var, data=data.frame(cbind(weightBy=weightBy,var=var))))
      ftab.NA <- round(xtabs(weightBy ~ var, data=data.frame(cbind(weightBy=weightBy,var=var)), exclude=NULL, na.action=na.pass),0)
      ftab.perc <- 100*round(prop.table(ftab.NA),6)
      ftab.valid <- 100*round(prop.table(ftab),6)
      #---------------------------------------------------
      # retrieve summary. we reproduce the variable from the table
      # matrix here because we have weights included
      #---------------------------------------------------
      # init values
      weightedvar <- c()
      # iterate all table values
      for (w in 1:length(ftab)) {
        # retrieve count of each table cell
        w_count <- ftab[[w]]
        # retrieve "cell name" which is identical to the variable value
        w_value <- as.numeric(names(ftab[w]))
        # append variable value, repeating it "w_count" times.
        weightedvar <- c(weightedvar, rep(w_value, w_count))
      }
      varsum <- weightedvar
    }
    # retrieve summary
    varsummary <- summary(varsum)
    # retrieve median
    var.median <- varsummary[[3]]
    # retrieve quartiles
    var.lowerq <- round(varsummary[[2]])
    var.upperq <- round(varsummary[[5]])
    #---------------------------------------------------
    # new data frame from frequencies for current variable
    #---------------------------------------------------
    df <- as.data.frame(cbind(freq=c(ftab.NA), perc=c(ftab.perc), valid=c(ftab.valid,0)))
    # add cumulative percentages
    df$cumperc <- cumsum(df$valid)
    # rename "NA" row
    rownames(df)[nrow(df)] <- "NA"
    # save rownames index numbers
    rowindexnumbers <- as.numeric(c(rownames(df)))
    # -------------------------------------
    # start table tag
    # -------------------------------------
    toWrite <- paste(toWrite, "<table>", "\n")
    # -------------------------------------
    # retrieve variable label
    # -------------------------------------
    varlab <- variableLabels[[cnt]]
    # -------------------------------------
    # table caption, variable label
    # -------------------------------------
    toWrite <- paste(toWrite, sprintf("  <caption>%s</caption>\n", varlab))
    # -------------------------------------
    # header row with column labels
    # -------------------------------------
    toWrite <- paste(toWrite, headerRow)
    # -------------------------------------
    # data rows with value labels
    # -------------------------------------
    # retrieve value labels
    vallab <- valueLabels[[cnt]]
    # determine value range
    minval <- min(var, na.rm=TRUE)
    maxval <- max(var, na.rm=TRUE)
    # determine catcount, which is +1 if minval = 0
    catcount <- ifelse(minval==0, maxval+1, maxval)
    # check if value labels are NULL
    if (is.null(vallab)) {
      # set range as value labels
      vallab <- as.character(c(minval:maxval))
    }
    # check whether value label length exceeds maximum value
    # (i.e. missing in upper category)
    if (catcount<length(vallab)) {
      # correct maximum value
      maxval <- length(vallab)
    }
    # set value range
    valrange <- c(minval:maxval)
    # iterate all labels, each one in one row
    for (j in 1:length(valrange)) {
      # search row index. we may have a zero value here
      # in case a certain category value is zero (zero counts)
      ri <- which(rowindexnumbers==valrange[j])
      # check for zero count
      if (is.null(ri) || length(ri)==0 || ri==0) {
        # create zero-count-row
        datarow <- c(0,0,0)
        zerorow <- TRUE
      }
      else {
        # retrieve data row
        datarow <- df[ri,]
        zerorow <- FALSE
      }
      # -------------------------------------
      # check if to skip zero rows
      # -------------------------------------
      if (skipZeroRows && zerorow) {
        # nothing here...
      }
      else {
        # -------------------------------------
        # access cell data via "celldata <- c(datarow[XY])
        # we have 4 data cells (freq, perc, valid and cumperc)
        # -------------------------------------
        # write table data row
        # -------------------------------------
        # check whether we have median row and whether it should be highlighted
        if (highlightMedian && ((j+minval)==(var.median+1))) {
          rowcss <- c("<tr class=\"mdrow\">")
        }
        else {
          # check whether we have lower quartile and whether it should be highlighted
          if (highlightQuartiles) {
            if(((j+minval)==(var.lowerq+1)) || ((j+minval)==(var.upperq+1))) {
              rowcss <- c("<tr class=\"qrow\">")
            }
            else {
              rowcss <- c("<tr>")
            }
          } 
          else {
            rowcss <- c("<tr>")
          }
        }
        # value label
        toWrite <- paste(toWrite, sprintf("  %s\n     <td class=\"leftalign\">%s</td>\n", rowcss, vallab[j]))
        # cell values, first value is integer
        toWrite <- paste(toWrite, sprintf("    <td class=\"centeralign\">%i</td>\n", as.integer(c(datarow[1])[[1]])))
        for (i in 2:4) {
          # following values are float
          toWrite <- paste(toWrite, sprintf("    <td class=\"centeralign\">%.2f</td>\n", c(datarow[i])[[1]]))
        }
        # close row-tag
        toWrite <- paste(toWrite, "  </tr>\n", "\n")
      }
    }
    # -------------------------------------
    # write last table data row with NA
    # -------------------------------------
    # retrieve data row
    datarow <- df[nrow(df),]
    # -------------------------------------
    # write table data row
    # -------------------------------------
    # value label
    toWrite <- paste(toWrite, sprintf("  <tr class=\"lasttablerow\">\n     <td class=\"leftalign\">%s</td>\n", stringMissingValue))
    # cell values, first value is integer
    toWrite <- paste(toWrite, sprintf("    <td class=\"centeralign\">%i</td>\n", as.integer(c(datarow[1])[[1]])))
    # 2nd value is float. we don't need 3rd and 4th value as they are always 0 and 100
    toWrite <- paste(toWrite, sprintf("    <td class=\"centeralign\">%.2f</td>\n     <td></td>\n     <td></td>\n", c(datarow[2])[[1]]))
    # -------------------------------------
    # add info for mean, standard deviation
    # -------------------------------------
    if (showSummary) {
      vartot <- length(var)
      varvalid <- vartot - length(var[which(is.na(var))])
      if (is.null(weightBy)) {
        mw <- mean(orivar, na.rm=TRUE)
      }
      else {
        mw <- weighted.mean(orivar, weightBy, na.rm=TRUE)
      }
      toWrite <- paste(toWrite, sprintf("  </tr>\n\n  <tr>\n    <td class=\"summary\" colspan=\"5\">total N=%i &middot; valid N=%i &middot; x&#772;=%.2f &middot; &sigma;=%.2f</td>\n", vartot, varvalid, mw, sd(orivar, na.rm=TRUE)))
    }
    # -------------------------------------
    # finish table
    # -------------------------------------
    toWrite = paste(toWrite, "  </tr>\n </table>", "\n")
    # -------------------------------------
    # add separator in case we have more than one table
    # -------------------------------------
    if (nvar>1) {
      toWrite = paste(toWrite, "\n<p class=\"abstand\">&nbsp;</p>\n", "\n")
    }
  }
  # -------------------------------------
  # finish html page
  # -------------------------------------
  toWrite = paste(toWrite, "</body></html>", "\n")
  # -------------------------------------
  # check if we have filename specified
  # -------------------------------------
  if (!is.null(file)) {
    # write file
    write(toWrite, file=file)
  }
  else {
    # else create and browse temporary file
    htmlFile <- tempfile(fileext=".html")
    write(toWrite, file=htmlFile)
    # check whether we have RStudio Viewer
    viewer <- getOption("viewer")
    if (!is.null(viewer)) {
      viewer(htmlFile)
    }
    else {
      utils::browseURL(htmlFile)    
    }
    # delete temp file
    # unlink(htmlFile)
  }
}
