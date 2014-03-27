#' @title Show frequencies as HTML table
#' @name sjt.frq
#' 
#' @description Shows (multiple) frequency tables as HTML file, or saves them as file.
#' 
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2014/02/20/no-need-for-spss-beautiful-output-in-r-rstats/}
#'              }
#' 
#' @seealso \code{\link{sjp.frq}} \cr
#'          \code{\link{sjt.xtab}}
#' 
#' @param data The variables which frequencies should be printed as table. Either use a single variable
#'          (vector) or a data frame where each column represents a variable (see examples.
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
#' @param weightBy A weight factor that will be applied to weight all cases from \code{data}.
#'          default is \code{NULL}, so no weights are used.
#' @param variableLabels A single character vector or a list of character vectors that indicate
#'          the variable names of those variables from \code{data} and will be used as variable labels
#'          in the output. Note that if multiple variables
#'          are supplied (as data frame), the variable labels must be supplied as \code{list} object
#'          (see examples).
#' @param valueLabels A list of character vectors that indicate the value labels of those variables 
#'          from \code{data}. Note that if multiple variables are supplied (as data frame), the 
#'          value labels must be supplied as nested \code{list} object (see examples).
#' @param autoGroupAt A value indicating at which length of unique values a variable from \code{data}
#'          is automatically grouped into smaller units (see \code{\link{sju.groupVar}}). Variables with large 
#'          numbers of unique values may be too time consuming when a HTML table is created and R would
#'          not respond any longer. Hence it's recommended to group such variables. Default value is 50,
#'          i.e. variables with 50 and more unique values will be grouped using \code{\link{sju.groupVar}} with
#'          \code{groupsize="auto"} parameter. By default, the maximum group count is 30. However, if
#'          \code{autoGroupAt} is less than 30, \code{autoGroupAt} groups are built. Default value is \code{NULL},
#'          i.e. auto-grouping is turned off.
#' @param alternateRowColors If \code{TRUE}, alternating rows are highlighted with a light gray
#'          background color.
#' @param stringValue String label for the very first table column containing the values (see
#'          \code{valueLabels}).
#' @param stringCount String label for the first table data column containing the counts. Default is \code{"N"}.
#' @param stringPerc String label for the second table data column containing the percentages, where the
#'          count percentages include missing values.
#' @param stringValidPerc String label for the third data table column containing the valid percentages, i.e. the
#'          count percentage value exluding possible missing values.
#' @param stringCumPerc String label for the last table data column containing the cumulative percentages.
#' @param stringMissingValue String label for the last table data row containing missing values.
#' @param highlightMedian If \code{TRUE}, the table row indicating the median value will
#'          be highlighted.
#' @param highlightQuartiles If \code{TRUE}, the table row indicating the lower and upper quartiles will
#'          be highlighted.
#' @param skipZeroRows If \code{TRUE}, rows with only zero-values are not printed. Default is \code{FALSE}.
#' @param showSummary If \code{TRUE} (default), a summary row with total and valid N as well as mean and
#'          standard deviation is shown.
#' @param showSkew If \code{TRUE}, the variable's skewness is added to the summary.
#'          The skewness is retrieved from the \code{\link{describe}} function of the \code{\link{psych}}
#'          package.
#' @param showKurtosis If \code{TRUE}, the variable's kurtosis is added to the summary.
#'          The kurtosis is retrieved from the \code{\link{describe}} function of the \code{\link{psych}}
#'          package.
#' @param skewString A character string, which is used as header for the skew column (see \code{showSkew})).
#'          Default is lower case Greek gamma.
#' @param kurtosisString A character string, which is used as header for the kurtosis column (see \code{showKurtosis})).
#'          Default is lower case Greek omega.
#' @param encoding The charset encoding used for variable and value labels. Default is \code{"UTF-8"}. Change
#'          encoding if specific chars are not properly displayed (e.g.) German umlauts).
#' @param CSS A \code{\link{list}} with user-defined style-sheet-definitions, according to the official CSS syntax (see
#'          \url{http://www.w3.org/Style/CSS/}). See return value \code{page.style} for details
#'          of all style-sheet-classnames that are used in this function. Parameters for this list need:
#'          \enumerate{
#'            \item the class-names with \code{"css."}-prefix as parameter name and
#'            \item each style-definition must end with a semicolon
#'          } 
#'          Examples:
#'          \itemize{
#'            \item \code{css.table='border:2px solid red;'} for a solid 2-pixel table border in red.
#'            \item \code{css.summary='font-weight:bold;'} for a bold fontweight in the summary row.
#'            \item \code{css.lasttablerow='border-bottom: 1px dotted blue;'} for a blue dotted border of the last table row.
#'          }
#'          See further examples below.
#' @param useViewer If \code{TRUE}, the function tries to show the HTML table in the IDE's viewer pane. If
#'          \code{FALSE} or no viewer available, the HTML table is opened in a web browser.
#' @param no.output If \code{TRUE}, the html-output is neither opened in a browser nor shown in
#'          the viewer pane and not even saved to file. This option is useful when the html output
#'          should be used in \code{knitr} documents. The html output can be accessed via the return
#'          value.
#' @return Invisibly returns a \code{\link{structure}} with
#'          \itemize{
#'            \item the web page style sheet (\code{page.style}),
#'            \item each frequency table as web page content (\code{page.content.list}),
#'            \item the complete html-output (\code{output.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
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
#' \dontrun{
#' sjt.frq(efc$e42dep)}
#' 
#' # plot and show frequency table of "e42dep" with labels
#' \dontrun{
#' sjt.frq(efc$e42dep,
#'         variableLabels=variables['e42dep'],
#'         valueLabels=values[['e42dep']])}
#' 
#' # plot frequencies of e42dep, e16sex and c172code in one HTML file
#' # and show table in RStudio Viewer Pane or default web browser
#' \dontrun{
#' sjt.frq(as.data.frame(cbind(efc$e42dep, efc$e16sex, efc$c172code)),
#'         variableLabels=list(variables['e42dep'], variables['e16sex'], variables['c172code']),
#'         valueLabels=list(values[['e42dep']], values[['e16sex']], values[['c172code']]))}
#' 
#' # plot larger scale including zero-counts
#' # indicating median and quartiles
#' \dontrun{
#' sjt.frq(efc$neg_c_7,
#'         variableLabels=variables['neg_c_7'],
#'         valueLabels=values[['neg_c_7']],
#'         highlightMedian=TRUE,
#'         highlightQuartiles=TRUE)}
#' 
#' # -------------------------------
#' # auto-detection of labels
#' # -------------------------------
#' efc <- sji.setVariableLabels(efc, variables)
#' \dontrun{
#' sjt.frq(data.frame(efc$e42dep, efc$e16sex, efc$c172code))}
#' 
#' # -------------------------------- 
#' # User defined style sheet
#' # -------------------------------- 
#' \dontrun{
#' sjt.frq(efc$e42dep,
#'         variableLabels=variables['e42dep'],
#'         valueLabels=values[['e42dep']],
#'         CSS=list(css.table="border: 2px solid;",
#'                  css.tdata="border: 1px solid;",
#'                  css.firsttablecol="color:#003399; font-weight:bold;"))}
#' 
#' @importFrom psych describe
#' @export
sjt.frq <- function (data,
                     file=NULL,
                     weightBy=NULL,
                     variableLabels=NULL,
                     valueLabels=NULL,
                     autoGroupAt=NULL,
                     alternateRowColors=FALSE,
                     stringValue="value",
                     stringCount="N",
                     stringPerc="raw %",
                     stringValidPerc="valid %",
                     stringCumPerc="cumulative %",
                     stringMissingValue="missings",
                     highlightMedian=FALSE,
                     highlightQuartiles=FALSE,
                     skipZeroRows=FALSE,
                     showSummary=TRUE,
                     showSkew=FALSE,
                     showKurtosis=FALSE,
                     skewString="&gamma;",
                     kurtosisString="&omega;",
                     encoding="UTF-8",
                     CSS=NULL,
                     useViewer=TRUE,
                     no.output=FALSE) {
  # -------------------------------------
  # table init
  # -------------------------------------
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.caption <- "caption"
  tag.summary <- "summary"
  tag.arc <- "arc"
  tag.qrow <- "qrow"
  tag.mdrow <- "mdrow"
  tag.abstand <- "abstand"
  tag.lasttablerow <- "lasttablerow"
  tag.firsttablerow <- "firsttablerow"
  tag.leftalign <- "leftalign"
  tag.centeralign <- "centeralign"
  tag.firsttablecol <- "firsttablecol"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "border-top:double; text-align:center; font-style:italic; font-weight:normal; padding-left:0.2cm; padding-right:0.2cm;"
  css.tdata <- "padding:0.2cm;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.summary <- "text-align:right; font-style:italic; font-size:0.9em; padding-top:0.1cm; padding-bottom:0.1cm;"
  css.arc <- "background-color:#eaeaea;"
  css.qrow <- "border-bottom: 1px solid #cc3333;"
  css.mdrow <- "font-weight:bolder; font-style:italic; color:#993333;"
  css.abstand <- "margin-bottom: 2em;"
  css.lasttablerow <- "border-top:1px solid; border-bottom:double;"
  css.firsttablerow <- "border-bottom:1px solid;"
  css.leftalign <- "text-align:left;"
  css.centeralign <- "text-align:center;"
  css.firsttablecol <- ""
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- CSS[['css.table']]
    if (!is.null(CSS[['css.thead']])) css.thead <- CSS[['css.thead']]
    if (!is.null(CSS[['css.tdata']])) css.tdata <- CSS[['css.tdata']]
    if (!is.null(CSS[['css.caption']])) css.caption <- CSS[['css.caption']]
    if (!is.null(CSS[['css.summary']])) css.summary <- CSS[['css.summary']]
    if (!is.null(CSS[['css.arc']])) css.arc <- CSS[['css.arc']]
    if (!is.null(CSS[['css.qrow']])) css.qrow <- CSS[['css.qrow']]
    if (!is.null(CSS[['css.mdrow']])) css.mdrow <- CSS[['css.mdrow']]
    if (!is.null(CSS[['css.abstand']])) css.abstand <- CSS[['css.abstand']]
    if (!is.null(CSS[['css.lasttablerow']])) css.lasttablerow <- CSS[['css.lasttablerow']]
    if (!is.null(CSS[['css.firsttablerow']])) css.firsttablerow <- CSS[['css.firsttablerow']]
    if (!is.null(CSS[['css.leftalign']])) css.leftalign <- CSS[['css.leftalign']]
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- CSS[['css.centeralign']]
    if (!is.null(CSS[['css.firsttablecol']])) css.firsttablecol <- CSS[['css.firsttablecol']]
  }
  # -------------------------------------
  # set style sheet
  # -------------------------------------
  page.style <- sprintf("<style>\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n%s { %s }\n.%s { %s }</style>",
                        tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata,
                        tag.summary, css.summary, tag.arc, css.arc, tag.qrow, css.qrow,
                        tag.mdrow, css.mdrow, tag.abstand, css.abstand, tag.lasttablerow, css.lasttablerow,
                        tag.firsttablerow, css.firsttablerow, tag.leftalign, css.leftalign,
                        tag.centeralign, css.centeralign, tag.caption, css.caption,
                        tag.firsttablecol, css.firsttablecol)
  # start writing content
  toWrite <- paste(toWrite, page.style)
  toWrite <- paste(toWrite, "\n</head>\n<body>\n")
  # -------------------------------------
  # auto-retrieve variable labels
  # -------------------------------------
  if (is.null(variableLabels)) {
    # init variable Labels as list
    variableLabels <- list()
    # check if we have data frame with several variables
    if (is.data.frame(data)) {
      # if yes, iterate each variable
      for (i in 1:ncol(data)) {
        # retrieve variable name attribute
        vn <- autoSetVariableLabels(data[,i])
        # if variable has attribute, add to variableLabel list
        if (!is.null(vn)) {
          variableLabels <- c(variableLabels, vn)
        }
        else {
          # else break out of loop
          variableLabels <- NULL
          break
        }
      }
    }
    # we have a single variable only
    else {
      # retrieve variable name attribute
      vn <- autoSetVariableLabels(data)
      # if variable has attribute, add to variableLabel list
      if (!is.null(vn)) {
        variableLabels <- c(variableLabels, vn)
      }
      else {
        # else reset variableLabels
        variableLabels <- NULL
      }
    }
  }
  # -------------------------------------
  # make data frame of single variable, so we have
  # unique handling for the data
  # -------------------------------------
  if (!is.data.frame(data)) {
    # check for auto-detection of labels
    if (is.null(valueLabels)) valueLabels <- autoSetValueLabels(data)
    # copy variable to data frame for unuqie handling
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
      # check for auto-detection of labels
      valueLabels <- c(valueLabels, list(autoSetValueLabels(dummy)))
      # and add label range to value labels list
      if (is.null(valueLabels)) valueLabels <- c(valueLabels, list(min(dummy, na.rm=TRUE):max(dummy, na.rm=TRUE)))
    }
  }
  # -------------------------------------
  # header row of table
  # -------------------------------------
  page.content.list <- list()
  headerRow <- sprintf("   <tr>\n     <th class=\"thead firsttablerow firsttablecol\">%s</th>\n     <th class=\"thead firsttablerow\">%s</th>\n     <th class=\"thead firsttablerow\">%s</th>\n     <th class=\"thead firsttablerow\">%s</th>\n     <th class=\"thead firsttablerow\">%s</th>\n   </tr>\n\n", stringValue, stringCount, stringPerc, stringValidPerc, stringCumPerc)
  # -------------------------------------
  # start iterating all variables
  # -------------------------------------
  for (cnt in 1:nvar) {
    # -----------------------------------------------
    # prepare data: create frequencies and weight them,
    # if requested. put data into a data frame
    #---------------------------------------------------
    # get variable
    orivar <- var <- as.numeric(data[,cnt])
    # -----------------------------------------------
    # check for length of unique values and skip if too long
    # -----------------------------------------------
    if (!is.null(autoGroupAt) && length(unique(var))>=autoGroupAt) {
      cat(sprintf("\nVariable %s with %i unique values was grouped...\n", colnames(data)[cnt], length(unique(var))))
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
    page.content <- "<table>\n"
    # -------------------------------------
    # retrieve variable label
    # -------------------------------------
    varlab <- variableLabels[[cnt]]
    # -------------------------------------
    # table caption, variable label
    # -------------------------------------
    page.content <- paste(page.content, sprintf("  <caption>%s</caption>\n", varlab))
    # -------------------------------------
    # header row with column labels
    # -------------------------------------
    page.content <- paste0(page.content, headerRow)
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
        # init default values
        rowstring <- ""
        # init default value for alternating colors
        if (alternateRowColors) rowstring <- ifelse(j %% 2 ==0, " arc", "")
        rowcss <- rowstring
        # check whether we have median row and whether it should be highlighted
        if (highlightMedian && ((j+minval)==(var.median+1))) {
          rowcss <- sprintf(" mdrow%s", rowstring)
        }
        # check whether we have lower quartile and whether it should be highlighted
        else {
          if (highlightQuartiles) {
            if(((j+minval)==(var.lowerq+1)) || ((j+minval)==(var.upperq+1))) {
              rowcss <- sprintf(" qrow%s", rowstring)
            }
          }
        }
        # value label
        page.content <- paste(page.content, sprintf("  <tr>\n     <td class=\"tdata leftalign firsttablecol%s\">%s</td>\n", rowcss, vallab[j]))
        # cell values, first value is integer
        page.content <- paste(page.content, sprintf("    <td class=\"tdata centeralign%s\">%i</td>\n", rowcss, as.integer(c(datarow[1])[[1]])))
        for (i in 2:4) {
          # following values are float
          page.content <- paste(page.content, sprintf("    <td class=\"tdata centeralign%s\">%.2f</td>\n", rowcss, c(datarow[i])[[1]]))
        }
        # close row-tag
        page.content <- paste(page.content, "  </tr>\n", "\n")
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
    page.content <- paste(page.content, sprintf("  <tr>\n     <td class=\"tdata leftalign lasttablerow firsttablecol\">%s</td>\n", stringMissingValue))
    # cell values, first value is integer
    page.content <- paste(page.content, sprintf("    <td class=\"tdata centeralign lasttablerow\">%i</td>\n", as.integer(c(datarow[1])[[1]])))
    # 2nd value is float. we don't need 3rd and 4th value as they are always 0 and 100
    page.content <- paste(page.content, sprintf("    <td class=\"tdata centeralign lasttablerow\">%.2f</td>\n     <td class=\"tdata lasttablerow\"></td>\n     <td class=\"tdata lasttablerow\"></td>\n", c(datarow[2])[[1]]))
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
      descr <- ""
      if (showSkew || showKurtosis) {
        pstat <- describe(data.frame(orivar))
        if (showSkew) descr <- sprintf(" &middot; %s=%.2f", skewString, pstat$skew)
        if (showKurtosis) descr <- sprintf("%s &middot; %s=%.2f", descr, kurtosisString, pstat$kurtosis)
      }
      page.content <- paste(page.content, sprintf("  </tr>\n  <tr>\n    <td class=\"tdata summary\" colspan=\"5\">total N=%i &middot; valid N=%i &middot; x&#772;=%.2f &middot; &sigma;=%.2f%s</td>\n", vartot, varvalid, mw, sd(orivar, na.rm=TRUE), descr))
    }
    # -------------------------------------
    # finish table
    # -------------------------------------
    page.content <- paste(page.content, "  </tr>\n </table>")
    # -------------------------------------
    # add table to return value list, so user can access each
    # single frequency table
    # -------------------------------------
    page.content.list[[length(page.content.list)+1]] <- page.content
    toWrite <- paste(toWrite, page.content, "\n")
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
  toWrite <- paste0(toWrite, "</body></html>")
  # -------------------------------------
  # replace class attributes with inline style,
  # useful for knitr
  # -------------------------------------
  # copy page content
  # -------------------------------------
  if (nvar>1) {
    knitr <- c()
    for (i in 1:length(page.content.list)) {
      knitr <- paste0(knitr, page.content.list[[i]], sprintf("\n<p style=\"%s\">&nbsp;</p>\n", css.abstand))
    }
  }
  else {
    knitr <- page.content
  }
  # -------------------------------------
  # set style attributes for main table tags
  # -------------------------------------
  knitr <- gsub("class=", "style=", knitr)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), knitr)
  knitr <- gsub("<caption", sprintf("<caption style=\"%s\"", css.caption), knitr)
  # -------------------------------------
  # replace class-attributes with inline-style-definitions
  # -------------------------------------
  knitr <- gsub(tag.tdata, css.tdata, knitr)
  knitr <- gsub(tag.thead, css.thead, knitr)
  knitr <- gsub(tag.firsttablerow, css.firsttablerow, knitr)
  knitr <- gsub(tag.firsttablecol, css.firsttablecol, knitr)
  knitr <- gsub(tag.leftalign, css.leftalign, knitr)  
  knitr <- gsub(tag.centeralign, css.centeralign, knitr)  
  knitr <- gsub(tag.lasttablerow, css.lasttablerow, knitr)  
  knitr <- gsub(tag.summary, css.summary, knitr)  
  knitr <- gsub(tag.arc, css.arc, knitr)  
  knitr <- gsub(tag.qrow, css.qrow, knitr)  
  knitr <- gsub(tag.mdrow, css.mdrow, knitr)  
  knitr <- gsub(tag.abstand, css.abstand, knitr)  
  # -------------------------------------
  # check if html-content should be outputted
  # -------------------------------------
  if (!no.output) {
    # -------------------------------------
    # check if we have filename specified
    # -------------------------------------
    if (!is.null(file)) {
      # write file
      write(knitr, file=file)
    }
    # -------------------------------------
    # else open in viewer pane
    # -------------------------------------
    else {
      # else create and browse temporary file
      htmlFile <- tempfile(fileext=".html")
      write(toWrite, file=htmlFile)
      # check whether we have RStudio Viewer
      viewer <- getOption("viewer")
      if (useViewer && !is.null(viewer)) {
        viewer(htmlFile)
      }
      else {
        utils::browseURL(htmlFile)    
      }
      # delete temp file
      # unlink(htmlFile)
    }
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjtfrq",
                       list(page.style = page.style,
                            page.content.list = page.content.list,
                            output.complete = toWrite,
                            knitr = knitr)))
}
