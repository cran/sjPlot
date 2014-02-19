#' @title Save data frame as HTML table
#' @name sjt.df
#' 
#' @description Save (or show) content of data frame (rows and columns) as HTML table.
#'                Helpful if you want a quick overview of a data frame's content.
#'
#' @param df A data frame that should be printed.
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
#' @param alternateRowColors If \code{TRUE}, alternating rows are highlighted with a light gray
#'          background color.
#' @param orderColumn Indicates a column, either by column name or by column index number,
#'          that should be orderd. Default order is ascending, which can be changed with
#'          \code{orderAscending} parameter. Default is \code{NULL}, hence the data frame
#'          is printed with no specific order. See examples for further details.
#' @param orderAscending If \code{TRUE} (default) and \code{orderColumn} is not \code{NULL},
#'          data frame is ordered according to the specified column in an ascending order.
#'          Use \code{FALSE} to apply descending order. See examples for further details.
#' @param stringObservation A string used for the first column name that indicates the observation or case number.
#'          Default is \code{"Observation"}.
#' @param encoding The charset encoding used for variable and value labels. Default is \code{"UTF-8"}. Change
#'          encoding if specific chars are not properly displayed (e.g.) German umlauts).
#'
#' @examples
#' # init dataset
#' data(efc)
#' 
#' # plot first 50 rows of first 5 columns of example data set
#' \dontrun{
#' sjt.df(efc[1:50,1:5])}
#' 
#' # plot first 20 rows of first 5 columns of example data set,
#' # ordered by column "e42dep" with alternating row colors
#' \dontrun{
#' sjt.df(efc[1:20,1:5], alternateRowColors=TRUE, orderColumn="e42dep")}
#' 
#' # plot first 20 rows of first 5 columns of example data set,
#' # ordered by 4th column in descending order.
#' \dontrun{
#' sjt.df(efc[1:20,1:5], orderColumn=4, orderAscending=FALSE)}
#' 
#' @export
sjt.df <- function (df,
                    file=NULL,
                    alternateRowColors=FALSE,
                    orderColumn=NULL,
                    orderAscending=TRUE,
                    stringObservation="Observation",
                    encoding="UTF-8") {
  # -------------------------------------
  # make data frame of single variable, so we have
  # unique handling for the data
  # -------------------------------------
  if (!is.data.frame(df)) {
    stop("Parameter needs to be a data frame!", call.=FALSE)
  }
  # -------------------------------------
  # Order data set if requested
  # -------------------------------------
  if (!is.null(orderColumn)) {
    # check whether orderColumn is numeric or character
    if (is.character(orderColumn)) {
      # retrieve column that equals orderColumn string
      nr <- which(colnames(df)==orderColumn)
      orderColumn <- as.numeric(nr)
    }
    # check for correct range
    if (is.numeric(orderColumn) && orderColumn>0 && orderColumn<=ncol(df)) {
      # retrieve order
      rfolge <- order(df[,orderColumn])
      # reverse order?
      if (!orderAscending) {
        rfolge <- rev(rfolge)
      }
      # sort dataframe
      df <- df[rfolge,]
    }
  }
  # -------------------------------------
  # table init
  # -------------------------------------
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n<style>\ntable { border-collapse:collapse; border:none }\nth { border-top: double; text-align:center; font-style:italic; font-weight:normal }\ntd, th { padding:0.2cm; text-align:center }\n.lasttablerow { border-top:1px solid; border-bottom: double }\n.firsttablerow { border-bottom:1px solid }\n.leftalign { text-align:left }\n.arc { background-color:#eaeaea }\n</style>\n</head>\n<body>\n", encoding)
  # -------------------------------------
  # get row and column count of data frame
  # -------------------------------------
  rowcnt <- nrow(df)
  colcnt <- ncol(df)
  # -------------------------------------
  # get row and column names of data frame
  # -------------------------------------
  rnames <- rownames(df)
  cnames <- colnames(df)
  # -------------------------------------
  # start table tag
  # -------------------------------------
  toWrite <- paste(toWrite, "<table>", "\n")
  # -------------------------------------
  # header row
  # -------------------------------------
  toWrite <- paste0(toWrite, sprintf("  <tr class=\"firsttablerow\">\n    <th>%s</th>", stringObservation))
  for (i in 1:colcnt) {
    # check variable type
    vartype <- c("unknown type")
    if (is.character(df[,i])) vartype <- c("character")
    else if (is.factor(df[,i])) vartype <- c("factor")
    else if (is.numeric(df[,i])) vartype <- c("numeric")
    else if (is.atomic(df[,i])) vartype <- c("atomic")
    # column names and variable as table headline
    toWrite <- paste0(toWrite, sprintf("  <th>%s<br>(%s)</th>", cnames[i], vartype))
  }
  toWrite <- paste0(toWrite, "  </tr>\n")
  # -------------------------------------
  # subsequent rows
  # -------------------------------------
  for (rcnt in 1:rowcnt) {
    # default row string
    rowstring <- c("<tr>")
    # if we have alternating row colors, set css
    if (alternateRowColors) rowstring <- ifelse(rcnt %% 2 ==0, "<tr class=\"arc\">", "<tr>")
    toWrite <- paste0(toWrite, sprintf("  %s", rowstring))
    # first table cell is rowname
    toWrite <- paste0(toWrite, sprintf("  <td class=\"leftalign\">%s</td>", rnames[rcnt]))
    # all columns of a row
    for (ccnt in 1:colcnt) {
      toWrite <- paste0(toWrite, sprintf("<td>%s</td>", df[rcnt,ccnt]))
    }
    # close row tag
    toWrite <- paste0(toWrite, "</tr>\n")
  }
  # -------------------------------------
  # finish html page
  # -------------------------------------
  toWrite = paste(toWrite, "</table>\n</body></html>", "\n")
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
                     