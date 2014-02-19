#' @title Save cross tables as HTML table
#' @name sjt.xtab
#' 
#' @description Save cross tables as HTML file.
#' 
#' @seealso \link{sjp.xtab}
#' 
#' @param var.row Variable that should be displayed in the table rows.
#' @param var.col Variable that should be displayed in the table columns.
#' @param var.grp An optional grouping variable that splits the data into several groups,
#'          depending on the amount of categories. See examples for details.
#' @param weightBy A weight factor that will be applied to weight all cases.
#'          Default is \code{NULL}, so no weights are used.
#' @param digits The amount of digits used for the percenage values inside table cells.
#'          Default is 1.
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
#' @param variableLabels A character vector of same length as supplied variables, with 
#'          the associated variable names. Following order is needed: name of \code{var.row},
#'          name of \code{var.col}, and - if \code{var.grp} is not \code{NULL} - name of \code{var.grp}.
#'          See examples for more details.
#' @param valueLabels A list of character vectors that indicate the value labels of the supplied
#'          variables. Following order is needed: value labels of \code{var.row},
#'          value labels  of \code{var.col}, and - if \code{var.grp} is not \code{NULL} - 
#'          value labels  of \code{var.grp}. \code{valueLabels} needs to be a \link{list} object.
#'          See examples for more details.
#' @param stringTotal String label for the total column / row header.
#' @param showCellPerc If \code{TRUE}, cell percentage values are shown.
#' @param showRowPerc If \code{TRUE}, row percentage values are shown.
#' @param showColPerc If \code{TRUE}, column percentage values are shown.
#' @param showExpected If \code{TRUE}, expected values are also shown.
#' @param showHorizontalLine If \code{TRUE}, data rows are separated with a horizontal line.
#' @param showSummary If \code{TRUE} (default), a summary row with chi-square statistics,
#'          Cramer's V or Phi-value etc. is shown.
#' @param tdcol.n Color for highlighting count (observed) values in table cells. Default is black.
#' @param tdcol.expected Color for highlighting expected values in table cells. Default is cyan.
#' @param tdcol.cell Color for highlighting cell percentage values in table cells. Default is red.
#' @param tdcol.row Color for highlighting row percentage values in table cells. Default is blue.
#' @param tdcol.col Color for highlighting column percentage values in table cells. Default is green.
#' @param highlightTotal If \code{TRUE}, the total column and row will be highlighted with a
#'          different background color. See \code{highlightColor}.
#' @param highlightColor If \code{highlightTotal} is \code{TRUE}, this color value will be used
#'          for painting the background of the total column and row. Default is a light grey.
#' @param percSign The percentage sign that is printed in the table cells, in HTML-format.
#'          Default is \code{"&nbsp;\%"}, hence the percentage sign has a non-breaking-space after
#'          the percentage value.
#' @param hundret Default value that indicates the 100-percent column-sums (since rounding values
#'          may lead to non-exact results). Default is \code{"100.0"}.
#' @param encoding The charset encoding used for variable and value labels. Default is \code{"UTF-8"}. Change
#'          encoding if specific chars are not properly displayed (e.g.) German umlauts).
#' @return Invisibly returns a \link{structure} with the web page style sheet (\code{page.style}) and the
#'          web page content (\code{page.content}) for further use.
#'          
#' @note The HTML tables can either be saved as file and manually opened (specify parameter \code{file}) or
#'         they can be saved as temporary files and will be displayed in the RStudio Viewer pane (if working with RStudio)
#'         or opened with the default web browser. Displaying resp. opening a temporary file is the
#'         default behaviour (i.e. \code{file=NULL}).
#'         
#' @examples 
#' # prepare sample data set
#' data(efc)
#' efc.labels <- sji.getValueLabels(efc)
#' 
#' # print simple cross table w/o labels
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep)}
#'          
#' # print cross table with labels and expected values
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep, 
#'          variableLabels=c("Elder's gender", "Elder's dependency"),
#'          valueLabels=list(efc.labels[['e16sex']], efc.labels[['e42dep']]),
#'          showExpected=TRUE)}
#' 
#' # print minimal cross table with labels, total col/row highlighted
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep, 
#'          variableLabels=c("Elder's gender", "Elder's dependency"),
#'          valueLabels=list(efc.labels[['e16sex']], efc.labels[['e42dep']]),
#'          showHorizontalLine=FALSE,
#'          showCellPerc=FALSE,
#'          highlightTotal=TRUE)}
#' 
#' # print cross table with labels and all percentages
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep, 
#'          variableLabels=c("Elder's gender", "Elder's dependency"),
#'          valueLabels=list(efc.labels[['e16sex']], efc.labels[['e42dep']]),
#'          showRowPerc=TRUE, showColPerc=TRUE)}
#' 
#' # print cross table with labels and all percentages, including
#' # grouping variable
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep, efc$c161sex, 
#'          variableLabels=c("Elder's gender", 
#'                           "Elder's dependency",
#'                           "Carer's gender"), 
#'          valueLabels=list(efc.labels[['e16sex']],
#'                           efc.labels[['e42dep']],
#'                           efc.labels[['c161sex']]),
#'          showRowPerc=TRUE, showColPerc=TRUE)}
#'          
#' @export
sjt.xtab <- function (var.row,
                      var.col,
                      var.grp=NULL,
                      weightBy=NULL,
                      digits=1,
                      file=NULL,
                      variableLabels=NULL,
                      valueLabels=NULL,
                      stringTotal="Total",
                      showCellPerc=TRUE,
                      showRowPerc=FALSE,
                      showColPerc=FALSE,
                      showExpected=FALSE,
                      showHorizontalLine=TRUE,
                      showSummary=TRUE,
                      tdcol.n="black",
                      tdcol.expected="#339999",
                      tdcol.cell="#993333",
                      tdcol.row="#333399",
                      tdcol.col="#339933",
                      highlightTotal=FALSE,
                      highlightColor="#f8f8f8",
                      percSign="&nbsp;%",
                      hundret="100.0",
                      encoding="UTF-8") {
  # -------------------------------------
  # init variable labels
  # -------------------------------------
  if(!is.null(variableLabels)) {
    s.var.row <- ifelse(length(variableLabels)>0, variableLabels[1], "var.row")
    s.var.col <- ifelse(length(variableLabels)>1, variableLabels[2], "var.col")
    s.var.grp <- ifelse(length(variableLabels)>2, variableLabels[3], "var.grp")
  }
  else {
    s.var.row <- "var.row"
    s.var.col <- "var.col"
    s.var.grp <- "var.grp"
  }
  # -------------------------------------
  # compute xtab
  # -------------------------------------
  # check if we have weights or not
  if (is.null(weightBy)) {
    # check if we have groupings or not
    if (is.null(var.grp)) {
      tab <- ftable(xtabs(~ var.row + var.col))
      coladd <- 2
    }
    else {
      tab <- ftable(xtabs(~ var.grp + var.row + var.col))
      coladd <- 3
    }
  }
  else {
    # check if we have groupings or not
    if (is.null(var.grp)) {
      tab <- ftable(xtabs(weightBy ~ var.row + var.col))
      coladd <- 2
    }
    else {
      tab <- ftable(xtabs(weightBy ~ var.grp + var.row + var.col))
      coladd <- 3
    }
  }
  # -------------------------------------
  # compute table percentages
  # -------------------------------------
  tab.cell <- round(100*prop.table(tab),digits)
  tab.row <- round(100*prop.table(tab,1),digits)
  tab.col <- round(100*prop.table(tab,2),digits)
  tab.expected <- as.table(round(as.array(margin.table(tab,1)) %*% t(as.array(margin.table(tab,2))) / margin.table(tab)))
  # -------------------------------------
  # determine total number of columns
  # we have an optional column for the grouping variable,
  # a column for var.row labels and the columns for the
  # var.col data. Finally, we have a "total" column
  # -------------------------------------
  totalncol <- ncol(tab)+coladd
  # -------------------------------------
  # init value labels
  # -------------------------------------
  labels.var.row <- labels.var.grp <- labels.var.col <- NULL
  if (is.null(valueLabels)) {
    # -------------------------------------
    # if we don't have value labels, retrieve labels
    # from the table attribute
    # -------------------------------------
    # retrieve row variable and label attributes
    vn <- attr(tab, "row.vars")
    # set default names for row value labels
    labels.var.row <- vn[[1]]
    # check whether we have group value labels as well
    if (length(vn)>1) {
      # set default names for group value labels
      labels.var.grp <- vn[[2]]
    }
    # retrieve row variable and label attributes
    vn <- attr(tab, "col.vars")
    # set default names for col value labels
    labels.var.col <- vn[[1]]
  }
  else {
    # -------------------------------------
    # check how many value labels have been supplied
    # and set value labels
    # -------------------------------------
    if (length(valueLabels)>0) {
      labels.var.row <- valueLabels[[1]]
    }
    else {
      labels.var.row <- seq_along(unique(na.omit(var.row)))
    }
    if (length(valueLabels)>1) {
      labels.var.col <- valueLabels[[2]]
    }
    else {
      labels.var.col <- seq_along(unique(na.omit(var.col)))
    }
    if (length(valueLabels)>2) {
      labels.var.grp <- valueLabels[[3]]
    }
    else {
      if (is.null(var.grp)) {
        labels.var.grp <- NULL
      }
      else {
        labels.var.grp <- seq_along(unique(na.omit(var.grp)))
      }
    }
  }
  # -------------------------------------
  # table init
  # -------------------------------------
  # set highlight color for style sheet
  ht <- ifelse(highlightTotal==TRUE, sprintf("background-color:%s", highlightColor), "")
  hl <- ifelse(showHorizontalLine==TRUE, "border-bottom:1px solid", "")
  # init web page header
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  # init style sheet
  page.style <- sprintf("<style>\ntable { border-collapse:collapse; border:none }\nth { border-top: double; text-align:center; font-style:italic; font-weight:normal }\ntable td { padding:0.2cm }\n.secondtablerow { border-bottom:1px solid; text-align:center }\n.leftalign { text-align:left; vertical-align:top }\n.centeralign { text-align:center }\n.lasttablerow { %s }\n.totcol { %s }\n.tothi { font-weight:bolder; font-style:italic }\n.td_n { color:%s }\n.td_c { color:%s }\n.td_rw { color:%s }\n.td_cl { color:%s }\n.td_ex { color:%s }\n.summary { text-align:right; font-size:0.9em; font-style:italic; border-top:double }\n.horline { %s }\n</style>", ht, ht, tdcol.n, tdcol.cell, tdcol.row, tdcol.col, tdcol.expected, hl)
  # start writing content
  toWrite <- paste(toWrite, page.style)
  toWrite <- paste(toWrite, "\n</head>\n<body>\n")
  # -------------------------------------
  # init first table row
  # -------------------------------------
  page.content <- "<table>\n"
  page.content <- paste(page.content, "  <tr class=\"firsttablerow\">\n")
  # -------------------------------------
  # check whether we have additional grouping column
  # -------------------------------------
  if (!is.null(var.grp)) {
    page.content <- paste(page.content, sprintf("    <th rowspan=\"2\">%s</th>\n", s.var.grp))
  }
  # -------------------------------------
  # column with row-variable-name
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <th rowspan=\"2\">%s</th>\n", s.var.row))
  # -------------------------------------
  # column with column-variable-name
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <th colspan=\"%i\">%s</th>\n", length(labels.var.col), s.var.col))
  # -------------------------------------
  # total-column
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <th class=\"tothi\" rowspan=\"2\">%s</th>\n", stringTotal))
  page.content <- paste(page.content, "  </tr>\n")
  # -------------------------------------
  # init second table row
  # -------------------------------------
  page.content <- paste(page.content, "\n  <tr class=\"secondtablerow\">\n")
  # -------------------------------------
  # column variable labels
  # -------------------------------------
  for (i in seq_along(labels.var.col)) {
    page.content <- paste(page.content, sprintf("    <td>%s</td>\n", labels.var.col[i]))
  }
  page.content <- paste(page.content, "  </tr>\n")
  # -------------------------------------
  # table content
  # -------------------------------------
  # retrieve index colums of group var, if we have any
  # if we have a grouping variable, we need to know at
  # which row a new category of group starts
  # -------------------------------------
  if (is.null(var.grp)) {
    group.var.rows <- NULL
  }
  else {
    group.var.rows <- seq(1,nrow(tab), by=length(labels.var.grp))
  }
  # -------------------------------------
  # if we have group vars, we need a repeating counter vor row value labels
  # -------------------------------------
  if (!is.null(group.var.rows)) {
    rowlabelcnt <- rep(1:length(labels.var.row), length(group.var.rows))
  }
  else {
    rowlabelcnt <- 1:length(labels.var.row)
  }
  # -------------------------------------
  # iterate all table data rows
  # -------------------------------------
  for (irow in 1:nrow(tab)) {
    # -------------------------------------
    # start new table row
    # -------------------------------------
    page.content <- paste(page.content, "\n  <tr>")
    # -------------------------------------
    # check for group var label, resp. if group var
    # starts with current row
    # -------------------------------------
    if (any(group.var.rows==irow)) {
      page.content <- paste(page.content, sprintf("\n    <td class=\"leftalign\" rowspan=\"%i\">%s</td>", length(labels.var.row), labels.var.grp[which(group.var.rows==irow)]))
    }
    # -------------------------------------
    # set row variable label
    # -------------------------------------
    page.content <- paste(page.content, sprintf("\n    <td class=\"leftalign\">%s</td>", labels.var.row[rowlabelcnt[irow]]))
    # -------------------------------------
    # iterate all data columns
    # -------------------------------------
    for (icol in 1:ncol(tab)) {
      # -------------------------------------
      # first table cell data contains observed values
      # -------------------------------------
      cellstring <- sprintf("<span class=\"td_n\">%i</span>", tab[irow,icol])
      # -------------------------------------
      # if we have expected values, add them to table cell
      # -------------------------------------
      if (showExpected) {
        cellstring <- paste(cellstring, sprintf("<br><span class=\"td_ex\">%s</span>", tab.expected[irow,icol]), sep="")
      }
      # -------------------------------------
      # if we have row-percentage, add percentage value to table cell
      # -------------------------------------
      if (showRowPerc) {
        cellstring <- paste(cellstring, sprintf("<br><span class=\"td_rw\">%s%s</span>", tab.row[irow,icol],percSign), sep="")
      }
      # -------------------------------------
      # if we have col-percentage, add percentage value to table cell
      # -------------------------------------
      if (showColPerc) {
        cellstring <- paste(cellstring, sprintf("<br><span class=\"td_cl\">%s%s</span>", tab.col[irow,icol], percSign), sep="")
      }
      # -------------------------------------
      # if we have cell-percentage, add percentage value to table cell
      # -------------------------------------
      if (showCellPerc) {
        cellstring <- paste(cellstring, sprintf("<br><span class=\"td_c\">%s%s</span>", tab.cell[irow,icol], percSign), sep="")
      }
      # -------------------------------------
      # write table cell data
      # -------------------------------------
      page.content <- paste(page.content, sprintf("\n    <td class=\"centeralign horline\">%s</td>", cellstring), sep="")
    }
    # -------------------------------------
    # after all data columns have been printed,
    # add a total column
    # -------------------------------------
    # first table cell data contains observed values
    # -------------------------------------
    cellstring <- sprintf("<span class=\"td_n\">%i</span>", rowSums(tab)[irow])
    # if we have expected values, add them to table cell
    if (showExpected) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_ex\">%s</span>", rowSums(tab.expected)[irow]), sep="")
    }
    # if we have row-percentage, add percentage value to table cell
    if (showRowPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_rw\">%s%s</span>", hundret, percSign), sep="")
    }
    # if we have col-percentage, add percentage value to table cell
    if (showColPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_cl\">%s%s</span>", rowSums(tab.cell)[irow], percSign), sep="")
    }
    # if we have cell-percentage, add percentage value to table cell
    if (showCellPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_c\">%s%s</span>", rowSums(tab.cell)[irow], percSign), sep="")
    }
    # write table cell data
    page.content <- paste(page.content, sprintf("\n    <td class=\"centeralign totcol horline\">%s</td>", cellstring), sep="")
    # close table row
    page.content <- paste(page.content, "\n  </tr>\n")
  }
  # ------------------------------
  # start new table row
  # this row contains the total row with sums for all columns
  # ------------------------------
  page.content <- paste(page.content, "\n  <tr class=\"lasttablerow\">\n    ", sep="")
  # check whether we have group-var, and if not, apply colspan
  if (!is.null(var.grp)) {
    page.content <- paste(page.content, sprintf("<td class=\"leftalign tothi\" colspan=\"2\">%s</td>", stringTotal), sep="")
  }
  else {
    page.content <- paste(page.content, sprintf("<td class=\"leftalign tothi\">%s</td>", stringTotal), sep="")
  }
  # --------------------------
  # iterate all data columns
  # --------------------------
  for (icol in 1:ncol(tab)) {
    # -------------------------------------
    # add total row, first table cell data contains observed values
    # -------------------------------------
    cellstring <- sprintf("<span class=\"td_n\">%i</span>", colSums(tab)[icol])
    # calculate total percentage value
    cellpercval <- round(100*colSums(tab)[icol]/sum(tab),digits)
    # if we have expected values, add them to table cell
    if (showExpected) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_ex\">%s</span>", colSums(tab.expected)[icol]), sep="")
    }
    # if we have row-percentage, add percentage value to table cell
    if (showRowPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_rw\">%s%s</span>", cellpercval, percSign), sep="")
    }
    # if we have col-percentage, add percentage value to table cell
    if (showColPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_cl\">%s%s</span>", hundret, percSign), sep="")
    }
    # if we have cell-percentage, add percentage value to table cell
    if (showCellPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_c\">%s%s</span>", cellpercval, percSign), sep="")
    }
    page.content <- paste(page.content, sprintf("\n    <td class=\"centeralign\">%s</td>", cellstring), sep="")
  }
  # --------------------------
  # the lower right table cell contains the complete
  # total values, i.e. all percentages are 100%
  # --------------------------
  cellstring <- sprintf("%s", sum(tab))
  if (showExpected) cellstring <- paste(cellstring, sprintf("<br>%s", sum(tab.expected)), sep="")
  if (showColPerc) cellstring <- paste(cellstring, sprintf("<br>%s%s", hundret, percSign), sep="")
  if (showRowPerc) cellstring <- paste(cellstring, sprintf("<br>%s%s", hundret, percSign), sep="")
  if (showCellPerc) cellstring <- paste(cellstring, sprintf("<br>%s%s", hundret, percSign), sep="")
  # write table cell data
  page.content <- paste(page.content, sprintf("\n    <td class=\"centeralign\">%s</td>", cellstring), sep="")
  # close table row
  page.content <- paste(page.content, "\n  </tr>\n")
  # -------------------------------------
  # table summary
  # -------------------------------------
  if (showSummary) {
    # -----------------------------------------------------------
    # Retrieve Phi coefficient for table
    # -----------------------------------------------------------
    getPhiValue <- function(x) {
      tab <- summary(loglm(~1+2, x))$tests
      phi <- sqrt(tab[2,1]/sum(x))
      return (phi)
    }
    # -----------------------------------------------------------
    # Retrieve Cramer's V coefficient for table
    # -----------------------------------------------------------
    getCramerValue <- function(x) {
      phi <- getPhiValue(x)
      cramer <- sqrt(phi^2/min(dim(x)-1))
      return (cramer)
    }
    # start new table row
    page.content <- paste(page.content, "\n  <tr class=\"summary\">\n    ", sep="")
    # calculate chi square value
    chsq <- chisq.test(tab)
    # check whether variables are dichotome or if they have more
    # than two categories. if they have more, use Cramer's V to calculate
    # the contingency coefficient
    if (nrow(tab)>2 || ncol(tab)>2) {
      kook <- sprintf("&Phi;<sub>c</sub>=%.3f", getCramerValue(tab))
    }
    else {
      kook <- sprintf("&Phi;=%.3f", getPhiValue(tab))
    }
    # create summary row
    page.content <- paste(page.content, sprintf("\n    <td colspan=\"%i\">&Chi;<sup>2</sup>=%.3f &middot; df=%i &middot; %s &middot; p=%.3f</td>", totalncol, chsq$statistic, chsq$parameter, kook, chsq$p.value), sep="")
    # close table row
    page.content <- paste(page.content, "\n  </tr>\n")
  }  
  # -------------------------------------
  # finish table
  # -------------------------------------
  page.content <- paste(page.content, "\n</table>")
  # -------------------------------------
  # add table to return value list, so user can access each
  # single frequency table
  # -------------------------------------
  toWrite <- paste(toWrite, page.content, "\n")
  # -------------------------------------
  # print legend
  # -------------------------------------
  toWrite <- paste(toWrite, sprintf("<p class=\"abstand\"><span class=\"td_n\">observed values</span> &middot; <span class=\"td_ex\">expected values</span> &middot; <span class=\"td_rw\">%% within %s</span> &middot; <span class=\"td_cl\">%% within %s</span> &middot; <span class=\"td_c\">%% of total</span></p>", s.var.row, s.var.col), "\n")
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
  invisible (structure(class = "sjtxtab",
                       list(page.style = page.style,
                            page.content = page.content)))
}
