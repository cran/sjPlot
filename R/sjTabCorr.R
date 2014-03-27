#' @title Show correlations as HTML table
#' @name sjt.corr
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2014/03/04/beautiful-table-outputs-in-r-part-2-rstats-sjplot/}
#'              }
#' 
#' @description Shows the results of a computed correlation as HTML table. Requires either 
#'                a data frame or a computed \code{\link{cor}}-object.
#'                
#' @seealso \code{\link{sjp.corr}}
#' 
#' @param data A correlation object, built with the R-\code{\link{cor}}-function, or a data frame
#'          which correlations should be calculated.
#' @param missingDeletion Indicates how missing values are treated. May be either
#'          \code{"listwise"} or \code{"pairwise"} (default).
#' @param corMethod Indicates the correlation computation method. May be one of
#'          \code{"spearman"} (default), \code{"pearson"} or \code{"kendall"}.
#' @param showPValues Whether significance levels (p-values) of correlations should 
#'          be printed or not.
#' @param pvaluesAsNumbers If \code{TRUE}, the significance levels (p-values) are printed as numbers.
#'          if \code{FALSE} (default), asterisks are used.
#' @param fadeNS If \code{TRUE} (default), non-significant correlation-values appear faded (by using
#'          a lighter grey text color).
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
#' @param varlabels The item labels that are printed along the first column/row. If no item labels are
#'          provided (default), the data frame's column names are used. Item labels must
#'          be a string vector, e.g.: \code{varlabels=c("Var 1", "Var 2", "Var 3")}.
#'          varlabels are detected automatically if \code{data} is a data frame where each variable has
#'          a \code{"variable.label"} attribute (see \code{\link{sji.setVariableLabels}}) for details).
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the variable labels are displayed in 
#'          one line and when a line break is inserted. Default is 40.
#' @param digits The amount of digits used the values inside table cells.
#'          Default is 2.
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
#'            \item the web page content (\code{page.content}),
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
#' # create data frame with 5 random variables
#' df <- as.data.frame(cbind(rnorm(10), rnorm(10), rnorm(10), rnorm(10), rnorm(10)))
#' 
#' # plot correlation matrix using circles
#' \dontrun{
#' sjt.corr(df)}
#' 
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' data(efc)
#' 
#' # retrieve variable and value labels
#' varlabs <- sji.getVariableLabels(efc)
#' 
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc)=="c83cop2")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc)=="c88cop7")
#'  
#' # create data frame with COPE-index scale
#' df <- as.data.frame(efc[,c(start:end)])
#' colnames(df) <- varlabs[c(start:end)]
#'
#' # we have high correlations here, because all items
#' # belong to one factor. See example from "sjp.pca". 
#' \dontrun{
#' sjt.corr(df, pvaluesAsNumbers=TRUE)}
#' 
#' # -------------------------------
#' # auto-detection of labels
#' # -------------------------------
#' efc <- sji.setVariableLabels(efc, varlabs)
#' \dontrun{
#' sjt.corr(efc[,c(start:end)])}
#' 
#' @export
sjt.corr <- function (data,
                      missingDeletion="pairwise",
                      corMethod="spearman",
                      showPValues=TRUE,
                      pvaluesAsNumbers=FALSE,
                      fadeNS=TRUE,
                      file=NULL, 
                      varlabels=NULL,
                      breakLabelsAt=40,
                      digits=3,
                      encoding="UTF-8",
                      CSS=NULL,
                      useViewer=TRUE,
                      no.output=FALSE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(varlabels) && is.data.frame(data)) {
    varlabels <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(data)) {
      # retrieve variable name attribute
      vn <- autoSetVariableLabels(data[,i])
      # if variable has attribute, add to variableLabel list
      if (!is.null(vn)) {
        varlabels <- c(varlabels, vn)
      }
      else {
        # else break out of loop
        varlabels <- NULL
        break
      }
    }
  }
  # ----------------------------
  # check for valid parameter
  # ----------------------------
  if (corMethod!="pearson" && corMethod!="spearman" && corMethod!= "kendall") {
    stop("Parameter 'corMethod' must be one of: pearson, spearman or kendall")
  }
  # ----------------------------
  # check if user has passed a data frame
  # or a pca object
  # ----------------------------
  if (class(data)=="matrix") {
    corr <- data
    cpvalues <- NULL
  }
  else {
    # missing deletion corresponds to
    # SPSS listwise
    if (missingDeletion=="listwise") {
      data <- na.omit(data)
      corr <- cor(data, method=corMethod)
    }
    # missing deletion corresponds to
    # SPSS pairwise
    else {
      corr <- cor(data, method=corMethod, use="pairwise.complete.obs")
    }
    #---------------------------------------
    # if we have a data frame as parameter,
    # compute p-values of significances
    #---------------------------------------
    computePValues <- function(df) {
      cp <- c()
      for (i in 1:ncol(df)) {
        pv <- c()
        for (j in 1:ncol(df)) {
          test <- cor.test(df[,i], df[,j], alternative="two.sided", method=corMethod)
          pv <- cbind(pv, round(test$p.value,5))
        }
        cp <- rbind(cp, pv)
      }
      return (cp)
    }
    cpvalues <- computePValues(data)
  }
  # --------------------------------------------------------
  # save original p-values
  # --------------------------------------------------------
  cpv <- cpvalues
  # --------------------------------------------------------
  # add column with significance value
  # --------------------------------------------------------
  if (!is.null(cpvalues)) {
    if (!pvaluesAsNumbers) {
      # --------------------------------------------------------
      # prepare function for apply-function. replace sig. p
      # with asterisks
      # --------------------------------------------------------
      fun.star <- function(x) {
        if (x>=0.05) x=""
        else if (x>=0.01 && x<0.05) x="*"
        else if (x>=0.001 && x<0.01) x="**"
        else if (x<0.001) x="***"
      }
    }
    else {
      # --------------------------------------------------------
      # prepare function for apply-function.
      # round p-values, keeping the numeric values
      # --------------------------------------------------------
      fun.star <- function(x) {
        round(x,digits)
      }
    }
    cpvalues <- apply(cpvalues, c(1,2), fun.star)
  }
  else {
    showPValues <- FALSE
  }
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(varlabels)) {
    varlabels <- row.names(corr)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  varlabels <- sju.wordwrap(varlabels, breakLabelsAt, "<br>")
  # -------------------------------------
  # init header
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
  tag.notsig <- "notsig"
  tag.pval <- "pval"
  tag.summary <- "summary"
  tag.centeralign <- "centeralign"
  tag.firsttablecol <- "firsttablecol"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "border-top:double black; padding:0.2cm;"
  css.tdata <- "padding:0.2cm;"
  css.centeralign <- "text-align:center;"
  css.firsttablecol <- "font-weight:bold;"
  css.notsig <- "color:#999999;"
  css.summary <- "border-bottom:double black; border-top:1px solid black; font-style:italic; font-size:0.9em; text-align:right;"
  css.pval <- "vertical-align:super;font-size:0.8em;"
  if (pvaluesAsNumbers) css.pval <- "font-style:italic;"
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- CSS[['css.table']]
    if (!is.null(CSS[['css.thead']])) css.thead <- CSS[['css.thead']]
    if (!is.null(CSS[['css.tdata']])) css.tdata <- CSS[['css.tdata']]
    if (!is.null(CSS[['css.summary']])) css.summary <- CSS[['css.summary']]
    if (!is.null(CSS[['css.notsig']])) css.notsig <- CSS[['css.notsig']]
    if (!is.null(CSS[['css.pval']])) css.pval <- CSS[['css.pval']]
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- CSS[['css.centeralign']]
    if (!is.null(CSS[['css.firsttablecol']])) css.firsttablecol <- CSS[['css.firsttablecol']]
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                         tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata,
                         tag.firsttablecol, css.firsttablecol, 
                         tag.centeralign, css.centeralign,
                         tag.notsig, css.notsig,
                         tag.pval, css.pval,
                         tag.summary, css.summary)
  # ------------------------
  # start content
  # ------------------------
  toWrite <- paste0(toWrite, page.style)
  toWrite = paste(toWrite, "\n</head>\n<body>", "\n")
  # -------------------------------------
  # start table tag
  # -------------------------------------
  page.content <- "<table>\n"
  # -------------------------------------
  # header row
  # -------------------------------------
  # write tr-tag
  page.content <- paste0(page.content, "  <tr>\n")
  # first column
  page.content <- paste0(page.content, "    <th class=\"thead\">&nbsp;</th>\n")
  # iterate columns
  for (i in 1:ncol(corr)) {
    page.content <- paste0(page.content, sprintf("    <th class=\"thead\">%s</th>\n", varlabels[i]))
  }
  # close table row
  page.content <- paste0(page.content, "  </tr>\n")
  # -------------------------------------
  # data rows
  # -------------------------------------
  # iterate all rows of df
  for (i in 1:nrow(corr)) {
    # write tr-tag
    page.content <- paste0(page.content, "  <tr>\n")
    # print first table cell
    page.content <- paste0(page.content, sprintf("    <td class=\"firsttablecol\">%s</td>\n", varlabels[i]))
    # --------------------------------------------------------
    # iterate all columns
    # --------------------------------------------------------
    for (j in 1:ncol(corr)) {
      # --------------------------------------------------------
      # leave out self-correlations
      # --------------------------------------------------------
      if (j==i) {
        page.content <- paste0(page.content, "    <td class=\"tdata\">&nbsp;</td>\n")
      }
      else {
        # --------------------------------------------------------
        # print table-cell-data (cor-value)
        # --------------------------------------------------------
        cellval <- round(corr[i,j],digits)
        # --------------------------------------------------------
        # check whether we want to show P-Values
        # --------------------------------------------------------
        if (showPValues) {
          if (pvaluesAsNumbers) {
            # --------------------------------------------------------
            # if we have p-values as number, print them in new row
            # --------------------------------------------------------
            cellval <- sprintf("%s<br><span class=\"pval\">(%.*f)</span>", cellval, digits, cpvalues[i,j])
          }
          else {
            # --------------------------------------------------------
            # if we have p-values as "*", add them
            # --------------------------------------------------------
            cellval <- sprintf("%s<span class=\"pval\">%s</span>", cellval, cpvalues[i,j])
          }
        }
        # --------------------------------------------------------
        # prepare css for not significant values
        # --------------------------------------------------------
        notsig <- ""
        # --------------------------------------------------------
        # check whether not significant values should be blurred
        # --------------------------------------------------------
        if (fadeNS) {
          # set css-class-attribute
          if (cpv[i,j] >=0.05) notsig <- " notsig"
        }
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign%s\">%s</td>\n", notsig, cellval))
      }
    }
    # close row
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # feedback...
  # -------------------------------------
  page.content <- paste0(page.content, "  <tr>\n")
  page.content <- paste0(page.content, sprintf("    <td colspan=\"%i\" class=\"summary\">", ncol(corr)+1))
  page.content <- paste0(page.content, sprintf("Computed correlation used %s-method with %s-deletion.", corMethod, missingDeletion))
  page.content <- paste0(page.content, "</td>\n  </tr>\n")
  # -------------------------------------
  # finish table
  # -------------------------------------
  page.content <- paste(page.content, "\n</table>")
  # -------------------------------------
  # finish html page
  # -------------------------------------
  toWrite <- paste(toWrite, page.content, "\n")
  toWrite <- paste0(toWrite, "</body></html>")
  # -------------------------------------
  # replace class attributes with inline style,
  # useful for knitr
  # -------------------------------------
  # copy page content
  # -------------------------------------
  knitr <- page.content
  # -------------------------------------
  # set style attributes for main table tags
  # -------------------------------------
  knitr <- gsub("class=", "style=", knitr)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), knitr)
  # -------------------------------------
  # replace class-attributes with inline-style-definitions
  # -------------------------------------
  knitr <- gsub(tag.tdata, css.tdata, knitr)
  knitr <- gsub(tag.thead, css.thead, knitr)
  knitr <- gsub(tag.centeralign, css.centeralign, knitr)
  knitr <- gsub(tag.notsig, css.notsig, knitr)  
  knitr <- gsub(tag.pval, css.pval, knitr)  
  knitr <- gsub(tag.summary, css.summary, knitr)  
  knitr <- gsub(tag.firsttablecol, css.firsttablecol, knitr)  
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
  invisible (structure(class = "sjtcorr",
                       list(page.style = page.style,
                            page.content = page.content,
                            output.complete = toWrite,
                            knitr = knitr)))
}
