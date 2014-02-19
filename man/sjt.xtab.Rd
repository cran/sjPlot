\name{sjt.xtab}
\alias{sjt.xtab}
\title{Save cross tables as HTML table}
\usage{
sjt.xtab(var.row, var.col, var.grp = NULL, weightBy = NULL, digits = 1,
  file = NULL, variableLabels = NULL, valueLabels = NULL,
  stringTotal = "Total", showCellPerc = TRUE, showRowPerc = FALSE,
  showColPerc = FALSE, showExpected = FALSE, showHorizontalLine = TRUE,
  showSummary = TRUE, tdcol.n = "black", tdcol.expected = "#339999",
  tdcol.cell = "#993333", tdcol.row = "#333399", tdcol.col = "#339933",
  highlightTotal = FALSE, highlightColor = "#f8f8f8",
  percSign = "&nbsp;\%", hundret = "100.0", encoding = "UTF-8")
}
\arguments{
  \item{var.row}{Variable that should be displayed in the
  table rows.}

  \item{var.col}{Variable that should be displayed in the
  table columns.}

  \item{var.grp}{An optional grouping variable that splits
  the data into several groups, depending on the amount of
  categories. See examples for details.}

  \item{weightBy}{A weight factor that will be applied to
  weight all cases.  Default is \code{NULL}, so no weights
  are used.}

  \item{digits}{The amount of digits used for the percenage
  values inside table cells.  Default is 1.}

  \item{file}{The destination file, which will be in
  html-format. If no filepath is specified, the file will
  be saved as temporary file and openend either in the
  RStudio View pane or in the default web browser.}

  \item{variableLabels}{A character vector of same length
  as supplied variables, with the associated variable
  names. Following order is needed: name of \code{var.row},
  name of \code{var.col}, and - if \code{var.grp} is not
  \code{NULL} - name of \code{var.grp}.  See examples for
  more details.}

  \item{valueLabels}{A list of character vectors that
  indicate the value labels of the supplied variables.
  Following order is needed: value labels of
  \code{var.row}, value labels of \code{var.col}, and - if
  \code{var.grp} is not \code{NULL} - value labels of
  \code{var.grp}. \code{valueLabels} needs to be a
  \link{list} object.  See examples for more details.}

  \item{stringTotal}{String label for the total column /
  row header.}

  \item{showCellPerc}{If \code{TRUE}, cell percentage
  values are shown.}

  \item{showRowPerc}{If \code{TRUE}, row percentage values
  are shown.}

  \item{showColPerc}{If \code{TRUE}, column percentage
  values are shown.}

  \item{showExpected}{If \code{TRUE}, expected values are
  also shown.}

  \item{showHorizontalLine}{If \code{TRUE}, data rows are
  separated with a horizontal line.}

  \item{showSummary}{If \code{TRUE} (default), a summary
  row with chi-square statistics, Cramer's V or Phi-value
  etc. is shown.}

  \item{tdcol.n}{Color for highlighting count (observed)
  values in table cells. Default is black.}

  \item{tdcol.expected}{Color for highlighting expected
  values in table cells. Default is cyan.}

  \item{tdcol.cell}{Color for highlighting cell percentage
  values in table cells. Default is red.}

  \item{tdcol.row}{Color for highlighting row percentage
  values in table cells. Default is blue.}

  \item{tdcol.col}{Color for highlighting column percentage
  values in table cells. Default is green.}

  \item{highlightTotal}{If \code{TRUE}, the total column
  and row will be highlighted with a different background
  color. See \code{highlightColor}.}

  \item{highlightColor}{If \code{highlightTotal} is
  \code{TRUE}, this color value will be used for painting
  the background of the total column and row. Default is a
  light grey.}

  \item{percSign}{The percentage sign that is printed in
  the table cells, in HTML-format.  Default is
  \code{"&nbsp;\%"}, hence the percentage sign has a
  non-breaking-space after the percentage value.}

  \item{hundret}{Default value that indicates the
  100-percent column-sums (since rounding values may lead
  to non-exact results). Default is \code{"100.0"}.}

  \item{encoding}{The charset encoding used for variable
  and value labels. Default is \code{"UTF-8"}. Change
  encoding if specific chars are not properly displayed
  (e.g.) German umlauts).}
}
\value{
Invisibly returns a \link{structure} with the web page
style sheet (\code{page.style}) and the web page content
(\code{page.content}) for further use.
}
\description{
Save cross tables as HTML file.
}
\note{
The HTML tables can either be saved as file and manually
opened (specify parameter \code{file}) or they can be saved
as temporary files and will be displayed in the RStudio
Viewer pane (if working with RStudio) or opened with the
default web browser. Displaying resp. opening a temporary
file is the default behaviour (i.e. \code{file=NULL}).
}
\examples{
# prepare sample data set
data(efc)
efc.labels <- sji.getValueLabels(efc)

# print simple cross table w/o labels
\dontrun{
sjt.xtab(efc$e16sex, efc$e42dep)}

# print cross table with labels and expected values
\dontrun{
sjt.xtab(efc$e16sex, efc$e42dep,
         variableLabels=c("Elder's gender", "Elder's dependency"),
         valueLabels=list(efc.labels[['e16sex']], efc.labels[['e42dep']]),
         showExpected=TRUE)}

# print minimal cross table with labels, total col/row highlighted
\dontrun{
sjt.xtab(efc$e16sex, efc$e42dep,
         variableLabels=c("Elder's gender", "Elder's dependency"),
         valueLabels=list(efc.labels[['e16sex']], efc.labels[['e42dep']]),
         showHorizontalLine=FALSE,
         showCellPerc=FALSE,
         highlightTotal=TRUE)}

# print cross table with labels and all percentages
\dontrun{
sjt.xtab(efc$e16sex, efc$e42dep,
         variableLabels=c("Elder's gender", "Elder's dependency"),
         valueLabels=list(efc.labels[['e16sex']], efc.labels[['e42dep']]),
         showRowPerc=TRUE, showColPerc=TRUE)}

# print cross table with labels and all percentages, including
# grouping variable
\dontrun{
sjt.xtab(efc$e16sex, efc$e42dep, efc$c161sex,
         variableLabels=c("Elder's gender",
                          "Elder's dependency",
                          "Carer's gender"),
         valueLabels=list(efc.labels[['e16sex']],
                          efc.labels[['e42dep']],
                          efc.labels[['c161sex']]),
         showRowPerc=TRUE, showColPerc=TRUE)}
}
\seealso{
\link{sjp.xtab}
}

