\name{sji.viewSPSS}
\alias{sji.viewSPSS}
\title{View SPSS data set structure}
\usage{
sji.viewSPSS(df, file = NULL, alternateRowColors = TRUE, showType = FALSE,
  showValues = TRUE, orderByName = FALSE, breakVariableNamesAt = 50,
  encoding = "UTF-8", CSS = NULL, useViewer = TRUE, no.output = FALSE)
}
\arguments{
  \item{df}{An imported data frame, imported by
  \code{\link{sji.SPSS}} function.}

  \item{file}{The destination file, which will be in
  html-format. If no filepath is specified, the file will
  be saved as temporary file and openend either in the
  RStudio View pane or in the default web browser.}

  \item{alternateRowColors}{If \code{TRUE}, alternating
  rows are highlighted with a light gray background color.}

  \item{showType}{If \code{TRUE}, the variable type is
  shown in a separate column. Since SPSS variable types are
  mostly \code{\link{numeric}} after import, this column is
  hidden by default.}

  \item{showValues}{If \code{TRUE} (default), the variable
  values and their associated value labels are shown as
  additional column.}

  \item{orderByName}{If \code{TRUE}, rows are ordered
  according to the variable names. By default, rows
  (variables) are ordered according to their order in the
  data frame.}

  \item{breakVariableNamesAt}{Wordwrap for lomg variable
  names. Determines how many chars of a variable name are
  displayed in one line and when a line break is inserted.
  Default value is 50, use \code{NULL} to turn off word
  wrap.}

  \item{encoding}{The charset encoding used for variable
  and value labels. Default is \code{"UTF-8"}. Change
  encoding if specific chars are not properly displayed
  (e.g.) German umlauts).}

  \item{CSS}{A \code{\link{list}} with user-defined
  style-sheet-definitions, according to the official CSS
  syntax (see \url{http://www.w3.org/Style/CSS/}). See
  return value \code{page.style} for details of all
  style-sheet-classnames that are used in this function.
  Parameters for this list need: \enumerate{ \item the
  class-names with \code{"css."}-prefix as parameter name
  and \item each style-definition must end with a semicolon
  } Examples: \itemize{ \item \code{css.table='border:2px
  solid red;'} for a solid 2-pixel table border in red.
  \item \code{css.summary='font-weight:bold;'} for a bold
  fontweight in the summary row.  \item
  \code{css.arc='color:blue;'} for a blue text color each
  2nd row.  } See further examples below.}

  \item{useViewer}{If \code{TRUE}, the function tries to
  show the HTML table in the IDE's viewer pane. If
  \code{FALSE} or no viewer available, the HTML table is
  opened in a web browser.}

  \item{no.output}{If \code{TRUE}, the html-output is
  neither opened in a browser nor shown in the viewer pane
  and not even saved to file. This option is useful when
  the html output should be used in \code{knitr} documents.
  The html output can be accessed via the return value.}
}
\value{
Invisibly returns a \code{\link{structure}} with \itemize{
\item the web page style sheet (\code{page.style}), \item
the web page content (\code{page.content}), \item the
complete html-output (\code{output.complete}) and \item the
html-table with inline-css for use with knitr
(\code{knitr}) } for further use.
}
\description{
Save (or show) content of an imported SPSS data file as
HTML table.  Similar to the SPSS variable view. This quick
overview shows variable ID numner, name, label, type and
associated value labels. The result can be considered as
"codeplan" of the data frame.
}
\examples{
# init dataset
data(efc)

# view variables
\dontrun{
sji.viewSPSS(efc)}

# view variables w/o values and value labels
\dontrun{
sji.viewSPSS(efc, showValues=FALSE)}

# view variables including variable typed, orderd by name
\dontrun{
sji.viewSPSS(efc, orderByName=TRUE, showType=TRUE)}

# ----------------------------------------------------------------
# User defined style sheet
# ----------------------------------------------------------------
\dontrun{
sji.viewSPSS(efc,
             CSS=list(css.table="border: 2px solid;",
                      css.tdata="border: 1px solid;",
                      css.arc="color:blue;"))}
}
\seealso{
\code{\link{sji.SPSS}} \cr \code{\link{sjt.df}}
}

