\name{sjt.corr}
\alias{sjt.corr}
\title{Show correlations as HTML table}
\usage{
sjt.corr(data, missingDeletion = "pairwise", corMethod = "spearman",
  showPValues = TRUE, pvaluesAsNumbers = FALSE, fadeNS = TRUE,
  file = NULL, varlabels = NULL, breakLabelsAt = 40, digits = 3,
  encoding = "UTF-8", CSS = NULL, useViewer = TRUE, no.output = FALSE)
}
\arguments{
  \item{data}{A correlation object, built with the
  R-\code{\link{cor}}-function, or a data frame which
  correlations should be calculated.}

  \item{missingDeletion}{Indicates how missing values are
  treated. May be either \code{"listwise"} or
  \code{"pairwise"} (default).}

  \item{corMethod}{Indicates the correlation computation
  method. May be one of \code{"spearman"} (default),
  \code{"pearson"} or \code{"kendall"}.}

  \item{showPValues}{Whether significance levels (p-values)
  of correlations should be printed or not.}

  \item{pvaluesAsNumbers}{If \code{TRUE}, the significance
  levels (p-values) are printed as numbers.  if
  \code{FALSE} (default), asterisks are used.}

  \item{fadeNS}{If \code{TRUE} (default), non-significant
  correlation-values appear faded (by using a lighter grey
  text color).}

  \item{file}{The destination file, which will be in
  html-format. If no filepath is specified, the file will
  be saved as temporary file and openend either in the
  RStudio View pane or in the default web browser.}

  \item{varlabels}{The item labels that are printed along
  the first column/row. If no item labels are provided
  (default), the data frame's column names are used. Item
  labels must be a string vector, e.g.:
  \code{varlabels=c("Var 1", "Var 2", "Var 3")}.}

  \item{breakLabelsAt}{Wordwrap for diagram labels.
  Determines how many chars of the variable labels are
  displayed in one line and when a line break is inserted.
  Default is 40.}

  \item{digits}{The amount of digits used the values inside
  table cells.  Default is 2.}

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
  \code{css.lasttablerow='border-bottom: 1px dotted blue;'}
  for a blue dotted border of the last table row.  } See
  further examples below.}

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
Shows the results of a computed correlation as HTML table.
Requires either a data frame or a computed
\code{\link{cor}}-object.
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
# create data frame with 5 random variables
df <- as.data.frame(cbind(rnorm(10), rnorm(10), rnorm(10), rnorm(10), rnorm(10)))

# plot correlation matrix using circles
\dontrun{
sjt.corr(df)}

# -------------------------------
# Data from the EUROFAMCARE sample dataset
# -------------------------------
data(efc)

# retrieve variable and value labels
varlabs <- sji.getVariableLabels(efc)

# recveive first item of COPE-index scale
start <- which(colnames(efc)=="c83cop2")
# recveive last item of COPE-index scale
end <- which(colnames(efc)=="c88cop7")

# create data frame with COPE-index scale
df <- as.data.frame(efc[,c(start:end)])
colnames(df) <- varlabs[c(start:end)]

# we have high correlations here, because all items
# belong to one factor. See example from "sjp.pca".
\dontrun{
sjt.corr(df, pvaluesAsNumbers=TRUE)}
}
\references{
\itemize{ \item
\url{http://strengejacke.wordpress.com/sjplot-r-package/} }
}
\seealso{
\code{\link{sjp.corr}}
}

