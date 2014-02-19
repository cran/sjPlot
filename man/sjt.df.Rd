\name{sjt.df}
\alias{sjt.df}
\title{Save data frame as HTML table}
\usage{
sjt.df(df, file = NULL, alternateRowColors = FALSE, orderColumn = NULL,
  orderAscending = TRUE, stringObservation = "Observation",
  encoding = "UTF-8")
}
\arguments{
  \item{df}{A data frame that should be printed.}

  \item{file}{The destination file, which will be in
  html-format. If no filepath is specified, the file will
  be saved as temporary file and openend either in the
  RStudio View pane or in the default web browser.}

  \item{alternateRowColors}{If \code{TRUE}, alternating
  rows are highlighted with a light gray background color.}

  \item{orderColumn}{Indicates a column, either by column
  name or by column index number, that should be orderd.
  Default order is ascending, which can be changed with
  \code{orderAscending} parameter. Default is \code{NULL},
  hence the data frame is printed with no specific order.
  See examples for further details.}

  \item{orderAscending}{If \code{TRUE} (default) and
  \code{orderColumn} is not \code{NULL}, data frame is
  ordered according to the specified column in an ascending
  order.  Use \code{FALSE} to apply descending order. See
  examples for further details.}

  \item{stringObservation}{A string used for the first
  column name that indicates the observation or case
  number.  Default is \code{"Observation"}.}

  \item{encoding}{The charset encoding used for variable
  and value labels. Default is \code{"UTF-8"}. Change
  encoding if specific chars are not properly displayed
  (e.g.) German umlauts).}
}
\description{
Save (or show) content of data frame (rows and columns) as
HTML table.  Helpful if you want a quick overview of a data
frame's content.
}
\examples{
# init dataset
data(efc)

# plot first 50 rows of first 5 columns of example data set
\dontrun{
sjt.df(efc[1:50,1:5])}

# plot first 20 rows of first 5 columns of example data set,
# ordered by column "e42dep" with alternating row colors
\dontrun{
sjt.df(efc[1:20,1:5], alternateRowColors=TRUE, orderColumn="e42dep")}

# plot first 20 rows of first 5 columns of example data set,
# ordered by 4th column in descending order.
\dontrun{
sjt.df(efc[1:20,1:5], orderColumn=4, orderAscending=FALSE)}
}

