\name{sjt.frq}
\alias{sjt.frq}
\title{Save frequencies as HTML table}
\usage{
sjt.frq(data, file = NULL, weightBy = NULL, variableLabels = NULL,
  valueLabels = NULL, autoGroupAt = 50, stringCount = "N",
  stringPerc = "raw \%", stringValidPerc = "valid \%",
  stringCumPerc = "cumulative \%", stringMissingValue = "missings",
  highlightMedian = FALSE, highlightQuartiles = FALSE,
  skipZeroRows = FALSE, showSummary = TRUE)
}
\arguments{
  \item{data}{The variables which frequencies should be
  printed as table. Either use a single variable (vector)
  or a data frame where each column represents a variable
  (see examples.}

  \item{file}{The destination file, which will be in
  html-format. If no filepath is specified, the file will
  be saved as temporary file and openend either in the
  RStudio View pane or in the default web browser.}

  \item{weightBy}{A weight factor that will be applied to
  weight all cases from \code{data}.  default is
  \code{NULL}, so no weights are used.}

  \item{variableLabels}{A single character vector or a list
  of character vectors that indicate the variable names of
  those variables from \code{data}. Note that if multiple
  variables are supplied (as data frame), the variable
  labels must be supplied as \code{list} object (see
  examples).}

  \item{valueLabels}{A list of character vectors that
  indicate the value labels of those variables from
  \code{data}. Note that if multiple variables are supplied
  (as data frame), the value labels must be supplied as
  nested \code{list} object (see examples).}

  \item{autoGroupAt}{A value indicating at which length of
  unique values a variable from \code{data} is
  automatically grouped into smaller units (see
  \link{sju.groupVar}). Variables with large numbers of
  unique values may be too time consuming when a HTML table
  is created and R would not respond any longer. Hence it's
  recommended to group such variables. Default value is 50,
  i.e. variables with 50 and more unique values will be
  grouped using \link{sju.groupVar} with
  \code{groupsize="auto"} parameter. By default, the
  maximum group count is 30. However, if \code{autoGroupAt}
  is less than 30, \code{autoGroupAt} groups are built.}

  \item{stringCount}{String label for the first table
  column containing the counts. Default is \code{"N"}.}

  \item{stringPerc}{String label for the second table
  column containing the percentages, where the count
  percentages include missing values.}

  \item{stringValidPerc}{String label for the third table
  column containing the valid percentages, i.e. the count
  percentage value exluding possible missing values.}

  \item{stringCumPerc}{String label for the last table
  column containing the cumulative percentages.}

  \item{stringMissingValue}{String label for the last table
  data row containing missing values.}

  \item{highlightMedian}{If \code{TRUE}, the table row
  indicating the median value will be highlighted.}

  \item{highlightQuartiles}{If \code{TRUE}, the table row
  indicating the lower and upper quartiles will be
  highlighted.}

  \item{skipZeroRows}{If \code{TRUE}, rows with only
  zero-values are not printed. Default is \code{FALSE}.}

  \item{showSummary}{If \code{TRUE} (default), a summary
  row with total and valid N as well as mean and standard
  deviation is shown.}
}
\description{
Save (multiple) frequency tables as HTML file.
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
# load sample data
data(efc)

# retrieve value and variable labels
variables <- sji.getVariableLabels(efc)
values <- sji.getValueLabels(efc)

# show frequencies of "e42dep" in RStudio Viewer Pane
# or default web browser
## Note that example may open browser, thus it is outcommented
# sjt.frq(efc$e42dep)

# plot and save frequency table of "e42dep" with labels
sjt.frq(efc$e42dep,
        file="dependency_labels.html",
        variableLabels=variables['e42dep'],
        valueLabels=values[['e42dep']])

# plot frequencies of e42dep, e16sex and c172code in one HTML file
# and show table in RStudio Viewer Pane or default web browser
## Note that example may open browser, thus it is outcommented
# sjt.frq(as.data.frame(cbind(efc$e42dep, efc$e16sex, efc$c172code)),
#         variableLabels=list(variables['e42dep'], variables['e16sex'], variables['c172code']),
#         valueLabels=list(values[['e42dep']], values[['e16sex']], values[['c172code']]))

# plot larger scale including zero-counts
# and save to file, indicating median and quartiles
sjt.frq(efc$neg_c_7,
        file="negativeimpact.html",
        variableLabels=variables['neg_c_7'],
        valueLabels=values[['neg_c_7']],
        highlightMedian=TRUE,
        highlightQuartiles=TRUE)
}
\seealso{
\link{sjp.frq}
}

