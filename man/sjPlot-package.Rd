\encoding{UTF-8} 
\name{sjPlot-package}
\alias{sjPlot-package}
\alias{sjPlot}
\docType{package}
\title{
sjPlot - data visualization for statistics in social science
}
\description{
  Collection of several plotting functions for data visualization using ggplot. Results of several statistical analyses (that are commonly used in social sciences) can be visualized using this package, including simple and cross tabulated frequencies, histograms, box plots, (generalized) linear models (forest plots), PCA, correlations, cluster analyses, scatter plots etc.
  
  Furthermore, this package contains some tools that are useful when carrying out data
  analysis or interpreting data, for instance data set import, variable recoding,
  determination of cluster groups, interpretation of interactions in linear models etc.
  
  \emph{What does this package do?}
  
  In short, the functions in this package mostly do two things:
  \enumerate{
    \item compute basic or advanced statistical analyses
    \item plot the results as ggplot-diagram
  }
  
  However, meanwhile the amount of functions has increased, hence you’ll also find some utility functions beside the plotting functions.
  
  \emph{How does this package help me?}
  
  Basically, this package either helps those users...
  \itemize{
    \item who have difficulties using and/or understanding all possibilities that ggplot offers to create plots, simply by providing intuitive function parameters, which allow for manipulating the appearance of plots; or
    \item who don’t want to set up complex ggplot-object each time from the scratch.
  }
  
  Furthermore, for advanced ggplot-users, the functions can return the prepared ggplot-object, which than can be manipulated even further (for instance, if you wish to specify certain parameters that cannot be modified via the sjPlot package).
  
  The functions follow specific naming conventions:
  \itemize{
    \item \code{sjc} - cluster analysis functions
    \item \code{sji} - data import functions
    \item \code{sjp} - plotting functions
    \item \code{sjt} - (HTML) table output functions
    \item \code{sju} - statistical utility and recode functions
  }
}
\details{
\tabular{ll}{
Package: \tab sjPlot\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2014-02-19\cr
License: \tab GPL-3\cr
}
}
\author{
Daniel Lüdecke \email{d.luedecke@uke.de}
}
\references{
\url{http://strengejacke.wordpress.com/sjplot-r-package/}
}
