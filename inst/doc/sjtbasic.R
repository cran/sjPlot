## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ---- message=FALSE, warning=FALSE---------------------------------------
# load packages
library(sjPlot)
library(sjmisc)
library(dplyr)
# load sample data set.
data(efc)

## ----eval=FALSE----------------------------------------------------------
#  # don't need to do this, because all sjt-functions
#  # use this code as default encoding-detection
#  if (.Platform$OS.type == "unix")
#    encoding <- "UTF-8"
#  else
#    encoding <- "Windows-1252"
#  
#  sjt.frq(efc$e15relat, encoding = encoding)

## ------------------------------------------------------------------------
sjt.frq(efc$e42dep)

## ------------------------------------------------------------------------
efc %>% 
  group_by(e16sex, c172code) %>% 
  select(e16sex, c172code, e42dep) %>% 
  sjtab()

## ----eval=TRUE, warning=FALSE--------------------------------------------
cat(sjt.frq(efc$e42dep, no.output = TRUE)$page.style)

## ----eval=TRUE, warning=FALSE--------------------------------------------
cat(sjt.frq(efc$e42dep, no.output = TRUE)$page.content.list[[1]])
# not that other sjt-functions don't return a page.content-list, but
# just '$page.content'...

## ------------------------------------------------------------------------
sjt.frq(
  efc$e42dep, 
  CSS = list(css.centeralign = 'text-align: left;', 
             css.caption = 'font-weight: normal; font-style: italic;', 
             css.firsttablecol = 'font-weight: bold;', 
             css.lasttablerow = 'border-top: 1px solid; border-bottom: none;', 
             css.summary = 'color: blue;')
)

## ------------------------------------------------------------------------
sjt.frq(efc$e42dep, CSS = list(css.summary = '+color: blue;'))

