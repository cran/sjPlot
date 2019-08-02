params <-
list(EVAL = TRUE)

## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, 
  comment = "#>", 
  message = FALSE,
  eval = TRUE
  # eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)

## ---- results='hide', message=FALSE, warning=FALSE-----------------------
# load required packages
library(sjPlot)
library(insight)
library(httr)
library(brms)

# load sample models

# zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")
# set.seed(123)
# m1 <- brm(bf(
#     count ~ persons + child + camper + (1 | persons),
#     zi ~ child + camper + (1 | persons)
#   ),
#   data = zinb,
#   family = zero_inflated_poisson()
# )
m1 <- insight::download_model("brms_zi_2")

# data(epilepsy)
# set.seed(123)
# epilepsy$visit <- as.numeric(epilepsy$visit)
# epilepsy$Base2 <- sample(epilepsy$Base, nrow(epilepsy), replace = TRUE)
# f1 <- bf(Base ~ zAge + count + (1 |ID| patient))
# f2 <- bf(Base2 ~ zAge + Trt + (1 |ID| patient))
# m2 <- brm(f1 + f2 + set_rescor(FALSE), data = epilepsy)
m2 <- insight::download_model("brms_mv_3")

## ------------------------------------------------------------------------
tab_model(m1)

## ------------------------------------------------------------------------
tab_model(m2)

## ------------------------------------------------------------------------
tab_model(m2, show.ci50 = TRUE)

## ------------------------------------------------------------------------
tab_model(m1, m2)

