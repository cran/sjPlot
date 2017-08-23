## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", warning = FALSE, message = FALSE)

## ------------------------------------------------------------------------
# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# sample data
data(efc)

## ---- results='hide'-----------------------------------------------------
# fit first model
fit1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
# fit second model 
fit2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + c172code, data = efc)
# Note that both models share the same predictors and only differ 
# in their dependent variable. See examples of stepwise models 
# later...

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2)

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2, depvar.labels = c("Barthel-Index", "Negative Impact"))

## ------------------------------------------------------------------------
sjt.lm(
  fit1,
  fit2,
  show.header = TRUE,
  string.est = "Estimate",
  string.ci = "Conf. Int.",
  string.p = "p-value",
  string.dv = "Response",
  string.pred = "Coefficients",
  string.interc = "Konstante",
  depvar.labels = c("Barthel-Index", "Negative Impact")
)

## ------------------------------------------------------------------------
sjt.lm(
  fit1,
  fit2,
  separate.ci.col = FALSE, # ci in same cell as estimates
  show.std = TRUE,         # also show standardized beta values
  p.numeric = FALSE        # "*" instead of numeric values
)       

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2, pred.labels = c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"))

## ---- results='hide'-----------------------------------------------------
# fit first model
fit1 <- lm(neg_c_7 ~ c160age + c172code + c161sex, data = efc)
# fit second model
fit2 <- lm(neg_c_7 ~ c160age + c172code + c161sex + c12hour, data = efc)
# fit second model
fit3 <- lm(neg_c_7 ~ c160age + c172code + e42dep + tot_sc_e, data = efc)

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2, fit3, 
       separate.ci.col = FALSE,
       show.aic = TRUE,
       show.fstat = TRUE)

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2, fit3, CSS = list(css.separatorcol = 'padding-right:1.5em; padding-left:1.5em;'))

## ---- results='hide'-----------------------------------------------------
# make education categorical
efc$c172code <- to_factor(efc$c172code)
# make dependency categorical
efc$e42dep <- to_factor(efc$e42dep)
# fit first model again (with c172code as factor)
fit1 <- lm(barthtot ~ c160age + c12hour + c172code + c161sex + e42dep, data = efc)
# fit second model again (with c172code as factor)
fit2 <- lm(neg_c_7 ~ c160age + c12hour + c172code + c161sex + e42dep, data = efc)

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2)

## ---- results='hide'-----------------------------------------------------
data(efc)
# make education categorical
efc$c172code <- to_factor(efc$c172code)
# make education categorical
efc$e42dep <- to_factor(efc$e42dep)
# make prettier variable labels
set_label(efc$c172code) <- "Education"
set_label(efc$e42dep) <- "Dependency"
# fit first model
fit1 <- lm(neg_c_7 ~ c160age + c172code + c161sex, data = efc)
# fit second model
fit2 <- lm(neg_c_7 ~ c160age + c172code + c161sex + c12hour, data = efc)
# fit third model
fit3 <- lm(neg_c_7 ~ c160age + c172code + e42dep + tot_sc_e, data = efc)

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2, fit3, group.pred = FALSE)

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2, fit3, group.pred = FALSE, CSS = list(css.topcontentborder = "+font-size: 0px;"))

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2, fit3, group.pred = FALSE, remove.estimates = 2)

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2, fit3, group.pred = FALSE, remove.estimates = c("c160age", "c161sex"))

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2, fit3, group.pred = FALSE, remove.estimates = c(2,5,6,10))

## ------------------------------------------------------------------------
sjt.lm(fit1, fit2, fit3,
       pred.labels = c("mid education", "hi education", "Hours of Care", 
                       "slight dependency", "moderate dependency", 
                       "severe dependency", "Service Usage"),
       remove.estimates = c("c160age", "c161sex"))

