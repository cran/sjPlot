# sjPlot 2.1.1

## General

* Some fixes needed to be compatible with the latest ggplot2-update.

## New functions

* `sjplot`, a pipe-friendly wrapper for some of this package's plotting-functions.
* `sjtab`, a pipe-friendly wrapper for some of this package's table-functions.


# sjPlot 2.1.0

## New functions

* `sjp.resid`, an experimental function to plot and analyze residuals from linear models.
* `plot_grid` to plot a list of ggplot-objects as arranged grid in a single plot.
* `set_theme` to use a preset of default themes for plots from the sjp-functions.

## Changes to functions

* For `sjp.glmer` and `sjp.lmer`, argument `show.ci` now also applies for plotting random effects (`type = "re"`, the default), so confidence intervals may not be calculated. This may be useful in some cases where computation of standard errors for random effects caused an error.
* Effect plots (`type = "eff"`) for `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer` should now better handle categorical variables and their labels, including using error bars insted of regions for confidence intervals.
* `table(*, exclude = NULL)` was changed to `table(*, useNA = "always")`, because of planned changes in upcoming R version 3.4.
* `get_option("p_zero")` was removed, and `sjt.lm`, `sjt.glm`, `sjt.lmer` and `sjt.glmer` get a `p.zero` argument.
* `sjp.setTheme` no longer sets default theme presets for plots; use `set_theme` instead.

## Bug fixes

* A bug introduced in update 2.0.2 caused an error in `sjp.lm` for `type = "std"`.
* Effect plots (`type = "eff"`) for `sjp.lm`, `sjp.glm`, `sjp.lmer` and `sjp.glmer` did not plot all predictors, when predictor name was not exactly specified in formula, but transformed inside formula (e.g. `log(pred + 1)`).
