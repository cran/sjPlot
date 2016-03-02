# sjPlot 1.9.2

## General

* Updates [package vignettes](http://strengejacke.de/sjPlot/) to work with the latest package versions.


## Changes to functions

* `sjp.lmm`  and `sjp.glmm` now also support linear mixed effects models (of class `merMod`).
* `sjp.int` now uses proper x-axis-tick-labels for `type = "eff"`, when predictor on x-axis is a factor with non-numeric factor-levels (or has label attributes).
* `sjp.glm` gets a `group.estimates` argument to group estimates in forest plots and colour them according to group assignment. Use arguments `show.legend` and `legendTitle` to modify group legend.
* `sjp.poly` now has better variable label detection for automatic axis labelling.
* `sjp.lmer` and `sjp.glmer` now support model diagnostics with `type = "ma"`.
* Better support for different model families in `sjp.glm`.
* Better axis labelling for `type = "poly"` in `sjp.lm` and `sjp.lmer` ([#110](https://github.com/sjPlot/devel/issues/110)).


## Bug fixes

* Fixed bug in `sjp.int`, where automatic y-axis-scaling for binary outcomes cut off parts of confidence region in some cases.
* Fixed bug in `sjp.lmer` and `sjp.glmer` with doubled y-axis for faceted random effect plots.
* `sjt.xtab` ignored value labels when weighting data ([#106](https://github.com/sjPlot/devel/issues/106)).
* Fixed bug with position of value labels in `sjp.xtab` ([#107](https://github.com/sjPlot/devel/issues/107)).
* Fixed bug in `sjp.likert` that plotted categories in wrong order when neutral category was lower than amount of categories ([#109](https://github.com/sjPlot/devel/issues/109)).
* Fixed bug in `sjp.grpfrq` with argument `autoGroupAt`.
* Fixed minor bugs in `sjp.lm` with axis range for forest plots.
* Fixed bug in `sjp.stackfrq`, where the use of argument `showSeparatorLine` caused an error.
