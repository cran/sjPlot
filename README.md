sjPlot - data visualization
------------------------------------------------------------------------------
Collection of several plotting / table output functions for visualizing data.

### Installation

#### Latest development build

To install the latest development snapshot (see latest changes below), type following command in the R console:

```r
devtools::install_github("sjPlot/devel")
```

#### Officiale, stable release
To install the latest stable release from CRAN, type following command in the R console:

```r
install.packages("sjPlot")
```

### Changelog of current stable build 1.6

#### General
* Comprehensive online manual available at [strengejacke.de](http://www.strengejacke.de/sjPlot/)
* Most geom-aesthetics and theme-options have been removed from all `sjp`-functions. This allows more flexibility in creating own themes or using other packages that modify theme appearance. Use the new function `sjp.setTheme` to globally change theme- and geom-aesthetics for sjp-functions, or use packages like `ggthemr` or `ggthemes` to tweak the plot appearance.
* Replaced _b_ with beta-sign in model-summary for `sjp.lm` and `sjp.lm1`.
* Renamed `sju.aov1.levene` to `sjs.aov1.levene`.
* Documentation / help files have been revised and cleaned up.

#### New functions
* New function `sjp.setTheme` to globally change theme- and geom-aesthetics for sjp-functions.
* New function `sjp.lmer` to plot random or fixed effects of linear mixed effects models.
* New function `sjp.glmer` to plot random or fixed effects of generalized linear mixed effects models.
* New function `sjt.grpmean` to print grouped means as HTML table.
* New function `sjs.etasq` to compute eta squared value for one-way-anovas.
* New function `sjs.se` to compute standard errors.

#### Changes to functions
* sjp-functions now have a generic `geom.colors` (and, if applicable, also `geom.size`) parameter to change geom-appearance. Most other aesthetics modifications should be made using `sjp.setTheme`.
* All `sjt`-functions now auto-detect the character encoding depending on the platform's os-type (i.e. `encoding`-parameter defaults to `NULL`). Use parameter `encoding` to select a specific character encoding for the sjt-functions.
* `sjp.likert` was completely re-written and now offers to also plot a neutral (_don't know_) category.
* `sji.getValueLabels` and `sji.getVariableLabels` now also accept single variables as parameter.
* `sjp.glm`, `sjp.lm`, `sjt.glm` and `sjt.lm` now automatically retrieve the predictor's variable labels, if possible. Note that factor variables have more "labels" (for each factor level) than possible variable label attributes, so automatic detection of varibale labels does not work in such cases.
* `sjp.corr` was revised and now plots circles only in upper and values only in lower triangle.
* Added further options to `type` parameter in `sjp.glm` to show additional plots with probability curves of coefficients.
* Added parameter `sort.frq` to `sjt.frq` to order frequencies ascending or descending.
* Added parameters `title`, `breakTitleAt` and `useResiduals` to `sjp.reglin`, so residuals may be plotted against predictors (for diagnostic purposes).
* Parameter `labelPos` in `sjp.grpfrq` now also allows centered label positioning.
* Parameter `valueLabelPosOnTop` in `sjp.xtab` was removed and replaced by `labelPos`, which also allows centered label positioning.
* `sjp.lm.ma` now also plots residuals against predictors with scatter plots, if `completeDiagnostic` is TRUE.
* `sju.setNA` now removes value label attributes and value label names from those values that have been set to `NA` (so NA's are not associated with any value labels).
* Changed behaviour of `transformTicks` parameter in `sjp.glm` and `sjp.glmm` to get more pretty breaks for gridlines.
* Renamed parameter `flipCoordinates` to `coord.flip`.
* Renamed parameter `useFacetGrid` to `facet.grid`.

#### Bug fixes
* Number of observations in `sjt.lm` was wrong in table - fixed.
* `sjp.frq`, `sjt.frq` and `sjt.xtab` did not show (leading) categories with zero-counts - fixed.
* Legend labels in `sjp.grpfrq` were not shown (occured during implementation of ggthemr-support) - fixed.
* Fixed bugs with wrong label association for zero-categories in `sjt.frq`.
* Fixed minor bugs in various `sjp`-functions, where ggthemr-theme-color were not applied to bar/point/line-colors.
* Fixed bug with dot plots in `sjp.grpfrq`.
* Fixed bug with retrieving interaction terms from fitted models in `sjp.emm.int`.
* Fixed bug in `sju.strpos` where returned indices in some cases were _1_ instead of correct match.


### References and documentation

- [Documentation](http://www.strengejacke.de/sjPlot/)
- [Weblog](http://strengejacke.wordpress.com/sjplot-r-package/)


### Citation

In case you want / have to cite my package, please use `citation('sjPlot')` for citation information. Since this package makes heavy use of the [ggplot-package](http://cran.r-project.org/web/packages/ggplot2/index.html), consider citing this package as well.
