sjPlot - Data Visualization for Statistics in Social Science
------------------------------------------------------------------------------
Collection of plotting and table output functions for data visualization. Results of various statistical analyses (that are commonly used in social sciences) can be visualized using this package, including simple and cross tabulated frequencies, histograms, box plots, (generalized) linear models, mixed effects models, PCA and correlation matrices, cluster analyses, scatter plots, Likert scales, interpretation of interaction terms in regression models, constructing index or score variables and much more.


### Installation

#### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("sjPlot/devel")
```

#### Officiale, stable release
To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjPlot")
```

### References, documentation and examples

- [Documentation and examples](http://www.strengejacke.de/sjPlot/)
- [Weblog](http://strengejacke.wordpress.com/sjplot-r-package/)


### Citation

In case you want / have to cite my package, please use `citation('sjPlot')` for citation information. Since this package makes heavy use of the [ggplot-package](http://cran.r-project.org/web/packages/ggplot2/index.html), consider citing this package as well.

### Changelog of current official release 1.8

#### General
* _Utility, recode and statistical test functions have been moved to another package called [sjmisc](https://github.com/sjPlot/sjmisc)!_
* R-Version dependency changed to R >= 3.1, due to import of `tidyr` and `dplyr` packages.
* `sjp.emm.int` is now deprecated. Use `sjp.int` with parameter `type = "emm"` to plot estimated marginal means. Estimated marginal means can now also be applied to `lmerMod`-objects from `lme4`-package.

#### New functions
* `sjt.mwu` to print Mann-Whitney-tests as HTML-table.

#### Changes to function 'sjp.int'
* `sjp.int` now supports `plm` objects (from plm-package).
* Added parameter `type` to `sjp.int` to plot different types of interactions, including estimated marginal means.
* Added parameter `legendTitle` to `sjp.int`.
* Added parameter `int.plot.index` to `sjp.int`, so only selected interaction terms may be plotted.
* Added parameter `showCI` to `sjp.int` (only applies to `type = "emm"` and `"eff"`) to add confidence intervals to estimated marginal means.
* Added parameter `facet.grid` to `sjp.int` to plot each effect in a separate plot.
* Parameter `moderatorValues` in `sjp.int` has two new options `zeromax` and `quart` for chosing the moderator values.
* Parameter `legendLabels` of `sjp.int` now accepts a list of character vectors, with one vector of legend labels for each interaction plot plotted.
* Parameter `title` of `sjp.int` now accepts a character vector of same length as interaction terms, with one title character string for each interaction plot plotted.
* Parameter `moderatorValues` in `sjp.int` has two new options `zeromax` and `quart` for chosing the moderator values.

#### Changes to other functions
* Linear mixed model methods (`sjp.int`, `sjp.lmer`) can now cope with `modMerLmerTest` objects (fitted with `lmerTest`-package).
* `sjp.lmer` now calculates approximate p-values based on Wald chi-squared tests.
* `sjp.lmer` and `sjp.glmer` now plot all random effects (when `type = "re"`) by default, instead of only the first random effect. Furthermore, parameter `ri.nr` now may be a numeric vector (instead of single numeric value) with several random effect index numbers.
* `sjp.glm` now supports plotting `logistf` objects.
* `sjp.glmm` and `sjp.lmm` now also accept a list of fitted models (see examples in `?sjp.glmm` and `?sjp.lmm`).
* `sjp.int` and `sjp.lm` now support `plm` objects (from plm-package).
* Parameters `orderBy` and `reverseOrder` in `sjp.stackfrq` and `sjt.stackfrq` were merged into new parameter `sort.frq`.
* Parameter `transformTicks` in `sjp.glm` and `sjp.glmm` now defaults to `TRUE`.
* Added parameter `emph.grp` to `sjp.lmer` and `sjp.glmer` to emphasize specific grouping levels when plot-type is either `fe.ri` or `ri.pc`.
* Added parameter `labelDigits` to functions `sjp.likert` and `sjp.stackfrq`, so digits of value labels can be changed.
* Added option `fe.pred` to `type`-parameter of `sjp.lmer` to plot slopes for each single fixed effect.
* Added parameter `bar` to `sjp.pca` to plot loadings of principle components as bar charts.
* Renamed parameters `y` and `x` in `sjp.xtab` into `var` and `grp`.
* Added further pre-set themes to `sjp.setTheme`.
* Minor improvements of `sjp.setTheme`.

#### Bug fixes
* `sjp.int` did not work for interaction terms of factors with more than two levels in mixed effects models (`merMod`-objects) - fixed.
* `sjp.glm` and `sjp.glmm` should catch axis limits, which are out of printable bounds, hence these function should no longer stop in such cases.
* `sjp.lmer` and `sjp.glmer` wrongly stated that paramter `ri.nr` was out of bound when `type` was `re`, `fe.ri` or `ri.pc` - fixed.
* Weights with decimals in `sjt.xtab` (e.g. `weightBy = abs(rnorm(100, 2, 1)`) caused an error - fixed.
* `sjp.int` did not work with interaction terms that used `AsIs` conversion (function `I`) - fixed.
