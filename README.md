sjPlot - data visualization
------------------------------------------------------------------------------
Collection of several plotting / table output functions for visualizing data.

### Installation

#### Latest development build

To install the latest development snapshot (see latest changes below), type following command in the R console:

```r
devtools::install_github("devel", "sjPlot")
```

#### Officiale, stable release
To install the latest stable release from CRAN, type following command in the R console:

```r
install.packages("sjPlot")
```

### Changelog of current development build

#### New functions
* `sju.groupString` to recode similar (close distance) elements of a character vector into a new, single string value.
* `sju.strpos` to find partial matching and close distance elements in strings.
* `sju.mean.n` to compute means for rows with at least n valid values (like SPSS MEAN.n function).

#### Changes to functions
* `sjt.frq` can now handle character vectors (string variables), see parameter `removeStringVectors`.
* `sjt.frq` can automatically group string values in character vectors according to their distance metrics (similarity). This will merge different but similar values into a new, single value.
* Prefixes of statistical test functions have been renamed from `sju` to `sjs`.
* Added `themr` option to `theme` parameter of all sjp-plotting function, so the sjPlot package can be used with the [ggthemr-package](https://github.com/cttobin/ggthemr). You may use the `ggthemr::swatch` function to retrieve theme-colors as bar/point/line-colors.
* Added parameter `axisLimits.y` to function `sjp.emm.int`
* Added parameter `axisLimits.y` to function `sjp.lm.int`
* `sjp.lm` now shows adjusted r-square in model summary.
* `sjc.cluster`, `sjc.qclus` and `sjc.dend` now also accept agglomeration options `ward.D` and `ward.D2` if you are using an R version later than 3.0.3.
* `sjt.itemanalysis` now calulates index scores based on `sju.mean.n` function (see parameter `minValidRowMeanValue`).

#### General
* Removed extracted single functions from other packages and added imports for those functions. sjPlot now imports `psych` and suggests `cluster` package.
* Removed packages `cluster`, `coin`, `lsmeans` and `lmtest` from required imports and moved them to suggested packages.

#### Bug fixes
* Parameter `theme` was ignored in `sjp.scatter` - fixed.


### References and documentation

- [RPubs documentaton](http://rpubs.com/sjPlot/)
- [Weblog](http://strengejacke.wordpress.com/sjplot-r-package/)


### Citation

In case you want / have to cite my package, please use `citation('sjPlot')` for citation information. Since this package makes heavy use of the [ggplot-package](http://cran.r-project.org/web/packages/ggplot2/index.html), consider citing this package as well.
