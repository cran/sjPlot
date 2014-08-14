Version 1.4.3
------------------------------------------------------------------------------
### New functions:
* "sjp.emm.int" to plot estimated marginal means (least-squares means) of linear models with interaction terms.

### Changes to functions:
* Added parameter "showRowNames" to "sjt.df", so first table column with row names can be omitted from output, when "describe" is set to "FALSE".
* "sju.mwu" now computes exacts p-values and additionally reports Z-value and effect-size r.
* Added parameter "useFacetGrid" to "sjp.lmm" to plot each model in a new panel.
* Added parameter "moderatorValues" to "sjp.lm.int" to indicate which values of an interaction's moderator variable should be used for plotting predicted values.
* Added parameter "showPercentageAxis" to "sjp.stackfrq" to show or hide the percentage values for the x-axis.
* Added parameter "reverseAxis.x" to "sjp.qclus"

### Bug fixes:
* Fixed minor bugs in "sjp.frq"
