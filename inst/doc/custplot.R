## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 5, warning = FALSE, message = FALSE)

if (!requireNamespace("sjmisc", quietly = TRUE) ||
    !requireNamespace("haven", quietly = TRUE) ||
    !requireNamespace("ggplot2", quietly = TRUE) ||
    !requireNamespace("sjlabelled", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

## -----------------------------------------------------------------------------
# load libraries
library(sjPlot)  # for plotting
library(sjmisc)  # for sample data
library(ggplot2) # to access ggplot-themes

# load sample data set
data(efc)

set_theme(
  geom.outline.color = "antiquewhite4", 
  geom.outline.size = 1, 
  geom.label.size = 2,
  geom.label.color = "grey50",
  title.color = "red", 
  title.size = 1.5, 
  axis.angle.x = 45, 
  axis.textcolor = "blue", 
  base = theme_bw()
)

plot_grpfrq(
  efc$e42dep, 
  efc$e16sex, 
  title = NULL, 
  geom.colors = c("cadetblue", "coral"), 
  geom.size = 0.4
)

## -----------------------------------------------------------------------------
# blank theme
set_theme(
  base = theme_blank(),
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 3
)

plot_grpfrq(
  efc$e42dep, 
  efc$e15relat, 
  geom.colors = "PuRd", 
  show.values = FALSE
)

## ---- eval=FALSE--------------------------------------------------------------
#  library(RColorBrewer)
#  display.brewer.all()

## -----------------------------------------------------------------------------
set_theme(geom.label.color = "white", geom.label.size = 3)

# labels appear very large due to export metrics
plot_grpfrq(efc$e42dep, efc$e16sex, coord.flip = TRUE)

## ----results='hide', echo=FALSE-----------------------------------------------
set_theme(
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 3
)

## -----------------------------------------------------------------------------
plot_grpfrq(efc$e42dep, efc$e16sex, expand.grid = TRUE)

## -----------------------------------------------------------------------------
set_theme(base = theme_light())
plot_frq(efc$e42dep)

## -----------------------------------------------------------------------------
library(sjmisc)
data(efc)
efc <- to_factor(efc, e42dep, c172code)
m <- lm(neg_c_7 ~ pos_v_4 + c12hour + e42dep + c172code, data = efc)

# reset theme
set_theme(base = theme_grey())

# forest plot of regression model
p <- plot_model(m)

# default theme
p
# pre-defined theme
p + theme_sjplot()

## -----------------------------------------------------------------------------
p + 
  theme_sjplot2() + 
  scale_color_sjplot("simply")

## -----------------------------------------------------------------------------
show_sjplot_pals()

## -----------------------------------------------------------------------------
set_theme(base = theme_bw(), axis.linecolor = "darkgreen")
plot_frq(efc$e42dep)

## -----------------------------------------------------------------------------
set_theme(
  base = theme_classic(),
  axis.tickslen = 0, # hides tick marks
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 3.5
)
  
plot_grpfrq(
  efc$e42dep,
  efc$e16sex,
  coord.flip = TRUE,
  show.axis.values = FALSE
) +
  theme(axis.line.x = element_line(color = "white"))

## -----------------------------------------------------------------------------
set_theme(
  base = theme_classic(), 
  legend.title.face = "italic", # title font face
  legend.inside = TRUE,         # legend inside plot
  legend.color = "grey50",      # legend label color
  legend.pos = "bottom right",  # legend position inside plot
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 3
)

plot_grpfrq(efc$e42dep, efc$e16sex, coord.flip = TRUE)

## -----------------------------------------------------------------------------
set_theme(
  base = theme_classic(), 
  axis.linecolor = "white",     # "remove" axis lines
  axis.textcolor.y = "darkred", # set axis label text only for y axis
  axis.tickslen = 0,            # "remove" tick marks
  legend.title.color = "red",   # legend title color
  legend.title.size = 2,        # legend title size
  legend.color = "green",       # legend label color
  legend.pos = "top",           # legend position above plot
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  geom.label.size = 3
)

plot_grpfrq(efc$e42dep, efc$e16sex)

