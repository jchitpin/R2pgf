## Motivation

* You like LaTeX
* You like consistent looking text and figures
* You use ggplot2 for plotting but dislike the aesthetic
* You want a (semi-) automated method of generating pgfplot figures from ggplot2 objects

## Workflow

1. `g <- ggplot(...)`
1. `g <- ggplot_build(g)`
1. `R2pgf::function(g, ...)`

## scatter2pgf

* 


