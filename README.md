## Motivation

* You like LaTeX
* You like consistent looking text and figures
* You use ggplot2 for plotting but dislike the aesthetic
* You want a (semi-) automated method of generating pgfplot figures from ggplot2 objects

## Workflow

1. `g <- ggplot(...)`
1. `g <- ggplot_build(g)`
1. `R2pgf::function(g, ...)`

## General features
* User-defined plot width and height
* Redesigned pgfplots marker table for ggplot2 shapes (only black supported for half-markers for now)

## scatter2pgf

* Plots all points separated by aesthetic
* (Marker) size, alpha, fill, and stroke, are all handled automatically
* `stat_smooth` linear regression is implemented (no standard error bars yet)
* `geom_line` for vertical lines is implemented (because vertical lines aren't handled by `stat_smooth`
* Aesthetic legend groups are automatically generated

