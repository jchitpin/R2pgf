## Motivation

* You like LaTeX
* You like consistent looking text and figures
* You use ggplot2 for plotting but dislike the aesthetic
* You want an automated method of generating pgfplot figures from ggplot2 objects

## Notes

* Depending on how complex your plots are, you may have to edit the .tex file for final tweaks
* Please double check the pgfplots graphs with the ggplot2 one to make sure everything looks right

## Updates

* This is a heavy work in progress!
* 30/05/2019 - First started on |scatter2pgf|
* TBD: Marker sizes and linewidth are directly converted to 'pt' (too small)
* TBD: Specifying legend aesthetic requires 'x' and 'y' column name for g
* TBD: Specifying legend aesthetic makes x/y-labels 'x' and 'y'
* TBD: Add support for separate fill/border colours

## Workflow

1. `g <- ggplot(...)`
1. `g <- ggplot_build(g)`
1. `R2pgf::function(g, ...)`

## General features
* User-defined plot width and height
* Redesigned pgfplots marker table for ggplot2 shapes (fill and border colours are the same)

<p align=middle>
  <img src="/README/ggplot2-shapes.png" height="200" />
  <img src="/README/R2pgf-markers.png" height="200" />
</p>

## scatter2pgf

* Plots all points separated by aesthetic
* (Marker) size, alpha, fill, and stroke, are all handled automatically (converted to pt)
* `stat_smooth` linear regression is implemented (no standard error bars yet)
* `geom_line` for vertical lines is implemented (because vertical lines aren't handled by `stat_smooth`
* Aesthetic legend groups are automatically generated (need to re-label x/y axis on the plots)

<p align=middle>
  <img src="/README/ggplot2-iris.png" height="200" />
  <img src="/README/R2pgf-iris.png" height="200" />
</p>

