## Load libraries and sample data
library(data.table)
library(ggplot2)
setwd("R")
source("/home/jchitpin/Downloads/Masters/Projects/MATRIX/RPackages/R2pgf/R/scatter2pgf.R")

data(iris)



## Must manually rename 'x' and 'y' columns as 'x' and 'y'
iris <- as.data.table(iris)
# Rename 'Sepal.Length'
setnames(iris, "Sepal.Length", "x")
# Rename 'Petal.Length'
setnames(iris, "Petal.Length", "y")

## Also must make sure there are no rows with duplicate 'x' and 'y' but in
## different 'Species' groups. Otherwise, extra legend entries will be added
## by mistake.
iris <- unique(iris, by = c("x", "y"))

## Create sample plot
g <- ggplot(data = iris,
            aes(x=x,
                y=y,
                color=as.factor(Species))) +
    stat_smooth(aes(group=Species), colour="black",
                method=lm, se=F, linetype="solid", geom="line") +
    geom_point(shape=16) +
    theme_bw() +
    labs(colour = "Species")
g

## |ggplot_build| to reformat 'g'
g <- ggplot_build(g)

## Running |scatter2pgf| will save a file in the current working directory
# legendLocation defaults are 'NE', 'SE', 'SW', 'NW'
# x/ylabNotation = 'norm' or 'exp' (not implemented)
# legendMat is a matrix that refers to the aesthetic name in the first column
# and the list layer in ggbuild$data[[]] in the second column

setwd("..")
scatter2pgf(ggbuild = g, filename = "iris.tex",
            width = "6", height = "4", legendLocation = "SE",
            xlabNotation = "norm", ylabNotation = "norm",
            legendMat = matrix(c("Species", "2"), nrow = 1, ncol = 2))
