library(data.table)
library(ggplot2)
data(iris)

iris <- as.data.table(iris)
iris[, `:=` (int=coef(lm(Petal.Length~Sepal.Length))[1],
             slope=coef(lm(Petal.Length~Sepal.Length))[2]),
     by=.(Species)]

ggplot(data = iris, aes(x=Sepal.Length, y=Petal.Length,
                        color=as.factor(Species))) +
    stat_smooth(aes(group=Species), colour="black",
                method=lm, se=F, linetype="solid", geom="line") +
    geom_point() +
    theme_bw() +
    labs(colour = "Species")

