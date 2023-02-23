## Setting up code for Github repo

library(tidyverse)
data("iris")

obs <- 100
x <- rnorm(obs, mean = 1, sd = 0.5)

ggplot(iris, aes(iris$Sepal.Length, iris$Sepal.Width)) + geom_point()
