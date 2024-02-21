## ----setup, include=FALSE, echo = FALSE--------------------------------------------------------------
if(!dir.exists('figs91')){dir.create('figs91')}


## ----echo = FALSE------------------------------------------------------------------------------------
options(scipen=1, digits=2)


## ----packages----------------------------------------------------------------------------------------
set.seed(60637)
library(ggplot2)


## ----fig = TRUE, width = 7, height=5, echo=FALSE-----------------------------------------------------
x <- runif(15, 1, 10)
y <- 3*x + rnorm(15, sd = 2)


ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_point() +
    theme_bw()




## ----fig = TRUE, width = 7, height=5, echo=FALSE-----------------------------------------------------
ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    theme_bw()




## ----fig = TRUE, width = 7, height=5, echo=FALSE-----------------------------------------------------
ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, formula = y ~ splines::bs(x, 13)) + 
  theme_bw()




## ----fig = TRUE, width = 7, height=5, echo=FALSE-----------------------------------------------------

ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_point() +
    theme_bw() + 
  coord_cartesian(ylim = c(0, 30), xlim = c(0,10))



## ----fig = TRUE, width = 7, height=5, echo=FALSE-----------------------------------------------------
minc <- function(c){
    sum((y[which(x <=c)] - mean(y[which(x <=c)]))^2) + 
        sum((y[which(x >c)] - mean(y[which(x >c)]))^2)
}

newmin <- x[which.min(sapply(x, minc))]+.1
df <- data.frame(x = x, y = y)
df$y_distance <- ifelse(df$x <= newmin, df$y - mean(df$y[which(df$x <= newmin)]), 
                        df$y - mean(df$y[which(df$x > newmin)]))

ggplot(, aes(x = x, y = y)) +
    geom_point() +
    theme_bw() +
    geom_vline(xintercept = newmin, color = 'orange') +
    annotate('text', y = 30, x = newmin-.4, label = 'c', color = 'orange') +
    annotate('text', y = c(10, 15), x = c(1.25, 7.5),
             label = paste0(expression(bar(Y)),'==',
                            round(c(mean(y[which(x <=newmin)]), mean(y[which(x >newmin)])), 3) ),
             color = 'blue', parse = TRUE) + 
  geom_segment(aes(x = c(0, newmin), 
                   y = c(mean(df$y[which(df$x <= newmin)]),
                                               mean(df$y[which(df$x > newmin)]))), 
                   xend = c(newmin, 10),
               yend = c(mean(df$y[which(df$x <= newmin)]),
                                               mean(df$y[which(df$x > newmin)])),
               color = "blue") + 
  geom_segment(aes(x = x, y = y, xend = x, 
                   yend = ifelse(df$x <= newmin, mean(df$y[which(df$x <= newmin)]), 
                                 mean(df$y[which(df$x > newmin)]))),
               color = 'blue', arrow = arrow(length = unit(0.2, "cm"))) +
  coord_cartesian(ylim = c(0, 30), xlim = c(0,10))



## ----eval = FALSE------------------------------------------------------------------------------------
## library(grf)
## set.seed(60637)
## 
## 
## n <- 500
## p <- 10
## X <- matrix(rnorm(n * p), n, p)
## W <- rbinom(n, 1, 0.5)
## Y <- pmax(X[, 1], 0) * W + X[, 2] +
##   pmin(X[, 3], 0) + rnorm(n)
## c.forest <- causal_forest(X, Y, W)

## ----echo=FALSE, eval = FALSE------------------------------------------------------------------------
## # Saving a plot in .svg can be done with the `DiagrammeRsvg` package.
## tree.plot = plot(tree)
## cat(DiagrammeRsvg::export_svg(tree.plot), file = '../assets/tree_plot.svg')


## ----fig = TRUE, width = 7, height=5, echo=FALSE-----------------------------------------------------
hist(rnorm(10))

