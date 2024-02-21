## ----setup, include=FALSE, echo = FALSE---------------------------------------
if(!dir.exists('figs82')){dir.create('figs82')}


## ----echo = FALSE-------------------------------------------------------------
options(scipen=1, digits=4)


## ----packages-----------------------------------------------------------------
set.seed(60637)
# For plotting:
library(ggplot2)
# library(devtools)
# devtools::install_github("wilkelab/ungeviz")
library(ungeviz)
library(ggridges)


## ----eval = FALSE, echo=TRUE--------------------------------------------------
## ggplot(iris, aes(Species, Sepal.Length,fill = Species)) +
##     geom_violin(alpha = 0.25, color = NA) +
##     geom_point(position = position_jitter(width = 0.3, height = 0), size = 0.5) +
##     geom_hpline(aes(colour = Species), stat = "summary", width = 0.6, size = 1.5, fun = 'mean')


## ----fig = TRUE, width = 5, height=5, echo=FALSE------------------------------
ggplot(iris, aes(Species, Sepal.Length,fill = Species)) +
    geom_violin(alpha = 0.25, color = NA) +
    geom_point(position = position_jitter(width = 0.3, height = 0), size = 0.5) +
    geom_hpline(aes(colour = Species), stat = "summary", width = 0.6, size = 1.5, fun = 'mean') + 
    theme_bw() +
    ylab('Sepal Length')


## ----eval = FALSE, echo=TRUE--------------------------------------------------
## ggplot(cacao_means, aes(x = estimate, y = location)) +
##     stat_confidence_density(aes(moe = std.error), confidence = 0.68, fill = "#81A7D6", height = 0.7) +
##     geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.3) +
##     geom_vpline(aes(x = estimate), size = 1.5, height = 0.7, color = "#D55E00")


## ----fig = TRUE, width = 5, height=5, echo=FALSE------------------------------
library(dplyr)
library(forcats)
library(broom)
library(emmeans)

cacao_lumped <- cacao %>%
    mutate(
        location = fct_lump(location, n = 10)
    )

cacao_means <- lm(rating ~ location, data = cacao_lumped) %>%
    emmeans("location") %>%
    tidy() %>%
    mutate(location = fct_reorder(location, estimate))

ggplot(cacao_means, aes(x = estimate, y = location)) +
    stat_confidence_density(aes(moe = std.error), confidence = 0.68, fill = "#81A7D6", height = 0.7) +
    geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.3) +
    geom_vpline(aes(x = estimate), size = 1.5, height = 0.7, color = "#D55E00") +
    xlim(2.8, 3.6) + 
    theme_bw() 


## ----eval = FALSE, echo=TRUE--------------------------------------------------
## ggplot(cacao_means, aes(x = estimate, y = location)) +
##     stat_confidence_density(
##         aes(moe = std.error, height = stat(density)), geom = "ridgeline",
##         confidence = 0.68, fill = "#81A7D6", alpha = 0.8, scale = 0.08, min_height = 0.1) +
##     geom_vpline(aes(x = estimate), size = 1.5, height = 0.5, color = "#D55E00")


## ----fig = TRUE, width = 5, height=5, echo=FALSE------------------------------
ggplot(cacao_means, aes(x = estimate, y = location)) +
    stat_confidence_density(
        aes(moe = std.error, height = stat(density)), geom = "ridgeline",
        confidence = 0.68, fill = "#81A7D6", alpha = 0.8, scale = 0.08,
        min_height = 0.1
    ) +
    geom_vpline(aes(x = estimate), size = 1.5, height = 0.5, color = "#D55E00") +
    xlim(2.8, 3.6) + 
    theme_bw()


## ----fig = TRUE, width = 5, height=5, echo=FALSE------------------------------
hist(rnorm(10))


## ----echo = FALSE-------------------------------------------------------------
f <- 'slides_82.Rnw'
knitr::purl(f)
knitr::Sweave2knitr(f)

