## ----setup, include=FALSE, echo = FALSE--------------------------------------------------------------
if(!dir.exists('figs81')){dir.create('figs81')}


## ----echo = FALSE------------------------------------------------------------------------------------
 options(scipen=1, digits=2)


## ----packages----------------------------------------------------------------------------------------
library(ggplot2)
library(estimatr)
library(modelsummary)
library(gridExtra)
set.seed(60637)


## ----------------------------------------------------------------------------------------------------
file <- "https://raw.githubusercontent.com/UChicago-pol-methods/SOSC13200-W24/main/data/card-krueger.csv"
dat <- read.csv(file, as.is = TRUE)

head(dat)


## ----echo = FALSE------------------------------------------------------------------------------------
dat_wide <- reshape(dat, direction = 'wide', idvar = 'id', 
                    v.names = c('fte', 'ft', 'pt', 'mgrs', 'wage', 'meal', 'hrsopen', 'bonus',
                                'ncalls','inctime','firstinc','nregs'),
                    drop = c('d_nj', 'Wave'),
                    timevar = 'd')
dat_wide$Y <- dat_wide$fte.1-dat_wide$fte.0

# discuss coding of gap
dat_wide$gap <- ifelse(dat_wide$nj==1 & dat_wide$wage.0<= 5.05,((5.05-dat_wide$wage.0)/dat_wide$wage.0),0)

# conditioning variables, based on table footnote
dat_wide <- dat_wide[which( (!is.na(dat_wide$Y) &
                              !is.na(dat_wide$wage.0) &
                              !is.na(dat_wide$wage.1) ) |
                              dat_wide$status == 3 # updated
                              ),]

lm1 <- lm_robust(Y ~ nj, data = dat_wide)
lm2 <- lm_robust(Y ~ nj + kfc + roys + wendys + co_owned, data = dat_wide)
lm3 <- lm_robust(Y ~ gap, data = dat_wide)
lm4 <- lm_robust(Y ~ gap + kfc + roys + wendys + co_owned, data = dat_wide)
lm5 <- lm_robust(Y ~ gap + kfc + roys + wendys + co_owned + centralj + southj + pa1 + pa2, data = dat_wide)


modelsummary(list(lm1, lm2, lm3, lm4, lm5), stars = TRUE,
             # coef_map = c('nj', 'gap'), 
             coef_rename = c(nj = 'New Jersey Dummy', gap = 'Initial Wage Gap'),
             coef_omit = 'Int|kfc|wendy|co|roys|central|south|pa',
             add_rows = as.data.frame(rbind(c('Controls for chain and ownership', 'no', 'yes', 'no', 'yes', 'yes'),
                              c('Controls for region', 'no', 'no', 'no', 'no', 'yes'))), 
             gof_omit = 'Std|R2',
             output = '../assets/card-krueger-table4.tex')



## ----fig = TRUE, width = 5, height=5, echo=FALSE-----------------------------------------------------
n <- 50000

x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

ggplot(df, aes(x, y)) +
  geom_point(alpha = 0.05) +
  geom_abline(slope = 2, intercept = 3, color = 'skyblue', lwd = 1.5) +
  coord_cartesian(xlim = c(0,1), ylim = c(1,7)) +
  theme_bw()



## ----fig = TRUE, width = 5, height=5, echo=FALSE-----------------------------------------------------
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(X), 
                   list(a = format(unname(coef(m)[1]), digits = 3),
                        b = format(unname(coef(m)[2]), digits = 3)))
  as.character(as.expression(eq));
}

n <- 100

x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

lm0a <- lm(y~x)
lm0laba <- lm_eqn(df)

g <- ggplot(df) +
  coord_cartesian(xlim = c(0,1), ylim = c(1,7)) +
  geom_abline(slope = 2, intercept = 3, color = 'skyblue', lwd = 1.5, alpha = 0.5) +
  theme_bw()

g <- g + geom_abline(intercept = coef(lm0a)[1], slope = coef(lm0a)[2], color = 'blue') +
  geom_text(aes(x = 0.25, y = 6.25, label = lm0laba), parse = TRUE, data.frame(), color = 'blue')
g + geom_point(aes(x = x, y = y), alpha = 0.5)



## ----fig = TRUE, width = 5, height=5, echo=FALSE-----------------------------------------------------
x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

lm0b <- lm(y~x)
lm0labb <- lm_eqn(df)

g <- g + geom_abline(intercept = coef(lm0b)[1], slope = coef(lm0b)[2], color = 'darkgreen') +
  geom_text(aes(x = 0.25, y = 6, label = lm0labb), parse = TRUE, data.frame(), color = 'darkgreen')
g + geom_point(aes(x = x, y = y), alpha = 0.5)



## ----fig = TRUE, width = 5, height=5, echo=FALSE-----------------------------------------------------
x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

lm0c <- lm(y~x)
lm0labc <- lm_eqn(df)


g <- g + geom_abline(intercept = coef(lm0c)[1], slope = coef(lm0c)[2], color = 'orange') +
  geom_text(aes(x = 0.25, y = 5.75, label = lm0labc), parse = TRUE, data.frame(), color = 'orange')
g + geom_point(aes(x = x, y = y), alpha = 0.5)



## ----fig = TRUE, width = 5, height=5, echo=FALSE-----------------------------------------------------
x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

lm0d <- lm(y~x)
lm0labd <- lm_eqn(df)



g <- g + geom_abline(intercept = coef(lm0d)[1], slope = coef(lm0d)[2], color = 'pink') +
  geom_text(aes(x = 0.25, y = 5.5, label = lm0labd), parse = TRUE, data.frame(), color = 'pink')
g + geom_point(aes(x = x, y = y), alpha = 0.5)



## ----fig = TRUE, width = 5, height=5, echo=FALSE-----------------------------------------------------
x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

lm0e <- lm(y~x)
lm0labe <- lm_eqn(df)

g <- g + geom_abline(intercept = coef(lm0e)[1], slope = coef(lm0e)[2], color = 'purple') +
  geom_text(aes(x = 0.25, y = 5.25, label = lm0labe), parse = TRUE, data.frame(), color = 'purple')
g + geom_point(aes(x = x, y = y), alpha = 0.5)


## ----fig = TRUE, width = 5, height=5, echo=FALSE-----------------------------------------------------
X <- runif(1e3)
Y <- 3*X + rnorm(1e3, sd = 0.57)
Y2 <- 3*X + rnorm(1e3)*X
# Yhat <- 3*X
# mean((Y-Yhat)^2)
# mean((Y2-Yhat)^2)


df <- data.frame(X, Y, Y2)

g1 <- ggplot(df, aes(x = X, y = Y)) + 
  geom_point(alpha = 0.5) + 
  coord_cartesian(ylim = c(-0.25, 5.25)) + 
  ggtitle('Homoskedastic data') + theme_bw() +
  geom_abline(slope = 3, intercept = 0, color = 'skyblue', lwd = 1.5, alpha = 0.85)

g2 <- ggplot(df, aes(x = X, y = Y2)) + 
  geom_point(alpha = 0.5) + 
  coord_cartesian(ylim = c(-0.25, 5.25)) +
  ggtitle('Heteroskedastic data') + theme_bw() +
  geom_abline(slope = 3, intercept = 0, color = 'skyblue', lwd = 1.5, alpha = 0.85)


grid.arrange(g1, g2, ncol=2)


## ----------------------------------------------------------------------------------------------------
summary(lm(y ~ x, data = data.frame(x = X, y = Y)))$coef[,1:2]
summary(lm(y ~ x, data = data.frame(x = X, y = Y2)))$coef[,1:2]


## ----------------------------------------------------------------------------------------------------
broom::tidy(lm_robust(y ~ x, data = data.frame(x = X, y = Y)))[,1:3]
broom::tidy(lm_robust(y ~ x, data = data.frame(x = X, y = Y2)))[,1:3]


## ----message=FALSE-----------------------------------------------------------------------------------
dfp <- data.frame(
  black = rep(c(0, 1), times = c(300, 400)),
  record = c(rep(c(0, 1), each = 150),
             rep(c(0, 1), each = 200)),
  call_back = c(
    # whites without criminal records
    rep(c(0, 1), times = c(99, 51)), # 150
    # whites with criminal records
    rep(c(0, 1), times = c(125, 25)), # 150; 
    # - callbacks could be 25 or 26
    # blacks without criminal records
    rep(c(0, 1), times = c(172, 28)), # 200
    # blacks with criminal records
    rep(c(0, 1), times = c(190, 10)) # 200
  )
)



## ----------------------------------------------------------------------------------------------------
model2 <- lm_robust(call_back ~ black*record, data = dfp)


## ----------------------------------------------------------------------------------------------------
summary(model2)


## ----------------------------------------------------------------------------------------------------
confint(model2)


## ----------------------------------------------------------------------------------------------------
summary(model2)
round(model2$p.value,5)


## ----------------------------------------------------------------------------------------------------
outmat <- replicate(1000, # do this 1000 times
                    {
                      # Take a sample of size n with replacement from the data
                      idx <- sample(1:nrow(dfp), replace = TRUE) 
                      # fit the model on the sampled data
                      lmx <- lm_robust(call_back ~ black*record, 
                                       data = dfp[idx,])
                      coef(lmx)
                    })
outmat <- t(outmat)
dim(outmat)
head(outmat, 4)


## ----------------------------------------------------------------------------------------------------
apply(outmat, 2, sd)


## ----------------------------------------------------------------------------------------------------
model2$std.error


## ----------------------------------------------------------------------------------------------------
t(apply(outmat, 2, quantile, probs = c(0.025, 0.075)))


## ----------------------------------------------------------------------------------------------------
confint(model2)


## ----fig = TRUE, width = 5, height=5, echo=FALSE-----------------------------------------------------
hist(rnorm(10))

