## ----echo=FALSE,message=FALSE-------------------------------------------------
knitr::opts_chunk$set(eval=TRUE)

## ----message=FALSE,warning=FALSE,class.source = 'fold-hide'-------------------
library(survextrap)
library(ggplot2)
mspline <- list(knots=1:10, degree=3, bsmooth=FALSE)
p_const <- mspline_constant_coefs(mspline)
p_naive <- rep(1/13, 13)
haz_unif <- mspline_plotdata(knots=mspline$knots, scale=10, coefs = p_const, bsmooth = FALSE)
plot_mspline(knots=mspline$knots, bsmooth=FALSE, scale=10, coefs = p_naive, tmax=11) + 
    geom_line(aes(x=time, y=haz), data=haz_unif, color="red", lwd=1.5, inherit.aes = FALSE) + 
    annotate(geom="text", x=2, y=1.5, color="red", label="h(t): coefficients `p_const`") +
    annotate(geom="text", x=1.5, y=2.5, color="blue", label="h(t): coefficients `p_naive`") + 
    xlab("Time t") + ylab("Hazard rate")

## -----------------------------------------------------------------------------
mspline <- list(knots=1:10, degree=3, bsmooth=TRUE)
p_const <- mspline_constant_coefs(mspline)
p_naive <- rep(1/11, 11)
haz_unif <- mspline_plotdata(knots=mspline$knots, scale=1, 
                             coefs = p_const, bsmooth = TRUE)
plot_mspline(knots=mspline$knots, bsmooth=TRUE, 
             scale=1, coefs = p_naive, tmax=11) + 
    geom_line(aes(x=time, y=haz), data=haz_unif, 
              color="red", lwd=1.5, inherit.aes = FALSE) + 
    annotate(geom="text", x=2, y=1.5, color="red", 
             label="h(t): coefficients `p_const`") +
    annotate(geom="text", x=1.5, y=2.5, color="blue", label="h(t): coefficients `p_naive`") + 
    xlab("Time t") + ylab("Hazard rate") + coord_cartesian(ylim=c(0,1))

## -----------------------------------------------------------------------------
plot_mspline(knots=c(1, 3, 5, 7),
     coefs=c(0.01, 0.5, 0.1, 2, 0.6), tmax=10, bsmooth=TRUE) + 
  coord_cartesian(ylim=c(0, 1.2))

## ----fig.height=3,class.source = 'fold-hide'----------------------------------
knots <- 1:10
mspline <- list(knots=knots)
p_mean <- mspline_constant_coefs(mspline)
set.seed(1)
p1 <- plot_prior_hazard(knots=knots, tmax=max(knots), 
                        coefs_mean = p_mean,
                        prior_hsd = p_gamma(2, 40),	     
                        prior_hscale = p_normal(0, 1),
                        nsim=10) + 	
  ylim(0, 1) + ggtitle(bquote(sigma ~ " ~ Gamma(2,40)"))
p2 <- plot_prior_hazard(knots=knots, tmax=max(knots), 
                        coefs_mean = p_mean,
                        prior_hsd = p_gamma(2, 1),	     
                        prior_hscale = p_normal(0, 1),
                        nsim=10) + 
  ylim(0, 1) + ggtitle(bquote(sigma ~ "~ Gamma(2,1)"))
gridExtra::grid.arrange(p1, p2, nrow=1)

## ----message=FALSE,warning=FALSE----------------------------------------------
library(dplyr)
library(survextrap)
colons2 <- colons %>% filter(rx %in% c("Obs", "Lev")) %>% droplevels()
rxph_mod <- survextrap(Surv(years, status) ~ rx, data=colons2, 
                       fit_method="opt", mspline=list(df=5))
plot(rxph_mod, tmax=10, niter=100)

## -----------------------------------------------------------------------------
nd_full <- data.frame(rx = c("Lev","Obs"))
nd_null <- data.frame(rx = c("Obs","Obs"))

## -----------------------------------------------------------------------------
plot(rxph_mod, tmax=10, wane_period = c(5, 10), niter=1000, 
     newdata = nd_full, newdata0 = nd_null)

## -----------------------------------------------------------------------------
rmst(rxph_mod, t=50, niter=100, newdata=nd_full)
rmst(rxph_mod, t=50, niter=20, newdata=nd_full, newdata0=nd_null, wane_period = c(5,10))

