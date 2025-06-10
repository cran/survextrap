## -----------------------------------------------------------------------------
library(SHELF)
p <- c(0.4, 0.6, 0.8)
bet <- fitdist(vals=p, probs=c(0.05, 0.5, 0.95), lower=0, upper=1)$Beta
bet

## ----message=FALSE,warning=FALSE----------------------------------------------
library(ggplot2)
ggplot(data.frame(p = c(0, 1)), aes(x = p)) +
  stat_function(fun = dbeta, args=list(shape1=bet$shape1, shape2=bet$shape2)) +
  ylab("Density")

## ----message=FALSE,warning=FALSE,results="hide"-------------------------------
library(survextrap)
nd_mod <- survextrap(Surv(years, status) ~ 1, data=colons, fit_method="opt")

## -----------------------------------------------------------------------------
print_priors(nd_mod)

## ----results="hide"-----------------------------------------------------------
round(nd_mod$coefs_mean,2)
sp <- nd_mod$mspline

## -----------------------------------------------------------------------------
nd_mod$prior_sample$haz_const()

## ----results="hide"-----------------------------------------------------------
p_meansurv(median=50, upper=80, mspline=sp)

## -----------------------------------------------------------------------------
prior_haz_const(mspline=sp, 
                prior_hscale = p_meansurv(median=50, upper=80, mspline=sp))

## -----------------------------------------------------------------------------
ndi_mod <- survextrap(Surv(years, status) ~ 1, data=colons, fit_method="opt",
                      prior_hscale = p_meansurv(median=50, upper=80, mspline=sp))
print_priors(ndi_mod)

## -----------------------------------------------------------------------------
set.seed(1)
haz_sim <- ndi_mod$prior_sample$haz(nsim=30)
ggplot(haz_sim, aes(x=time, y=haz, group=rep)) + 
  geom_line() + xlab("Years") + ylab("Hazard") + ylim(0,0.05)

## ----results="hide"-----------------------------------------------------------
set.seed(1)
haz_sim <- prior_sample_hazard(knots=sp$knots, degree=sp$degree, 
                               coefs_mean = ndi_mod$coefs_mean,   
                               prior_hsd = p_gamma(2,1),   
                               prior_hscale = p_meansurv(median=50, upper=80, mspline=sp),
                               tmax=3, nsim=30)

## ----results="hide"-----------------------------------------------------------
set.seed(1)
haz_sim <- prior_sample_hazard(knots=sp$knots, degree=sp$degree, 
                               coefs_mean = ndi_mod$coefs_mean,   
                               prior_hsd = p_gamma(2,20),   
                               prior_hscale = p_meansurv(median=50, upper=80, mspline=sp),
                             tmax=3, nsim=30)
ggplot(haz_sim, aes(x=time, y=haz, group=rep)) + 
  geom_line() + xlab("Years") + ylab("Hazard") + ylim(0,0.05)

## -----------------------------------------------------------------------------
set.seed(1)
ndi_mod$prior_sample$haz_sd()

## -----------------------------------------------------------------------------
set.seed(1)
prior_haz_sd(mspline=sp,                              
             prior_hsd = p_gamma(2,5),   
             prior_hscale = p_meansurv(median=50, upper=80, mspline=sp), 
             quantiles = c(0.1, 0.5, 0.9),
             nsim=1000)

## ----message=FALSE,warning=FALSE,results="hide"-------------------------------
library(dplyr)
nd_mod <- survextrap(Surv(years, status) ~ 1, data=colons, fit_method="mcmc",
                     chains=1, iter=1000,
                     prior_hsd = p_gamma(2,5))
qgamma(c(0.025, 0.975), 2, 5)
summary(nd_mod) %>% filter(variable=="hsd") %>% select(lower, upper)

## -----------------------------------------------------------------------------
prior_hr(p_normal(0, 2.5))

## -----------------------------------------------------------------------------
p_hr(median=1, upper=10)$scale
prior_hr(p_hr(median=1, upper=10))

## -----------------------------------------------------------------------------
prior_hr_sd(mspline=sp,                              
            prior_hsd = p_gamma(2,5),   
            prior_hscale = p_meansurv(median=50, upper=80, mspline=sp),
            prior_loghr = p_normal(0,1),
            prior_hrsd = p_gamma(2,3),
            formula = ~ treat,
            newdata = list(treat = 1),
            newdata0 = list(treat = 0),
            quantiles = c(0.05, 0.95),
            nsim=1000)

