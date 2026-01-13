## ----echo=FALSE,message=FALSE-------------------------------------------------
knitr::opts_chunk$set(eval=TRUE, message=FALSE, warning=FALSE, cache=TRUE)

## ----echo=FALSE,message=FALSE-------------------------------------------------
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 2) # CRAN check limits to 2 cores
# options(mc.cores = parallel::detectCores()) # CRAN check limits to 2 cores

## -----------------------------------------------------------------------------
library(survextrap)
library(ggplot2)
library(dplyr)
survminer::ggsurvplot(survfit(Surv(years, status) ~ 1, data=colons), data=colons)

## ----results="hide"-----------------------------------------------------------
nd_mod <- survextrap(Surv(years, status) ~ 1, data=colons, chains=1)
plot(nd_mod, show_knots=TRUE, tmax=5)

## ----results="hide"-----------------------------------------------------------
nd_mod2 <- survextrap(Surv(years, status) ~ 1, data=colons, chains=1, 
                      mspline = list(add_knots=4))
plot(nd_mod2, tmax=5)

## ----results="hide"-----------------------------------------------------------
nd_modr <- survextrap(Surv(years, status) ~ 1, data=colons, chains=1, 
                      smooth_model = "exchangeable",
                      mspline = list(add_knots=4))
plot(nd_modr, tmax=5)

## -----------------------------------------------------------------------------
head(summary(nd_mod))

## -----------------------------------------------------------------------------
rmst(nd_mod2, t = c(3,10,1000), niter=100)

## -----------------------------------------------------------------------------
rmst(nd_mod2, t = c(3,10,1000),
     summ_fns = list(mean=mean, median=median,
                     ~quantile(.x, c(0.25, 0.75))),
     niter=50)

## ----results="hide"-----------------------------------------------------------
extdat <- data.frame(start = c(5, 10, 15, 20), 
                     stop =  c(10, 15, 20, 25), 
                     n = c(100, 100, 100, 100), 
                     r = c(50, 40, 30, 20))

## ----results="hide"-----------------------------------------------------------
nde_mod <- survextrap(Surv(years, status) ~ 1, data=colons, 
                      fit_method="opt", external = extdat,
                      mspline = list(add_knots=c(4, 10, 25)))
plot(nde_mod)

## -----------------------------------------------------------------------------
mean(nde_mod, niter=100)
rmst(nde_mod, c(3, 10, 1000), niter=100)

## ----results="hide"-----------------------------------------------------------
levs <- c("trial", "external")
colons$dataset <- factor("trial", level=levs)
extdat$dataset <- factor("external", level=levs)

## ----results="hide"-----------------------------------------------------------
ndec_mod <- survextrap(Surv(years, status) ~ dataset, data=colons, 
                      external = extdat,
                      fit_method = "opt",
                      mspline = list(add_knots=c(4, 10, 25)))

## -----------------------------------------------------------------------------
summary(ndec_mod) %>% filter(variable=="hr")

## -----------------------------------------------------------------------------
rmst(ndec_mod, t=3, niter=100)

## -----------------------------------------------------------------------------
SHELF::fitdist(vals=c(0.1, 0.3, 0.5), probs=c(0.025, 0.5, 0.975),
               lower=0, upper=1)$Beta

## ----results="hide"-----------------------------------------------------------
extdat <- data.frame(start = c(10), stop =  c(15), 
                     n = c(20), r = c(6))
nde_mod <- survextrap(Surv(years, status) ~ 1, data=colons,
                      external = extdat, fit_method="opt")
plot(nde_mod)

## -----------------------------------------------------------------------------
mean(nde_mod, niter = 100)

## -----------------------------------------------------------------------------
coxph(Surv(years, status) ~ rx, data=colons)

## -----------------------------------------------------------------------------
rxph_mod <- survextrap(Surv(years, status) ~ rx, data=colons, refresh=0, fit_method="opt")
summary(rxph_mod) |>
    filter(variable=="loghr")

## -----------------------------------------------------------------------------
plot(rxph_mod, niter=100)

## -----------------------------------------------------------------------------
extdat <- data.frame(start = c(5, 10), stop =  c(10, 15), 
                     n = c(100, 100), r = c(50, 40), 
                     rx = "Obs")
rxphe_mod <- survextrap(Surv(years, status) ~ rx, data=colons, 
                      external = extdat, refresh=0, fit_method="opt")
rmst(rxphe_mod, niter=100, t=20)
plot(rxphe_mod, niter=100, tmax=5)
plot_hazard(rxphe_mod, niter=100, tmax=20)

## -----------------------------------------------------------------------------
nd <- data.frame(rx = c("Obs","Lev+5FU"))
survival(rxph_mod, t=c(5,10), newdata=nd)

## -----------------------------------------------------------------------------
ref_pop <- data.frame(rx = c("Obs","Lev+5FU"))
survival(rxph_mod, t = c(5,10), newdata = standardise_to(ref_pop))

## -----------------------------------------------------------------------------
survival(rxph_mod, t = c(5,10), newdata = standardise_to(ref_pop, random=TRUE))

## ----results="hide"-----------------------------------------------------------
rxnph_mod <- survextrap(Surv(years, status) ~ rx, data=colons, nonprop=TRUE, fit_method="opt")
plot(rxnph_mod, niter=200)

## -----------------------------------------------------------------------------
nd <- data.frame(rx = c("Lev+5FU","Lev"))
plot_hazard_ratio(rxnph_mod, newdata=nd) + 
  coord_cartesian(ylim=c(0,5)) 

## ----eval=FALSE---------------------------------------------------------------
# rxph_mod$loo

## ----eval=FALSE---------------------------------------------------------------
# rxnph_mod$loo

## ----results="hide"-----------------------------------------------------------
plot(survfit(Surv(t, status) ~ 1, data=curedata))

noncure_mod <- survextrap(Surv(t, status) ~ 1, data=curedata, fit_method = "opt")
plot_survival(noncure_mod,tmax=5) 

cure_mod <- survextrap(Surv(t, status) ~ 1, data=curedata, cure=TRUE, iter=300, chains=1, loo=FALSE)
plot(cure_mod, tmax=10, niter=20) 

## -----------------------------------------------------------------------------
curec_mod <- survextrap(Surv(t, status) ~ 1, data=curedata, cure=~x, fit_method="opt")
summary(curec_mod) %>% 
    filter(variable %in% c("pcure", "logor_cure", "or_cure"))

## -----------------------------------------------------------------------------
bh <- data.frame(time=c(0, 3, 5, 10), 
                 hazard=c(0.01, 0.05, 0.1, 0.2))

## -----------------------------------------------------------------------------
mod <- survextrap(Surv(years, status) ~ 1, data=colons, fit_method="opt")
mod_bh <- survextrap(Surv(years, status) ~ 1, data=colons, fit_method="opt", backhaz=bh)

## -----------------------------------------------------------------------------
plot_survival(mod, tmax=15, niter=20) + 
  geom_line(data=survival(mod_bh, tmax=15, niter=20), aes(y=median, x=t), col="red", lwd=1.2)

plot_hazard(mod, tmax=15, niter=20) + 
  geom_line(data=hazard(mod_bh, tmax=15, niter=20), aes(y=median, x=t), col="red", lwd=1.2)

## -----------------------------------------------------------------------------
bh_strata <- data.frame(time = rep(bh$time, 2),
                        hazard = c(bh$hazard, bh$hazard*1.2),
                        agegroup = rep(c("Under 70", "Over 70"),each=4))
colons$agegroup <- cut(colons$age, breaks=c(0,70,Inf), 
                       right=FALSE, labels=c("Under 70","Over 70"))
bh_strata

## -----------------------------------------------------------------------------
mod_bhs <- survextrap(Surv(years, status) ~ 1, data=colons, fit_method="opt",
                      backhaz=bh_strata, backhaz_strata="agegroup")

## -----------------------------------------------------------------------------
nd <- data.frame(agegroup = c("Over 70", "Under 70"))
plot_hazard(mod, tmax=15, ci=FALSE) + 
  geom_line(data = hazard(mod_bhs, newdata=nd, tmax=15, niter=300), 
            aes(y=median, x=t, col=agegroup), lwd=1.2) + 
  ylim(0,0.5)

## ----results="hide"-----------------------------------------------------------
colonsb <- colons
colonsb$bh <- rep(0.05, nrow(colons))
modb <- survextrap(Surv(years, status) ~ 1, data=colonsb, backhaz="bh", fit_method="opt")

## ----results="hide"-----------------------------------------------------------
mod <- survextrap(Surv(years, status) ~ 1, data=colonsb, fit_method="opt")
plot_hazard(mod) + 
    geom_line(data=hazard(modb), col="blue")

