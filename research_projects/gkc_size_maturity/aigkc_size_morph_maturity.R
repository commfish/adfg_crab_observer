# notes ----
## AIGKC size at morph maturity
## Tyler Jackson
## 7/8/2021

# load ----
library(tidyverse)
library(bbmle) # for mle2 function minimizer
library(ggfortify) # for lm diagnostic plots in ggplot similar to plot.lm()
library(segmented)

# ggplot theme
theme_set(theme_classic())


# function to call segmented package
f_seg <- function(data, psi){
  l_mod <- lm(y~x, data)
  seg_mod <- segmented(l_mod, seg.Z = ~x, psi = psi)
  return(seg_mod)
}
## SM50 from logistic regression
f_sm50 <- function(x){as.numeric(-coef(x)[1] / coef(x)[2])}
## segemented regression likelihood profile
f_profile_seg <- function(seg_fit, plot_path){
  # define functions
  ## linear predictor
  f_bpreg <- function(x, b0, b1, b2, psi){
    y = b0 + b1*x + b2*(x-psi)*(x>psi)
    return(y)
  }
  ## NLL
  logLik_seg <- function(x, y, b0, b1, b2, psi, rse){
    fit = f_bpreg(x, b0, b1, b2, psi)
    log_like = sum(dnorm(x = y, mean = fit, sd = rse, log = T))
    return(-log_like)
  }
  
  # extract data and parameter profile ranges
  x = seg_fit$model$x
  y = seg_fit$model$y
  range_b0 = sort(c(coef(seg_fit)[1] - coef(seg_fit)[1], coef(seg_fit)[1] + coef(seg_fit)[1]), decreasing = F)
  b0 = round(seq(range_b0[1], range_b0[2], by = abs(coef(seg_fit)[1] / 50)), 3)
  range_b1 = sort(c(coef(seg_fit)[2] - coef(seg_fit)[2], coef(seg_fit)[2] + coef(seg_fit)[2]), decreasing = F)
  b1 = seq(range_b1[1], range_b1[2], by = abs(coef(seg_fit)[2] / 50))
  range_b2 = sort(c(coef(seg_fit)[3] - coef(seg_fit)[3], coef(seg_fit)[3] + coef(seg_fit)[3]), decreasing = F)
  b2 = seq(range_b2[1], range_b2[2], by = abs(coef(seg_fit)[3] / 50))
  psi = seq(min(x), max(x), by = abs(seg_fit$psi[2] / 500))
  
  ## profiles
  ## b0 ----
  b0_fixed_fit = list()
  for(i in 1:length(b0)){
    b0_fixed_fit[i] <- mle2(logLik_seg,
                            start = list(b1 = coef(seg_fit)[2], 
                                         b2 = coef(seg_fit)[3],
                                         psi = seg_fit$psi[1],
                                         rse = summary(seg_fit)$sigma),
                            data = list(x = x, y = y, b0 = b0[i]),
                            method = "Nelder-Mead",
                            optimizer = "nlminb",
                            control = list(maxit=1e6))
  }
  tibble(b0 = b0,
         mod = b0_fixed_fit) %>%
    mutate(ll = purrr::map_dbl(mod, logLik)) -> profile_b0
  profile_b0 %>%
    ggplot()+
    geom_line(aes(x = b0, y = -ll), color = 4)+
    geom_vline(xintercept = as.numeric(coef(seg_fit)[1]), linetype = 2)+
    xlab(~ paste(beta[0]))+
    ylab(~"Negative Log-likelihood") -> p_b0  
  
  ## b1 ----
  b1_fixed_fit = list()
  for(i in 1:length(b1)){
    b1_fixed_fit[i] <- mle2(logLik_seg,
                            start = list(b0 = coef(seg_fit)[1], 
                                         b2 = coef(seg_fit)[3], 
                                         psi = seg_fit$psi[1],
                                         rse = summary(seg_fit)$sigma),
                            data = list(x = x, y = y,  b1 = b1[i]),
                            method = "Nelder-Mead",
                            optimizer = "nlminb",
                            control = list(maxit=1e6))
  }
  tibble(b1 = b1,
         mod = b1_fixed_fit) %>%
    mutate(ll = purrr::map_dbl(mod, logLik)) -> profile_b1
  profile_b1 %>%
    ggplot()+
    geom_line(aes(x = b1, y = -ll), color = 4)+
    geom_vline(xintercept = as.numeric(coef(seg_fit)[2]), linetype = 2)+
    xlab(~ paste(beta[1]))+
    ylab(~"Negative Log-likelihood") -> p_b1  
  
  ## b2 ----
  b2_fixed_fit = list()
  for(i in 1:length(b2)){
    b2_fixed_fit[i] <- mle2(logLik_seg,
                            start = list(b0 = coef(seg_fit)[1], 
                                         b1 = coef(seg_fit)[2], 
                                         psi = seg_fit$psi[1],
                                         rse = summary(seg_fit)$sigma),
                            data = list(x = x, y = y, b2 = b2[i]),
                            method = "Nelder-Mead",
                            optimizer = "nlminb",
                            control = list(maxit=1e6))
  }
  tibble(b2 = b2,
         mod = b2_fixed_fit) %>%
    mutate(ll = purrr::map_dbl(mod, logLik)) -> profile_b2
  profile_b2 %>%
    ggplot()+
    geom_line(aes(x = b2, y = -ll), color = 4)+
    geom_vline(xintercept = as.numeric(coef(seg_fit)[3]), linetype = 2)+
    xlab(~ paste(beta[2]))+
    ylab(~"Negative Log-likelihood") -> p_b2  
  
  ## psi ----
  psi_fixed_fit = list()
  for(i in 1:length(psi)){
    psi_fixed_fit[i] <- mle2(logLik_seg,
                             start = list(b0 = coef(seg_fit)[1], 
                                          b1 = coef(seg_fit)[2], 
                                          b2 = coef(seg_fit)[3],
                                          rse = summary(seg_fit)$sigma),
                             data = list(x = x, y = y, psi = psi[i]),
                             method = "Nelder-Mead",
                             optimizer = "nlminb",
                             control = list(maxit=1e6))
  }
  tibble(psi = psi,
         mod = psi_fixed_fit) %>%
    mutate(ll = purrr::map_dbl(mod, logLik)) -> profile_psi
  profile_psi %>%
    ggplot()+
    geom_line(aes(x = psi, y = -ll), color = 4)+
    geom_vline(xintercept = seg_fit$psi[2], linetype = 2)+
    xlab(~ paste(psi))+
    ylab(~"Negative Log-likelihood") -> p_psi
  ggsave(gsub("fit", "psi", plot_path), p_psi, height = 3, width = 4, units = "in")
  
  ## output ----
  ### plot
  ggsave(plot_path, cowplot::plot_grid(p_b0, p_b1, p_b2, p_psi), height = 6, width = 7, units = "in")
  ### output
  out = list(b0 = profile_b0,
             b1 = profile_b1,
             b2 = profile_b2,
             psi = profile_psi)
  return(out)
  
}

# data ----

## EAG (1) and WAG (2) data
aigkc <- read_csv("./aigkc/data/AIGKC2018_20CLCHData.csv") %>%
              rename_all(~c("year", "source", "cl", "ch", "area", "ln_chcl")) %>%
              mutate(area = case_when(area == 1 ~ "EAG",
                                      area == 2 ~ "WAG"),
                     source = ifelse((source == "CoopertiveSurvey"&year==2020), "SpecialObserver", source)) %>%
              dplyr::select(-ln_chcl)

## region datasets
eag <- filter(aigkc, area == "EAG",
              # exclude extreme obvious outlier
              !(cl < 100 & ch > 40))
wag <- filter(aigkc, area == "WAG",
              # exclude extreme obvious outlier
              !(cl < 60 & ch > 30))

# eda ----
aigkc %>%
  mutate(area = toupper(area),
         source = case_when(source == "CoopertiveSurvey" ~ " Co-op Survey",
                            source == "Observer" ~ "Observer",
                            source == "RetainedCatch" ~ "Ret. Catch",
                            source == "SpecialObserver" ~ "Obs. Special Project")) %>%
  ggplot()+
  geom_point(aes(x = log(cl), y = log(ch), color = source), alpha = 0.5)+
  labs(x = "ln Carpace Length (mm)", y = "ln Chela Height (S) (mm)", color = NULL) +
  facet_wrap(~area, nrow = 2)+
  theme(strip.background = element_rect(color = NA),
        legend.position = "bottom") -> x
ggsave("./research_projects/gkc_size_maturity/figures/aigkc_data_source.png",
       plot = x, 
       height = 5, width = 5, units = "in")



# fit ln ch~cl eag all data ----

# all data, no effect of year
eag %>%
  mutate(x = log(cl), y = log(ch)) %>%
  # remove the outlier
  f_seg(., psi = 4.8) -> eag_ln_fit
summary(eag_ln_fit)
autoplot(eag_ln_fit, alpha = 0.5)
# profile parameters
eag_ln_fit_prof <- f_profile_seg(eag_ln_fit, "./research_projects/gkc_size_maturity/figures/eag_ln_fit_profile.png")

# flag potential outliers
eag %>%
  dplyr::select(cl, ch) %>%
  rename_all(~c("x", "y")) %>%
  mutate_all(log) %>%
  # add cooks distance
  mutate(cook_d = cooks.distance(eag_ln_fit),
         cook_out = cook_d > (4 / nrow(.))) %>%
  # add dfbetas
  bind_cols(as_tibble(dfbetas(eag_ln_fit)) %>%
              rename_all(~c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma"))) %>%
  mutate(dfbetas_out = (dbs_b0 > (2 / sqrt(5433)) | dbs_b1 > (2 / sqrt(5433)) | dbs_b2 > (2 / sqrt(5433)) | dbs_gamma > (2 / sqrt(5433))),
         out = (cook_out + dfbetas_out) > 0) -> eag_noout
# plot flagged data points
ggplot()+
  geom_point(data = eag_noout, aes(x = x, y = y, color = out), alpha = 0.5)+
  scale_color_manual(values = c("grey", "red"))+
  theme(legend.position = "none")+
  labs(x = "ln Carapace Length (mm)", y = "ln Chela Height (S) (mm)")

# refit
eag_noout %>%
  filter(!out) %>%
  f_seg(., psi = 4.9) -> eag_ln_fit_noout
summary(eag_ln_fit_noout)
autoplot(eag_ln_fit_noout, alpha = 0.5)
# profile parameters
eag_ln_fit_prof_noout <- f_profile_seg(eag_ln_fit_noout, "./research_projects/gkc_size_maturity/figures/eag_ln_fit_noout_profile.png")

# extract psi
eag_ln_psi <- eag_ln_fit_noout$psi[2]
eag_ln_psi_95ci <- eag_ln_fit_noout$psi[2] + qnorm(c(0.025, 0.975)) * eag_ln_fit_noout$psi[3]
## backtransform and bias correct
eag_ln_psi_bt <- exp(eag_ln_fit_noout$psi[2] + eag_ln_fit_noout$psi[3] / 2)
eag_ln_psi_bt_95ci <- eag_ln_psi_bt  + qnorm(c(0.025,0.975)) * sqrt(eag_ln_psi_bt^2*(exp(eag_ln_fit_noout$psi[3]^2)-1))

# plot fit without outliers
eag_noout %>%
  filter(!out) %>%
ggplot()+
  geom_point(aes(x = x, y = y), color = "grey", alpha = 0.5)+
  geom_line(aes(x = x, y = eag_ln_fit_noout$fitted.values), color = "red")+
  geom_vline(xintercept = c(eag_ln_psi, eag_ln_psi_95ci), linetype = c(1, 2, 2))+
  labs(x = "ln Carapace Length (mm)", y = "ln Chela Height (S) (mm)") -> x
ggsave("./research_projects/gkc_size_maturity/figures/eag_ln_fit_noout.png",
       plot = x, 
       height = 3, width = 4, units = "in")

## diagnostic plots
ggsave("./research_projects/gkc_size_maturity/figures/eag_ln_fit_noout_diagnostic.png",
       plot = autoplot(eag_ln_fit_noout, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")

# fit ch~cl eag all data----

eag %>%
  mutate(x = cl, y = ch) %>%
  f_seg(., psi = 130) -> eag_fit
summary(eag_fit)
autoplot(eag_fit, alpha = 0.5)
# profile parameters
eag_fit_prof <- f_profile_seg(eag_fit, "./research_projects/gkc_size_maturity/figures/eag_fit_profile.png")

# check is removing outliers changes the fit at all
eag %>%
  dplyr::select(cl, ch) %>%
  rename_all(~c("x", "y")) %>%
  #mutate_all(log) %>%
  # add cooks distance
  mutate(cook_d = cooks.distance(eag_fit),
         cook_out = cook_d > (4 / nrow(.))) %>%
  # add dfbetas
  bind_cols(as_tibble(dfbetas(eag_fit)) %>%
              rename_all(~c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma"))) %>%
  mutate(dfbetas_out = (dbs_b0 > (2 / sqrt(5433)) | dbs_b1 > (2 / sqrt(5433)) | dbs_b2 > (2 / sqrt(5433)) | dbs_gamma > (2 / sqrt(5433))),
         out = (cook_out + dfbetas_out) > 0) %>%  filter(!out) %>%
  f_seg(., psi = 130)

# extract psi
eag_psi <- eag_fit$psi[2]
eag_psi_95ci <- eag_fit$psi[2] + qnorm(c(0.025, 0.975)) * eag_fit$psi[3]

# plot fit without outliers
eag %>%
  ggplot()+
  geom_point(aes(x = cl, y = ch), color = "grey", alpha = 0.5)+
  geom_line(aes(x = cl, y = eag_fit$fitted.values), color = "red")+
  geom_vline(xintercept = c(eag_psi, eag_psi_95ci), linetype = c(1, 2, 2))+
  labs(x = "Carapace Length (mm)", y = "Chela Height (S) (mm)") -> x
ggsave("./research_projects/gkc_size_maturity/figures/eag_fit.png",
       plot = x, 
       height = 3, width = 4, units = "in")

## diagnostic plots
ggsave("./research_projects/gkc_size_maturity/figures/eag_fit_diagnostic.png",
       plot = autoplot(eag_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")


# fit ln ch~cl wag all data ----

# all data, no effect of year
wag %>%
  mutate(x = log(cl), y = log(ch)) %>%
  # remove the outlier
  f_seg(., psi = 4.8) -> wag_ln_fit
summary(wag_ln_fit)
autoplot(wag_ln_fit, alpha = 0.5)
# profile parameters
wag_ln__fit_prof <- f_profile_seg(wag_ln_fit, "./research_projects/gkc_size_maturity/figures/wag_ln_fit_profile.png")

# flag potential outliers
wag %>%
  dplyr::select(cl, ch) %>%
  rename_all(~c("x", "y")) %>%
  mutate_all(log) %>%
  # add cooks distance
  mutate(cook_d = cooks.distance(wag_ln_fit),
         cook_out = cook_d > (4 / nrow(.))) %>%
  # add dfbetas
  bind_cols(as_tibble(dfbetas(wag_ln_fit)) %>%
              rename_all(~c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma"))) %>%
  mutate(dfbetas_out = (dbs_b0 > (2 / sqrt(5433)) | dbs_b1 > (2 / sqrt(5433)) | dbs_b2 > (2 / sqrt(5433)) | dbs_gamma > (2 / sqrt(5433))),
         out = (cook_out + dfbetas_out) > 0) -> wag_noout
# plot flagged data points
ggplot()+
  geom_point(data = wag_noout, aes(x = x, y = y, color = out), alpha = 0.5)+
  scale_color_manual(values = c("grey", "red"))+
  theme(legend.position = "none")+
  labs(x = "ln Carapace Length (mm)", y = "ln Chela Height (S) (mm)")

# refit
wag_noout %>%
  filter(!out) %>%
  f_seg(., psi = 4.9) # no real change

# extract psi
wag_ln_psi <- wag_ln_fit$psi[2]
wag_ln_psi_95ci <- wag_ln_fit$psi[2] + qnorm(c(0.025, 0.975)) * wag_ln_fit$psi[3]
## backtransform and bias correct
wag_ln_psi_bt <- exp(wag_ln_fit_noout$psi[2] + wag_ln_fit_noout$psi[3] / 2)
wag_ln_psi_bt_95ci <- wag_ln_psi_bt  + qnorm(c(0.025,0.975)) * sqrt(wag_ln_psi_bt^2*(exp(wag_ln_fit$psi[3]^2)-1))

# plot fit without outliers
wag %>%
  ggplot()+
  geom_point(aes(x = log(cl), y = log(ch)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = log(cl), y = wag_ln_fit$fitted.values), color = "red")+
  geom_vline(xintercept = c(wag_ln_psi, wag_ln_psi_95ci), linetype = c(1, 2, 2))+
  labs(x = "ln Carapace Length (mm)", y = "ln Chela Height (S) (mm)") -> x
ggsave("./research_projects/gkc_size_maturity/figures/wag_ln_fit.png",
       plot = x, 
       height = 3, width = 4, units = "in")

## diagnostic plots
ggsave("./research_projects/gkc_size_maturity/figures/wag_ln_fit_diagnostic.png",
       plot = autoplot(wag_ln_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")

# fit ch~cl wag all data ----
wag %>%
  mutate(x = cl, y = ch) %>%
  f_seg(., psi = 130) -> wag_fit
summary(wag_fit)
autoplot(wag_fit, alpha = 0.5)
# profile parameters
wag_fit_prof <- f_profile_seg(wag_fit, "./research_projects/gkc_size_maturity/figures/wag_fit_profile.png")

# check is removing outliers changes the fit at all
wag %>%
  dplyr::select(cl, ch) %>%
  rename_all(~c("x", "y")) %>%
  #mutate_all(log) %>%
  # add cooks distance
  mutate(cook_d = cooks.distance(wag_fit),
         cook_out = cook_d > (4 / nrow(.))) %>%
  # add dfbetas
  bind_cols(as_tibble(dfbetas(wag_fit)) %>%
              rename_all(~c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma"))) %>%
  mutate(dfbetas_out = (dbs_b0 > (2 / sqrt(5433)) | dbs_b1 > (2 / sqrt(5433)) | dbs_b2 > (2 / sqrt(5433)) | dbs_gamma > (2 / sqrt(5433))),
         out = (cook_out + dfbetas_out) > 0) %>%  filter(!out) %>%
  f_seg(., psi = 130)

# extract psi
wag_psi <- wag_fit$psi[2]
wag_psi_95ci <- wag_fit$psi[2] + qnorm(c(0.025, 0.975)) * wag_fit$psi[3]

# plot fit without outliers
wag %>%
  ggplot()+
  geom_point(aes(x = cl, y = ch), color = "grey", alpha = 0.5)+
  geom_line(aes(x = cl, y = wag_fit$fitted.values), color = "red")+
  geom_vline(xintercept = c(wag_psi, wag_psi_95ci), linetype = c(1, 2, 2))+
  labs(x = "Carapace Length (mm)", y = "Chela Height (S) (mm)") -> x
ggsave("./research_projects/gkc_size_maturity/figures/wag_fit.png",
       plot = x, 
       height = 3, width = 4, units = "in")

## diagnostic plots
ggsave("./research_projects/gkc_size_maturity/figures/wag_fit_diagnostic.png",
       plot = autoplot(wag_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")

## actual cooperative survey only
### log-transformed
wag %>%
  filter(source == "CoopertiveSurvey",
         year == 2018) %>%
  mutate(x = log(cl), y = log(ch)) %>%
  f_seg(., psi = 4.7) -> wag_ln_coop_fit
summary(wag_ln_coop_fit)
# profile parameters
wag_ln_coop_fit_prof <- f_profile_seg(wag_ln_coop_fit, "./research_projects/gkc_size_maturity/figures/wag_ln_coop_fit_profile.png")

# plot fit 
wag %>%
  filter(source == "CoopertiveSurvey",
         year == 2018) %>%
  ggplot()+
  geom_point(aes(x = log(cl), y = log(ch)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = log(cl), y = wag_ln_coop_fit$fitted.values), color = "red")+
  labs(x = "Carapace Length (mm)", y = "Chela Height (S) (mm)") 

## diagnostic plots
ggsave("./research_projects/gkc_size_maturity/figures/wag_ln_coop_fit_diagnostic.png",
       plot = autoplot(wag_ln_coop_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")

### untransformed
wag %>%
  filter(source == "CoopertiveSurvey",
         year == 2018) %>%
  mutate(x = cl, y = ch) %>%
  f_seg(., psi = 125) -> wag_coop_fit

# plot fit 
wag %>%
  filter(source == "CoopertiveSurvey",
         year == 2018) %>%
  ggplot()+
  geom_point(aes(x = cl, y = ch), color = "grey", alpha = 0.5)+
  geom_line(aes(x = cl, y = wag_coop_fit$fitted.values), color = "red")+
  labs(x = "Carapace Length (mm)", y = "Chela Height (S) (mm)") 

## diagnostic plots
ggsave("./research_projects/gkc_size_maturity/figures/wag_coop_fit_diagnostic.png",
       plot = autoplot(wag_coop_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")


## 2020 special project sampling
### log-transformed
wag %>%
  filter(source == "CoopertiveSurvey",
         year == 2020) %>%
  mutate(x = log(cl), y = log(ch)) %>%
  f_seg(., psi = 4.7) -> wag_ln_specobs_fit
# profile parameters
wag_ln_specobs_fit_prof <- f_profile_seg(wag_ln_specobs_fit, "./research_projects/gkc_size_maturity/figures/wag_ln_specobs_fit_profile.png")
# plot fit 
wag %>%
  filter(source == "CoopertiveSurvey",
         year == 2020) %>%
  ggplot()+
  geom_point(aes(x = log(cl), y = log(ch)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = log(cl), y = wag_ln_specobs_fit$fitted.values), color = "red")+
  labs(x = "Carapace Length (mm)", y = "Chela Height (S) (mm)") 
## diagnostic plots
ggsave("./research_projects/gkc_size_maturity/figures/wag_ln_specobs_fit_diagnostic.png",
       plot = autoplot(wag_coop_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")

### untransformed
wag %>%
  filter(source == "CoopertiveSurvey",
         year == 2020) %>%
  mutate(x = cl, y = ch) %>%
  f_seg(., psi = 125) -> wag_specobs_fit
# plot fit 
wag %>%
  filter(source == "CoopertiveSurvey",
         year == 2020) %>%
  ggplot()+
  geom_point(aes(x = cl, y = ch), color = "grey", alpha = 0.5)+
  geom_line(aes(x = cl, y = wag_specobs_fit$fitted.values), color = "red")+
  labs(x = "Carapace Length (mm)", y = "Chela Height (S) (mm)") 
## diagnostic plots
ggsave("./research_projects/gkc_size_maturity/figures/wag_specobs_fit_diagnostic.png",
       plot = autoplot(wag_specobs_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")
