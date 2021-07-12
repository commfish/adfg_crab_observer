# notes ----
## PIGKC size at morph maturity
## Tyler Jackson
## 7/8/2021

# load ----
library(tidyverse)
library(bbmle) # for mle2 function minimizer
library(ggfortify) # for lm diagnostic plots in ggplot similar to plot.lm()
library(mclust) # for gmm and em
library(scatterplot3d) # plotting gmm assignments in 3D
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

pigkc <- read_csv("./pigkc/data/pigkc_maturity_tmp.csv") %>%
             rename_all(tolower) %>%
             dplyr::select(-15) %>%
             filter(!is.na(cl_mm),
                    !is.na(chela_h1_mm))

# eda ----
pigkc %>%
  dplyr::select(cl_mm, chela_h1_mm, chela_h2_mm, chela_w_mm, weight_g) %>%
  rename_all(~c("CL", "CH J", "CH S", "ChW", "Wt")) %>%
  GGally::ggpairs()+
  theme(axis.text = element_text(size = 6)) -> x
ggsave("./research_projects/gkc_size_maturity/figures/pigkc_corrplot.png",
       plot = x, height = 5, width = 6, units = "in")

# fit allometric model using NLL ----

## function to produce nll
f_allometric_nll <- function(x, y, ln_alpha, ln_beta, ln_sigma){
  # compute predicted y
  y_pred = exp(ln_alpha) * exp(x*exp(ln_beta))
  # compute NLL
  nll = -sum(dnorm(x = log(y + 1e-6), mean = log(y_pred + 1e-6), sd = exp(ln_sigma), log = TRUE), na.rm = T)
  return(nll)
}

## optimizer
fit <- bbmle::mle2(f_allometric_nll,
                   start = list(ln_alpha = log(1), ln_beta = log(0.5), ln_sigma = log(0.2)),
                   data = list(x = pigkc$cl_mm, y = pigkc$chela_h1_mm),
                   method = "Nelder-Mead",
                   optimizer = "nlminb",
                   control = list(maxit=1e6))

## extract paramters w/ bias correction
alpha <- exp(coef(fit)[1] + sqrt(diag(vcov(fit)))[1]/2)
beta <- exp(coef(fit)[2] + sqrt(diag(vcov(fit)))[2]/2)

## plot
pigkc %>%
  ggplot()+
  geom_point(aes(x = cl_mm, y = chela_h1_mm), color = "grey", alpha = 0.5)+
  geom_line(aes(x = cl_mm, y = alpha*exp(cl_mm*beta)), size = 1, color = 4)+
  labs(x = "CL (mm)", y = "CH Jewett (mm)")

# no transformation breakpoint ----

## fit
pigkc %>%
  rename(x = cl_mm, y = chela_h1_mm) %>%
  f_seg(., 130) -> chcl_fit
summary(chcl_fit)
chcl_prof <- f_profile_seg(chcl_fit, "./research_projects/gkc_size_maturity/figures/chcl_profile.png")

## breakpoint
bp_chcl <- chcl_fit$psi[2]
bp_chcl_95ci <- chcl_fit$psi[2] + qnorm(c(0.025,0.975))*chcl_fit$psi[3]
             
# plots
## fit
pigkc %>%
  ggplot()+
  geom_point(aes(x = cl_mm, y = chela_h1_mm), color = "grey", alpha = 0.5)+
  geom_line(aes(x = cl_mm, y = chcl_fit$fitted.values), color = 2)+
  geom_vline(xintercept = c(bp_chcl, bp_chcl_95ci), linetype = c(1, 2, 2))+
  labs(x = "Carapace Length (mm)", y = "Chela Height (Jewett) (mm)") -> x
ggsave("./research_projects/gkc_size_maturity/figures/chcl_bp_fit.png",
       plot = x, height = 3, width = 4, units = "in")

## diagnostic plots
autoplot(chcl_fit, alpha = 0.5)
ggsave("./research_projects/gkc_size_maturity/figures/chcl_bp_diagnostic.png",
       plot = autoplot(chcl_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")

## identify outliers
pigkc %>%
  dplyr::select(cl_mm, chela_h1_mm) %>%
  rename_all(~c("x", "y")) %>%
  # add cooks distance
  mutate(cook_d = cooks.distance(chcl_fit),
         cook_out = cook_d > (4 / nrow(.))) %>%
  # add dfbetas
  bind_cols(as_tibble(dfbetas(chcl_fit)) %>%
              rename_all(~c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma"))) %>%
  mutate(dfbetas_out = (dbs_b0 > (2 / sqrt(1048)) | dbs_b1 > (2 / sqrt(1048)) | dbs_b2 > (2 / sqrt(1048)) | dbs_gamma > (2 / sqrt(1048))),
         out = (cook_out + dfbetas_out) > 0) -> pigkc_out

## cooks distance plot
pigkc_out %>%
  ggplot()+
  geom_point(aes(x = rownames(pigkc_out), y = cook_d, color = cook_out), alpha = 0.5)+
  geom_hline(yintercept = 4/1048, linetype = 2)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")+
  scale_color_manual(values = c("black", "red"))+
  labs(x = NULL, y = "Cook's Distance") -> x
ggsave("./research_projects/gkc_size_maturity/figures/chcl_cooksd.png",
       plot = x, height = 3, width = 4, units = "in")

## dbetas plot
pigkc_out %>%
  dplyr::select(dbs_b0, dbs_b1, dbs_b2, dbs_gamma, dfbetas_out) %>%
  mutate(index = rownames(.)) %>%
  pivot_longer(c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma")) %>%
  mutate(dfbetas_out = abs(value) > 2/sqrt(1048)) %>%
  mutate(name = recode_factor(name, `dbs_b0` = "beta[0]", `dbs_b1` = "beta[1]", dbs_b2 = "beta[2]", dbs_gamma = "gamma[psi]")) %>%
  ggplot()+
  geom_point(aes(x = index, y = value, color = dfbetas_out), alpha = 0.5)+
  geom_hline(yintercept = c(2/sqrt(1048), -2/sqrt(1048)), linetype = 2)+
  labs(x = NULL, y = "DFBETAS")+
  facet_wrap(~name, label = "label_parsed")+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", strip.background = element_rect(color = NA))+
  scale_color_manual(values = c("black", "red")) -> x
ggsave("./research_projects/gkc_size_maturity/figures/chcl_dfbetas.png",
       plot = x, height = 5, width = 6, units = "in")

## all outliers
pigkc_out %>%
  ggplot()+
  geom_point(aes(x = x, y = y, color = out), alpha = 0.5)+
  scale_color_manual(values = c("black", "red"))+
  labs(x = "Carapace Length (mm)", y = "Chela Height (Jewett) (mm)")+
  theme(legend.position = "none")

## re-fit without outliers
pigkc_out %>%
  filter(out == F) %>%
  f_seg(., psi = 130) -> chcl_fit_no_out

## fit
pigkc %>%
  rename(x = cl_mm, y = chela_h2_mm) %>%
  f_seg(., 130) -> ch2cl_fit
summary(ch2cl_fit)
autoplot(ch2cl_fit, alpha = 0.5)
## breakpoint
bp_ch2cl <- ch2cl_fit$psi[2]
bp_ch2cl_95ci <- ch2cl_fit$psi[2] + qnorm(c(0.025,0.975))*ch2cl_fit$psi[3]

# ln x,y transformation breakpoint ----

## fit ln ch~cl
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_h1_mm)) %>%
  f_seg(., log(120)) -> ln_chcl_fit
summary(ln_chcl_fit)
ln_chcl_prof <- f_profile_seg(ln_chcl_fit, "./research_projects/gkc_size_maturity/figures/ln_chcl_profile.png")

## breakpoint
bp_chcl_ln <- ln_chcl_fit$psi[2]
bp_chcl_ln_95ci <- c(ln_chcl_fit$psi[2] + qnorm(c(0.025,0.975))*ln_chcl_fit$psi[3])
## backtransform and bias correct
bp_chcl_ln_bt <- exp(ln_chcl_fit$psi[2] + ln_chcl_fit$psi[3] / 2)
bp_chcl_ln_95ci_bt <- bp_chcl_ln_bt + qnorm(c(0.025,0.975)) * sqrt(bp_chcl_ln_bt^2*(exp(ln_chcl_fit$psi[3]^2)-1))

# plots
## fit
pigkc %>%
  ggplot()+
  geom_point(aes(x = log(cl_mm), y = log(chela_h1_mm)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = log(cl_mm), y = ln_chcl_fit$fitted.values), color = 2)+
  geom_vline(xintercept = c(bp_chcl_ln, bp_chcl_ln_95ci), linetype = c(1, 2, 2))+
  labs(x = "ln Carapace Length (mm)", y = "ln Chela Height (Jewett) (mm)") -> x
ggsave("./research_projects/gkc_size_maturity/figures/ln_chcl_bp_fit.png",
       plot = x, height = 3, width = 4, units = "in")
## diagnostic plots
autoplot(ln_chcl_fit, alpha = 0.5)
ggsave("./research_projects/gkc_size_maturity/figures/ln_chcl_bp_diagnostic.png",
       plot = autoplot(ln_chcl_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")

## identify outliers
pigkc %>%
  dplyr::select(cl_mm, chela_h1_mm) %>%
  rename_all(~c("x", "y")) %>%
  mutate_all(log) %>%
  # add cooks distance
  mutate(cook_d = cooks.distance(ln_chcl_fit),
         cook_out = cook_d > (4 / nrow(.))) %>%
  # add dfbetas
  bind_cols(as_tibble(dfbetas(ln_chcl_fit)) %>%
              rename_all(~c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma"))) %>%
  mutate(dfbetas_out = (dbs_b0 > (2 / sqrt(1048)) | dbs_b1 > (2 / sqrt(1048)) | dbs_b2 > (2 / sqrt(1048)) | dbs_gamma > (2 / sqrt(1048))),
         out = (cook_out + dfbetas_out) > 0) -> pigkc_ln_out

## cooks distance plot
pigkc_ln_out %>%
  ggplot()+
  geom_point(aes(x = rownames(pigkc_ln_out), y = cook_d, color = cook_out), alpha = 0.5)+
  geom_hline(yintercept = 4/1048, linetype = 2)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")+
  scale_color_manual(values = c("black", "red"))+
  labs(x = NULL, y = "Cook's Distance") -> x
ggsave("./research_projects/gkc_size_maturity/figures/ln_chcl_cooksd.png",
       plot = x, height = 3, width = 4, units = "in")

## dbetas plot
pigkc_ln_out %>%
  dplyr::select(dbs_b0, dbs_b1, dbs_b2, dbs_gamma, dfbetas_out) %>%
  mutate(index = rownames(.)) %>%
  pivot_longer(c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma")) %>%
  mutate(dfbetas_out = abs(value) > 2/sqrt(1048)) %>%
  mutate(name = recode_factor(name, `dbs_b0` = "beta[0]", `dbs_b1` = "beta[1]", dbs_b2 = "beta[2]", dbs_gamma = "gamma[psi]")) %>%
  ggplot()+
  geom_point(aes(x = index, y = value, color = dfbetas_out), alpha = 0.5)+
  geom_hline(yintercept = c(2/sqrt(1048), -2/sqrt(1048)), linetype = 2)+
  labs(x = NULL, y = "DFBETAS")+
  facet_wrap(~name, label = "label_parsed")+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", strip.background = element_rect(color = NA))+
  scale_color_manual(values = c("black", "red")) -> x
ggsave("./research_projects/gkc_size_maturity/figures/ln_chcl_dfbetas.png",
       plot = x, height = 5, width = 6, units = "in")

## all outliers
pigkc_ln_out %>%
  ggplot()+
  geom_point(aes(x = x, y = y, color = out), alpha = 0.5)+
  scale_color_manual(values = c("black", "red"))+
  labs(x = "Carapace Length (mm)", y = "Chela Height (Jewett) (mm)")+
  theme(legend.position = "none")

## re-fit without outliers
pigkc_ln_out %>%
  filter(out == F) %>%
  f_seg(., psi = 4.7) -> ln_chcl_fit_no_out
ln_chcl_no_out_prof <- f_profile_seg(ln_chcl_fit_no_out, "./research_projects/gkc_size_maturity/figures/ln_chcl_no_out_profile.png")

## breakpoint
bp_chcl_ln_no_out <- ln_chcl_fit_no_out$psi[2]
bp_chcl_ln_no_out_95ci <- c(ln_chcl_fit_no_out$psi[2] + qnorm(c(0.025,0.975))*ln_chcl_fit_no_out$psi[3])
## backtransform and bias correct
bp_chcl_ln_no_out_bt <- exp(ln_chcl_fit_no_out$psi[2] + ln_chcl_fit_no_out$psi[3] / 2)
bp_chcl_ln_no_out_95ci_bt <- bp_chcl_ln_no_out_bt + qnorm(c(0.025,0.975)) * sqrt(bp_chcl_ln_no_out_bt^2*(exp(ln_chcl_fit_no_out$psi[3]^2)-1))

# plot with fits of untransformed and transformed data
pigkc_ln_out %>%
  mutate(fit1 = predict(ln_chcl_fit, newdata = .),
         fit2 = predict(ln_chcl_fit_no_out, newdata = .)) %>%
  ggplot()+
  geom_point(aes(x = x, y = y, alpha = out), color = "grey")+
  geom_line(aes(x = x, y = fit1), color = "red", alpha = 0.2)+
  geom_line(aes(x = x, y = fit2), color = "red")+
  geom_vline(xintercept = c(bp_chcl_ln, bp_chcl_ln_95ci, bp_chcl_ln_no_out, bp_chcl_ln_no_out_95ci),
             linetype = c(1, 2, 2, 1, 2, 2),
             alpha = c(0.2, 0.2, 0.2, 1, 1, 1))+
  scale_alpha_manual(values = c(0.7, 0.2))+
  theme(legend.position = "none")+
  labs(x = "ln Carapace Length (mm)", y = "ln Chela Height (Jewett) (mm)") -> x
ggsave("./research_projects/gkc_size_maturity/figures/ln_chcl_bp_noout_fit.png",
       plot = x, height = 3, width = 4, units = "in")
  
# other chela measurement ln transformed breakpoint fits ----
## fit ln ch2~cl
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_h2_mm)) %>%
  f_seg(., log(130)) -> ln_ch2cl_fit
summary(ln_ch2cl_fit)

## re-fit without outliers
pigkc %>%
  dplyr::select(cl_mm, chela_h2_mm) %>%
  rename_all(~c("x", "y")) %>%
  mutate_all(log) %>%
  # add cooks distance
  mutate(cook_d = cooks.distance(ln_ch2cl_fit),
         cook_out = cook_d > (4 / nrow(.))) %>%
  # add dfbetas
  bind_cols(as_tibble(dfbetas(ln_ch2cl_fit)) %>%
              rename_all(~c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma"))) %>%
  mutate(dfbetas_out = (dbs_b0 > (2 / sqrt(1048)) | dbs_b1 > (2 / sqrt(1048)) | dbs_b2 > (2 / sqrt(1048)) | dbs_gamma > (2 / sqrt(1048))),
         out = (cook_out + dfbetas_out) > 0) %>%
  # remove outliers
  filter(out == F) %>%
  f_seg(., log(130))

## breakpoint
bp_ch2cl_ln <- ln_ch2cl_fit$psi[2]
bp_ch2cl_ln_95ci <- ln_ch2cl_fit$psi[2] + qnorm(c(0.025, 0.975))*ln_ch2cl_fit$psi[3]
## backtransform and bias correct
bp_ch2cl_ln_bt <- exp(ln_ch2cl_fit$psi[2] + ln_ch2cl_fit$psi[3] / 2)
bp_ch2cl_ln_95ci_bt <- bp_ch2cl_ln_bt + qnorm(c(0.025,0.975)) * sqrt(bp_ch2cl_ln_bt^2*(exp(ln_ch2cl_fit$psi[3]^2)-1))

# plots
## fit
pigkc %>%
  filter(!is.na(chela_h2_mm)) %>%
  ggplot()+
  geom_point(aes(x = log(cl_mm), y = log(chela_h2_mm)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = log(cl_mm), y = ln_ch2cl_fit$fitted.values), color = 2)+
  geom_vline(xintercept = c(bp_ch2cl_ln, bp_ch2cl_ln_95ci), linetype = c(1, 2, 2))+
  labs(x = "ln Carapace Length (mm)", y = "ln Chela Height (Standard) (mm)") -> p1
## diagnostic plots
autoplot(ln_ch2cl_fit, alpha = 0.5)
ggsave("./research_projects/gkc_size_maturity/figures/ln_ch2cl_bp_diagnostic.png",
       plot = autoplot(ln_ch2cl_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")


## fit ln chela width (cw)~cl
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_w_mm)) %>%
  f_seg(., log(130)) -> ln_cwcl_fit
summary(ln_cwcl_fit)
ln_cwcl_prof <- f_profile_seg(ln_cwcl_fit, "./research_projects/gkc_size_maturity/figures/ln_cwcl_profile.png")

## re-fit without outliers
pigkc %>%
  dplyr::select(cl_mm, chela_w_mm) %>%
  filter(!is.na(chela_w_mm)) %>%
  rename_all(~c("x", "y")) %>%
  mutate_all(log) %>%
  # add cooks distance
  mutate(cook_d = cooks.distance(ln_cwcl_fit),
         cook_out = cook_d > (4 / nrow(.))) %>%
  # add dfbetas
  bind_cols(as_tibble(dfbetas(ln_cwcl_fit)) %>%
              rename_all(~c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma"))) %>%
  mutate(dfbetas_out = (dbs_b0 > (2 / sqrt(1048)) | dbs_b1 > (2 / sqrt(1048)) | dbs_b2 > (2 / sqrt(1048)) | dbs_gamma > (2 / sqrt(1048))),
         out = (cook_out + dfbetas_out) > 0) %>%
  # remove outliers
  filter(out == F) %>%
  f_seg(., log(130)) 


## breakpoint
bp_cwcl_ln <- ln_cwcl_fit$psi[2]
bp_cwcl_ln_95ci <- c(ln_cwcl_fit$psi[2] + qnorm(c(0.025, 0.975))*ln_cwcl_fit$psi[3])
## backtransform and bias correct
bp_cwcl_ln_bt <- exp(ln_cwcl_fit$psi[2] + ln_cwcl_fit$psi[3] / 2)
bp_cwcl_ln_95ci_bt <- bp_cwcl_ln_bt + qnorm(c(0.025,0.975)) * sqrt(bp_cwcl_ln_bt^2*(exp(ln_cwcl_fit$psi[3]^2)-1))

# plots
## fit
pigkc %>%
  filter(!is.na(chela_w_mm)) %>%
  ggplot()+
  geom_point(aes(x = log(cl_mm), y = log(chela_w_mm)), color = "grey", alpha = 0.5)+
  #geom_smooth(aes(x = log(cl_mm), y = log(chela_w_mm)))+
  geom_line(aes(x = log(cl_mm), y = ln_cwcl_fit$fitted.values), color = 2)+
  geom_vline(xintercept = c(bp_cwcl_ln, bp_cwcl_ln_95ci), linetype = c(1, 2, 2))+
  labs(x = "ln Carapace Length (mm)", y = "ln Chela Width (mm)") -> p2
## diagnostic plots
autoplot(ln_cwcl_fit, alpha = 0.5)
ggsave("./research_projects/gkc_size_maturity/figures/ln_cwcl_bp_diagnostic.png",
       plot = autoplot(ln_cwcl_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")  

## fit ln chela width (cw)*ch~cl
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_w_mm * chela_h1_mm)) %>%
  f_seg(., log(130)) -> ln_cwchcl_fit
summary(ln_cwchcl_fit)
ln_cwchcl_prof <- f_profile_seg(ln_cwchcl_fit, "./research_projects/gkc_size_maturity/figures/ln_cwchcl_profile.png")

## re-fit without outliers
pigkc %>%
  dplyr::select(cl_mm, chela_h1_mm, chela_w_mm) %>%
  filter(!is.na(chela_w_mm)) %>%
  rename_all(~c("x", "y", "z")) %>%
  mutate(y = y * z) %>%
  mutate_all(log) %>%
  # add cooks distance
  mutate(cook_d = cooks.distance(ln_cwchcl_fit),
         cook_out = cook_d > (4 / nrow(.))) %>%
  # add dfbetas
  bind_cols(as_tibble(dfbetas(ln_cwchcl_fit)) %>%
              rename_all(~c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma"))) %>%
  mutate(dfbetas_out = (dbs_b0 > (2 / sqrt(1048)) | dbs_b1 > (2 / sqrt(1048)) | dbs_b2 > (2 / sqrt(1048)) | dbs_gamma > (2 / sqrt(1048))),
         out = (cook_out + dfbetas_out) > 0) %>%
  # remove outliers
  filter(out == F) %>%
  f_seg(., log(130)) 

## breakpoint
bp_cwchcl_ln <- ln_cwchcl_fit$psi[2]
bp_cwchcl_ln_95ci <- bp_cwchcl_ln +  qnorm(c(0.025,0.975))*ln_cwchcl_fit$psi[3]
## backtransform and bias correct
bp_cwchcl_ln_bt <- exp(ln_cwchcl_fit$psi[2] + ln_cwchcl_fit$psi[3] / 2)
bp_cwchcl_ln_95ci_bt <- bp_cwchcl_ln_bt + qnorm(c(0.025,0.975)) * sqrt(bp_cwchcl_ln_bt^2*(exp(ln_chclcl_fit$psi[3]^2)-1))

# plots
## fit
pigkc %>%
  filter(!is.na(chela_w_mm)) %>%
  ggplot()+
  geom_point(aes(x = log(cl_mm), y = log(chela_w_mm * chela_h1_mm)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = log(cl_mm), y = ln_cwchcl_fit$fitted.values), color = 2)+
  geom_vline(xintercept = c(bp_cwchcl_ln, bp_cwchcl_ln_95ci), linetype = c(1, 2, 2))+
  labs(x = "ln Carapace Length (mm)", y = expression("ln ChW x CH (J) ("~mm^2~")")) -> p3
## diagnostic plots
autoplot(ln_cwchcl_fit, alpha = 0.5)
ggsave("./research_projects/gkc_size_maturity/figures/ln_cwchcl_bp_diagnostic.png",
       plot = autoplot(ln_cwchcl_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in")  


## fit ln chela height/cl~cl
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_h1_mm / cl_mm)) %>%
  f_seg(., log(130)) -> ln_chclcl_fit
summary(ln_chclcl_fit)
ln_chclcl_prof <- f_profile_seg(ln_chclcl_fit, "./research_projects/gkc_size_maturity/figures/ln_chclcl_profile.png")

## re-fit without outliers
pigkc %>%
  dplyr::select(cl_mm, chela_h1_mm) %>%
  rename_all(~c("x", "y")) %>%
  mutate(y = y / x) %>%
  mutate_all(log) %>%
  # add cooks distance
  mutate(cook_d = cooks.distance(ln_chclcl_fit),
         cook_out = cook_d > (4 / nrow(.))) %>%
  # add dfbetas
  bind_cols(as_tibble(dfbetas(ln_chclcl_fit)) %>%
              rename_all(~c("dbs_b0", "dbs_b1", "dbs_b2", "dbs_gamma"))) %>%
  mutate(dfbetas_out = (dbs_b0 > (2 / sqrt(1048)) | dbs_b1 > (2 / sqrt(1048)) | dbs_b2 > (2 / sqrt(1048)) | dbs_gamma > (2 / sqrt(1048))),
         out = (cook_out + dfbetas_out) > 0) %>%
  # remove outliers
  filter(out == F) %>%
  f_seg(., log(130)) -> ln_chclcl_fit
summary(ln_chclcl_fit)

## breakpoint
bp_chclcl_ln <- ln_chclcl_fit$psi[2]
bp_chclcl_ln_95ci <- bp_chclcl_ln + qnorm(c(0.025,0.975)) * ln_chclcl_fit$psi[3]
## backtransform and bias correct
bp_chclcl_ln_bt <- exp(ln_chclcl_fit$psi[2] + ln_chclcl_fit$psi[3] / 2)
bp_chclcl_ln_95ci_bt <- bp_chclcl_ln_bt + qnorm(c(0.025,0.975)) * sqrt(bp_chclcl_ln_bt^2*(exp(ln_chclcl_fit$psi[3]^2)-1))


# plots
## fit
pigkc %>%
  ggplot()+
  geom_point(aes(x = log(cl_mm), y = log(chela_h1_mm/cl_mm)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = log(cl_mm), y = ln_chclcl_fit$fitted.values), color = 2)+
  geom_vline(xintercept = c(bp_chclcl_ln, bp_chclcl_ln_95ci), linetype = c(1, 2, 2))+
  labs(x = "ln Carapace Length (mm)", y = "ln CH (J) / CL") -> p4
## diagnostic plots
autoplot(ln_chclcl_fit, alpha = 0.5)
ggsave("./research_projects/gkc_size_maturity/figures/ln_chclcl_bp_diagnostic.png",
       plot = autoplot(ln_chclcl_fit, alpha = 0.5, colour = "grey", label.size = 0), 
       height = 6, width = 7, units = "in") 


# all data plots
ggsave("./research_projects/gkc_size_maturity/figures/pigkc_ln_seg.png",
       plot = cowplot::plot_grid(p1, p2, p3, p4, nrow = 2), 
       height = 6, width = 7, units = "in")

# linear fit to continuous allometry ----

## fit ln ch~cl
pigkc %>%
  mutate(x = cl_mm, y = log(chela_h1_mm)) %>%
  lm(y~x, data = .) -> allo_fit
summary(allo_fit)
 
# plots
## fit
pigkc %>%
  ggplot()+
  geom_point(aes(x = cl_mm, y = log(chela_h1_mm)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = cl_mm, y = allo_fit$fitted.values), color = 2)+
  labs(x = "Carapace Length (mm)", y = "ln Chela Height (mm)") -> x
ggsave("./research_projects/gkc_size_maturity/figures/exponential_fit.png",
       plot = x, height = 3, width = 4, units = "in")

## diagnostic plots
autoplot(allo_fit, alpha = 0.5, which = 1, ncol = 1, label.size = 0, colour = "grey") +  ggtitle(label = NULL) -> x
ggsave("./research_projects/gkc_size_maturity/figures/exponential_resid.png",
       plot = x, height = 3, width = 4, units = "in")

## fit ln ch~cl
pigkc %>%
  mutate(x = cl_mm, y = log(chela_h1_mm*chela_w_mm)) %>%
  lm(y~x, data = .) -> allo_fit2
summary(allo_fit2)

# plots
## fit
pigkc %>%
  filter(!is.na(chela_w_mm)) %>%
  ggplot()+
  geom_point(aes(x = cl_mm, y = log(chela_h1_mm*chela_w_mm)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = cl_mm, y = allo_fit2$fitted.values), color = 2)+
  #geom_smooth(aes(x = cl_mm, y = log(chela_h1_mm)))+
  labs(x = "Carapace Length (mm)", y = "ln Chela Height (mm)")

## diagnostic plots
autoplot(allo_fit2, alpha = 0.5, 1)



# fit two lines ----

## custom function to implement method of Somerton 1980
f_2lm <- function(data, x, y, juv_max_x, mat_min_x, maxit = 1000){
  
  # assign individuals in the middle range
  data %>%
    rename(x = as.name(x),
           y = as.name(y)) %>%
    filter(x > juv_max_x,
           x < mat_min_x) -> mid
  
  data %>%
    rename(x = as.name(x),
           y = as.name(y)) %>%
    filter(x <= juv_max_x | x >= mat_min_x) %>%
    mutate(mature = ifelse(x <= juv_max_x, 0, 1)) -> data2
  
  
  assigned_dat = list()
  for(i in 1:maxit){
    # juvenile and mature lines
    if(i == 1) {
      juv_lm = lm(y~x, data2, subset = mature == 0)
      mat_lm = lm(y~x, data2, subset = mature == 1)
    } else {
      juv_lm  = lm(y~x, assigned_dat[[i-1]], subset = mature == 0)
      mat_lm  = lm(y~x, assigned_dat[[i-1]], subset = mature == 1)
    }
    
    mid %>%
      # juvenile residuals, mature residuals, assign point to lines
      mutate(juv_r = y - predict.lm(juv_lm, newdata = .),
             mat_r = y - predict.lm(mat_lm, newdata = .),
             mature = ifelse(abs(juv_r) < abs(mat_r), 0, 1)) -> tmp
    
    data2 %>%
      bind_rows(tmp %>%
                  dplyr::select(x, y, mature)) -> assigned_dat[[i]]
    
    if(i >= 3){
      
      if(sum((assigned_dat[[i]] %>% dplyr::select(x,y,mature) == assigned_dat[[i-1]] %>% dplyr::select(x,y,mature)) & (assigned_dat[[i]] %>% dplyr::select(x,y,mature) == assigned_dat[[i-2]] %>% dplyr::select(x,y,mature))) == prod(dim(assigned_dat[[i]] %>% dplyr::select(x,y,mature)))){
        break
      }
    }
  }
  
  ## compute F stat
  # get rss of model with 1 line
  rss_1line = sum(lm(y~x,assigned_dat[[i]])$residuals^2)
  # get rss of model with 2 lines
  juv_lm = lm(y~x, assigned_dat[[i]], subset = mature == 0)
  mat_lm = lm(y~x, assigned_dat[[i]], subset = mature == 1)
  rss_2line = sum(juv_lm$residuals^2) + sum(mat_lm$residuals^2)
  # F stat
  stat = ((rss_1line - rss_2line) / 2) / ((rss_2line / (nrow(assigned_dat[[i]]) - 4)))
  
  # go back to original variable names
  names(assigned_dat[[i]]) <- gsub("x", x, names(assigned_dat[[i]]))
  names(assigned_dat[[i]]) <- gsub("y", y, names(assigned_dat[[i]]))
  
  # output
  out = list(assigned_data = assigned_dat[[i]],
             juvenile_lm = juv_lm,
             mature_lm = mat_lm,
             statistic = stat,
             p_value = pf(stat, 2, nrow(assigned_dat[[i]]) - 4, lower.tail = F),
             iterations = i)
  return(out)
}


## fit the two line methods with three sets of threshold values
## set 1
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_h1_mm)) %>%
  f_2lm(., "x", "y", 4.5, 5.1) -> ln_2line_fit_1

ggplot()+
  geom_point(data = ln_2line_fit_1$assigned_dat, aes(x = x, y = y, color = factor(mature)), alpha = 0.5)+
  geom_line(aes(x = ln_2line_fit_1$juvenile_lm$model$x, y = ln_2line_fit_1$juvenile_lm$fitted.values))+
  geom_line(aes(x = ln_2line_fit_1$mature_lm$model$x, y = ln_2line_fit_1$mature_lm$fitted.values))+
  geom_vline(xintercept = c(4.5, 5.1), color = "grey80", linetype = 2)+
  labs(x = "Carapce Length (mm)", y = "Chela Height (Jewett) (mm)", color = NULL)+
  scale_color_discrete(labels = c("immature", "mature"))+
  theme(legend.position = c(0.25,0.9)) -> p1
## set 2
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_h1_mm)) %>%
  f_2lm(., "x", "y", 4.7, 5.0) -> ln_2line_fit_2
ggplot()+
  geom_point(data = ln_2line_fit_2$assigned_dat, aes(x = x, y = y, color = factor(mature)), alpha = 0.5)+
  geom_line(aes(x = ln_2line_fit_2$juvenile_lm$model$x, y = ln_2line_fit_2$juvenile_lm$fitted.values))+
  geom_line(aes(x = ln_2line_fit_2$mature_lm$model$x, y = ln_2line_fit_2$mature_lm$fitted.values))+
  geom_vline(xintercept = c(4.7, 5.0), color = "grey80", linetype = 2)+
  labs(x = "Carapce Length (mm)", y = "Chela Height (Jewett) (mm)", color = NULL)+
  scale_color_discrete(labels = c("immature", "mature"))+
  theme(legend.position = c(0.25,0.9)) -> p2
## set 3
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_h1_mm)) %>%
  f_2lm(., "x", "y", 4.6, 4.9) -> ln_2line_fit_3
ggplot()+
  geom_point(data = ln_2line_fit_3$assigned_dat, aes(x = x, y = y, color = factor(mature)), alpha = 0.5)+
  geom_line(aes(x = ln_2line_fit_3$juvenile_lm$model$x, y = ln_2line_fit_3$juvenile_lm$fitted.values))+
  geom_line(aes(x = ln_2line_fit_3$mature_lm$model$x, y = ln_2line_fit_3$mature_lm$fitted.values))+
  geom_vline(xintercept = c(4.6, 4.9), color = "grey80", linetype = 2)+
  labs(x = "Carapce Length (mm)", y = "Chela Height (Jewett) (mm)", color = NULL)+
  scale_color_discrete(labels = c("immature", "mature"))+
  theme(legend.position = c(0.25,0.9)) -> p3

cowplot::plot_grid(p1, p2, p3, nrow = 1) -> x
ggsave("./research_projects/gkc_size_maturity/figures/pigkc_2linefit.png",
       plot = x, height = 3, width = 7, units = "in")

## optimize the thresholds for known juvenile/mature
# f_objective = function(data, x, y, par, maxit){
#   
#   # unlist par
#   juv_max_x = max(par[1], min(data[,x]))
#   mat_min_x = min(par[2], max(data[,x]))
#   
#   penal2 = ifelse((par[2] >  max(data[,x])) | (par[1] < min(data[,x])), 100000, 1)
#   
#   # fit model
#   fit = f_2lm(data, x, y, juv_max_x, mat_min_x, maxit)
#   
#   # penalty on slope difference
#   mat_y = juv_max_x * fit$mature_lm$coefficients[2] + fit$mature_lm$coefficients[1]
#   juv_y = juv_max_x * fit$juvenile_lm$coefficients[2] + fit$juvenile_lm$coefficients[1]
#   penal = ifelse(mat_y > juv_y, 1, 1000)
#   
#   # objective function
#   obj_fxn = as.numeric((sum(fit$juvenile_lm$residuals^2) + sum(fit$mature_lm$residuals^2)) * penal * penal2)
#   
#   return(obj_fxn)
# }    

# # optimizer
# try =  optim(par = c(45, 120), 
#              fn = f_objective,
#              data = tmp,
#              x = x,
#              y = y, 
#              maxit = maxit,
#              #method="L-BFGS-B",
#              #lower = c(min(data[,x])*1.1, min(data[,x])*1.1),
#              #upper = c(max(data[,x])*0.9, max(data[,x])*0.9),
#              hessian=FALSE,
#              control=list(trace=TRUE, maxit=1e4))




# gmm ----
## select dataset
pigkc %>%
  dplyr::select(cl_mm, chela_h1_mm, chela_w_mm) %>%
  filter(complete.cases(.)) -> pigkc_gmm
## initial gmm parameterizations
init <- mclustBIC(pigkc_gmm)
## run em algorithm
em_mod <- Mclust(pigkc_gmm, x = init, G = 2)
summary(em_mod)

as_tibble(em_mod$z) %>%
  rename_all(~c("imm", "mat")) %>%
  pivot_longer(1:2, names_to = "class", values_to = "prob")%>%
  ggplot()+
  geom_histogram(aes(x = prob))+
  facet_wrap(~class)

# join assignments to data
pigkc_gmm %>%
  mutate(maturity = em_mod$classification,
         maturity = maturity - 1) -> pigkc_gmmfit
  
# density plots by measure
ggplot(pigkc_gmmfit)+
  geom_density(aes(x = cl_mm, fill = factor(maturity)), alpha = 0.3)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("Immature", "Mature"))+
  labs(x = "Carapace Length (mm)", y = "Density", fill = NULL)+
  theme(legend.position = c(0.3, 0.91))+
  coord_cartesian(expand = 0) -> p1
ggplot(pigkc_gmmfit)+
  geom_density(aes(x = chela_h1_mm, fill = factor(maturity)), alpha = 0.3)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("Immature", "Mature"))+
  labs(x = "Chela Height (mm)", y = "Density", fill = NULL)+
  theme(legend.position = "none")+
  coord_cartesian(expand = 0) -> p2
ggplot(pigkc_gmmfit)+
  geom_density(aes(x = chela_w_mm, fill = factor(maturity)), alpha = 0.3)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("Immature", "Mature"))+
  labs(x = "Chela Width (mm)", y = "Density", fill = NULL)+
  theme(legend.position = "none")+
  coord_cartesian(expand = 0) -> p3

ggsave("./research_projects/gkc_size_maturity/figures/pigkc_gmm_marginal_dist.png",
       plot = cowplot::plot_grid(p1, p2, p3, nrow = 1),
       height = 3, width = 7, units = "in")

## 3d scatter plot of classification
colors <- c("#E69F00", "#56B4E9")
colors <- colors[as.numeric(em_mod$classification)]
png("./research_projects/gkc_size_maturity/figures/pigkc_gmm_3d.png", res = 380, height = 4.5, width = 5, units = "in")
scatterplot3d(pigkc_gmm, pch = 16, color = alpha(colors, 0.3), angle = 30,
              xlab = "CL (mm)", ylab = "CH (J) (mm)", zlab = "ChW (mm)", y.margin.add = T)
dev.off()
  
## fit binomial regression
glm(maturity ~ cl_mm, family = "binomial", data = pigkc_gmmfit) -> bimod
## point est
sm50_est <- f_sm50(bimod)
## run bootstrap  ci
set.seed(5374)
pigkc_gmmfit %>%
  nest() %>%
  mutate(boot_samp = purrr::map(data, ~rsample::bootstraps(., 1000))) %>%
  unnest(boot_samp) %>%
  mutate(models = purrr::map(splits, .f = function(x){glm(maturity ~ cl_mm, family = "binomial", data = x)}),
         sm50 = map_dbl(models, f_sm50)) %>%
  pull(sm50) -> sm50_dist
sm50_95ci <- quantile(sm50_dist, c(0.0275, 0.95))
## plot
pigkc_gmmfit %>%
  ggplot()+
  geom_point(aes(x = cl_mm, y = maturity), alpha = 0.5, color = "grey", shape = 124)+
  geom_line(aes(x = cl_mm, y = predict(bimod, type = "response")), 
            color = "blue")+
  labs(x = "Carapce Length (mm)", y = "Probability Morph. Mature") -> x

ggsave("./research_projects/gkc_size_maturity/figures/pigkc_gmm_logistic_fit.png",
       plot = x, height = 3, width = 4, units = "in")


## gmm clustering with skewed data
### heavy right skew
pigkc_gmm_sim1 <- list()
gmm_sim1 <- list()
sim1_bimods <- list()
sim1_sm50 <- NULL
for (i in 1:100) {
  pigkc_gmm %>%
    arrange(cl_mm) %>%
    sample_n(size = nrow(.), replace = T,
             weight = sort(rlnorm(nrow(.), meanlog = 0, sdlog = 1), decreasing = F)) -> pigkc_gmm_sim
  pigkc_gmm_sim1[[i]] <- pigkc_gmm_sim
  pigkc_gmm_sim %>%
    ## initial gmm parameterizations
    mclustBIC() %>%
    ## run em algorithm
    Mclust(pigkc_gmm_sim, x = ., G = 2) -> gmm_sim
  gmm_sim1[[i]] <- gmm_sim
  pigkc_gmm_sim %>%
    mutate(maturity = gmm_sim$classification,
           maturity = maturity - 1) %>%
    glm(maturity ~ cl_mm, family = "binomial", data = .) -> bimod_sim
  sim1_sm50[i] <- f_sm50(bimod_sim)
  sim1_bimods[[i]] <- bimod_sim
  print(i)
}
# plot assignments
for (i in 1:100){
  sim1_bimods[[i]]$data %>%
    ggplot()+
    geom_point(aes(x = log(cl_mm), y = log(chela_h1_mm), color = factor(maturity)), alpha = 0.3) -> x
  ggsave(paste0("./research_projects/gkc_size_maturity/figures/right_skew/plot", i, ".png"),
         plot = x, height = 3, width = 4, units = "in")
}

# example distribution of CL
pigkc_gmm_sim1[[i]] %>%
  ggplot()+
  geom_density(aes(x = cl_mm), fill = "purple", alpha = 0.3)+
  labs(x = "Carapace Length (mm)", y = "Density")+
  coord_cartesian(expand = 0) -> hsim1

tibble(sm50 = sim1_sm50) %>%
  ggplot()+
  geom_histogram(aes(x = sm50), binwidth = 1, fill = "purple", color = "black", alpha = 0.5)+
  labs(x = "Size at 50% Maturity", y = "Count") -> hsm50sim1



### heavy left skew
pigkc_gmm_sim2 <- list()
gmm_sim2 <- list()
sim2_bimods <- list()
sim2_sm50 <- NULL
for (i in 1:100) {
  pigkc_gmm %>%
    arrange(cl_mm) %>%
    sample_n(size = nrow(.), replace = T,
             weight = sort(rlnorm(nrow(.), meanlog = 0, sdlog = 1), decreasing = T)) -> pigkc_gmm_sim
  pigkc_gmm_sim2[[i]] <- pigkc_gmm_sim
  pigkc_gmm_sim %>%
    ## initial gmm parameterizations
    mclustBIC() %>%
    ## run em algorithm
    Mclust(pigkc_gmm_sim, x = ., G = 2) -> gmm_sim
  gmm_sim2[[i]] <- gmm_sim
  pigkc_gmm_sim %>%
    mutate(maturity = gmm_sim$classification,
           maturity = maturity - 1) %>%
    glm(maturity ~ cl_mm, family = "binomial", data = .) -> bimod_sim
  sim2_sm50[i] <- f_sm50(bimod_sim)
  sim2_bimods[[i]] <- bimod_sim
  print(i)
}
# plot assignments
for (i in 1:100){
  sim2_bimods[[i]]$data %>%
    ggplot()+
    geom_point(aes(x = log(cl_mm), y = log(chela_h1_mm), color = factor(maturity)), alpha = 0.3) -> x
  ggsave(paste0("./research_projects/gkc_size_maturity/figures/left_skew/plot", i, ".png"),
         plot = x, height = 3, width = 4, units = "in")
}


# example distribution of CL
pigkc_gmm_sim2[[i]] %>%
  ggplot()+
  geom_density(aes(x = cl_mm), fill = "yellow", alpha = 0.3)+
  labs(x = "Carapace Length (mm)", y = "Density")+
  coord_cartesian(expand = 0) -> hsim2
tibble(sm50 = sim2_sm50) %>%
  ggplot()+
  geom_histogram(aes(x = sm50), binwidth = 1, fill = "yellow", color = "black", alpha = 0.5)+
  labs(x = "Size at 50% Maturity", y = "Count") -> hsm50sim2


ggsave("./research_projects/gkc_size_maturity/figures/pigkc_gmm_sim_cl_density.png",
       plot = cowplot::plot_grid(ggplot()+
                                   geom_density(data = pigkc_gmm, aes(x = cl_mm), fill = "grey", alpha = 0.5)+
                                   coord_cartesian(expand = 0)+
                                   labs(x = "Carapace Length (mm)", y = "Density"),
                                 hsim2, hsim1, 
                                 nrow = 1), 
       height = 3, width = 7, units = "in")


ggsave("./research_projects/gkc_size_maturity/figures/pigkc_gmm_sim_sm50.png",
       plot = cowplot::plot_grid(ggplot()+
                                   geom_histogram(data = tibble(sm50 = sm50_dist), aes(x = sm50), binwidth = 0.2, color = "black", fill = "grey", alpha = 0.5)+
                                   coord_cartesian(expand = 0)+
                                   labs(x = "Size at 50% Maturity", y = "Count"),
                                 hsm50sim2, hsm50sim1, 
                                 nrow = 1), 
       height = 3, width = 7, units = "in")





# examine sample size with segmented regression ----

## compute segmented regression over a range of sample sizes, untransformed data
nsize <- seq(200, 1048, by = 8)
models <- list()
mod <- list()
for(i in 1:length(nsize)){
  for (j in 1:20){
    pigkc %>%
      mutate(x = cl_mm, y = chela_h1_mm) %>%
      sample_n(size = nsize[i], replace = F) %>%
      f_seg(., 130) -> mod[[j]]
  }
  models[[i]] = mod
  print(i)
}
tibble(nsize = nsize,
       fits = models) %>%
  unnest(fits) %>%
  # extract psi 
  mutate(psi = purrr::map_dbl(fits, function(x){x$psi[2]}),
         psi_se = purrr::map_dbl(fits, function(x){x$psi[3]})) -> untrans_nsamp
untrans_nsamp %>%
  group_by(nsize) %>%
  summarise(range = max(psi) - min(psi)) %>%
  filter(range <= 5) %>%
  arrange(-range)

# psi plot
untrans_nsamp %>%
  ggplot()+
  geom_point(aes(x = nsize, y = psi), alpha = 0.3)+
  ylab(~ paste(psi))+
  xlab("Sample Size") -> p1
# psi se plot
untrans_nsamp %>%
  ggplot()+
  geom_point(aes(x = nsize, y = psi_se), alpha = 0.3)+
  ylab(~ paste(sigma[psi]))+
  xlab("Sample Size") -> p2
ggsave("./research_projects/gkc_size_maturity/figures/untrans_samp_size.png",
       plot = cowplot::plot_grid(p1, p2, nrow = 2), height = 6, width = 6, units = "in")

## compute segmented regression over a range of sample sizes, transformed data
for(i in 1:length(nsize)){
  for (j in 1:20){
    pigkc %>%
      mutate(x = log(cl_mm), y = log(chela_h1_mm)) %>%
      sample_n(size = nsize[i], replace = F) %>%
      f_seg(., 4.7) -> mod[[j]]
  }
  models[[i]] = mod
  print(i)
}
# plot
tibble(nsize = nsize,
       fits = models) %>%
  unnest(fits) %>%
  # extract psi 
  mutate(psi = purrr::map_dbl(fits, function(x){x$psi[2]}),
         psi_se = purrr::map_dbl(fits, function(x){x$psi[3]})) -> trans_nsamp

trans_nsamp %>%
  filter(nsize > 950) %>%
  print(n = 100)

# psi plot
trans_nsamp %>%
  ggplot()+
  geom_point(aes(x = nsize, y = psi), alpha = 0.3)+
  ylab(~ paste(psi))+
  xlab("Sample Size") -> p1
# psi se plot
trans_nsamp %>%
  ggplot()+
  geom_point(aes(x = nsize, y = psi_se), alpha = 0.3)+
  ylab(~ paste(sigma[psi]))+
  xlab("Sample Size") -> p2
ggsave("./research_projects/gkc_size_maturity/figures/trans_samp_size.png",
       plot = cowplot::plot_grid(p1, p2, nrow = 2), height = 6, width = 6, units = "in")

## compute segmented regression over a range of sample sizes, transformed data without outliers
nsize <- seq(200, nrow(pigkc_ln_out), by = 8)
models <- list()
mod <- list()
for(i in 1:length(nsize)){
  for (j in 1:20){
    pigkc_ln_out %>%
      filter(out == F) %>%
      sample_n(size = nsize[i], replace = F) %>%
      f_seg(., 4.7) -> mod[[j]]
  }
  models[[i]] = mod
  print(i)
}
# plot
tibble(nsize = nsize[1:98],
       fits = models) %>%
  unnest(fits) %>%
  # extract psi 
  mutate(psi = purrr::map_dbl(fits, function(x){x$psi[2]}),
         psi_se = purrr::map_dbl(fits, function(x){x$psi[3]})) -> trans_nsamp_no_out
# psi plot
trans_nsamp_no_out %>%
  ggplot()+
  geom_point(aes(x = nsize, y = psi), alpha = 0.3)+
  ylab(~ paste(psi))+
  xlab("Sample Size")




for(i in 1:length(nsize)){
  for (j in 1:20){
    pigkc %>%
      mutate(x = log(cl_mm), y = log(chela_h2_mm)) %>%
      sample_n(size = nsize[i], replace = F) %>%
      f_seg(., 4.7) -> mod[[j]]
  }
  models[[i]] = mod
  print(i)
}
# plot
tibble(nsize = nsize,
       fits = models) %>%
  unnest(fits) %>%
  # extract psi 
  mutate(psi = purrr::map_dbl(fits, function(x){x$psi[2]}),
         psi_se = purrr::map_dbl(fits, function(x){x$psi[3]})) -> trans_nsamp

trans_nsamp %>%
  filter(nsize > 950) %>%
  print(n = 100)

# psi plot
trans_nsamp %>%
  ggplot()+
  geom_point(aes(x = nsize, y = psi), alpha = 0.3)+
  ylab(~ paste(psi))+
  xlab("Sample Size") -> p1
# psi se plot
trans_nsamp %>%
  ggplot()+
  geom_point(aes(x = nsize, y = psi_se), alpha = 0.3)+
  ylab(~ paste(sigma[psi]))+
  xlab("Sample Size") -> p2
ggsave("./research_projects/gkc_size_maturity/figures/trans_samp_size.png",
       plot = cowplot::plot_grid(p1, p2, nrow = 2), height = 6, width = 6, units = "in")
# bootstrap ch~cl with skew distributions and evaluate sgemented regression fit ----

##bootstrap resample with skew distributions (same as GMM procedure)
### heavy right skew
pigkc_sim1 <- list()
seg_sim1 <- list()
psi_sim1 <- NULL
psi_se_sim1 <- NULL
for (i in 1:100) {
  pigkc %>%
    arrange(cl_mm) %>%
    sample_n(size = nrow(.), replace = T,
             weight = sort(rlnorm(nrow(.), meanlog = 0, sdlog = 1), decreasing = F)) -> pigkc_sim
  pigkc_sim1[[i]] <- pigkc_sim
  pigkc_sim %>%
    mutate(x = cl_mm, y = chela_h1_mm) %>%
    f_seg(., psi = 130) -> seg_sim
  seg_sim1[[i]] <- seg_sim
  psi_sim1[i] <- seg_sim$psi[2]
  psi_se_sim1[i] <- seg_sim$psi[3]
  print(i)
}
# plot cl distribution
ggplot(pigkc_sim1[[i]]) +
  geom_density(aes(x = cl_mm), fill = "purple", alpha = 0.5)+
  labs(x = "Carapace Length (mm)", y = "Density")+
  coord_cartesian(expand = 0) -> p1
# plot distribution of psi
ggplot()+
  geom_histogram(aes(x = psi_sim1), binwidth = 1, color = "grey40", fill = "purple", alpha = 0.5)+
  xlab(~paste(psi))+
  ylab("Frequency")+
  geom_vline(xintercept = c(133.4, 130.2, 136.7), linetype = c(1, 2, 2))+
  coord_cartesian(expand = 0, xlim = c(109, 141)) -> x1
# plot distribution of psi se
ggplot()+
  geom_density(aes(x = psi_se_sim1))


### heavy left skew
pigkc_sim2 <- list()
seg_sim2 <- list()
psi_sim2 <- NULL
psi_se_sim2 <- NULL
for (i in 1:100) {
  pigkc %>%
    arrange(cl_mm) %>%
    sample_n(size = nrow(.), replace = T,
             weight = sort(rlnorm(nrow(.), meanlog = 0, sdlog = 1), decreasing = T)) -> pigkc_sim
  pigkc_sim2[[i]] <- pigkc_sim
  pigkc_sim %>%
    mutate(x = cl_mm, y = chela_h1_mm) %>%
    f_seg(., psi = 130) -> seg_sim
  seg_sim2[[i]] <- seg_sim
  psi_sim2[i] <- seg_sim$psi[2]
  psi_se_sim2[i] <- seg_sim$psi[3]
  print(i)
}
# plot cl distribution
ggplot(pigkc_sim2[[i]])+
  geom_density(aes(x = cl_mm), fill = "yellow", alpha = 0.5)+
  labs(x = "Carapace Length (mm)", y = "Density")+
  coord_cartesian(expand = 0) -> p2

# plot distribution of psi
ggplot()+
  geom_histogram(aes(x = psi_sim2), binwidth = 1, color = "grey40", fill = "yellow", alpha = 0.5)+
  xlab(~paste(psi))+
  ylab("Frequency")+
  geom_vline(xintercept = c(133.4, 130.2, 136.7), linetype = c(1, 2, 2))+
  coord_cartesian(expand = 0, xlim = c(109, 141)) -> x2
# plot distribution of psi se
ggplot()+
  geom_density(aes(x = psi_se_sim2))

# plot fit to data
for(i in 1:100){
  ggplot(pigkc_sim2[[i]]) +
    geom_point(aes(x = cl_mm, y = chela_h1_mm), color = "grey", alpha = 0.3)+
    geom_line(aes(x = cl_mm, y = seg_sim2[[i]]$fitted.values))+
    geom_vline(xintercept = psi_sim2[i], linetype = 2)+
    labs(x = "Carapce Length", y = "Chela Height (Jewett)") -> x
  ggsave(paste0("./research_projects/gkc_size_maturity/figures/seg_left_skew/p", i, ".png"), plot = x, height = 3, width = 3, units = "in")
}


## plots for document
ggsave("./research_projects/gkc_size_maturity/figures/boot_skew_cl.png", 
       plot = cowplot::plot_grid(ggplot(pigkc)+
                                   geom_density(aes(x = cl_mm), fill = "grey40", alpha = 0.5)+
                                   labs(x = "Carapace Length (mm)", y = "Density")+
                                   coord_cartesian(expand = 0),
                                 p1, p2, nrow = 1), 
       height = 3, width = 7, units = "in")

ggsave("./research_projects/gkc_size_maturity/figures/boot_skew_psi.png", 
       plot = cowplot::plot_grid(x1, x2, nrow = 1), 
       height = 3, width = 6, units = "in")






##bootstrap resample with skew distributions (same as GMM procedure)
### heavy right skew
pigkc_sim1 <- list()
seg_sim1 <- list()
psi_sim1 <- NULL
psi_se_sim1 <- NULL
for (i in 1:100) {
  pigkc %>%
    arrange(cl_mm) %>%
    sample_n(size = nrow(.), replace = T,
             weight = sort(rlnorm(nrow(.), meanlog = 0, sdlog = 1), decreasing = F)) -> pigkc_sim
  pigkc_sim1[[i]] <- pigkc_sim
  pigkc_sim %>%
    mutate(x = log(cl_mm), y = log(chela_h1_mm)) %>%
    f_seg(., psi = log(133)) -> seg_sim
  seg_sim1[[i]] <- seg_sim
  if(length(seg_sim$coef) == 4) {psi_sim1[i] <- seg_sim$psi[2]} else{psi_sim1[i] <- NA}
  if(length(seg_sim$coef) == 4) {psi_se_sim1[i] <- seg_sim$psi[3]} else{psi_sim1[i] <- NA}
  print(i)
}
# plot cl distribution
ggplot(pigkc_sim1[[i]]) +
  geom_density(aes(x = log(cl_mm)), fill = "yellow", alpha = 0.5)+
  labs(x = "Carapace Length (mm)", y = "Density")+
  coord_cartesian(expand = 0) -> p1


ggplot(pigkc_sim1[[i]]) +
  geom_point(aes(log(cl_mm), log(chela_h1_mm)))+
  geom_line(aes(log(cl_mm), seg_sim1[[i]]$fitted.values))
# plot distribution of psi
ggplot()+
  geom_histogram(aes(x = psi_sim1), color = "grey40", fill = "yellow", alpha = 0.5)+
  xlab(~paste(psi))+
  ylab("Frequency")+
  geom_vline(xintercept = c(4.88, 4.84, 4.91), linetype = c(1, 2, 2))+
  coord_cartesian(expand = 0, xlim = c(4.5, 5)) -> x1
# plot distribution of psi se
ggplot()+
  geom_density(aes(x = psi_se_sim1))

### heavy left skew
pigkc_sim2 <- list()
seg_sim2 <- list()
psi_sim2 <- NULL
psi_se_sim2 <- NULL
for (i in 1:100) {
  pigkc %>%
    arrange(cl_mm) %>%
    sample_n(size = nrow(.), replace = T,
             weight = sort(rlnorm(nrow(.), meanlog = 0, sdlog = 1), decreasing = T)) -> pigkc_sim
  pigkc_sim2[[i]] <- pigkc_sim
  pigkc_sim %>%
    mutate(x = log(cl_mm), y = log(chela_h1_mm)) %>%
    f_seg(., psi = log(130)) -> seg_sim
  seg_sim2[[i]] <- seg_sim
  if(length(seg_sim$coef) == 4) {psi_sim2[i] <- seg_sim$psi[2]} else{psi_sim2[i] <- NA}
  if(length(seg_sim$coef) == 4) {psi_se_sim2[i] <- seg_sim$psi[3]} else{psi_sim2[i] <- NA}
  print(i)
}
# plot cl distribution
ggplot(pigkc_sim2[[i]]) +
  geom_density(aes(x = log(cl_mm)), fill = "purple", alpha = 0.5)+
  labs(x = "Carapace Length (mm)", y = "Density")+
  coord_cartesian(expand = 0) -> p2
# plot distribution of psi
ggplot()+
  geom_histogram(aes(x = psi_sim2), color = "grey40", fill = "purple", alpha = 0.5)+
  xlab(~paste(psi))+
  ylab("Frequency")+
  geom_vline(xintercept = c(4.88, 4.84, 4.91), linetype = c(1, 2, 2))+
  coord_cartesian(expand = 0, xlim = c(4.5, 5)) -> x2
# plot distribution of psi se
ggplot()+
  geom_density(aes(x = psi_se_sim2))

## plots for document
ggsave("./research_projects/gkc_size_maturity/figures/boot_skew_cl.png", 
       plot = cowplot::plot_grid(ggplot(pigkc)+
                                   geom_density(aes(x = log(cl_mm)), fill = "grey40", alpha = 0.5)+
                                   labs(x = "Carapace Length (mm)", y = "Density")+
                                   coord_cartesian(expand = 0),
                                 p1, p2, nrow = 1), 
       height = 3, width = 7, units = "in")

ggsave("./research_projects/gkc_size_maturity/figures/boot_skew_psi.png", 
       plot = cowplot::plot_grid(x1, x2, nrow = 1), 
       height = 3, width = 6, units = "in")

