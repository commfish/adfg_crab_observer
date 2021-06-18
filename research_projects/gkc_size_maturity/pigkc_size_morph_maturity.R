# notes ----
## PIGKC size at morph maturity
## Tyler Jackson
## 6/17/2021

# load ----
library(tidyverse)
library(bbmle)
library(MASS)
if (!require("segmented")) install.packages("segmented");library(segmented)

# ggplot theme
theme_set(theme_classic())

# function to call segmented package in purrr
f_seg <- function(data, psi){
  l_mod <- lm(y~x, data)
  seg_mod <- segmented(l_mod, seg.Z = ~x, psi = psi)
  return(seg_mod)
}


# data ----

pigkc <- read_csv("./pigkc/data/pigkc_maturity_tmp.csv") %>%
             rename_all(tolower) %>%
             dplyr::select(-15) %>%
             filter(!is.na(cl_mm),
                    !is.na(chela_h1_mm))

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
  geom_point(aes(x = cl_mm, y = chela_h1_mm))+
  geom_line(aes(x = cl_mm, y = alpha*exp(cl_mm*beta)), size = 1, color = 2)

pigkc %>%
  ggplot()+
  geom_point(aes(x = alpha*exp(cl_mm*beta), y = chela_h1_mm - alpha*exp(cl_mm*beta)))+
  geom_smooth(aes(x = alpha*exp(cl_mm*beta), y = chela_h1_mm - alpha*exp(cl_mm*beta)))


# no transformation breakpoint ----

## fit
pigkc %>%
  rename(x = cl_mm, y = chela_h1_mm) %>%
  f_seg(., 130) -> chcl_fit
summary(chcl_fit)

## breakpoint
bend_chcl <- chcl_fit$psi[2]
bend_chcl_95ci <- c(chcl_fit$psi[2] + qnorm(0.025)*chcl_fit$psi[3], chcl_fit$psi[2] + qnorm(0.975)*chcl_fit$psi[3])
             
# plots
## fit
pigkc %>%
  ggplot()+
  geom_point(aes(x = cl_mm, y = chela_h1_mm), color = "grey")+
  geom_line(aes(x = cl_mm, y = chcl_fit$fitted.values), size = 1, color = 2)+
  geom_vline(xintercept = c(bend_chcl, bend_chcl_95ci), linetype = c(1, 2, 2), size = 1)+
  labs(x = "Carapace Length (mm)", y = "Chela Height (mm)")
## residuals
ggplot()+
  geom_point(aes(x = chcl_fit$fitted.values, y = chcl_fit$residuals))+
  geom_hline(yintercept = 0)+
  geom_smooth(aes(x = chcl_fit$fitted.values, y = chcl_fit$residuals))+
  labs(x = "Fitted Values", y = "Residuals")
qqnorm(MASS::stdres(chcl_fit));qqline(MASS::stdres(chcl_fit), col = 4)


# ln x,y transformation breakpoint ----

## fit ln ch~cl
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_h1_mm)) %>%
  f_seg(., log(130)) -> ln_chcl_fit
summary(ln_chcl_fit)
mean(ln_chcl_fit$residuals^2) # mse

## breakpoint
bend_chcl_ln <- ln_chcl_fit$psi[2]
bend_chcl_ln_95ci <- c(ln_chcl_fit$psi[2] + qnorm(0.025)*ln_chcl_fit$psi[3], ln_chcl_fit$psi[2] + qnorm(0.975)*ln_chcl_fit$psi[3])

bend_ln <- exp(ln_chcl_fit$psi[2] + ln_chcl_fit$psi[3] / 2)

# plots
## fit
pigkc %>%
  ggplot()+
  geom_point(aes(x = log(cl_mm), y = log(chela_h1_mm)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = log(cl_mm), y = ln_chcl_fit$fitted.values), color = 2)+
  geom_vline(xintercept = c(bend_chcl_ln, bend_chcl_ln_95ci), linetype = c(1, 2, 2))+
  labs(x = "ln Carapace Length (mm)", y = "ln Chela Height (mm)") -> p1
## residuals
ggplot()+
  geom_point(aes(x = ln_chcl_fit$fitted.values, y = ln_chcl_fit$residuals), color = "grey", alpha = 0.5)+
  geom_hline(yintercept = 0)+
  geom_smooth(aes(x = ln_chcl_fit$fitted.values, y = ln_chcl_fit$residuals))+
  labs(x = "Fitted Values", y = "Residuals") -> r1
qqnorm(MASS::stdres(ln_chcl_fit));qqline(MASS::stdres(ln_chcl_fit), col = 4)


## fit ln chela width (cw)~cl
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_w_mm)) %>%
  f_seg(., log(130)) -> ln_cwcl_fit
summary(ln_cwcl_fit)

## breakpoint
bend_cwcl_ln <- ln_cwcl_fit$psi[2]
bend_cwcl_ln_95ci <- c(ln_cwcl_fit$psi[2] + qnorm(0.025)*ln_cwcl_fit$psi[3], ln_cwcl_fit$psi[2] + qnorm(0.975)*ln_cwcl_fit$psi[3])


# plots
## fit
pigkc %>%
  filter(!is.na(chela_w_mm)) %>%
  ggplot()+
  geom_point(aes(x = log(cl_mm), y = log(chela_w_mm)), color = "grey", alpha = 0.5)+
  #geom_smooth(aes(x = log(cl_mm), y = log(chela_w_mm)))+
  geom_line(aes(x = log(cl_mm), y = ln_cwcl_fit$fitted.values), color = 2)+
  geom_vline(xintercept = c(bend_cwcl_ln, bend_cwcl_ln_95ci), linetype = c(1, 2, 2))+
  labs(x = "ln Carapace Length (mm)", y = "ln Chela Width (mm)") -> p2
## residuals
ggplot()+
  geom_point(aes(x = ln_cwcl_fit$fitted.values, y = ln_cwcl_fit$residuals), color = "grey", alpha = 0.5)+
  geom_hline(yintercept = 0)+
  geom_smooth(aes(x = ln_cwcl_fit$fitted.values, y = ln_cwcl_fit$residuals))+
  labs(x = "Fitted Values", y = "Residuals") -> r2
  
qqnorm(MASS::stdres(ln_cwcl_fit));qqline(MASS::stdres(ln_cwcl_fit), col = 4)
  

## fit ln chela width (cw)*ch~cl
pigkc %>%
  mutate(x = log(cl_mm), y = log(chela_w_mm * chela_h1_mm)) %>%
  f_seg(., log(130)) -> ln_cwchcl_fit
summary(ln_cwcl_fit)

## breakpoint
bend_cwchcl_ln <- ln_cwchcl_fit$psi[2]
bend_cwchcl_ln_95ci <- c(ln_cwchcl_fit$psi[2] + qnorm(0.025)*ln_cwchcl_fit$psi[3], ln_cwchcl_fit$psi[2] + qnorm(0.975)*ln_cwchcl_fit$psi[3])


# plots
## fit
pigkc %>%
  filter(!is.na(chela_w_mm)) %>%
  ggplot()+
  geom_point(aes(x = log(cl_mm), y = log(chela_w_mm * chela_h1_mm)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = log(cl_mm), y = ln_cwchcl_fit$fitted.values), color = 2)+
  geom_vline(xintercept = c(bend_cwchcl_ln, bend_cwchcl_ln_95ci), linetype = c(1, 2, 2))+
  labs(x = "ln Carapace Length (mm)", y = "ln ChW (mm) + ln CH (mm)") -> p3
## residuals
ggplot()+
  geom_point(aes(x = ln_cwchcl_fit$fitted.values, y = ln_cwchcl_fit$residuals), color = "grey", alpha = 0.5)+
  geom_hline(yintercept = 0)+
  geom_smooth(aes(x = ln_cwchcl_fit$fitted.values, y = ln_cwchcl_fit$residuals))+
  labs(x = "Fitted Values", y = "Residuals") -> r3
qqnorm(MASS::stdres(ln_cwchcl_fit));qqline(MASS::stdres(ln_cwchcl_fit), col = 4)


# all data plots
ggsave("./research_projects/gkc_size_maturity/figures/pigkc_ln_seg.png",
       plot = cowplot::plot_grid(p1, p2, p3, nrow = 3), 
       height = 8, width = 4, units = "in")


# linear fit to alternative allometry ----

## fit ln ch~cl
pigkc %>%
  mutate(x = cl_mm, y = log(chela_h1_mm)) %>%
  lm(y~x, data = .) -> allo_fit
summary(allo_fit)
mean(allo_fit$residuals^2) # mse
 
# plots
## fit
pigkc %>%
  ggplot()+
  geom_point(aes(x = cl_mm, y = log(chela_h1_mm)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = cl_mm, y = allo_fit$fitted.values), color = 2)+
  #geom_smooth(aes(x = cl_mm, y = log(chela_h1_mm)))+
  labs(x = "Carapace Length (mm)", y = "ln Chela Height (mm)")

## residuals
ggplot()+
  geom_point(aes(x = allo_fit$fitted.values, y = allo_fit$residuals))+
  geom_hline(yintercept = 0)+
  geom_smooth(aes(x = allo_fit$fitted.values, y = allo_fit$residuals))+
  labs(x = "Fitted Values", y = "Residuals")
qqnorm(MASS::stdres(allo_fit));qqline(MASS::stdres(allo_fit), col = 4)


## fit ln ch~cl
pigkc %>%
  mutate(x = cl_mm, y = log(chela_h1_mm*chela_w_mm)) %>%
  lm(y~x, data = .) -> allo_fit2
summary(allo_fit2)
mean(allo_fit2$residuals^2) # mse

# plots
## fit
pigkc %>%
  filter(!is.na(chela_w_mm)) %>%
  ggplot()+
  geom_point(aes(x = cl_mm, y = log(chela_h1_mm*chela_w_mm)), color = "grey", alpha = 0.5)+
  geom_line(aes(x = cl_mm, y = allo_fit2$fitted.values), color = 2)+
  #geom_smooth(aes(x = cl_mm, y = log(chela_h1_mm)))+
  labs(x = "Carapace Length (mm)", y = "ln Chela Height (mm)")

## residuals
ggplot()+
  geom_point(aes(x = allo_fit2$fitted.values, y = allo_fit2$residuals))+
  geom_hline(yintercept = 0)+
  geom_smooth(aes(x = allo_fit2$fitted.values, y = allo_fit2$residuals))+
  labs(x = "Fitted Values", y = "Residuals")
qqnorm(MASS::stdres(allo_fit));qqline(MASS::stdres(allo_fit), col = 4)



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

plot(y~x, d_ln)

ln_2line_fit = f_2lm(d_ln, "x", "y", 4.5, 5.1)
ggplot()+
  geom_point(data = ln_2line_fit$assigned_dat, aes(x = x, y = y, color = factor(mature)))+
  geom_line(aes(x = ln_2line_fit$juvenile_lm$model$x, y = ln_2line_fit$juvenile_lm$fitted.values), size = 1)+
  geom_line(aes(x = ln_2line_fit$mature_lm$model$x, y = ln_2line_fit$mature_lm$fitted.values), size = 1)+
  labs(x = "ln Carapce Length(mm)", y = "ln Chela Height (mm)", color = NULL)+
  scale_color_discrete(labels = c("immature", "mature"))+
  theme(legend.position = c(0.2,.9))
  
ln_2line_fit = f_2lm(d_ln, "x", "y", 4.7, 5.0)
ggplot()+
  geom_point(data = ln_2line_fit$assigned_dat, aes(x = x, y = y, color = factor(mature)))+
  geom_line(aes(x = ln_2line_fit$juvenile_lm$model$x, y = ln_2line_fit$juvenile_lm$fitted.values), size = 1)+
  geom_line(aes(x = ln_2line_fit$mature_lm$model$x, y = ln_2line_fit$mature_lm$fitted.values), size = 1)+
  labs(x = "ln Carapce Length(mm)", y = "ln Chela Height (mm)", color = NULL)+
  scale_color_discrete(labels = c("immature", "mature"))+
  theme(legend.position = c(0.2,.9))

twoline_fit = f_2lm(lin, "x", "y", 120, 140)
ggplot()+
  geom_point(data = twoline_fit$assigned_dat, aes(x = x, y = y, color = factor(mature)))+
  geom_line(aes(x = twoline_fit$juvenile_lm$model$x, y = twoline_fit$juvenile_lm$fitted.values), size = 1)+
  geom_line(aes(x = twoline_fit$mature_lm$model$x, y = twoline_fit$mature_lm$fitted.values), size = 1)+
  labs(x = "Carapce Length(mm)", y = "Chela Height (mm)", color = NULL)+
  scale_color_discrete(labels = c("immature", "mature"))+
  theme(legend.position = c(0.2,.9))

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



plot(log(y) ~ x, lin)

tmp_fit = lm(log(y) ~ x, lin)

#plot(tmp_fit)

plot(log(y) ~ x, lin)
lines(sort(tmp_fit$fitted.values) ~ sort(x), lin, col = 2, lwd = 3)


sum(tmp_fit$residuals^2)
sum(d_ln_fit$residuals^2)


plot(y ~ x, lin)

