#-----------------------------
# パッケージ読み込み
#-----------------------------
library(tidyverse)
library(data.table)
library(lme4)

source("./roundrobin.R")

#-----------------------------
# load data
#-----------------------------

path <- "df_PSAM_FDG_striatum.csv"

#-----------------------------
# filter
#-----------------------------
dat <- path %>%
  fread() %>%
  as_tibble() %>%
  dplyr::filter(str_detect(cond,"u817")) %>%
  mutate(dose=str_remove(cond,"u817-"),dose=as.numeric(dose))

#-----------------------------
# nest
#-----------------------------

dat_nest <-
  dat %>%
  group_nest(.) %>%
  mutate(model = list(1:5)) %>%
  unnest(model)

#-----------------------------
# models
#-----------------------------

model1 <- function(data){
  lm(suvrd ~ log_dose, data = data)
}
model2 <- function(data){
  lm(suvrd ~ 1, data = data)
}
model3 <- function(data){
  lme4::lmer(suvrd ~ log_dose + (log_dose|Region), data = data)
}
model4 <- function(data){
  lme4::lmer(suvrd ~ log_dose + (0 + log_dose|Region), data = data)
}
model5 <- function(data){
  lme4::lmer(suvrd ~ log_dose + (1|Region), data = data)
}
list_model <-
  list(model1, model2, model3, model4, model5)

#-----------------------------
# fit models
#-----------------------------
dat_fit <-
  dat_nest %>%
  mutate(data = map(data, ~ mutate(., log_dose = log(dose)))) %>%
  mutate(fit = map2(data, list_model, ~.y(.x))) %>%
  mutate(AIC = map_dbl(fit, AIC))

dat_fit %>%
  arrange(AIC) %>%
  mutate(dAIC = AIC - min(AIC))

#-----------------------------
# predict min AIC
#-----------------------------
dat_pred <-
  dat_fit %>%
  dplyr::filter(AIC == min(AIC)) %>%
  mutate(pred = map(fit, predict)) %>%
  unnest(c(data, pred))

dat %>%
  ggplot() +
  aes(log(dose), suvrd, shape = Region) +
  geom_point() +
  geom_path(data = dat_pred,
            aes(x = log_dose,
                y = pred,
                color = Region))+
  theme_classic()

#-----------------------------
# log(dose)とdSUVRの間に相関があるかないか
#-----------------------------
dat %>%
  mutate(log_dose = log(dose)) %>%
  aov(suvrd ~ log_dose + Error(Region), data = .) %>%
  summary()


#-----------------------------
# comparison 817vs792 only
#-----------------------------
dat_817vs792 <-
  path %>%
  fread() %>%
  as_tibble() %>%
  dplyr::filter(str_detect(cond, "-100"))
  dplyr::filter(str_detect(cond,"vare",negate=TRUE))

#paired T-test
t.test(suvrd~cond,data=dat_817vs792,paired=TRUE)

#Wilcoxon signed rank exact test
wilcox.test(suvrd~cond,data=dat_817vs792,paired=T,alternative="two.sided")
