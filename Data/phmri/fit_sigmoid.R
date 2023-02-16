
library(tidyverse)
library(data.table)
library(cmdstanr)
library(bayesplot)

source("f_extstan.R")
model <- "sigmoid.stan" %>% cmdstan_model()

data_raw <-
  "../data/data_scaled.csv" %>% 
  fread() %>% 
  as_tibble()

"Lt_CAU"    
"Lt_M1_leg"
"Lt_PU"     
"Rt_CAU"   
"Rt_INS"    
"Rt_M1"    
"Rt_PU" 

.ROI <- "Lt_CAU"
#.condition <- "uPSEM817"
.condition <- "DCZ"

data_i <-
  data_raw %>% 
  filter(ROI == .ROI, condition == .condition)

data_stan <-
  list(N = nrow(data_i),
       x = data_i$time,
       y = data_i$average)



fit <-
  model$sample(
    data_stan,
    iter_warmup = 2000,
    iter_sampling = 5000,
    chains = 4,
    parallel_chains = 4
    )


fit$save_output_files("cmdstan/")

fit$print(c("a", "b", "c"), pd_out)

fit$draws(c("a", "b", "c")) %>% 
  bayesplot::mcmc_dens_overlay()

ggsave(str_c("fig/mcmc_dens_overlay_",
             .condition, "_", .ROI, ".png"),
       width = 8, height = 2.5)



fit$draws(c("a", "b", "c")) %>% 
  bayesplot::mcmc_trace()

ggsave(str_c("fig/traceplot_",
             .condition, "_", .ROI, ".png"),
       width = 8, height = 2.5)


a <- fit$draws("a") %>% map()
b <- fit$draws("b") %>% map()
c <- fit$draws("c") %>% map()

params <- 
  data.frame(params = letters[1:3]) %>% 
  mutate(map = purrr::map_dbl(params, ~fit$draws(.) %>% map),
         mean = purrr::map_dbl(params, ~fit$draws(.) %>% mean),
         median = purrr::map_dbl(params, ~fit$draws(.) %>% median),
         sd = purrr::map_dbl(params, ~fit$draws(.) %>% sd),
         q_0025 = purrr::map_dbl(params, ~fit$draws(.) %>% quntile95(.) %>% .[1]),
         q_0975 = purrr::map_dbl(params, ~fit$draws(.) %>% quntile95(.) %>% .[2]),
         ess_tail95 = purrr::map_dbl(params, ~fit$draws(.) %>% ess_tail95),
         r_hat = purrr::map_dbl(params, ~fit$draws(.) %>% r_hat)
  )

str_c("data/parameters/", .condition, "_", .ROI, ".csv") %>% 
  write_csv(params, .)

dat_pred <-
  tibble(time = seq(-5, 15, by = 0.1)) %>% 
  mutate(average = a / (1 + exp(-b * (time - c))) + 1)

ggplot(data = data_i) +
  aes(x=time, y=average) +
  geom_hline(yintercept = 1,
             color = "darkgrey") +
  geom_point(alpha = 0.7) +
  geom_path(data = dat_pred) +
  theme_classic()

ggsave(str_c("fig/sigmoid_stan_",
             .condition, "_", .ROI, ".png"),
       width = 4, height = 2.5)

