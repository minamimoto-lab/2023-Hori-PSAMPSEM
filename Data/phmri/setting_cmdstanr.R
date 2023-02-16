

install.packages("cmdstanr", 
                 repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
install_cmdstan(cores = 2)

install.packages("bridgesampling")
install.packages("posterior")
install.packages("bayesplot")

library(bridgesampling)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

