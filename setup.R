if(!require(pacman)) install.packages("pacman")
pacman::p_load(vioplot, sm, beanplot, survival, graphics, stats, usethis)
#install.packages
library(usethis)
usethis::use_git_config(user.name = "Simon Thornley", 
               user.email = "sithor@gmail.com")

## use this as password through Rstudio
usethis::create_github_token()
