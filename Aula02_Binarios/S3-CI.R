#################################################
#   Universidade Federal do Rio Grande do Sul   #
#       PPG Cardiologia/PPG Epidemiologia       #
#                                               #
#              CAR00080/2023                    #
#                                               #
#        Lucas Helal    Patricia K. Ziegelmann. #
#        lucas.helal.    patricia.ziegelmann.   #
#                                               #
#           mailto:x.y@ufrgs.br                 #
#                                               #
#            github.com/lhelal1                 #
#                                               #
#  Autor: Lucas Helal                           #
#  Data: 25102023                               #
#  Script: CI.                                  #
#                                               #
#################################################

# Pacotes

require(pacman)
pacman::p_load(tidyverse,
               ggplot2,
               dplyr,
               epitools,
)


#### --------------
# Dados aleatorios
data <- c(21, 24, 18, 26, 19, 22, 25, 20, 23, 17)

# Media
mean_data <- mean(data)

# SE
standard_error <- 1.5  #

# CI
confidence_level <- 0.95


z_critical <- qnorm((1 + confidence_level) / 2)
margin_of_error <- z_critical * standard_error


confidence_interval <- c(mean_data - margin_of_error, mean_data + margin_of_error)


print(confidence_interval)

#### --------------------------

# Seed
set.seed(10)

# Dados aleatórios
data <- sample(10:29, 50, replace = TRUE)

# 95% a partir do teste t
conf_interval <- t.test(data, conf.level = 0.95)$conf.int
print(conf_interval)
