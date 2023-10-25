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
#  Script: RR e OR                              #
#                                               #
#################################################

# Pacotes

require(pacman)
pacman::p_load(tidyverse,
               ggplot2,
               dplyr,
               epitools,
               )

# Opção com install packages

install.packages("epitools")
library(epitools)


# RR
RRtable<-matrix(c(1017,2260,165,992),nrow = 2, ncol = 2)
riskratio.wald(RRtable)

# OR
ORtable<-matrix(c(1017,2260,165,992),nrow = 2, ncol = 2)
ORtable

oddsratio.wald(ORtable)




