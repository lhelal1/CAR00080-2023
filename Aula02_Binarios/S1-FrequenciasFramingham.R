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
#  Script: Tabelas de Frequências               #
#                                               #
#################################################


# Pacotes

require(tidyverse)
require(gtools)
require(class)
require(caret)
require(DT)

# Ler banco

framingham <- read.csv("framingham.csv")
datatable(framingham)

