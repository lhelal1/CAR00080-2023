## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Prof. Lucas Helal
##
## Date Created: 2023-11-17
##
## Copyright (c) LH, 2023
## CC BY 1.0 - Open Source
## Email: lucas.helal@ufrgs.br
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

## set working directory and configs

setwd("~/Users/lucashelal/LH_P/git/")

## ---------------------------

options(scipen = 6, digits = 4) floating point
memory.limit(30000000)     # allocated RAM

## ---------------------------

## base packages

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rio)
library(here)
library(data.table)

## --------------------------- Start Here -------------------------- ##

table1 <- dt|>

  # Desfecho sumarizado por uma variavel de interesse
  ###############################################
group_by(var1, desfecho)  |>
  summarise(
    N = n(),  # desfecho em linhas por classe de variavel
    new_var = median(var2, na.rm=T)) |>           # nova variavel com estatistica que quiser dada variavel preexistente

  # adicionando totais
  ############
bind_rows(                                           # transforma em mini tabelas
  dt |>
    filter(!is.na(desfecho) & var1 != "Missing")  |> # condicao
    group_by(desfecho) %>%                            # agrupado somente por desfecho
    summarise(
      N = n(),                                       # numero de linhas de todo o banco
      new_var = median(var2, na.rm=T))) |>      # estatistica da var2 do banco todo
  mutate(
    N_desfechos = N_evento + N_nao_evento,
    Pct_evento = scales::percent(N_evento / N_desfechos, 0.1),
    Pct_n_evento = 100 - Pct_evento)  |>
  select(
    var1, N_desfechos,
    N_nao_evento, Pct == nao_evento, var2 == nao_evento,
    N_evento, Pct == evento, var2 == evento)  %>%
  arrange(N_desfechos)

table1  # print

#####

# com flextable

table1_flex <- flextable(table1)
table1_flex |> autofit()
table1_flex <- table1_flex  |>
  width(j=1, width = 2.7) |>
  width(j=2, width = 1.5) |>
  width(j=c(4,5,7,8), width = 1)

table1_flex
