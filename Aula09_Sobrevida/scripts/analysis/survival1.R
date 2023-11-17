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

#create a new data called linelist_surv from the linelist_case_data

dt_surv <-  dt |>

  filter(
    # removendo datas erradas de inicio/fim
    date_outcome > date_onset)

 mutate(
    # se censura a direita, criando evento
    event = ifelse(is.na(outcome) | outcome == "Recover", 0, 1),

    # criando variavel followup
    futime = as.double(date_outcome - date_onset),

    # categorizando idade
    age_cat_small = case_when(
      age_years < 5  ~ "0-4",
      age_years >= 5 & age_years < 20 ~ "5-19",
      age_years >= 20   ~ "20+"),
    age_cat_small = fct_relevel(age_cat_small, "0-4", "5-19", "20+")
  )

