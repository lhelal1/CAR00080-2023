## ---------------------------------------------------------------------------------
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
## ---------------------------------------------------------------------------------
##
## Notes:
##
##
## ---------------------------------------------------------------------------------

## set working directory and configs

setwd("~/Users/lucashelal/LH_P/git/")

## ---------------------------

options(scipen = 6, digits = 4) #floating point
memory.limit(30000000)     # allocated RAM

## ---------------------------

## base packages
install.packages("pacman")
pacman::p_load(tidyverse,
dplyr,
ggplot2,
lubridate,
rio,
here,
data.table,
fs,
janitor,
epikit,
matchmaker,
skimr,
gtsummary,
rstatix,
scales,
flextable,
mice,
naniar,
car,
psych,
DescTools,
RVAideMemoire,
readr,
readxl,
haven,
survival,
survminer,
Publisher)
## ----------------------------------- Start Here --------------------------------- ##


## reading files

dt <- import(here("CAR80_A09", "data", "survival_1.csv"), na = c("Missing",
                                                                 "999",
                                                                 "",
                                                                 " ",
                                                                 "99",
                                                                 "."))

## dictionary

dict <- dt |>              # begin: linelist with dictionary as first row
  head(1)  |>                              # keep only column names and first dictionary row
  pivot_longer(cols = everything(),       # pivot all columns to long format
               names_to = "Column",       # assign new column names
               values_to = "Description")


## directory tree

fs::dir_tree(path = here("CAR80_A09"), recurse = TRUE)

## silent script running

source(here("CAR80_A09", "scripts", "configA09.R"))


## summary stats
summary(dt)
summary(dt$var)
summary(dt$var)[[2]]

dt |>
  get_summary_stats(
    var1, var2, var3, var4,
    type = "common"
  )

## with janitor

dt |>
  tabyl(var1, var2) |>
  adorn_pct_formatting(digits = 1) |>
  adorn_totals(where = "row") |>
  adorn_percentages(denominator = "row") |>
  adorn_ns(position = "front") |>
  adorn_title(
    row_name = "XXX",
    col_name = "YYY",
    placement = "combined") |>
  flextable::flextable() |>
flextable::autofit() |>
  flextable::save_as_html(path = "table1.html")

## stats

### criando tabela 2x2

table22 <- dt |>
  tabyl(var1, var2, show_na = FALSE)

chisq.test(table22)


### contagens

dt |>
  group_by(var1)  |>
  summarise(n_rows = n())


dt |>
  count(var1, var2, var3, varn)


### proporcoe dado o desfecho

dt_prop <- dt |>
  group_by(var1) |>
  count(var2) |>
  mutate(percent = scales::percent(n / sum(n)))

### plot

dt |>
  count(var1, var2) %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = var2, # desfecho
      fill = var1, # grupo
      y = n))

### condicionais

dt |>
  group_by(var1) |>
  summarise(
   new_var2 = max(var2[var3 == "yes"], na.rm = T),
    new_var2 = max(var2[var3 == "no"], na.rm = T)
  )
