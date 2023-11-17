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

# import dataset

library(readr)
raw_leukemia <- read_delim("data/raw_leukemia.csv",
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(raw_leukemia)
