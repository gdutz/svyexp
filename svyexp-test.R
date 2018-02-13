rm(list=ls(all=TRUE)) # REMOVE ALL OBJECTS FROM WORKSPACE

library(tidyverse)
library(mitools)
library(ggplot2)
library(svyexp)

source("tidy-test.R", encoding = "UTF-8")

datapath <- "C:/daten/piaac/"

cntry <- c("deu")
# cntry <- c("aut",
#            "aut",
#            "bel",
#            "ita",
#            "esp",
#            "fra",
#            "can",
#            "gbr",
#            "usa",
#            "dnk",
#            "nor",
#            "swe",
#            "usa",
#            "isr",
#            "fin")

variables <- c("gender_r", "imyrcat", "gender_r",
               "age_r", "ageg10lfs", "edlevel3", "pared", "homlang",
               "nativelang", "c_q01a")

# Modell 1 ----

glm_formula1 <- pvlit ~ imyrcat
glm_formula2 <- pared ~ imyrcat

results1 <- piaac_glm(datapath, cntry, variables, formula = glm_formula1, pv = "pvlit", FUN = tidy)
results2 <- piaac_glm(datapath, cntry, variables, formula = glm_formula2, FUN = tidy)

results <- MIcombine(results1$deu, df.complete = 80)
test <- glm_summary(results)


glm_summary(results2[[1]])
summary(results2)
