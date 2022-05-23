##Rural and Island Community Councils

library(tidyverse)
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(magrittr)

#read in the data

dta <- read_excel("Raw Data/Rural Council Survey Raw Data.xlsx") %>%
  filter(
    `Q1. Please indicate whether you are happy to participate in this survey and consent to your data being used in this way` ==
      "I am happy to participate in this survey") %>%
  select(-c(1:9, 83:84)) %>%
  select(-(contains("please specify") | contains("which of these") | contains("what information")))

root_questions <- read.csv("Raw Data/reference_table.csv")

source("functions/plot_function_dev.R")

#create plots and save as JPEG files========================================

q10 <- plot_stats(dta, "Q10", 30, root_questions)
q11 <- plot_stats(dta, "Q11", 30, root_questions)
q13 <- plot_stats(dta, "Q13", 30, root_questions)
q14 <- plot_stats(dta, "Q14", 30, root_questions)
q15 <- plot_stats(dta, "Q15", 30, root_questions)
q16 <- plot_stats(dta, "Q16", 30, root_questions)
q17 <- plot_stats(dta, "Q17", 30, root_questions)

ggsave("q10.JPEG", q10, device = NULL, path = "plots")
ggsave("q11.JPEG", q11, device = NULL, path = "plots")
ggsave("q13.JPEG", q13, device = NULL, path = "plots")
ggsave("q14.JPEG", q14, device = NULL, path = "plots")
ggsave("q15.JPEG", q15, device = NULL, path = "plots")
ggsave("q16.JPEG", q16, device = NULL, path = "plots")
ggsave("q17.JPEG", q17, device = NULL, path = "plots")