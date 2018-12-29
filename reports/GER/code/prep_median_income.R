

# ------------------------------------------------------------------------
#
# Prepare Data for Middle Class Indicator R-Script
#
# -------------------------------------------------------------------------

# Definition of Middle Class According to IW
# Poverty Risk: Below 60% of Median Income
# Lower Middle Class: 60%-80% of Median Income
# Middle Class: 80-150% of Median Income
# Upper Middle Class: 150%-250% of Median Income
# Rich: more than 250% of Median Income


library(laeken)
library(dplyr)
library(survey)
library(convey)

#source("fetch_data_GER.R")

# Creating Survey Objects -----------------------------------------------------

#get data 

silc.pos.p1 <- readRDS("GER_pos_p1.RData")
silc.pos.p2 <- readRDS("GER_pos_p2.RData")
post_tax_p1 <- readRDS("GER_post_tax_p1_table.RData")

silc.p1.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.pos.p1) %>% convey_prep()

silc.p2.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.pos.p2) %>% convey_prep()



# Prepare Middle Class Indicator-----------------------------------------------


# create median income for income concepts

#or unweighted median since we weight in analysis??

silc.pos.p1 <- silc.pos.p1 %>%
  mutate(median_p11 = incMedian(income_p1_1, weights = rb050, years = rb010))

silc.pos.p1 <- silc.pos.p1 %>% group_by(rb010) %>%
  mutate(median_p11 = incMedian(income_p1_2, weights = rb050, years = rb010))

silc.pos.p1 <- silc.pos.p1 %>% group_by(rb010) %>%
  mutate(median_p12 = incMedian(income_p1_3, weights = rb050, years = rb010))

silc.pos.p1 <- silc.pos.p1 %>% group_by(rb010) %>%
  mutate(median_p13 = incMedian(income_p1_1, weights = rb050, years = rb010))

silc.pos.p2 <- silc.pos.p2 %>% group_by(rb010) %>%
  mutate(median_p21 = incMedian(income_p2_1, weights = rb050, years = rb010))

silc.pos.p2 <- silc.pos.p2 %>% group_by(rb010) %>%
  mutate(median_p22 = incMedian(income_p2_2, weights = rb050, years = rb010))

silc.pos.p2 <- silc.pos.p2 %>% group_by(rb010) %>%
  mutate(median_p23 = incMedian(income_p2_3, weights = rb050, years = rb010))

# create variables with percent of median income 

silc.pos.p1 <- silc.pos.p1 %>% mutate(percent_p11 = income_p1_1/median_p11*100)
silc.pos.p1 <- silc.pos.p1 %>% mutate(percent_p12 = income_p1_2/median_p12*100)
silc.pos.p1 <- silc.pos.p1 %>% mutate(percent_p13 = income_p1_3/median_p13*100)

silc.pos.p2 <- silc.pos.p2 %>% mutate(percent_p21 = income_p2_1/median_p21*100)
silc.pos.p2 <- silc.pos.p2 %>% mutate(percent_p22 = income_p2_2/median_p22*100)
silc.pos.p2 <- silc.pos.p2 %>% mutate(percent_p23 = income_p2_3/median_p23*100)

