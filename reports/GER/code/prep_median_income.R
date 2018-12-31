

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

silc.pos.p1 <- silc.pos.p1 %>% group_by(rb010) %>%
  mutate(median_p11 = incMedian(income_p1_1, weights = rb050, years = rb010))

silc.pos.p1 <- silc.pos.p1 %>% group_by(rb010) %>%
  mutate(median_p12 = incMedian(income_p1_2, weights = rb050, years = rb010))

silc.pos.p1 <- silc.pos.p1 %>% group_by(rb010) %>%
  mutate(median_p13 = incMedian(income_p1_3, weights = rb050, years = rb010))


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

# dummies for middle class (welches einkommenskonzept????)

silc.pos.p1 <- silc.pos.p1 %>% mutate(upmiddle = as.numeric(percent_p13 > 150 
                                                            & percent_p13 < 250))
silc.pos.p1 <- silc.pos.p1 %>% mutate(middle = as.numeric(percent_p13 > 80 
                                                            & percent_p13 < 150))

silc.pos.p1 <- silc.pos.p1 %>% mutate(lowmiddle = as.numeric(percent_p13 > 60 
                                                            & percent_p13 < 80))

silc.pos.p1 <- silc.pos.p1 %>% mutate(totalmiddle = as.numeric(percent_p13 > 60 
                                                             & percent_p13 < 250))



silc.pos.p2 <- silc.pos.p2 %>% mutate(upmiddle = as.numeric(percent_p23 > 150 
                                                            & percent_p23 < 250))
silc.pos.p2 <- silc.pos.p2 %>% mutate(middle = as.numeric(percent_p23 > 80 
                                                          & percent_p23 < 150))

silc.pos.p2 <- silc.pos.p2 %>% mutate(lowmiddle = as.numeric(percent_p23 > 60 
                                                            & percent_p23 < 80))
silc.pos.p2 <- silc.pos.p2 %>% mutate(totalmiddle = as.numeric(percent_p23 > 60 
                                                             & percent_p23 < 250))


# table for middle class group over years for all people


middleclass.p1 <-  
  silc.pos.p1 %>% group_by(rb010) %>% summarise(upperrel = sum(upmiddle)/n(),
                                                middlerel = sum(middle)/n(),
                                                lowerrel = sum(lowmiddle)/n(),
                                                totalrel = sum(totalmiddle)/n(),
                                                median_p13 = incMedian(income_p1_3, weights = rb050,
                                                                      years = rb010))

middleclass.p1 <-  silc.pos.p1 %>% group_by(rb010) %>% summarise 

middleclass.p2 <-  
  silc.pos.p2 %>% group_by(rb010) %>% summarise(upperrel = sum(upmiddle)/n(),
                                                middlerel = sum(middle)/n(),
                                                lowerrel = sum(lowmiddle)/n(),
                                                totalrel = sum(totalmiddle)/n(),
                                                median_p23 = incMedian(income_p2_3, weights = rb050,
                                                                       years = rb010))

                                                
# save data
saveRDS(middleclass.p1, file="GER_middleclass_p1.RData")
saveRDS(middleclass.p2, file="GER_middleclass_p2.RData")

# table percent middle class for single adult hh

silc.single <- silc.pos.p1 %>% filter(hx040 == 1 & age > 20 & age < 65)
middleclass.single <-  
  silc.single%>% group_by(rb010) %>% summarise(upper_single = sum(upmiddle)/n(),
                                                middle_single = sum(middle)/n(),
                                                lower_single = sum(lowmiddle)/n(),
                                                total_single = sum(totalmiddle)/n(),
                                               median_p13_single = incMedian(income_p1_3, 
                                                                      weights = rb050,
                                                                      years = rb010))

# table percent middle class for old

silc.old <- silc.pos.p1 %>% filter(age > 65)
middleclass.old <-  
  silc.old%>% group_by(rb010) %>% summarise(upper_old= sum(upmiddle)/n(),
                                               middle_old = sum(middle)/n(),
                                               lower_old = sum(lowmiddle)/n(),
                                               total_old = sum(totalmiddle)/n(),
                                            median_p13_old = incMedian(income_p1_3, 
                                                                       weights = rb050,
                                                                   years = rb010))

# table middle class with two children (needs adjustment load parents id)
silc.pos.p1 <- silc.pos.p1 %>% mutate(child = as.numeric(age <= 17))
silc.pos.p1 <- silc.pos.p1 %>% 
  group_by(id_h) %>%
  mutate(children = sum(child))

# filter for two children
silc.two <- silc.pos.p1 %>% filter(children == 2)

middleclass.two <-  
  silc.two%>% group_by(rb010) %>% summarise(upper_two = sum(upmiddle)/n(),
                                               middle_two = sum(middle)/n(),
                                               lower_two = sum(lowmiddle)/n(),
                                               total_two = sum(totalmiddle)/n(),
                                            median_p13_two = incMedian(income_p1_3, 
                                                                       weights = rb050,
                                                                   years = rb010))
# filter for more than two children 
silc.many <- silc.pos.p1 %>% filter(children > 2)

middleclass.many <-  
  silc.many%>% group_by(rb010) %>% summarise(upper_many = sum(upmiddle)/n(),
                                            middle_many = sum(middle)/n(),
                                            lower_many = sum(lowmiddle)/n(),
                                            total_many = sum(totalmiddle)/n(),
                                            median_p13_many = incMedian(income_p1_3, 
                                                                        weights = rb050,
                                                                   years = rb010))