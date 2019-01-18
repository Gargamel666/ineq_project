

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

silc.p1 <- readRDS("GER_p1.RData")
silc.p2 <- readRDS("GER__p2.RData")


silc.p1.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.p1) %>% convey_prep()

silc.p2.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.p2) %>% convey_prep()



# Prepare Middle Class Indicator-----------------------------------------------


# create median income for income concepts

#or unweighted median since we weight in analysis??

silc.p1 <- silc.p1 %>% group_by(rb010) %>%
  mutate(median_p11 = incMedian(income_p1_1, weights = rb050, years = rb010))

silc.p1 <- silc.p1 %>% group_by(rb010) %>%
  mutate(median_p12 = incMedian(income_p1_2, weights = rb050, years = rb010))

silc.p1 <- silc.p1 %>% group_by(rb010) %>%
  mutate(median_p13 = incMedian(income_p1_3, weights = rb050, years = rb010))


silc.p2 <- silc.p2 %>% group_by(rb010) %>%
  mutate(median_p21 = incMedian(income_p2_1, weights = rb050, years = rb010))

silc.p2 <- silc.p2 %>% group_by(rb010) %>%
  mutate(median_p22 = incMedian(income_p2_2, weights = rb050, years = rb010))

silc.p2 <- silc.p2 %>% group_by(rb010) %>%
  mutate(median_p23 = incMedian(income_p2_3, weights = rb050, years = rb010))

# create variables with percent of median income 

silc.p1 <- silc.p1 %>% mutate(percent_p11 = income_p1_1/median_p11*100)
silc.p1 <- silc.p1 %>% mutate(percent_p12 = income_p1_2/median_p12*100)
silc.p1 <- silc.p1 %>% mutate(percent_p13 = income_p1_3/median_p13*100)

silc.p2 <- silc.p2 %>% mutate(percent_p21 = income_p2_1/median_p21*100)
silc.p2 <- silc.p2 %>% mutate(percent_p22 = income_p2_2/median_p22*100)
silc.p2 <- silc.p2 %>% mutate(percent_p23 = income_p2_3/median_p23*100)

# dummies for middle class (welches einkommenskonzept????)

silc.p1 <- silc.p1 %>% mutate(upmiddle = as.numeric(percent_p13 > 150 
                                                            & percent_p13 < 250))
silc.p1 <- silc.p1 %>% mutate(middle = as.numeric(percent_p13 > 80 
                                                            & percent_p13 < 150))

silc.p1 <- silc.p1 %>% mutate(lowmiddle = as.numeric(percent_p13 > 60 
                                                            & percent_p13 < 80))

silc.p1 <- silc.p1 %>% mutate(totalmiddle = as.numeric(percent_p13 > 60 
                                                             & percent_p13 < 250))



silc.p2 <- silc.p2 %>% mutate(upmiddle = as.numeric(percent_p23 > 150 
                                                            & percent_p23 < 250))
silc.p2 <- silc.p2 %>% mutate(middle = as.numeric(percent_p23 > 80 
                                                          & percent_p23 < 150))

silc.p2 <- silc.p2 %>% mutate(lowmiddle = as.numeric(percent_p23 > 60 
                                                            & percent_p23 < 80))
silc.p2 <- silc.p2 %>% mutate(totalmiddle = as.numeric(percent_p23 > 60 
                                                             & percent_p23 < 250))


# table for middle class group ---------------------------------------------

headers <- c('Year', 'Upper Middle', 'Strict Middle', 'Lower Middle', 'Total Middle', 'Median Income')
options(digits = 4)

# all people


middleclass.p1 <-  
  silc.p1 %>% group_by(rb010) %>% summarise(upperrel = sum(upmiddle)/n()*100,
                                                middlerel = sum(middle)/n()*100,
                                                lowerrel = sum(lowmiddle)/n()*100,
                                                totalrel = sum(totalmiddle)/n()*100,
                                                median_p13 = incMedian(income_p1_3, weights = rb050,
                                                                      years = rb010))
colnames(middleclass.p1) <- headers

middleclass.p2 <-  
  silc.p2 %>% group_by(rb010) %>% summarise(upperrel = sum(upmiddle)/n()*100,
                                                middlerel = sum(middle)/n()*100,
                                                lowerrel = sum(lowmiddle)/n()*100,
                                                totalrel = sum(totalmiddle)/n()*100,
                                                median_p23 = incMedian(income_p2_3, weights = rb050,
                                                                       years = rb010))
colnames(middleclass.p2) <- headers
                                                


# table percent middle class for single adult hh

silc.single <- silc.p1 %>% filter(hx040 == 1 & age > 20 & age < 65)
middleclass.single <-  
  silc.single%>% group_by(rb010) %>% summarise(upper_single = sum(upmiddle)/n()*100,
                                                middle_single = sum(middle)/n()*100,
                                                lower_single = sum(lowmiddle)/n()*100,
                                                total_single = sum(totalmiddle)/n()*100,
                                               median_p13_single = incMedian(income_p1_3, 
                                                                      weights = rb050,
                                                                      years = rb010))
colnames(middleclass.single) <- headers


# table percent middle class for old


silc.old <- silc.p1 %>% filter(age > 65)
middleclass.old <-  
  silc.old%>% group_by(rb010) %>% summarise(upper_old = sum(upmiddle)/n()*100,
                                               middle_old = sum(middle)/n()*100,
                                               lower_old = sum(lowmiddle)/n()*100,
                                               total_old = sum(totalmiddle)/n()*100,
                                            median_p13_old = incMedian(income_p1_3, 
                                                                       weights = rb050,
                                                                   years = rb010))

middleclass.old$upper_old <- round(middleclass.old$upper_old, 2)

colnames(middleclass.old) <- headers


# table middle class with two children (needs adjustment load parents id)
silc.p1 <- silc.p1 %>% mutate(child = as.numeric(age <= 17))
silc.p1$child[(silc.p1$rb220 != 0 | silc.p1$rb230 !=0 ) & silc.p1$age <= 24] <- 1
silc.p1 <- silc.p1 %>% 
  group_by(id_h) %>%
  mutate(children = sum(child))



# filter for two children
silc.two <- silc.p1 %>% filter(children == 2)

middleclass.two <-  
  silc.two%>% group_by(rb010) %>% summarise(upper_two = sum(upmiddle)/n()*100,
                                               middle_two = sum(middle)/n()*100,
                                               lower_two = sum(lowmiddle)/n()*100,
                                               total_two = sum(totalmiddle)/n()*100,
                                            median_p13_two = incMedian(income_p1_3, 
                                                                       weights = rb050,
                                                                   years = rb010))
colnames(middleclass.two) <- headers


# filter for more than two children 
silc.many <- silc.p1 %>% filter(children > 2)

middleclass.many <-  
  silc.many%>% group_by(rb010) %>% summarise(upper_many = sum(upmiddle)/n()*100,
                                            middle_many = sum(middle)/n()*100,
                                            lower_many = sum(lowmiddle)/n()*100,
                                            total_many = sum(totalmiddle)/n()*100,
                                            median_p13_many = incMedian(income_p1_3, 
                                                                        weights = rb050,
                                                                   years = rb010))
colnames(middleclass.many) <- headers

# save tables
saveRDS(middleclass.p1, file="GER_middleclass_p1.RData")
saveRDS(middleclass.p2, file="GER_middleclass_p2.RData")
saveRDS(middleclass.many, file="GER_middleclass_many.RData")
saveRDS(middleclass.two, file="GER_middleclass_two.RData")
saveRDS(middleclass.single, file="GER_middleclass_single.RData")
saveRDS(middleclass.old, file="GER_middleclass_old.RData")