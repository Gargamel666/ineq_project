# -----------------------------------------------------------------------------
#
# Indicators R-Script Germany
#
# -----------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)

#source("fetch_data_GER.R")

# Creating Survey Objects -----------------------------------------------------

#get data 




# Indicators ------------------------------------------------------------------

# P1 EUROSTAT -----------------------------------------------------------------
silc.p1 <- readRDS("GER_p1.RData")
silc.p2 <- readRDS("GER_p2.RData")


silc.p1.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.p1) %>% convey_prep()

silc.p2.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.p2) %>% convey_prep()



# Pre-tax factor income (Canberra: primary income) ----------------------------

# Mean
years_mean_p1_1 <- svyby(~income_p1_1,
                         ~as.factor(rb010), silc.p1.svy, svymean)

# Median
years_median_p1_1 <- svyby(~income_p1_1, ~rb010, silc.p1.svy,
                           svyquantile, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p1_1 <- svyby(~income_p1_1, ~as.factor(rb010), silc.p1.svy, svygini)

# P80/P20
years_p80p20_p1_1 <- svyby(~income_p1_1, ~rb010, silc.p1.svy, svyqsr)

# Top 10% share

top_p1_1 <- subset(silc.p1.svy, income_p1_1 >= as.numeric(
  svyquantile(~income_p1_1, silc.p1.svy, quantile=c(0.9))))

topnum_p1_1 <- svyby(~income_p1_1, ~rb010, top_p1_1, svytotal)

topden_p1_1 <- svyby(~income_p1_1, ~rb010, silc.p1.svy, svytotal)

years_top10_p1_1 <- topnum_p1_1 / topden_p1_1

# Pre-tax national income -----------------------------------------------------


# Mean
years_mean_p1_2 <- svyby(~income_p1_2, ~as.factor(rb010), silc.p1.svy, svymean)

# Median
years_median_p1_2 <- svyby(~income_p1_2, ~rb010, silc.p1.svy,
                           svyquantile, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p1_2 <- svyby(~income_p1_2, ~as.factor(rb010), silc.p1.svy, svygini)

# P80/P20
years_p80p20_p1_2 <- svyby(~income_p1_2, ~rb010, silc.p1.svy, svyqsr)

# Top 10% share
top_p1_2 <- subset(silc.p1.svy, income_p1_2 >= as.numeric(
  svyquantile(~income_p1_2, silc.p1.svy, quantile=c(0.9))))

topnum_p1_2 <- svyby(~income_p1_2, ~rb010, top_p1_2, svytotal)

topden_p1_2 <- svyby(~income_p1_2, ~rb010, silc.p1.svy, svytotal)

years_top10_p1_2 <- topnum_p1_2 / topden_p1_2

# Post-tax disposable income --------------------------------------------------

# Mean
years_mean_p1_3 <- svyby(~income_p1_3, ~as.factor(rb010), silc.p1.svy, svymean)

# Median
years_median_p1_3 <- svyby(~income_p1_3, ~rb010, silc.p1.svy,
                           svyquantile, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p1_3 <- svyby(~income_p1_3, ~as.factor(rb010), silc.p1.svy, svygini)

# P80/P20
years_p80p20_p1_3 <- svyby(~income_p1_3, ~rb010, silc.p1.svy, svyqsr)

# Top 10% share


top_p1_3 <- subset(silc.p1.svy, income_p1_3 >= as.numeric(
  svyquantile(~income_p1_3, silc.p1.svy, quantile=c(0.9))))

topnum_p1_3 <- svyby(~income_p1_3, ~rb010, top_p1_3, svytotal)

topden_p1_3 <- svyby(~income_p1_3, ~rb010, silc.p1.svy, svytotal)

years_top10_p1_3 <- topnum_p1_3 / topden_p1_3


# -----------------------------------------------------------------------------

# P2 WID WORLD ----------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ----------------------------

# Mean
years_mean_p2_1 <- svyby(~income_p2_1, ~as.factor(rb010), silc.p2.svy, svymean)

# Median
years_median_p2_1 <- svyby(~income_p2_1, ~rb010, silc.p2.svy,
                           svyquantile, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p2_1 <- svyby(~income_p2_1, ~as.factor(rb010), silc.p2.svy, svygini)

# P80/P20
years_p80p20_p2_1 <- svyby(~income_p2_1, ~rb010, silc.p2.svy, svyqsr)

# Top 10% share

top_p2_1 <- subset(silc.p2.svy, income_p2_1 >= as.numeric(
  svyquantile(~income_p2_1, silc.p2.svy, quantile=c(0.9))))

topnum_p2_1 <- svyby(~income_p2_1, ~rb010, top_p2_1, svytotal)

topden_p2_1 <- svyby(~income_p2_1, ~rb010, silc.p2.svy, svytotal)

years_top10_p2_1 <- topnum_p2_1 / topden_p2_1

# Pre-tax national income -----------------------------------------------------


# Mean
years_mean_p2_2 <- svyby(~income_p2_2, ~as.factor(rb010), silc.p2.svy, svymean)

# Median
years_median_p2_2 <- svyby(~income_p2_2, ~rb010, silc.p2.svy,
                           svyquantile, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p2_2 <- svyby(~income_p2_2, ~as.factor(rb010), silc.p2.svy, svygini)

# P80/P20
years_p80p20_p2_2 <- svyby(~income_p2_2, ~rb010, silc.p2.svy, svyqsr)

# Top 10% share


top_p2_2 <- subset(silc.p2.svy, income_p2_2 >= as.numeric(
  svyquantile(~income_p2_2, silc.p2.svy, quantile=c(0.9))))

topnum_p2_2 <- svyby(~income_p2_2, ~rb010, top_p2_2, svytotal)

topden_p2_2 <- svyby(~income_p2_2, ~rb010, silc.p2.svy, svytotal)

years_top10_p2_2 <- topnum_p2_2 / topden_p2_2


# Post-tax disposable income --------------------------------------------------

# Mean
years_mean_p2_3 <- svyby(~income_p2_3, ~as.factor(rb010), silc.p2.svy, svymean)

# Median
years_median_p2_3 <- svyby(~income_p2_3, ~rb010, silc.p2.svy,
                           svyquantile, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p2_3 <- svyby(~income_p2_3, ~as.factor(rb010), silc.p2.svy, svygini)

# P80/P20
years_p80p20_p2_3 <- svyby(~income_p2_3, ~rb010, silc.p2.svy, svyqsr)

# Top 10% share

top_p2_3 <- subset(silc.p2.svy, income_p2_3 >= as.numeric(
  svyquantile(~income_p2_3, silc.p2.svy, quantile=c(0.9))))

topnum_p2_3 <- svyby(~income_p2_3, ~rb010, top_p2_3, svytotal)

topden_p2_3 <- svyby(~income_p2_3, ~rb010, silc.p2.svy, svytotal)

years_top10_p2_3 <- topnum_p2_3 / topden_p2_3

# Tables ----------------------------------------------------------------------
measures <-c('Years', 'Mean', 'Median', 'Gini','P80P20', 'Top10')
income_concept <- c('Pre-tax factor income','Pre-tax national income', 
                    'Post-tax disposable income')


option(digits=5)
# P1 Eurostat

# pre-tax factor income
pre.tax.fac.p1 <- data.frame(Years = c(2005:2017), years_mean_p1_1, years_median_p1_1, 
                              years_gini_p1_1, years_p80p20_p1_1, 
                             years_top10_p1_1, row.names=NULL)

pre.tax.fac.p1 <- pre.tax.fac.p1[,-grep("se", colnames(pre.tax.fac.p1))] 
pre.tax.fac.p1 <- pre.tax.fac.p1[,-grep("rb", colnames(pre.tax.fac.p1))] 

colnames(pre.tax.fac.p1) <- measures

# pre-tax national income

pre.tax.nat.p1 <- data.frame(Years = c(2005:2017), years_mean_p1_2, years_median_p1_2, 
                             years_gini_p1_2, years_p80p20_p1_2, 
                             years_top10_p1_2, row.names=NULL)

pre.tax.nat.p1 <- pre.tax.nat.p1[,-grep("se", colnames(pre.tax.nat.p1))] 
pre.tax.nat.p1 <- pre.tax.nat.p1[,-grep("rb", colnames(pre.tax.nat.p1))] 

colnames(pre.tax.nat.p1) <- measures

# post-tax disposable income
post.tax.p1 <- data.frame(Years = c(2005:2017), years_mean_p1_3, years_median_p1_3, 
                             years_gini_p1_3, years_p80p20_p1_3, 
                             years_top10_p1_3, row.names=NULL)

post.tax.p1 <- post.tax.p1[,-grep("se", colnames(post.tax.p1))] 
post.tax.p1 <- post.tax.p1[,-grep("rb", colnames(post.tax.p1))] 

colnames(post.tax.p1) <- measures

# P2 WID WORLD

# pre-tax factor income
pre.tax.fac.p2 <- data.frame(Years = c(2005:2017), years_mean_p2_1, years_median_p2_1, 
                             years_gini_p2_1, years_p80p20_p2_1, 
                             years_top10_p2_1, row.names=NULL)

pre.tax.fac.p2 <- pre.tax.fac.p2[,-grep("se", colnames(pre.tax.fac.p2))] 
pre.tax.fac.p2 <- pre.tax.fac.p2[,-grep("rb", colnames(pre.tax.fac.p2))] 

colnames(pre.tax.fac.p2) <- measures

# pre-tax national income

pre.tax.nat.p2 <- data.frame(Years = c(2005:2017), years_mean_p2_2, years_median_p2_2, 
                             years_gini_p2_2, years_p80p20_p2_2, 
                             years_top10_p2_2, row.names=NULL)

pre.tax.nat.p2 <- pre.tax.nat.p2[,-grep("se", colnames(pre.tax.nat.p2))] 
pre.tax.nat.p2 <- pre.tax.nat.p2[,-grep("rb", colnames(pre.tax.nat.p2))] 

colnames(pre.tax.nat.p2) <- measures

# post-tax disposable income
post.tax.p2 <- data.frame(Years = c(2005:2017), years_mean_p2_3, years_median_p2_3, 
                          years_gini_p2_3, years_p80p20_p2_3, 
                          years_top10_p2_3, row.names=NULL)

post.tax.p2 <- post.tax.p2[,-grep("se", colnames(post.tax.p2))] 
post.tax.p2 <- post.tax.p2[,-grep("rb", colnames(post.tax.p2))] 

colnames(post.tax.p2) <- measures

# express Gini and top10% in percent
pre.tax.fac.p1 <- pre.tax.fac.p1 %>% mutate(Gini = Gini * 100, 
                                            Top10 = Top10 * 100)
pre.tax.fac.p2 <- pre.tax.fac.p2 %>% mutate(Gini = Gini * 100, 
                                            Top10 = Top10 * 100)
pre.tax.nat.p1 <- pre.tax.nat.p1 %>% mutate(Gini = Gini * 100, 
                                            Top10 = Top10 * 100)
pre.tax.nat.p2 <- pre.tax.nat.p2 %>% mutate(Gini = Gini * 100, 
                                            Top10 = Top10 * 100)
post.tax.p1 <- post.tax.p1 %>% mutate(Gini = Gini * 100, 
                                            Top10 = Top10 * 100)
post.tax.p2 <- post.tax.p2 %>% mutate(Gini = Gini * 100, 
                                            Top10 = Top10 * 100)

# round tables
pre.tax.fac.p1 <- round(pre.tax.fac.p1, 2)
pre.tax.fac.p1 <- pre.tax.fac.p1 %>% mutate_at(vars(Mean, Median), funs(round(., 0)))
pre.tax.fac.p2 <- round(pre.tax.fac.p2, 2)
pre.tax.fac.p2 <- pre.tax.fac.p2 %>% mutate_at(vars(Mean, Median), funs(round(., 0)))
pre.tax.nat.p1 <- round(pre.tax.nat.p1, 2)
pre.tax.nat.p1 <- pre.tax.nat.p1 %>% mutate_at(vars(Mean, Median), funs(round(., 0)))
pre.tax.nat.p2 <- round(pre.tax.nat.p2, 2)
pre.tax.nat.p2 <- pre.tax.nat.p2 %>% mutate_at(vars(Mean, Median), funs(round(., 0)))
post.tax.p1 <- round(post.tax.p1, 2)
post.tax.p1 <- post.tax.p1 %>% mutate_at(vars(Mean, Median), funs(round(., 0)))
post.tax.p2 <- round(post.tax.p2, 2)
post.tax.p2 <- post.tax.p2 %>% mutate_at(vars(Mean, Median), funs(round(., 0)))

# save tables

stop("switch to table folder!")
saveRDS(pre.tax.fac.p1, file="GER_pre_tax_fac_p1_table.RData")
saveRDS(pre.tax.nat.p1, file="GER_pre_tax_nat_p1_table.RData")
saveRDS(post.tax.p1, file="GER_post_tax_p1_table.RData")
saveRDS(pre.tax.fac.p2, file="GER_pre_tax_fac_p2_table.RData")
saveRDS(pre.tax.nat.p2, file="GER_pre_tax_nat_p2_table.RData")
saveRDS(post.tax.p2, file="GER_post_tax_p2_table.RData")

# Fin -------------------------------------------------------------------------