# -----------------------------------------------------------------------------
#
# Indicators R-Script Germany
#
# -----------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)

# -----------------------------------------------------------------------------

#country <- "DE"


# Source the Setup scripts to provide merged household and personal data
#source("fetch_data_GER.R")



# Creating Survey Objects -----------------------------------------------------

silc.p1.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.pos.p1) %>% convey_prep()

silc.p2.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.pos.p2) %>% convey_prep()

# Subset nur positive Einkommen
silc.p1.svy.16 <- subset(silc.p1.svy, rb010 == 2016)
silc.p2.svy.16 <- subset(silc.p2.svy, rb010 == 2016)

# Indicators ------------------------------------------------------------------

# P1 EUROSTAT -----------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ----------------------------

# Mean
mean_p1_1 <- svymean(~income_p1_1,silc.p1.svy.16)

# Median
median_p1_1 <- svyquantile(~income_p1_1,silc.p1.svy.16,quantile=c(0.5))

# Gini
gini_p1_1 <- svygini(~income_p1_1,silc.p1.svy.16)

# P80/P20
p80p20_p1_1 <- svyqsr(~income_p1_1,silc.p1.svy.16)

# Top 10% share
top10_p1_1 <- svytotal(~income_p1_1,subset(silc.p1.svy.16, income_p1_1>=
                                             as.numeric(svyquantile(~income_p1_1,silc.p1.svy.16,quantile=c(0.9)))),
                       na.rm=TRUE)/svytotal(~income_p1_1,silc.p1.svy.16,na.rm=TRUE)

# Pre-tax national income -----------------------------------------------------

# Mean
mean_p1_2 <- svymean(~income_p1_2,silc.p1.svy.16)

# Median
median_p1_2 <- svyquantile(~income_p1_2,silc.p1.svy.16,quantile=c(0.5))

# Gini
gini_p1_2 <- svygini(~income_p1_2,silc.p1.svy.16)

# P80/P20
p80p20_p1_2 <- svyqsr(~income_p1_2,silc.p1.svy.16)

# Top 10% share
top10_p1_2 <- svytotal(~income_p1_2,subset(silc.p1.svy.16, income_p1_2>=
                                             as.numeric(svyquantile(~income_p1_2,silc.p1.svy.16,quantile=c(0.9)))),
                       na.rm=TRUE)/svytotal(~income_p1_2,silc.p1.svy.16,na.rm=TRUE)

# Post-tax disposable income --------------------------------------------------

# Mean
mean_p1_3 <- svymean(~income_p1_3,silc.p1.svy.16)

# Median
median_p1_3 <- svyquantile(~income_p1_3,silc.p1.svy.16,quantile=c(0.5))

# Gini
gini_p1_3 <- svygini(~income_p1_3,silc.p1.svy.16)

# P80/P20
p80p20_p1_3 <- svyqsr(~income_p1_3,silc.p1.svy.16)

# Top 10% share
top10_p1_3 <- svytotal(~income_p1_3,subset(silc.p1.svy.16, income_p1_3>=
                                             as.numeric(svyquantile(~income_p1_3,silc.p1.svy.16,quantile=c(0.9)))),
                       na.rm=TRUE)/svytotal(~income_p1_3,silc.p1.svy.16,na.rm=TRUE)

# -----------------------------------------------------------------------------

# P2 WID WORLD ----------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ----------------------------

# Mean
mean_p2_1 <- svymean(~income_p2_1,silc.p2.svy.16)

# Median
median_p2_1 <- svyquantile(~income_p2_1,silc.p2.svy.16,quantile=c(0.5))

# Gini
gini_p2_1 <- svygini(~income_p2_1,silc.p2.svy.16)

# P80/P20
p80p20_p2_1 <- svyqsr(~income_p2_1,silc.p2.svy.16)

# Top 10% share
top10_p2_1 <- svytotal(~income_p2_1,subset(silc.p2.svy.16, income_p2_1>=
                                             as.numeric(svyquantile(~income_p2_1,silc.p2.svy.16,quantile=c(0.9)))),
                       na.rm=TRUE)/svytotal(~income_p2_1,silc.p2.svy.16,na.rm=TRUE)

# Pre-tax national income -----------------------------------------------------

# Mean
mean_p2_2 <- svymean(~income_p2_2,silc.p2.svy.16)

# Median
median_p2_2 <- svyquantile(~income_p2_2,silc.p2.svy.16,quantile=c(0.5))

# Gini
gini_p2_2 <- svygini(~income_p2_2,silc.p2.svy.16)

# P80/P20
p80p20_p2_2 <- svyqsr(~income_p2_2,silc.p2.svy.16)

# Top 10% share
top10_p2_2 <- svytotal(~income_p2_2,subset(silc.p2.svy.16, income_p2_2>=
                                             as.numeric(svyquantile(~income_p2_2,silc.p2.svy.16,quantile=c(0.9)))),
                       na.rm=TRUE)/svytotal(~income_p2_2,silc.p2.svy.16,na.rm=TRUE)

# Post-tax disposable income --------------------------------------------------

# Mean
mean_p2_3 <- svymean(~income_p2_3,silc.p2.svy.16)

# Median
median_p2_3 <- svyquantile(~income_p2_3,silc.p2.svy.16,quantile=c(0.5))

# Gini
gini_p2_3 <- svygini(~income_p2_3,silc.p2.svy.16)

# P80/P20
p80p20_p2_3 <- svyqsr(~income_p2_3,silc.p2.svy.16)

# Top 10% share
top10_p2_3 <- svytotal(~income_p2_3,subset(silc.p2.svy.16, income_p2_3>=
                                             as.numeric(svyquantile(~income_p2_3,silc.p2.svy.16,quantile=c(0.9)))),
                       na.rm=TRUE)/svytotal(~income_p2_3,silc.p2.svy.16,na.rm=TRUE)

# Tables ----------------------------------------------------------------------

measures <-c('Mean', 'Median', 'Gini','P80/P20','Top 10% share')
income_concept <- c('Pre-tax factor income','Pre-tax national income', 
                    'Post-tax disposable income')

# P1 Eurostat
table1 <- data.frame(round(c(mean_p1_1, median_p1_1, gini_p1_1, p80p20_p1_1, 
                             top10_p1_1), digits = 4),
                     round(c(mean_p1_2, median_p1_2, gini_p1_2, p80p20_p1_2, 
                             top10_p1_2), digits = 4), 
                     round(c(mean_p1_3, median_p1_3, gini_p1_3, p80p20_p1_3, 
                             top10_p1_3), digits = 4), row.names = measures)

colnames(table1) <- income_concept

# P2 WID WORLD
table2 <- data.frame(round(c(mean_p2_1, median_p2_1, gini_p2_1, p80p20_p2_1, 
                             top10_p2_1), digits = 4),
                     round(c(mean_p2_2, median_p2_2, gini_p2_2, p80p20_p2_2, 
                             top10_p2_2), digits = 4), 
                     round(c(mean_p2_3, median_p2_3, gini_p2_3, p80p20_p2_3, 
                             top10_p2_3), digits = 4), row.names = measures)

colnames(table2) <- income_concept