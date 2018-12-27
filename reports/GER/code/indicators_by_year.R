# -----------------------------------------------------------------------------
#
# Indicators R-Script Germany
#
# -----------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)


# Creating Survey Objects -----------------------------------------------------

silc.p1.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.pos.p1) %>% convey_prep()

silc.p2.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.pos.p2) %>% convey_prep()





# Indicators ------------------------------------------------------------------

# P1 EUROSTAT -----------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ----------------------------
#p1.year.svy<--grouped_df(silc.p1.svy, rb010)

#%>%
 # summarise(mean_p1_1 = mean(income_p1_1))

# Mean
years_mean_p1_1 <- svyby(~income_p1_1,
                         ~as.factor(rb010), silc.p1.svy, svymean)

# Median
years_median_p1_1 <- svyby(~income_p1_1, ~as.factor(rb010), silc.p1.svy,
                           svyquantile, ~total.inc, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p1_1 <- svyby(~income_p1_1, ~as.factor(rb010), silc.p1.svy, svygini)

# P80/P20
years_p80p20_p1_1 <- svyby(~income_p1_1, ~as.factor(rb010), silc.p1.svy, svyqsr, 
                           0.2, 0.8)

# Top 10% share

# Pre-tax national income -----------------------------------------------------


# Mean
years_mean_p1_2 <- svyby(~income_p1_2, ~as.factor(rb010), silc.p1.svy, svymean)

# Median
years_median_p1_2 <- svyby(~income_p1_2, ~as.factor(rb010), silc.p1.svy,
                           svyquantile, ~total.inc, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p1_2 <- svyby(~income_p1_2, ~as.factor(rb010), silc.p1.svy, svygini)

# P80/P20
years_p80p20_p1_2 <- svyby(~income_p1_2, ~as.factor(rb010), silc.p1.svy, svyqsr, 
                           0.2, 0.8)

# Top 10% share


# Post-tax disposable income --------------------------------------------------

# Mean
years_mean_p1_3 <- svyby(~income_p1_3, ~as.factor(rb010), silc.p1.svy, svymean)

# Median
years_median_p1_3 <- svyby(~income_p1_3, ~as.factor(rb010), silc.p1.svy,
                           svyquantile, ~total.inc, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p1_3 <- svyby(~income_p1_3, ~as.factor(rb010), silc.p1.svy, svygini)

# P80/P20
years_p80p20_p1_3 <- svyby(~income_p1_3, ~as.factor(rb010), silc.p1.svy, svyqsr, 
                           0.2, 0.8)

# Top 10% share


# -----------------------------------------------------------------------------

# P2 WID WORLD ----------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ----------------------------

# Mean
years_mean_p2_1 <- svyby(~income_p2_1, ~as.factor(rb010), silc.p2.svy, svymean)

# Median
years_median_p2_1 <- svyby(~income_p2_1, ~as.factor(rb010), silc.p2.svy,
                           svyquantile, ~total.inc, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p2_1 <- svyby(~income_p2_1, ~as.factor(rb010), silc.p2.svy, svygini)

# P80/P20
years_p80p20_p2_1 <- svyby(~income_p2_1, ~as.factor(rb010), silc.p2.svy, svyqsr, 
                           0.2, 0.8)

# Top 10% share

# Pre-tax national income -----------------------------------------------------


# Mean
years_mean_p2_2 <- svyby(~income_p2_2, ~as.factor(rb010), silc.p2.svy, svymean)

# Median
years_median_p2_2 <- svyby(~income_p2_2, ~as.factor(rb010), silc.p2.svy,
                           svyquantile, ~total.inc, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p2_2 <- svyby(~income_p2_2, ~as.factor(rb010), silc.p2.svy, svygini)

# P80/P20
years_p80p20_p2_2 <- svyby(~income_p2_2, ~as.factor(rb010), silc.p2.svy, svyqsr, 
                           0.2, 0.8)

# Top 10% share


# Post-tax disposable income --------------------------------------------------

# Mean
years_mean_p2_3 <- svyby(~income_p2_3, ~as.factor(rb010), silc.p2.svy, svymean)

# Median
years_median_p2_3 <- svyby(~income_p2_3, ~as.factor(rb010), silc.p2.svy,
                           svyquantile, ~total.inc, quantiles = c(0.5), 
                           keep.var = FALSE)


# Gini
years_gini_p2_3 <- svyby(~income_p2_3, ~as.factor(rb010), silc.p2.svy, svygini)

# P80/P20
years_p80p20_p2_3 <- svyby(~income_p2_3, ~as.factor(rb010), silc.p2.svy, svyqsr, 
                           0.2, 0.8)

# Top 10% share

# Tables ----------------------------------------------------------------------
#####not yeat working
measures <-c('Mean', 'Median', 'Gini','P80/P20')
income_concept <- c('Pre-tax factor income','Pre-tax national income', 
                    'Post-tax disposable income')

# P1 Eurostat
pre.tax.fac.p1 <- data.frame(c(years_mean_p1_1, years_median_p1_1, 
                                     years_gini_p1_1, years_p80p20_p1_1, year = 2005:2013))


pre.tax.fac.p1 <- pre.tax.fac.p1[,-grep("se" | "as.factor", colnames(pre.tax.fac.p1))]                             
#digits=nachkomma

colnames(pre.tax.fac.p1) <- measures

# P2 WID WORLD
table2 <- data.frame(round(c(mean_p2_1, median_p2_1, gini_p2_1, p80p20_p2_1, 
                             top10_p2_1), digits = 4),
                     round(c(mean_p2_2, median_p2_2, gini_p2_2, p80p20_p2_2, 
                             top10_p2_2), digits = 4), 
                     round(c(mean_p2_3, median_p2_3, gini_p2_3, p80p20_p2_3, 
                             top10_p2_3), digits = 4), row.names = measures)

colnames(table2) <- income_concept