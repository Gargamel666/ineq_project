# ------------------------------------------------------------------------
#
# Plots
#
# -------------------------------------------------------------------------
library(ggplot2)
library(dygraphs)
library(xts)
library(dplyr)
library(survey)
library(convey)
options(scipen=999)

#source("indicators_by_year.R")
#source("prep_median_income.R")

# load median income 
middleclass.p1 <- readRDS("/data/GER_middleclass_p1.RData")
middleclass.p2 <- readRDS("/data/GER_middleclass_p2.RData")
middleclass.two <- readRDS("/data/GER_middleclass_two.RData")
middleclass.single <- readRDS("/data/GER_middleclass_single.RData")
middleclass.old <- readRDS("/data/GER_middleclass_old.RData")

names <- c('year', 'upper', 'middle', 'lower', 'total', 'median')
colnames(middleclass.p1) <- names

# load p1 and p2
silc.p1 <- readRDS("data/GER_p1_inflation.RData")
silc.p2 <- readRDS("data/GER_p2_inflation.RData")


silc.p1.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.p1) %>% convey_prep()

silc.p2.svy <- svydesign(ids =  ~ id_h,
                         weights = ~rb050,
                         data = silc.p2) %>% convey_prep()

options(scipen=999)


# middle class development -----------------------------------------------------

# Give the chart file a name.
png(file = "reports/GER/img/middleclassp1_strict_total.jpg")

# Plot the bar chart.
plot(middleclass.p1$year, middleclass.p1$middle, type = "o", xlab = "Year", ylab = "Percent", 
     main = "Middle Class", ylim = c(40, max(middleclass.p1$middle, 
                                            middleclass.p1$total)))

lines(middleclass.p1$year, middleclass.p1$total, type = "o", lty = 2)


legend("topright", legend=c("Strict Middle Class", 
                            "Middle Class"),
       lty=1:2, cex = 0.8)


dev.off()

# Give the chart file a name.
png(file = "reports/GER/img/middleclassp1_upper_lower.jpg")

# Plot the line chart.
plot(middleclass.p1$year, middleclass.p1$upper, type = "o", xlab = "Year", ylab = "Percent", 
     main = "Upper and Lower Middle Class", ylim = c(10, max(middleclass.p1$upper, 
                                             middleclass.p1$lower)))

lines(middleclass.p1$year, middleclass.p1$lower, type = "o", lty = 2)


legend("topright", legend=c("Upper Middle Class", 
                            "Lower Middle Class"),
       lty=1:2, cex = 0.8)


dev.off()

# densities ------------------------------------------------------------------

library(psych)
silc.p1.07 <- silc.p1 %>% filter(rb010 == 2007)   
silc.p2.07 <- silc.p2 %>% filter(rb010 == 2007)
silc.p1.12 <- silc.p1 %>% filter(rb010 == 2012)   
silc.p2.12 <- silc.p2 %>% filter(rb010 == 2012)
silc.p1.17 <- silc.p1 %>% filter(rb010 == 2017)   
silc.p2.17 <- silc.p2 %>% filter(rb010 == 2017)
describeBy(silc.p1$income_p1_1, silc.p1$rb010)
describeBy(silc.p2$income_p2_1, silc.p2$rb010)
describeBy(silc.p2$income_p2_2, silc.p2$rb010)
hist(silc.p1.06$income_p1_1)



png(file = "reports/GER/img/densityp11.png")
plot(density(silc.p1.07$income_p1_1), main = "",
     xlim = c(-5000, 100000), ylim = c(0, 7e-05))
lines(density(silc.p1.12$income_p1_1), lty = 2)
lines(density(silc.p1.17$income_p1_1), lty = 3)
legend("topright", legend=c("2007", 
                            "2012", "2017"),
       lty=1:3, cex = 2)
dev.off()

png(file = "reports/GER/img/densityp11closer.png")
plot(density(silc.p1.07$income_p1_1), main = "",
     xlim = c(-5000, 35000), ylim = c(0, 7e-05))
lines(density(silc.p1.12$income_p1_1), lty = 2)
lines(density(silc.p1.17$income_p1_1), lty = 3)
legend("topright", legend=c("2007", 
                            "2012", "2017"),
       lty=1:3, cex = 2)
dev.off()

png(file = "reports/GER/img/densityp21.png")
plot(density(silc.p2.07$income_p2_1), "",
     xlim = c(-5000, 100000), ylim = c(0, 7e-05))
lines(density(silc.p2.12$income_p2_1), lty = 2)
lines(density(silc.p2.17$income_p2_1), lty = 3)
legend("topright", legend=c("2007", 
                            "2012", "2017"),
       lty=1:3, cex = 2)
dev.off()


png(file = "reports/GER/img/densityp13.png")
plot(density(silc.p1.07$income_p1_3), main = "",
     xlim = c(-5000, 100000), ylim = c(0, 7e-05))
lines(density(silc.p1.12$income_p1_3), lty = 2)
lines(density(silc.p1.17$income_p1_3), lty = 3)
legend("topright", legend=c("2007", 
                            "2012", "2017"),
       lty=1:3, cex = 2)
dev.off()

png(file = "reports/GER/img/reports/GER/img/densityp13closer.png")
plot(density(silc.p1.07$income_p1_3), main = "",
     xlim = c(-5000, 50000), ylim = c(0, 7e-05))
lines(density(silc.p1.12$income_p1_3), lty = 2)
lines(density(silc.p1.17$income_p1_3), lty = 3)
legend("topright", legend=c("2007", 
                            "2012", "2017"),
       lty=1:3, cex = 2)
dev.off()

png(file = "reports/GER/img/densityp23.png")
plot(density(silc.p2.07$income_p2_3), "",
     xlim = c(-5000, 100000), ylim = c(0, 7e-05))
lines(density(silc.p2.12$income_p2_3), lty = 2)
lines(density(silc.p2.17$income_p2_3), lty = 3)
legend("topright", legend=c("2007", 
                            "2012", "2017"),
       lty=1:3, cex = 2)
dev.off()

# Lorenz curves ----------------------------------------------------------------

library(ineq)

png(file = "reports/GER/img/Lorenzp1.png")
plot(Lc(silc.p1.17$income_p1_1), main = "")
lines(Lc(silc.p1.17$income_p1_2), lty = 2)
lines(Lc(silc.p1.17$income_p1_3), lty = 3)
legend("topleft", legend=c("Pre-tax factor income", 
                            "Pre-tax national income", "Post-tax disposable income"),
       lty=1:3, cex = 1.3)
dev.off()


png(file = "reports/GER/img/Lorenzp2.png")
plot(Lc(silc.p2.17$income_p2_1), main = "")
lines(Lc(silc.p2.17$income_p2_2), lty = 2)
lines(Lc(silc.p2.17$income_p2_3), lty = 3)
legend("topleft", legend=c("Pre-tax factor income", 
                           "Pre-tax national income", "Post-tax disposable income"),
       lty=1:3, cex = 1.3)
dev.off()

