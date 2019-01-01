# ------------------------------------------------------------------------
#
# Plots
#
# -------------------------------------------------------------------------
library(ggplot2)
library(dygraphs)
library(xts)

# load median income 
middleclass.p1 <- readRDS("GER_middleclass_p1.RData")
middleclass.p2 <- readRDS("GER_middleclass_p2.RData")
middleclass.two <- readRDS("GER_middleclass_two.RData")
middleclass.single <- readRDS("GER_middleclass_single.RData")
middleclass.old <- readRDS("GER_middleclass_old.RData")

names <- c('year', 'upper', 'middle', 'lower', 'total', 'median')
colnames(middleclass.p1) <- names

# create plot




# Give the chart file a name.
png(file = "middleclassp1_strict_total.jpg")

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
png(file = "middleclassp1_upper_lower.jpg")

# Plot the bar chart.
plot(middleclass.p1$year, middleclass.p1$upper, type = "o", xlab = "Year", ylab = "Percent", 
     main = "Upper and Lower Middle Class", ylim = c(10, max(middleclass.p1$upper, 
                                             middleclass.p1$lower)))

lines(middleclass.p1$year, middleclass.p1$lower, type = "o", lty = 2)


legend("topright", legend=c("Upper Middle Class", 
                            "Lower Middle Class"),
       lty=1:2, cex = 0.8)


dev.off()
