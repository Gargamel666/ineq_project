# ------------------------------------------------------------------------
#
# Indicators for Germany
# Autoren: Gust
# Datum: 2018-11-23
#
# -------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)


# Source the Setup scripts to provide merged household and personal data
source("R/_connection.R")
source("R/_setup.R")


# Subsetting --------------------------------------------------------------

# To get useful results we may want to subset to only positive income
silc.pd.inc <- silc.pd %>% filter(py010g > 0)
silc.hd.inc <- silc.hd %>% filter(hy010 > 0)

# Creating Survey Objects -------------------------------------------------

silc.pd.svy <- svydesign(ids =  ~ id_h,
                         strata = ~db020,
                         weights = ~pb040,
                         data = silc.pd.inc) %>% convey_prep()

silc.hd.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.hd.inc) %>% convey_prep()



# Indicators by Year --------------------------------------------------------------

# Mean Income
#

svyby(~total.inc, ~as.factor(db010), silc.pd.svy, svymean)
svyby(~hy010, ~as.factor(db010), silc.hd.svy, svymean)

# Median Income
#

svyby(~total.inc, ~as.factor(db010), silc.pd.svy,
      svyquantile, ~total.inc, quantiles = c(0.5), keep.var = FALSE)
svyby(~hy010, ~as.factor(db010), silc.hd.svy,
      svyquantile, ~hy010, quantiles = c(0.5), keep.var = FALSE)

# Decile Points
#

svyby(~total.inc, ~as.factor(db010), silc.pd.svy,
      svyquantile, ~total.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)
svyby(~hy010, ~as.factor(db010), silc.hd.svy,
      svyquantile, ~total.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

# Quantile Share Ratio
#

svyby(~total.inc, ~as.factor(db010), silc.pd.svy, svyqsr, 0.2, 0.8)
svyby(~hy010, ~as.factor(db010), silc.hd.svy, svyqsr, 0.2, 0.8)

# Top 10% Income Share not ready!!!
#
#for(db010 in 2005:2013){
svytotal(~total.inc, subset(silc.pd.svy, db010==c(2013) & total.inc >=
                              as.numeric(svyquantile(~total.inc, silc.pd.svy, quantile = 0.9)))) /
  svytotal(~total.inc, subset(silc.pd.svy, db010==c(2013)))
#}

svytotal(~hy010, subset(silc.hd.svy, db010==2013 & hy010 >=
                          as.numeric(svyquantile(~hy010, silc.hd.svy, quantile = 0.9)))) /
  svytotal(~hy010,subset(silc.hd.svy, db010==2013))




# Gini Coefficient
#

svyby(~total.inc, ~as.factor(db010), silc.pd.svy, svygini)
svyby(~hy010, ~as.factor(db010), silc.hd.svy, svygini)

# Theil Index
#

svyby(~total.inc, ~as.factor(db010), silc.pd.svy,
      svygei, epsilon = 1)
svyby(~hy010, ~as.factor(db010), silc.hd.svy,
      svygei, epsilon = 1)

