
library(dplyr)
library(survey)
library(convey)


# Source the Setup scripts to provide merged household and personal data
source("data/_connection.R")
source("reports/GER/code/fetch_data_GER.R")



# Create total personal income --------------------------------------------

# Find string "py" (i.e. income variables) for summing up total personal income. 
#silc.pd <- silc.pd %>% 
 # mutate(total.inc = rowSums(silc.pd[, grep("py", colnames(silc.pd))], 
 #                            na.rm = TRUE)) 
###macht das sinn??


# Subsetting --------------------------------------------------------------

# To get useful results we want to subset to only positive income
silc.pd.inc <- silc.pd %>% filter(py010g > 0)
silc.hd.inc <- silc.hd %>% filter(hy010 > 0)

##only inclde individuals >=20 years!!!

#detect NAs -----------------------------------------------------------------
colSums(is.na(silc.hd.inc))
#no NAs in hd 
colSums(is.na(silc.pd.inc))
#pl060, pl100 and py050n contain NAs

##set NAs equal to zero  
silc.pd.inc$pl060[is.na(silc.pd.inc$pl060)] <- 0
silc.pd.inc$pl100[is.na(silc.pd.inc$pl100)] <- 0
silc.pd.inc$py050n[is.na(silc.pd.inc$py050n)] <- 0

#check if NAs are zero 
colSums(is.na(silc.pd.inc))
summary(silc.pd$pl100)
summary(silc.pd.inc$pl100)
#note: pl100 now median of 0 since there were many NAs..... 

# Creating Income groups according to P1 ----------------------------------------

#drop all variables we do not need for income 

silc.pd.agg<- silc.pd.inc[, -grep("pb", colnames(silc.pd.inc))]
silc.pd.agg<- silc.pd.agg[, -grep("pl", colnames(silc.pd.agg))]
silc.pd.agg<- silc.pd.agg[, -grep("px", colnames(silc.pd.agg))]
silc.pd.agg<- silc.pd.agg[, -grep("db", colnames(silc.pd.agg))]

#aggregate data over id_h
silc.pd.agg <- aggregate(. ~ id_h, silc.pd.agg, sum) 

#add h data to aggregated p file by id_h
silc.phd.inc <- left_join(silc.pd.agg, silc.hd.inc)


# create three income concepts

silc.phd.inc <- silc.phd.inc %>% mutate(pretaxfac_une = py010g + py050g + hy090g + 
                          hy110g  + hy040g + hy090g + py080g) 
silc.phd.inc <- silc.phd.inc %>% mutate(pretaxfac =  pretaxfac_une / hx050)

names(silc.phd.inc$pretaxfac) <- "Pre-tax factor income"



silc.phd.inc <- silc.phd.inc %>% mutate(pretaxnat_une = pretaxfac_une + 
                                          py090g + py100g) 
silc.phd.inc <- silc.phd.inc %>% mutate(pretaxnat =  pretaxnat_une / hx050)

names(silc.phd.inc$pretaxnat) <- "Pre-tax national income"




silc.phd.inc <- silc.phd.inc %>% mutate(posttaxdis_une = pretaxnat_une + py110g + py120g
                                + py130g + py140g + hy050g + hy060g + hy070g 
                                + hy080g - hy120g - hy130g - hy140g) 

silc.phd.inc <- silc.phd.inc %>% mutate(posttaxdis =  posttaxdis_une / hx050)
names(silc.phd.inc$posttaxdis) <- "Post-tax disposable income"

#mistake somewhere since hy020 is not equal to posttaxdis_une

#now we have all variables on hh level

#drop hb020 und db020, Information DE bereints in ID vorhanden 
#drop hb010, Jahr bereits in db010 enthalten 

silc.phd.inc %>% 
  mutate(hb020 = NULL, db020=NULL, hb010=NULL)

