# Setup -------------------------------------------------------------------
# Angepasst für die Daten von 2005 bis 2013 

library(dplyr)
if(!exists(c("DE", "2009:2013"))) {
  stop("Please specify country and year.")
}


# Prepare Data ------------------------------------------------------------

# Download data
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 == 'DE') %>%
  select(pb010, pb030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, 
         py120g, py130g, py140g) %>%
  collect(n = Inf)

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 == 'DE') %>%
  select(hb010, hb020, hb030, hy020, hy030g, hy040g, hy050g, hy060g, hy070g, 
         hy080g, hy090g, hy110g, hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

silc.d <- tbl(pg, "dd") %>%
  filter(db020 == 'DE') %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "rr") %>% 
  filter( rb020 == 'DE') %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rb220, rb230, rx030) %>%
  collect(n = Inf)


# Dowload data for 2014-2017
c07p <- tbl(pg, "c07p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c08p <- tbl(pg, "c08p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c09p <- tbl(pg, "c09p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c10p <- tbl(pg, "c10p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c11p <- tbl(pg, "c11p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c12p <- tbl(pg, "c12p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030,py021g) %>% collect(n = Inf)

c13p <- tbl(pg, "c13p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c14p <- tbl(pg, "c14p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, py010g, py050g, py080g, py090g, py100g, py110g, 
         py120g, py130g, py140g, py021g) %>% collect(n = Inf)

c15p <- tbl(pg, "c15p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, py010g, py050g, py080g, py090g, py100g, py110g, 
         py120g, py130g, py140g, py021g) %>% collect(n = Inf)

c16p <- tbl(pg, "c16p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, py010g, py050g, py080g, py090g, py100g, py110g, 
         py120g, py130g, py140g, py021g) %>% collect(n = Inf)

c17p <- tbl(pg, "c17p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, py010g, py050g, py080g, py090g, py100g, py110g, 
         py120g, py130g, py140g, py021g) %>% collect(n = Inf)

cxxp <- bind_rows(c07p, c08p, c09p, c10p, c11p, c12p, c13p, c14p, c15p, c16p, c17p)
silc.p <- full_join(silc.p, cxxp) 





c14h <- tbl(pg, "c14h") %>% filter(hb020 == 'DE') %>% 
  select(hb010, hb020, hb030, hy020, hy030g, hy040g, hy050g, hy060g, hy070g, 
         hy080g, hy090g, hy110g, hy120g, hy130g, hy140g, hx040, 
         hx050) %>% collect(n = Inf)

c15h <- tbl(pg, "c15h") %>% filter(hb020 == 'DE') %>% 
  select(hb010, hb020, hb030, hy020, hy030g, hy040g, hy050g, hy060g, hy070g, 
         hy080g, hy090g, hy110g, hy120g, hy130g, hy140g, hx040, 
         hx050) %>% collect(n = Inf)

c16h <- tbl(pg, "c16h") %>% filter(hb020 == 'DE') %>% 
  select(hb010, hb020, hb030, hy020, hy030g, hy040g, hy050g, hy060g, hy070g, 
         hy080g, hy090g, hy110g, hy120g, hy130g, hy140g, hx040, 
         hx050) %>% collect(n = Inf)

c17h <- tbl(pg, "c17h") %>% filter(hb020 == 'DE') %>% 
  select(hb010, hb020, hb030, hy020, hy030g, hy040g, hy050g, hy060g, hy070g, 
         hy080g, hy090g, hy110g, hy120g, hy130g, hy140g, hx040, 
         hx050) %>% collect(n = Inf)

cxxh <- bind_rows(c14h, c15h, c16h, c17h)
silc.h <- full_join(silc.h, cxxh)




c14r <- tbl(pg, "c14r") %>% filter(rb020 == 'DE') %>% 
  select(rb010, rb020, rb030, rb050, rb080, rb090, rb220, rb230, rx030) %>% collect(n = Inf)

c15r <- tbl(pg, "c15r") %>% filter(rb020 == 'DE') %>% 
  select(rb010, rb020, rb030, rb050, rb080, rb090, rb220, rb230, rx030) %>% collect(n = Inf)

c16r <- tbl(pg, "c16r") %>% filter(rb020 == 'DE') %>% 
  select(rb010, rb020, rb030, rb050, rb080, rb090, rb220, rb230, rx030) %>% collect(n = Inf)

c17r <- tbl(pg, "c17r") %>% filter(rb020 == 'DE') %>% 
  select(rb010, rb020, rb030, rb050, rb080, rb090, rb220, rb230, rx030) %>% collect(n = Inf)


cxxr <- bind_rows(c14r, c15r, c16r, c17r)
silc.r <- full_join(silc.r, cxxr)

# Merge Data Sets-------------------------------------------------------------

# generate car variable 
silc.p$car <- silc.p$py020g
silc.p$car <- ifelse(silc.p$pb010 > 2006, silc.p$py021g, silc.p$car)
#silc.p <- silc.p %>% mutate(car = replace(car, pb010 > 2006, py021g))

# rm(cxxp)

# Rename rb030, pb030 to personal_id
silc.r <- silc.r %>% mutate(personal_id = paste0(rb030, rb010))
silc.p <- silc.p %>% mutate(personal_id = paste0(pb030, pb010))

# merge silc.r and silc.p
silc.rp <- left_join(silc.r, silc.p)

# Create age, household ID, gender variables
silc.rp <- silc.rp %>% 
  mutate(age = rb010 - rb080,
         gender = factor(rb090, labels = c('Male','Female')),
         id_h = paste0(rx030, rb010)) 

# Create unique IDs for merging, merge country and household ID h,d

silc.h <- silc.h %>% mutate(id_h = paste0(hb030, hb010))

silc.d <- silc.d %>% mutate(id_h = paste0(db030, db010))

# Merge silc.rp and silc.h
silc.rph <- left_join(silc.rp, silc.h, by = c("id_h", "rb010" = "hb010", 
                                              "rb020" = "hb020", "rx030" = "hb030"))

# Replace NA's in silc.rph by 0
silc.rph[is.na(silc.rph)] <- 0




# P1 EUROSTAT -----------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) 

# Sum up personal income
silc.rph <- silc.rph %>% mutate(pincome1 = py010g + py050g + py080g + car)

# Sum up personal income of HH
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sum_pincome1 = sum(pincome1))

# Equivalised HH income per person
silc.rph <- silc.rph %>% 
  mutate(income_p1_1 = (sum_pincome1 + hy110g + hy040g + hy090g) / hx050)

# Pre-tax national income 

# Sum up personal income 
silc.rph <- silc.rph %>% mutate(pincome2 = py090g + py100g)

# Sum up personal income of HH
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sum_pincome2 = sum(pincome2))

# Equivalised HH income per person
silc.rph <- silc.rph %>%
  mutate(income_p1_2 = income_p1_1 + (sum_pincome2 / hx050))

# Post-tax national income 

# Sum up personal income 
silc.rph <- silc.rph %>% mutate(pincome3 = py110g + py120g + py130g + py140g)

# Sum up personal income of HH
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sum_pincome3 = sum(pincome3))

# Equivalised HH income per person
silc.rph <- silc.rph %>%
  mutate(income_p1_3 = income_p1_2 + 
                          (sum_pincome3 + hy050g + hy060g + hy070g + hy080g 
                           - hy120g - hy130g - hy140g) / hx050)

silc.rph$test <- silc.rph$hy020/silc.rph$hx050 #should be eqaul to income p1_3


# P2 WID.WORLD ----------------------------------------------------------------

# Generate variable ("n"): count of hh members age >= 20 
silc.rph <- silc.rph %>% 
  add_count(age >= 20, id_h)

# Achtung: Variablen stimmen nur noch für Personen >= 20 Jahre. 

# Pre-tax factor income (Canberra: primary income) 
silc.rph <- silc.rph %>%
  mutate(income_p2_1 = py010g + car + py050g + py080g + 
           (hy110g + hy040g + hy090g)/n)

# Pre-tax national income 
silc.rph <- silc.rph %>%
  mutate(income_p2_2 = income_p2_1 + py090g + py100g)

# Post-tax disposable income 
silc.rph <- silc.rph %>%
  mutate(income_p2_3 = income_p2_2 + py110g + py120g + py130g + py140g + 
           (hy050g + hy060g + hy070g + hy080g - hy120g - hy130g - hy140g)/n)

# Subsetting ------------------------------------------------------------------

# To get useful results we subset to income >= 0
silc.pos.p1 <- silc.rph %>% filter(income_p1_1 > 0, income_p1_2 > 0, 
                                   income_p1_3 > 0)


# Also subset to age >=20
silc.pos.p2 <- silc.rph %>% filter(income_p2_1 > 0, income_p2_2 > 0, 
                                   income_p2_3 > 0, age >= 20)   

#save data (careful first setwd to local folder or /data)
stop("do not save in git folder!")
#saveRDS(silc.pos.p1, file="GER_pos_p1.RData")
#saveRDS(silc.pos.p2, file="GER_pos_p2.RData")

# Fin -------------------------------------------------------------------------
