



# ------------------------------------------------------------------------
#
# Fetch Data for Middle Class Indicator HH R-Script
#
# -------------------------------------------------------------------------

library(dplyr)
# Prepare Data ------------------------------------------------------------

# Download data
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 == 'DE') %>%
  select(pb010, pb030, pb170, pb160, pb210, pl040, pl140, pe010) %>%
  collect(n = Inf)

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 == 'DE') %>%
  select(hb010, hb020, hb030,  hx040, hx050, hx080, hx090, hy010, hy020) %>%
  collect(n = Inf)


silc.r <- tbl(pg, "rr") %>% 
  filter( rb020 == 'DE') %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rb210, rb220, rb230, rx030) %>%
  collect(n = Inf)


# Dowload data for 2014-2016


c14p <- tbl(pg, "c14p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, pb170, pb160, pb210, pl040, pl140, pe010) %>% collect(n = Inf)

c15p <- tbl(pg, "c15p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, pb170, pb160, pb210, pl040, pl140, pe010) %>% collect(n = Inf)

c16p <- tbl(pg, "c16p") %>% filter(pb020 == 'DE') %>% 
  select(pb010, pb030, pb170, pb160, pb210, pl040, pl140,pe010) %>% collect(n = Inf)

cxxp <- bind_rows(c14p, c15p, c16p)
silc.p <- full_join(silc.p, cxxp) 





c14h <- tbl(pg, "c14h") %>% filter(hb020 == 'DE') %>% 
  select(hb010, hb020, hb030,  hx040, hx050, hx080, hx090, hy010, hy020) %>% collect(n = Inf)

c15h <- tbl(pg, "c15h") %>% filter(hb020 == 'DE') %>% 
  select(hb010, hb020, hb030,  hx040, hx050, hx080, hx090, hy010, hy020) %>% collect(n = Inf)

c16h <- tbl(pg, "c16h") %>% filter(hb020 == 'DE') %>% 
  select(hb010, hb020, hb030,  hx040, hx050, hx080, hx090, hy010, hy020) %>% collect(n = Inf)

cxxh <- bind_rows(c14h, c15h, c16h)
silc.h <- full_join(silc.h, cxxh)




c14r <- tbl(pg, "c14r") %>% filter(rb020 == 'DE') %>% 
  select(rb010, rb020, rb030, rb050, rb080, rb090, rb210, rx030) %>% collect(n = Inf)

c15r <- tbl(pg, "c15r") %>% filter(rb020 == 'DE') %>% 
  select(rb010, rb020, rb030, rb050, rb080, rb090, rb210, rx030) %>% collect(n = Inf)

c16r <- tbl(pg, "c16r") %>% filter(rb020 == 'DE') %>% 
  select(rb010, rb020, rb030, rb050, rb080, rb090, rb210, rx030) %>% collect(n = Inf)


cxxr <- bind_rows(c14r, c15r, c16r)
silc.r <- full_join(silc.r, cxxr)

# Merge Data Sets-------------------------------------------------------------



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


# create dummy for dependent child (below 18 or below 24 and still schooling)
#source https://ec.europa.eu/eurostat/statistics-explained/index.php?title=EU_statistics_on_income_and_living_conditions_(EU-SILC)_methodology_%E2%80%93_concepts_and_contents&oldid=373925
silc.rph <- silc.rph %>% mutate(child = as.numeric(age <= 17))
silc.rph$child[(silc.rph$rb220 != 0 | silc.rph$rb230 !=0 ) & silc.rph$age <= 24] <- 1
silc.rph <- silc.rph %>% 
  group_by(id_h) %>%
  mutate(children = sum(child))


#create subset with positive income, we use equalised disposible hh income
# hx090 is also used for poverty risc caculation in silc 

silc.pos <- silc.rph %>% filter(hx090 > 0)  

#save data (careful first setwd to local folder or /data)
stop("do not save in git folder!")
#(silc.pos, file="GER_pos_hh.RData")
# Fin -------------------------------------------------------------------------