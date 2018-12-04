# Setup -------------------------------------------------------------------
# Angepasst für die Daten von 2005 bis 2013 

library(dplyr)
if(!exists(c("DE", "2009:2013"))) {
  stop("Please specify country and year.")
}


# Prepare Data ------------------------------------------------------------

# Download data
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 %in% "DE" & pb010 %in% c(2009, 2010, 2011,2012, 2013)) %>%
  select(pb010, pb020, pb030, pb040, pb150, pl060,pl100, py010g, py050g, py080g,
  py090g, py100g, py110g, py120g, py130g, py140g, py050n, px010, px030) %>% 
  collect(n = Inf)

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% "DE" & hb010 %in% c(2009, 2010, 2011,2012, 2013)) %>%
  select(hb010, hb020, hb030, hx010, hx050, hy010, hy020, hy110g, hy120g, hy130g,
  hy140g, hy040g, hy050g, hy060g, hy070g, hy080g, hy090g) %>%
  collect(n = Inf)

silc.d <- tbl(pg, "dd") %>%
  filter(db020 %in% "DE" & db010 %in% c(2009, 2010, 2011,2012, 2013)) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "rr") %>% 
  filter(rb020 %in% "DE" & rb010 %in% c(2009, 2010, 2011,2012, 2013)) %>%
  select(rb010, rx030, rb020, rb030, rb050) %>%
  collect(n = Inf)

# Create unique IDs for merging by country, year, and hh ID
silc.p <- silc.p %>% mutate(id_h = paste0(pb020, px030, pb010))

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030, hb010))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030, db010))

silc.r <- silc.r %>% mutate(id_h = paste0(rb020, rx030, rb010))

# Merge the datasets
silc.pd <- left_join(silc.p, silc.d %>% select(id_h, db010, db020, db040, db090))

silc.hd <- left_join(silc.h, silc.d)


# Fin ---------------------------------------------------------------------

message("Prepared data for DE")

