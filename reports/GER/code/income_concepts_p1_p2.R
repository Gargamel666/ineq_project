


# Rename rb030, pb030 to personal_id
silc.r <- silc.r %>% mutate(personal_id = paste0(rb030, rb010))
silc.p <- silc.p %>% mutate(personal_id = paste0(pb030, pb010))

# merge silc.r and silc.p
silc.rp <- left_join(silc.r, silc.p)

# Create age, household ID, gender variables
silc.rp <- silc.rp %>% 
  mutate(age = rb010 - rb080,
         gender = factor(rb090,labels = c('Male','Female')),
         id_h = paste0(rb020,rx030, rb010)) 

# Create unique IDs for merging, merge country and household ID h,d

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030, hb010))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030, db010))

# Merge silc.rp and silc.h
silc.rph <- left_join(silc.rp, silc.h)

# Replace NA's in silc.rph by 0
silc.rph[is.na(silc.rph)] <- 0

# P1 EUROSTAT -----------------------------------------------------------------

# Pre-tax factor income (Canberra: primary income) ----------------------------

# Sum up personal income
silc.rph <- silc.rph %>% mutate(pincome1 = py010g + py050g + py080g)
# company car missing

# Sum up personal income of HH
silc.rph <- silc.rph %>% group_by(id_h) %>% 
  do({sum_pincome1 = sum(distinct(.,pincome1)$pincome1); 
  mutate(., sum_pincome1 = sum_pincome1)})

# Equivalised HH income per person
silc.rph <- silc.rph %>% 
  mutate(pretaxfac = ((sum_pincome1 + hy110g + hy040g + hy090g) / hx050))

# Pre-tax national income -----------------------------------------------------

# Sum up personal income 
silc.rph <- silc.rph %>% mutate(pincome2 = py090g + py100g)

# Sum up personal income of HH
silc.rph <- silc.rph %>% group_by(id_h) %>% 
  do({sum_pincome2 = sum(distinct(.,pincome2)$pincome2); 
  mutate(., sum_pincome2 = sum_pincome2)})

# Equivalised HH income per person
silc.rph <- silc.rph %>%
  mutate(pretaxnat = (pretaxfac + sum_pincome2 / hx050))

# Post-tax national income ----------------------------------------------------

# Sum up personal income 
silc.rph <- silc.rph %>% mutate(pincome3 = py110g + py120g + py130g + py140g)

# Sum up personal income of HH
silc.rph <- silc.rph %>% group_by(id_h) %>% 
  do({sum_pincome3 = sum(distinct(.,pincome3)$pincome3); 
  mutate(., sum_pincome3 = sum_pincome3)})

# Equivalised HH income per person
silc.rph <- silc.rph %>%
  mutate(posttaxdis = (pretaxnat + 
                          (sum_pincome3 + hy050g + hy060g + hy070g + hy080g 
                           - hy120g - hy130g - hy140g) / hx050))


# P2 WID.WORLD ----------------------------------------------------------------


# Generate variable ("n"): count of hh members age >= 20 
silc.rph <- silc.rph %>% 
  add_count(age >= 20, id_h)

# create subset for younger 20----------------------------------------

silc.rph.adult <- silc.rph %>% filter(age >= 20)

# Pre-tax factor income (Canberra: primary income) ----------------------------
silc.rph <- silc.rph %>%
  mutate(pretaxfac = py010g + py050g + py080g + (hy110g + hy040g + hy090g)/n)

# Pre-tax national income -----------------------------------------------------
silc.rph <- silc.rph %>%
  mutate(pretaxnat = pretaxfac + py090g + py100g)

# Post-tax disposable income --------------------------------------------------
silc.rph <- silc.rph %>%
  mutate(posttaxdis = pretaxnat + py110g + py120g + py130g + py140g + 
           (hy050g + hy060g + hy070g + hy080g - hy120g - hy130g - hy140g)/n)

# Fin -------------------------------------------------------------------------
