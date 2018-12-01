#(1) Einkommen aus Arbeit (inkl. Selbstständige)
#gross employee cash or near cash income (PY010G), 
#company car (PY021G), 
#gross cash benefits or losses from self-employment (including royalties) (PY050G),
#income received by people aged under 16 (HY110G);

#Für Einkommen aus Arbeit, neuer Dataframe 

eink <- full_join(silc.pd.inc, silc.hd.inc)

# Neue Spalte Einkommen aus Arbeit 
# (py021g nicht möglich zu laden) 
EK.Ar <- einkommen %>% mutate(EK.Arbeit = py010g + py050n + hy110g)




#(2) Vermögenseinkommen
#income from rental of a property or land (HY040G),
#interests, dividends, profit from capital investments in unincorporated business (HY090G),
#pensions received from individual private plans (PY080G),

# neuer Dataframe 

Ek.Ver <- full_join(silc.pd, silc.hd.inc)

#neue Spalte Eink.Verm 
Einkommen.Ver <- Ek.Ver %>% mutate(Eink.Verm = hy040g + hy090g + py080g)




