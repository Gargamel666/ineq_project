#P2 (wid.world):Nur Personen >= 20 Jahre & partial sharing of resources 
#Bilden Sie die Einkommensaggregate, persönliche Einkommen bleiben den jeweiligen Personen vorbehalten, Einkommensarten #die nur auf Haushaltsebene verfügbar sind werden durch die Anzahl der HH Mitglieder >= 20 Jahren dividiert und allen zu gleichen Teilen zugeordnet. Personen unter 20 Jahren sollen vor der Berechnung der Ungleichheitsindikatoren ausgeschlossen werden. 

#-> Einkommen Pro Person (über 20) berechnen, Haushaltseinkommen durch Personenanzahl teilen und zuordnen 
# Einkommen Pro Person PY010G, PY050G, PY080G, PY090G, PY100G, PY110G, PY120G, PY130G, PY140G
# Einkommen Haushalt HY110G, HY040G, HY090G, HY050G, HY070G, HY080G, HY120G, HY130G, HY140G

# Source the Setup scripts to provide merged household and personal data
source("data/_connection.R")
source("reports/GER/code/fetch_data_GER.R")

#Einkommen pro Person (p Daten)

#positive Income 

silc.pd.inc <- silc.pd %>% filter(py010g > 0)
silc.hd.inc <- silc.hd %>% filter(hy010 > 0)

#set NA`s to zero 
silc.pd.inc$pl060[is.na(silc.pd.inc$pl060)] <- 0
silc.pd.inc$pl100[is.na(silc.pd.inc$pl100)] <- 0
silc.pd.inc$py050n[is.na(silc.pd.inc$py050n)] <- 0

#Make new column personal income (Ek.pers)

silc.pd.pers <- silc.pd.inc %>%
  mutate(Ek.pers = py010g + py050g + py080g + py090g + py100g + py110g + py120g + py130g+py140g)

#devide HY variables by number of people in housholde >=20
`