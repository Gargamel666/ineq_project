library(eurostat)
library(dplyr)
library(ggplot2)
library(svglite)

# DATA ------------------------------------------------------------------------

pov <- get_eurostat("ilc_li02") # Quote der von Armut bedrohten Personen nach 
# Armutsgefährdungsgrenze, Alter und Geschlecht

quants <- get_eurostat("ilc_di01") # Einkommensverteilung nach Quantilen

qsr <- get_eurostat("ilc_di11") # S80/S20 Einkommensquintilverhältnis 
# nach Geschlecht und nach Altersklassen

gini <- get_eurostat("ilc_di12") # Gini-Koeffizient des verfügbaren 
# Äquivalenzeinkommens 

inc <- get_eurostat("ilc_di04") # Mean and median income by household type

# GINI ------------------------------------------------------------------------

gini <- gini %>% filter(geo == "DE") %>% 
  rename(Gini = "values")

# plot


gini_plot <- ggplot() +
  geom_line(aes(y = gini$Gini,x = gini$time), color = "pink", size = 1) +
              labs(color = '', x = "Jahr", y = "Gini Koeffizient", 
                   title = "Gini-Koeffizient des verfügbaren Äquivalenzeinkommens",
                   subtitle = "1995 - 2017",
                   caption = "Quelle: Eurostat") +
  expand_limits(y = c(22, 33)) + ggsave(file='gini_plot.svg',height=4,width=7)
gini_plot


# MEDIAN/MEAN -----------------------------------------------------------------

mean <- inc %>% filter(geo == "DE", hhtyp == "TOTAL", unit == "EUR", 
                       indic_il == "MEI_E")  %>% rename("Mittelwert" = values)


median <- inc %>% filter(geo == "DE", hhtyp == "TOTAL", unit == "EUR", 
                       indic_il == "MED_E")  %>% rename("Median" = values)
  

# plot
income_plot <- ggplot() +
  geom_line(mapping = aes(y = mean$Mittelwert, x = mean$time, 
                          color = "Mittleres Einkommen"), size = 1) +
  geom_line(mapping = aes(y = median$Median, x = median$time, 
                          color = "Median Einkommen"), size = 1) +   
  scale_color_manual(values = c('Mittleres Einkommen' = 'pink',
                                'Median Einkommen' = 'darkblue')) +
  labs(color = '', x = "Jahr", y = "Euro", 
       title = "Verfügbares Mittleres und Median Äquivalenzeinkommen",
       subtitle = "1995 - 2017",
       caption = "Quelle: Eurostat") + 
  expand_limits(y = c(10000, 30000)) + ggsave(file='income_plot.svg',height=4,width=7)

income_plot

# Plot the bar chart.



# Top 10% ---------------------------------------------------------------------

quants <- quants %>% filter(geo == "DE", indic_il == "SHARE", currency == "EUR",
                            quantile == "D10") %>%
  rename("Anteil Top 10%" = "values")

# plot
top10_plot <- ggplot() +
  geom_line(aes(y = quants$"Anteil Top 10%",x = quants$time), color = "pink", size = 1) +
  labs(color = '', x = "Jahr", y = "Anteil in %", 
       title = "Anteil des oberen Dezils am nationalen Äquivalenznationaleinkommen ",
       subtitle = "1995 - 2017",
       caption = "Quelle: Eurostat") +
  expand_limits(y = c(20, 25)) + ggsave(file='top10_plot.svg',height=4,width=7)

top10_plot

# ARMUTSGEFÄHRDUNG ------------------------------------------------------------

pov <- pov %>% filter(geo == "DE", age == "TOTAL", indic_il == "LI_R_MD60", 
                      sex == "T", unit == "PC")  %>%
  rename(Armutsgefährdungsquote = "values")

# plot
pov_plot <- ggplot() +
  geom_line(aes(y = pov$Armutsgefährdungsquote,x = pov$time), color = "pink", size = 1) +
  labs(color = '', x = "Jahr", y = "Armutsgefährdungsquote", 
       title = "Armutsgefährdungsquote (Grenze: 60% des medianen Äquivalenzeinkommens nach Sozialleistungen)",
       subtitle = "1995 - 2017",
       caption = "Quelle: Eurostat") +
  expand_limits(y = c(10, 20)) + ggsave(file='pov_plot.svg',height=4,width=7)

pov_plot


# S80/20 ----------------------------------------------------------------------

qsr <- qsr %>% filter(geo == "DE", age == "TOTAL", sex == "T")  %>% rename( "S80/20" = "values")

pov_plot <- ggplot() +
  geom_line(aes(y = qsr$"S80/20",x = qsr$time), color = "pink", size = 1) +
  labs(color = '', x = "Jahr", y = "P80/P20-Ratio", 
       title = "P80/P20 Ratio (Äquivalenzeinkommens nach Sozialleistungen)",
       subtitle = "1995 - 2017",
       caption = "Quelle: Eurostat") +
  expand_limits(y = c(3, 6)) + ggsave(file='P80P20_plot.svg',height=4,width=7)

P80P20_plot  
# TABELLE ---------------------------------------------------------------------

indikatoren_bel <- data.frame(gini$time, mean$Mittelwert, median$Median, 
                              gini$Gini, quants$`Anteil Top 10%`, qsr$`S80/20`, 
                              pov$Armutsgefährdungsquote)

colnames(indikatoren_bel) <- c("Jahr", "Mittelwert", "Median", "Gini", 
                               "Anteil der Top 10%", "S80/20", 
                               "Armutsgefährdungsquote")

indikatoren_bel$Jahr <- substr(indikatoren_bel$Jahr, 1, 4)
write.csv(indikatoren_bel, file = "table_bel", row.names = F)
