d <- read.csv2(paste0("data/data_clean_tot.csv"), na.strings = "")
info_especes <- read.csv2("library/info_especes.csv", na.strings = "")
info_especes <- dplyr::select(info_especes, -c("NAME_SPECIES"))
d <- merge(d, info_especes)


d <- subset(d, COMPTAGE == 'OUI' & USN != "USN00" & YEAR >= 2012)

dusn <- aggregate(TOTAL_COUNT ~ USN + DATE + MONTH, data = d, FUN = sum)
dusn <- group_by(dusn, DATE)
dusn <- summarise(dusn, USN, DATE, MONTH, TOTAL_COUNT, TOT = sum(TOTAL_COUNT))

dusn$SAISON <- ifelse(dusn$MONTH <= 2 | dusn$MONTH == 12, "Hivernage", ifelse(dusn$MONTH >= 5 & dusn$MONTH <= 7, "Repro", "Migration"))

dusn_mean <- aggregate(TOTAL_COUNT ~ USN + SAISON, data = dusn, FUN = mean)
dusn_mean <- group_by(dusn_mean, SAISON)
dusn_mean <- summarise(dusn_mean, USN, SAISON, TOTAL_COUNT, TOT = sum(TOTAL_COUNT))
dusn_mean$prop <- dusn_mean$TOTAL_COUNT/dusn_mean$TOT


nat <- c("Limosa lapponica", "Calidris canutus", "Calidris minuta", "Anas clypeata", "Tringa nebularia", "Tringa erythropus",
         "Tringa totanus", "Actitis hypoleucos", "Numenius arquata", "Numenius phaeopus", "Pluvialis squatarola", "Calidris alpina")
internat <- c("Recurvirostra avosetta", "Limosa limosa", "Anas acuta", "Charadrius hiaticula", "Platalea leucorodia", "Calidris alpina")

d <- subset(d, SP %in% c(nat, internat))

dusn <- aggregate(TOTAL_COUNT ~ USN + DATE + SP + MONTH, data = d, FUN = sum)
dusn <- complete(dusn, SP, nesting(DATE, MONTH, USN))
dusn <- complete(dusn, USN, nesting(DATE, MONTH, SP))
dusn$TOTAL_COUNT[is.na(dusn$TOTAL_COUNT)] <- 0

sp <- sort(unique(dusn$SP))

for(s in sp){
  #browser()
  dusn_sp <- filter(dusn, SP == s)
  dusn_sp <- aggregate(TOTAL_COUNT ~ USN + MONTH + SP, data = dusn_sp, FUN = mean)
  
  dusn_sp <- group_by(dusn_sp, MONTH)
  dusn_sp <- summarise(dusn_sp, SP, MONTH, USN, TOTAL_COUNT, TOT = sum(TOTAL_COUNT), PROP = TOTAL_COUNT/TOT)
  dusn_sp$PROP[dusn_sp$PROP == 0] <- NA
  
  
  gg <- ggplot(dusn_sp, aes(x = MONTH, y = USN, colour = PROP, size = PROP)) +
    geom_count() +
    labs(title = paste0("Répartition mensuelle de l'espèce ", s, " sur chaque USN")) +
    scale_color_viridis(option = 'F', direction = -1)
  print(gg)
  ggsave(paste0("output/USN/dotplot_", gsub(" ", "_", s), ".png"))
}


