## Tendances par espèces

tendances_saison <- function(){
  # Calculer les tendances au sein d'une année : dresser une courbe-type de population en utilisant les données
  # des 15 dernières années et la modéliser (ressemblera certainement à une sinusoïde)
  # Y représenter la moyenne, les écarts-type et les extrêmes sous forme de courbe et de bandeaux
  # Y superposer la courbe de l'année passée (lastYear) modélisée
  
  d <- read.csv2(paste0("data/data_clean_tot.csv"), na.strings = "")
  info_especes <- read.csv2("library/info_especes.csv", na.strings = "")
  info_especes <- dplyr::select(info_especes, -c("NAME_SPECIES"))
  d <- merge(d, info_especes)
  
  nat <- c("Limosa lapponica", "Calidris canutus", "Calidris minuta", "Anas clypeata", "Tringa nebularia", "Tringa erythropus",
           "Tringa totanus", "Actitis hypoleucos", "Numenius arquata", "Numenius phaeopus", "Pluvialis squatarola", "Calidris alpina")
  internat <- c("Recurvirostra avosetta", "Limosa limosa", "Anas acuta", "Charadrius hiaticula", "Platalea leucorodia", "Calidris alpina")
  
  
  d <- subset(d, d$COMPTE == "TRUE" & (d$SP %in% nat | d$SP %in% internat) & d$COMPTAGE == "OUI")
  d$DATE <- as.Date.character(d$DATE, format = "%Y-%m-%d")
  dcompt <- aggregate(TOTAL_COUNT ~ DATE + MONTH + YEAR + SP, data = d, FUN = sum)
  dcompt$DM <- str_sub(dcompt$DATE,-5,-1)
  dcompt$DM <- as.Date.character(dcompt$DM, format = "%m-%d")
  
  species <- sort(unique(dcompt$SP))
  
  dstats <- dcompt %>%
            group_by(SP, DM) %>%
            summarise(MEAN = mean(TOTAL_COUNT), sd = sd(TOTAL_COUNT))
  dcompt <- merge(dcompt, dstats)
  
  for(s in species){
    dtemp <- subset(dcompt, dcompt$SP == s)
    gg <- ggplot(dtemp, aes(x = DM, y = MEAN, ymin = MEAN - sd, ymax = MEAN + sd)) +
      geom_line() +
      #geom_point(aes(y = TOTAL_COUNT)) +
      geom_ribbon(alpha = 0.3, fill = 'royalblue1')
    print(gg)
    
    dtemp <- subset(dcompt, dcompt$SP == s)
    gg <- dtemp %>% filter(YEAR >= 2016) %>%
      ggplot(aes(x = DATE, y = TOTAL_COUNT)) +
      geom_line()
      #geom_point(aes(y = TOTAL_COUNT)) +
      #geom_ribbon(alpha = 0.3, fill = 'royalblue1')
    print(gg)
    
    dtemp <- dtemp %>% complete(DATE = seq(as.Date("1982-01-01"), as.Date("2021-12-31"), by = "1 day"))
    
    temp_ts <- ts(dtemp %>% dplyr::select(-DATE),
                        start = c(dtemp$DATE[1] %>% year(), 1),
                        frequency = 365.25)
    autoplot(temp_ts, facets = TRUE) +
      scale_x_continuous(breaks = 1982:2021)
  }
}



tendances_annuelles <- function(){
  # Facile à calculer entre 1982 et 2007
  # Facile à calculer entre 2008 et 2021
  # Comment faire pour faire la transition malgré le changement de protocole ?
  
  d <- read.csv2(paste0("data/data_clean_tot.csv"), na.strings = "")
  info_especes <- read.csv2("library/info_especes.csv", na.strings = "")
  info_especes <- dplyr::select(info_especes, -c("NAME_SPECIES"))
  d <- merge(d, info_especes)
  
  nat <- c("Limosa lapponica", "Calidris canutus", "Calidris minuta", "Anas clypeata", "Tringa nebularia", "Tringa erythropus",
           "Tringa totanus", "Actitis hypoleucos", "Numenius arquata", "Numenius phaeopus", "Pluvialis squatarola", "Calidris alpina")
  internat <- c("Recurvirostra avosetta", "Limosa limosa", "Anas acuta", "Charadrius hiaticula", "Platalea leucorodia", "Calidris alpina")
  
  
  d <- subset(d, d$COMPTE == "TRUE" & (d$SP %in% nat | d$SP %in% internat) & d$COMPTAGE == "OUI")
  d$DATE <- as.Date.character(d$DATE, format = "%Y-%m-%d")
  
  dsp <- read.csv("library/sp.csv", sep=";")
  d <- merge(d, dsp)
  
  d$CODE_DECADE <- ifelse(as.integer(str_sub(d$DATE, -2)) <= 10, d$MONTH * 10 + 1,
                             ifelse(as.integer(str_sub(d$DATE, -2)) >= 21, d$MONTH * 10 + 3, d$MONTH * 10 + 2))
  d$SAISON <- ifelse(d$CODE_DECADE >= d$HIV_START | d$CODE_DECADE <= d$HIV_END, "Hivernage",
                        ifelse(d$CODE_DECADE <= d$REP_END & d$CODE_DECADE >= d$REP_START, "Repro", "Migration"))
  
  dcompt <- aggregate(TOTAL_COUNT ~ DATE + MONTH + YEAR + SP + SAISON, data = d, FUN = sum)
  dcompt <- complete(dcompt, SP, DATE)
  dcompt$YEAR <- year(dcompt$DATE)
  dcompt$MONTH <- month(dcompt$DATE)
  dcompt$MDAY <- day(dcompt$DATE)
  
  dcompt <- merge(dcompt, dsp)
  
  dcompt$CODE_DECADE <- ifelse(as.integer(str_sub(dcompt$DATE, -2)) <= 10, dcompt$MONTH * 10 + 1,
                          ifelse(as.integer(str_sub(dcompt$DATE, -2)) >= 21, dcompt$MONTH * 10 + 3, dcompt$MONTH * 10 + 2))
  dcompt$SAISON <- ifelse(dcompt$CODE_DECADE >= dcompt$HIV_START | dcompt$CODE_DECADE <= dcompt$HIV_END, "Hivernage",
                     ifelse(dcompt$CODE_DECADE <= dcompt$REP_END & dcompt$CODE_DECADE >= dcompt$REP_START, "Repro", "Migration"))
  dcompt <- dplyr::select(dcompt, 0:7)
  
  dcompt <- subset(dcompt, !(SP %in% c("Anas acuta", "Anas clypeata") & is.na(TOTAL_COUNT) & MDAY != 15))
  dcompt$TOTAL_COUNT[is.na(dcompt$TOTAL_COUNT)] <- 0
  
  dcompt <- aggregate(TOTAL_COUNT ~ YEAR + SP + SAISON, data = dcompt, FUN = mean)
  dcompt <- complete(dcompt, YEAR, SP, SAISON)
  dcompt$TOTAL_COUNT[is.na(dcompt$TOTAL_COUNT)] <- 0
  
  saison <- sort(unique(dcompt$SAISON))
  sp <- sort(unique(dcompt$SP))
  
  for(s in sp){
    
    gg <- ggplot(data = filter(dcompt, SP == s), aes(x = YEAR, y = TOTAL_COUNT)) +
      geom_line(aes(color = SAISON), size = 0.8) +
      labs(x = "Année", y = "Abondance",
           title = paste0("Variations interannuelles des effectifs de ", s)) +
      scale_color_manual(values = alpha(c("cornflowerblue", "gold", "red"), 1))
    print(gg)
    
    ggsave(paste0("output/Tendances/tend_", gsub(" ", "_", s), ".png"))
  }
  
  msp <- subset(dcompt, TOTAL_COUNT != 0)
  msp <- group_by(msp, SP, SAISON)
  msp <- summarise(msp, MEAN = mean(TOTAL_COUNT))
  
  dcompt <- merge(dcompt, msp)
  colnames(dcompt)[c(4,5)] <- c('MEAN_YEAR', 'MEAN_GLOB')
  
  write.csv2(dcompt, file = "library/comp_year_global.csv", row.names=FALSE,na="",quote=FALSE)
  
  return(d)
}
