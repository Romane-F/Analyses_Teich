#d <- read.csv2("data/data_clean_tot.csv", na.strings = "")


### FONCTION : CTI

### Entrée : data : dataframe
###          startRep, endRep, startHiv, endHiv : integer. Bornes de la période d'étude
###          saison : character. "hiver" ou "repro". Laisser "" pour les deux saisons.
###          hors sp : character. Vecteur des espèces à écarter de l'analyse. Laisser "" pour tout conserver.
###          class : bool. Calculer les contributions par classes (famille, temp, cycle migratoire)
###          splitHiv, splitRep : integer. Vecteur de dates de rupture (découpe l'analyse).
###                               Laisser NULL pour ne pas faire de rupture.
###          saveFig : bool. Sauvegarder ou non les figures

### Sortie : Rien.

### Description : Calcule l'indice thermique de la communauté, son évolution interannuelle,
###               les contributions de chaque espèce et groupe d'espèces à son évolution,
###               pour les saisons de nidification et d'hivernage.

CTI <- function(data = d, startRep = firstYear, endRep = lastYear, startHiv = firstYear, endHiv = lastYear,
                saison = "", hors_sp = "", class = TRUE, splitHiv = NULL, splitRep = NULL, saveFig = TRUE,
                fileSTI = "STI.csv", fileLog = fileLog){

    catlog(c("\n===================================\n           CALCUL DU CTI\n=================================== \n\n"),fileLog)
  catlog(c(" - Sélection des espèces étudiées\n"), fileLog)
  
  ## Ecarte les espèces renseignées dans hors_sp
  data <- subset(data, data$SP %nin% hors_sp & data$COMPTAGE == "OUI")
  
  ## Groupe les goélands pour lisser l'effet du changement de classif entre argenté, leuco et pontique.
  data$SP <- ifelse(data$SP == "Larus argentatus" |
                   data$SP == "Larus cachinnans" |
                   data$SP == "Larus michahellis",
                 "Larus magna sp", data$SP)
  
  ## Somme les observations par date.
  dCTI <- aggregate(TOTAL_COUNT ~ DATE + MONTH + YEAR + SP, data = data, FUN = sum)
  dCTI <- complete(dCTI, SP, DATE)
  dCTI$YEAR <- year(dCTI$DATE)
  dCTI$MONTH <- month(dCTI$DATE)
  dCTI$MDAY <- day(dCTI$DATE)
  
  dCTI <- subset(dCTI, !(SP %in% c("Anas acuta", "Anas clypeata") & is.na(TOTAL_COUNT) & MDAY != 15))
  dCTI$TOTAL_COUNT[is.na(dCTI$TOTAL_COUNT)] <- 0
  
  catlog(c(" - Chargement du fichier contenant les STI\n\n"), fileLog)
  STI <- read.csv(paste0("library/", fileSTI), sep=";")
  
  ## Calculs pour l'hivernage.
  if(saison != "repro"){
    catlog(c(" - Calcul du CTI des communautés hivernantes\n"), fileLog)
    p <- c(startHiv, splitHiv, endHiv)
    STI_hiver <- STI %>% dplyr::select(c('SP', 'STI_hiv_mean', 'STI_hiv_sd')) %>% subset(!is.na(STI$STI_hiv_mean))
    
    ## Groupe décembre de l'année n-1 avec janvier et février de l'année n, sélectionne sur les dates
    ## et moyenne par mois.
    dCTI$YEAR <- ifelse(dCTI$MONTH == 12, dCTI$YEAR + 1, dCTI$YEAR)
    dCTI_hiv <- dCTI %>% subset((MONTH == 12 | MONTH <= 2) & YEAR >= startHiv & YEAR <= endHiv)
    dCTI_hiv <- aggregate(TOTAL_COUNT ~ YEAR + SP, data = dCTI_hiv, FUN = mean)
    
    ## Boucle for : fait les calculs pour chaque période définie par start, end et split.
    for(i in 1:(length(p)-1)){
      ## Adapte les données au script de P. Gaüzère
      if(i > 1) p[i] <- p[i]+1
      catlog(c("   - Calcul du CTI pour la période", p[i], "-", p[i+1], "\n"), fileLog)
      #browser()
      census <- subset(dCTI_hiv, dCTI_hiv$YEAR >= p[i] & dCTI_hiv$YEAR <= p[i+1])
      colnames(census) <- c("date", "species", "n")
      species <- STI_hiver %>% mutate(STI_hiv_v = STI_hiv_sd**2)
      colnames(species) <- c("species", "trait_val", "sd_trait_val", "v_trait_val")
      
      sp <- sort(unique(species$species))
      census <- census %>% complete(species, date)
      census$n[is.na(census$n)] <- 0
      census <- subset(census, census$species %in% sp)
      
      #census$n <- log(census$n + 1)
      
      ## Calcul du CTI par an avec les intervalles de confiance par bootstrap
      catlog(c("   - Calcul des intervalles de confiance\n"), fileLog)
      cti <- cwi_stratified(census, species, bootstrap = T, bootstrap_n = 100)
      
      ## Modèle linéaire du CTI
      mod1 <- lm(cwm ~ date, cti)
      print(summary(mod1))
      
      cti$fit1 <- fitted(mod1)
      
      ## Création et affichage du graphe de tendances
      catlog(c("   - Affichage des tendances\n\n"), fileLog)
      gg <- ggplot(cti, aes(x = date, y = cwm, ymin = bootstrap_cwm_lower_ci, ymax = bootstrap_cwm_higher_ci)) +
        geom_line(color = 'royalblue1') + geom_point(color = 'royalblue1') +
        geom_ribbon(alpha = 0.3, fill = 'royalblue1') +
        geom_line(aes(y = fit1), color = 'red') +
        labs(title = paste0("Indice thermique des communautés annuel en période d'hivernage entre ", p[i],
                            " et ", p[i+1]),
             x = "Année", y = "CTI")
      
      print(gg)
      ## Sauvegarde du graphe
      if(saveFig) ggsave(paste0("output/CTI/CTI_hiv_", p[i], "_", p[i+1], ".png"))
      
      ## Calcul des contributions spécifiques
      catlog(c("   - Calcul des contributions spécifiques sur la période", p[i], "-", p[i+1], "\n"), fileLog)
      ctrb <- trend_contrib(census, species, "trait_val")
      ctrb$type <- ifelse(ctrb$originality > 0 & ctrb$dp > 0, "Increasing hot",
                          ifelse(ctrb$originality > 0 & ctrb$dp < 0, "Decreasing hot",
                                 ifelse(ctrb$originality < 0 & ctrb$dp > 0, "Increasing cold", "Decreasing cold")))
      
      ## Création et affichage du diagramme des contributions spécifiques
      catlog(c("   - Affichage des contributions\n\n"), fileLog)
      ctrb <- ctrb[order(ctrb$contrib, decreasing = TRUE),]
      gg <- ggplot(data = ctrb, aes(x = contrib, y = reorder(species, contrib), fill = type)) +
        scale_fill_manual(values = c("#ABD9E8", "#FDAE61", "#2B79B4", "#D71719")) +
        geom_col() +
        labs(title = paste0("Contributions spécifiques à la variation du CTI entre ", p[i], " et ", p[i+1]),
             x = "Contribution", y = "Espèces")
      print(gg)
      ## Sauvegarde du diagramme
      if(saveFig) ggsave(paste0("output/CTI/contrib_sp_CTI_hiv_", p[i], "_", p[i+1], ".png"), width = 9, height = 7.5, units = "in")
      
      
      cti_hiv <- cti
      
      ## Création et affichage des diagrammes de contribution groupée
      if(class == T){
        
        ## Groupé par type de contribution au CTI
        catlog(c("   - Calcul des contributions par groupe thermique sur la période", p[i], "-", p[i+1], "\n"), fileLog)
        grouped <- aggregate(contrib ~ type, data = ctrb, FUN = sum)

        catlog(c("   - Affichage des contributions\n\n"), fileLog)
        gg <- ggplot(data = grouped, aes(x = contrib, y = reorder(type, contrib), fill = type)) +
          scale_fill_manual(values = c("#ABD9E8", "#FDAE61", "#2B79B4", "#D71719")) +
          geom_col() +
          labs(title = paste0("Contributions à la variation du CTI entre ", p[i], " et ", p[i+1]),
               x = "Contribution", y = "Classes")
        print(gg)
        if(saveFig) ggsave(paste0("output/CTI/contrib_grp_CTI_hiv_", p[i], "_", p[i+1], ".png"))
        
        
        ## Groupé par famille
        catlog(c("   - Calcul des contributions par famille sur la période", p[i], "-", p[i+1], "\n"), fileLog)
        info_especes <- read.csv("library/info_especes.csv", sep=";")
        grouped <- merge(ctrb, info_especes, by.x = "species", by.y = "SP")
        
        catlog(c("   - Affichage des contributions\n\n"), fileLog)
        grouped <- aggregate(contrib ~ FAMILLE, data = grouped, FUN = sum)
        gg <- ggplot(data = grouped, aes(x = contrib, y = sort(FAMILLE), fill = FAMILLE)) +
          #scale_fill_manual(values = c("#ABD9E8", "#FDAE61", "#2B79B4", "#D71719")) +
          geom_col() +
          labs(title = paste0("Contribution de chaque famille à la variation du CTI entre ", p[i], " et ", p[i+1]),
               x = "Contribution", y = "Familles")
        print(gg)
        if(saveFig) ggsave(paste0("output/CTI/contrib_fam_CTI_hiv_", p[i], "_", p[i+1], ".png"))
      }
    }
  }
    
    
  ## Calculs pour la période de repro. Script strictement analogue à celui de la partie hivernage.
  if(saison != "hiver"){
    catlog(c(" - Calcul du CTI des communautés nicheuses\n"), fileLog)
    p <- c(startRep, splitRep, endRep)
    STI_repro <- STI %>% dplyr::select(c('SP', 'STI_rep_mean', 'STI_rep_sd')) %>% subset(!is.na(STI$STI_rep_mean))
    
    dCTI_rep <- dCTI %>% subset(MONTH >= 5 & MONTH <= 7 & YEAR >= startRep & YEAR <= endRep)
    dCTI_rep <- aggregate(TOTAL_COUNT ~ YEAR + SP, data = dCTI_rep, FUN = mean)
    
    for(i in 1:(length(p)-1)){
      if(i > 1) p[i] <- p[i]+1
      catlog(c("   - Calcul du CTI pour la période", p[i], "-", p[i+1], "\n"), fileLog)
      census <- subset(dCTI_rep, dCTI_rep$YEAR >= p[i] & dCTI_rep$YEAR <= p[i+1])
      colnames(census) <- c("date", "species", "n")
      species <- STI_repro %>% mutate(STI_rep_v = STI_rep_sd**2)
      colnames(species) <- c("species", "trait_val", "sd_trait_val", "v_trait_val")
    
      sp <- sort(unique(species$species))
      census <- census %>% complete(species, date)
      census$n[is.na(census$n)] <- 0
      census <- subset(census, census$species %in% sp)
    
      #census$n <- log(census$n + 1)
    
      catlog(c("   - Calcul des intervalles de confiance\n"), fileLog)
      cti <- cwi_stratified(census, species, bootstrap = T, bootstrap_n = 100)
    
      mod1 <- lm(cwm ~ date, cti)
      print(summary(mod1))
    
      cti$fit1 <- fitted(mod1)
    
      catlog(c("   - Affichage des tendances\n\n"), fileLog)
      gg <- ggplot(cti, aes(x = date, y = cwm, ymin = bootstrap_cwm_lower_ci, ymax = bootstrap_cwm_higher_ci)) +
        geom_line(color = 'royalblue1') + geom_point(color = 'royalblue1') +
        geom_ribbon(alpha = 0.3, fill = 'royalblue1')
      
      if(!is.null(splitRep)){
        gg <- gg + geom_line(aes(y = fit1), color = 'red')
      }
      
      gg <- gg + labs(title = paste0("Indice thermique des communautés annuel en période de nidification entre ", p[i],
                            " et ", p[i+1]),
             x = "Année", y = "CTI")
    
      print(gg)
      if(saveFig) ggsave(paste0("output/CTI/CTI_rep_", p[i], "_", p[i+1], ".png"))
    
      catlog(c("   - Calcul des contributions spécifiques sur la période", p[i], "-", p[i+1], "\n"), fileLog)
      ctrb <- trend_contrib(census, traits = species, "trait_val")
      ctrb$type <- ifelse(ctrb$originality > 0 & ctrb$dp > 0, "Increasing hot",
                          ifelse(ctrb$originality > 0 & ctrb$dp < 0, "Decreasing hot",
                                 ifelse(ctrb$originality < 0 & ctrb$dp > 0, "Increasing cold", "Decreasing cold")))
    
      catlog(c("   - Affichage des contributions\n\n"), fileLog)
      ctrb <- ctrb[order(ctrb$contrib, decreasing = TRUE),]
      gg <- ggplot(data = ctrb, aes(x = contrib, y = reorder(species, contrib), fill = type)) +
        scale_fill_manual(values = c("#ABD9E8", "#FDAE61", "#2B79B4", "#D71719")) +
        geom_col() +
        labs(title = paste0("Contributions spécifiques à la variation du CTI entre ", p[i], " et ", p[i+1]),
             x = "Contribution", y = "Espèces")
      print(gg)
      if(saveFig) ggsave(paste0("output/CTI/contrib_sp_CTI_rep_", p[i], "_", p[i+1], ".png"), width = 9, height = 7.5, units = "in")
    
    
      cti_rep <- cti
    
      if(class == T){
      
        ## Groupé par type de contribution au CTI
        catlog(c("   - Calcul des contributions par groupe thermique sur la période", p[i], "-", p[i+1], "\n"), fileLog)
        grouped <- aggregate(contrib ~ type, data = ctrb, FUN = sum)
        
        catlog(c("   - Affichage des contributions\n\n"), fileLog)
        gg <- ggplot(data = grouped, aes(x = contrib, y = reorder(type, contrib), fill = type)) +
          scale_fill_manual(values = c("#ABD9E8", "#FDAE61", "#2B79B4", "#D71719")) +
          geom_col() +
          labs(title = paste0("Contributions à la variation du CTI entre ", p[i], " et ", p[i+1]),
               x = "Contribution", y = "Classes")
        print(gg)
        if(saveFig) ggsave(paste0("output/CTI/contrib_grp_CTI_rep_", p[i], "_", p[i+1], ".png"))
      
      
        ## Groupé par famille
        catlog(c("   - Calcul des contributions par famille sur la période", p[i], "-", p[i+1], "\n"), fileLog)
        info_especes <- read.csv("library/info_especes.csv", sep=";")
        grouped <- merge(ctrb, info_especes, by.x = "species", by.y = "SP")
      
        catlog(c("   - Affichage des contributions\n\n"), fileLog)
        grouped <- aggregate(contrib ~ FAMILLE, data = grouped, FUN = sum)
        gg <- ggplot(data = grouped, aes(x = contrib, y = reorder(FAMILLE, contrib), fill = FAMILLE)) +
          #scale_fill_manual(values = c("#ABD9E8", "#FDAE61", "#2B79B4", "#D71719")) +
          geom_col() +
          labs(title = paste0("Contributions de chaque famille à la variation du CTI entre ", p[i], " et ", p[i+1]),
               x = "Contribution", y = "Familles")
        print(gg)
        if(saveFig) ggsave(paste0("output/CTI/contrib_fam_CTI_rep_", p[i], "_", p[i+1], ".png"))
      }
    }
  }
  
  return(cti_hiv)
}
