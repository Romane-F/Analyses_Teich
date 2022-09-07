### FONCTION : ANOVA_poisson

### Entrée : data : dataframe.
###          facteurs : character. Paramètre(s) dont on veut étudier l'influence.
###          saveFig : bool. Sauvegarder ou non les figures.

### Sortie : dtot : dataframe.

### Description : Modélise les données de comptages suivant une loi de Poisson et
###               applique des ANOVA sur ces modèles. Produit les boxplots et
###               violinplots correspondants pour faciliter la visualisation.

ANOVA_poisson <- function(data = d, dates = dates, facteurs = "all",
                          saveFig = TRUE, fileLog = fileLog){
  catlog(c("\n===================================\nINFLUENCE DES PARAMETRES EXTERIEURS\n=================================== \n"),fileLog)
  catlog(c("\n-----------------------------------\n          Calcul des ANOVA\n----------------------------------- \n\n"),fileLog)
  
  if(facteurs == "all") facteurs = c("COEFF", "DEMIJOURNEE")
  
  catlog(c(" - Chargement des fichiers d'informations espèces et dates de comptage\n\n"), fileLog)
  
  ## Sélectionne uniquement les espèces patrimoniales
  nat <- c("Limosa lapponica", "Calidris canutus", "Calidris minuta", "Anas clypeata", "Tringa nebularia", "Tringa erythropus",
           "Tringa totanus", "Actitis hypoleucos", "Numenius arquata", "Numenius phaeopus", "Pluvialis squatarola", "Calidris alpina")
  internat <- c("Recurvirostra avosetta", "Limosa limosa", "Anas acuta", "Charadrius hiaticula", "Platalea leucorodia", "Calidris alpina")
  
  data <- subset(data, (SP %in% nat | SP %in% internat) & (YEAR <= 2006 | COMPTAGE == "OUI"))
  
  sp <- sort(unique(data$SP))
  
  catlog(c(" - Affectation des saisons biologiques aux données\n\n"), fileLog)
  
  ## Affecte les saisons selon la biologie de l'espèce
  data$CODE_DECADE <- ifelse(as.integer(str_sub(data$DATE, -2)) <= 10, data$MONTH * 10 + 1,
                             ifelse(as.integer(str_sub(data$DATE, -2)) >= 21, data$MONTH * 10 + 3, data$MONTH * 10 + 2))
  dsp <- read.csv("library/sp.csv", sep=";")
  data <- merge(data, dsp, by = "SP")
  
  data$SAISON <- ifelse(data$CODE_DECADE >= data$HIV_START | data$CODE_DECADE <= data$HIV_END, "Hivernage",
                        ifelse(data$CODE_DECADE <= data$REP_END & data$CODE_DECADE >= data$REP_START, "Repro", "Migration"))
  
  msp <- read.csv("library/comp_year_global.csv", sep = ";", dec = ",")
  data <- merge(data, msp)
  
  ## Somme chaque comptage sur l'ensemble de la réserve
  dtot <- aggregate(TOTAL_COUNT ~ DATE + YEAR + MONTH + SP + COEFF + DEMIJOURNEE + SAISON,
                    data = data, FUN = sum)
  
  nsp <- group_by(dtot, DATE, COEFF, DEMIJOURNEE)
  nsp <- count(nsp)
  
  ## Supprime une partie des données anormalement faibles (effectifs pas intégralement comptées)
  ## Crée des seuils à 5% de la population moyenne d'espèce pour chaque saison et élimine ce qui y est inférieur
  ## Sauf pour les dates de comptage où tout est conservé
  catlog(c(" - Résolution du problème de distribution : suppression des comptages partiels\n\n"), fileLog)
  data <- subset(data, MEAN_YEAR >= 0.2*MEAN_GLOB)
  nsp_seuil <- filter(nsp, n > 3)
  data <- subset(data, DATE %in% nsp_seuil$DATE | COMPTAGE == 'OUI')
  
  ## Recrée les absences : lorsque l'espèce n'a pas été vue lors du comptage, crée une ligne avec TOTAL_COUNT = 0.
  data <- complete(data, SP, nesting(DATE, COEFF, YEAR, MONTH, DEMIJOURNEE))
  data$TOTAL_COUNT[is.na(data$TOTAL_COUNT)] <- 0
  
  dtot <- aggregate(TOTAL_COUNT ~ DATE + YEAR + MONTH + SP + COEFF + DEMIJOURNEE + SAISON,
                    data = data, FUN = sum)
  
  species <- sort(unique(dtot$SP))
  saison <- sort(unique(dtot$SAISON))
  
  ## Crée un df qui va stocker un résumé des résultats de significativité
  tfact <- as.data.frame(expand.grid(species,saison))
  colnames(tfact) <- c("SP", "SAISON")
  
  ## Boucle sur l'ensemble des facteurs étudiés
  for(f in facteurs){
    browser()
    catlog(c(paste0("Calcul de l'influence de la variable ", f, "\n")), fileLog)
    if(class(dtot[[f]]) != "character") dtot[[f]] <- as.character(dtot[[f]])
    dtot[[f]] <- as.factor(dtot[[f]])
    
    tfact[f] <- NA
    
    ## Boucle sur l'ensemble des espèces
    for (s in species){
      danova_sp <- subset(dtot, dtot$SP == s)
      #par(mfrow = c(3,1))
      saison <- sort(unique(danova_sp$SAISON))
      
      ## Boucle sur chaque saison
      for (ss in saison){
        
        #browser()
        catlog(c(paste0(" - Analyse pour l'espèce ", s, " en saison de ", ss, "\n")), fileLog)
        danova <- subset(danova_sp, danova_sp$SAISON == ss)
        
        ## On n'effectue l'analyse que si le nombre d'observations est >= 30
        if(nrow(subset(danova, danova$TOTAL_COUNT >= 1)) >= 30){
          catlog(c(paste0("   --> Affichage\n")), fileLog)
          ## Affichage et sauvegarde d'un violinplot
          gg <- ggplot(danova, aes_string(x = f, y = "TOTAL_COUNT", fill = f, colour = f)) +
            geom_boxplot(alpha = 0.5, outlier.alpha = 0.5) +
            geom_jitter(alpha = 0.3) +
            #coord_cartesian(ylim = c(NA, quantile(danova$TOTAL_COUNT, 0.99))) +
            labs(x = f, y = "Abondance",
                 title = paste0("Abondance de l'espèce ", s,
                                " en fonction de la variable ", f))
          print(gg)
          if(saveFig) ggsave(paste0("output/ANOVA/bp_", gsub(" ", "_", s), "_", ss, "_", f, ".png"))
          
          gg <- ggplot(danova, aes_string(x = f, y = "TOTAL_COUNT", fill = f, colour = f)) +
            geom_violin(alpha = 0.5, outlier.alpha = 0.5) +
            geom_jitter(alpha = 0.5) +
            #coord_cartesian(ylim = c(NA, quantile(danova$TOTAL_COUNT, 0.99))) +
            labs(x = f, y = "Abondance",
                 title = paste0("Abondance de l'espèce ", s,
                                " en fonction de la variable ", f))
          print(gg)
          if(saveFig) ggsave(paste0("output/ANOVA/violin_", gsub(" ", "_", s), "_", ss, "_", f, ".png"))
          
          ## Modélisation GLM avec loi de Poisson
          catlog(c(paste0("   --> Modélisation pour l'espèce ", s, " en saison de ", ss, "\n")), fileLog)
          form <- as.formula(paste0("TOTAL_COUNT ~ ", f))
          fit <- glm(form, data = danova, family = "poisson")
          dispersion <- deviance(fit)/df.residual(fit)
          
          if (dispersion > 1){
            
            ## Lorsque les données sont surdispersées, modélisation avec quasi-Poisson
            form <- as.formula(paste0("TOTAL_COUNT ~ ", f))
            fit <- glm(form, data = danova, family = "quasipoisson")
          }
          
          ## ANOVA sur le modèle
          catlog(c("   --> ANOVA sur le modèle\n"), fileLog)
          anova <- Anova(fit, test = "F")
          summary(anova)
          
          ## Comparaisons multiples avec Tukey, seulement si l'ANOVA est significative (tps de traitement)
          if(anova$`Pr(>F)` <= 0.05){

            catlog(c("       --> Analyse de Tukey : groupes différents\n"), fileLog)
            mc_tukey <- eval(parse(text = paste0("glht(fit, mcp('", f, "' = 'Tukey'))")))
            plot(mc_tukey, main = paste0("Analyse Tukey pour l'espèce ", s, " en saison de ", ss)) 
            
            ## Affichage du résultat de Tukey dans le log
            tuk.cld <- print(cld(mc_tukey, level = 0.05)$mcletters$Letters)
            catlog(c(paste0(as.character(sort(unique(danova[[f]]))), sep = "\t")), fileLog)
            catlog(c('\n'), fileLog)
            catlog(c(paste0(tuk.cld, sep = "\t")), fileLog)
            catlog(c('\n\n'), fileLog)
            
            ## Sauvegarde du résultat de l'ANOVA
            if(anova$`Pr(>F)` <= 0.001){
              code_etoile <- "***"
            } else if(anova$`Pr(>F)` <= 0.01){
              code_etoile <- "**"
            } else code_etoile <- "*"
            
            tfact[[f]] <- ifelse(tfact$SP == s & tfact$SAISON == ss, code_etoile, tfact[[f]])
            
          } else catlog(c("       --> Non significatif\n\n"), fileLog)
        } else catlog(c("   --> Nombre de données insuffisant\n\n"), fileLog)
      }
      
      ## Affichage et sauvegarde de boxplots synthétiques
      catlog(c("   - Affichage des boxplots avec les trois facteurs\n\n"), fileLog)
      gg <- ggplot(danova_sp, aes(x = DEMIJOURNEE, y = TOTAL_COUNT, fill = DEMIJOURNEE, colour = DEMIJOURNEE)) +
        #geom_jitter(alpha = 0.5, outlier.alpha = 0.5) +
        geom_boxplot(alpha = 0.5, outlier.alpha = 0.5) +
        facet_grid(cols = vars(SAISON)) +
        labs(x = "Demi-journée", y = "Abondance",
             title = paste0("Abondance de l'espèce ", s,
                            " en fonction de la demi-journée")) +
        theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(color = "gray", fill = NA))
      print(gg)
      ## Sauvegarde du graphe
      if(saveFig) ggsave(paste0("output/ANOVA/boxplot_", gsub(" ", "_", s), ".png"))
    }
  }
  
  ## Ecriture du CSV résumé des résultats d'ANOVA
  catlog(c(" - Ecriture de la table de résumé\n\n"), fileLog)
  write.csv2(tfact, paste0("output/ANOVA/signif_table.csv"), row.names=FALSE,na="",quote=FALSE)

  return(dtot)
}
