Div_index <- function(data = d, dates = dates, index = "all", CLASS, mod = FALSE, all_species = FALSE,
                      fileLog = fileLog, saveFig = TRUE){
  
  catlog(c("\n===================================\n       INDICES DE DIVERSITE\n=================================== \n"),fileLog)
  catlog(c(paste0("        Calcul par ", CLASS, "\n-----------------------------------\n\n")), fileLog)
  catlog(c(" - Préparation des données\n\n"), fileLog)
  
  info_especes <- read.csv("C:/Users/roman/Documents/ENSAT/3A/Stage/Script_R/library/info_especes.csv", sep=";")
  
  ## Rassemblement des USN partiellement comptées (roselières) avec les USN les plus proches (MC et MO)
  data <- subset(data, data$COMPTAGE == "OUI")
  data$USN[data$USN == "USN09" | data$USN == "USN10"] <- "USN08"
  data$USN[data$USN == "USN11"] <- "USN12"
  
  if(index == "all") index <- c("simpson", "shannon", "invsimpson")

  ## Suppression des espèces non comptées si all_sp = FALSE
  if(all_species == FALSE){
    sp <- sort(unique(info_especes$SP[info_especes$COMPTE == "TRUE"]))
    data <- subset(data, data$SP %in% sp)
  }
  
  ## Regroupement des données partielles
  if(CLASS == "USN"){
    ddiv <- aggregate(TOTAL_COUNT ~ DATE + MONTH + YEAR + SP + USN, data = data, FUN = sum)
    ddiv <- subset(ddiv, ddiv$USN != "USN00" & ddiv$YEAR >= 2008)
    ddiv <- subset(ddiv, ddiv$DATE %in% dates$DATE)
    class <- sort(unique(ddiv$USN))
  } else if(CLASS == "YEAR"){
    ddiv <- aggregate(TOTAL_COUNT ~ DATE + MONTH + YEAR + SP, data = data, FUN = sum)
    ddiv <- subset(ddiv, ddiv$YEAR >= 2008)
    ddiv <- subset(ddiv, ddiv$DATE %in% dates$DATE)
    class <- sort(unique(ddiv$YEAR))
  }
  
  ## Selection des données de comptage
  ddiv$SAISON <- ifelse(ddiv$MONTH >= 11 | ddiv$MONTH <= 2, "Hivernage",
                          ifelse(ddiv$MONTH >= 5 & ddiv$MONTH <= 7, "Repro", "Migration"))
  saison <- sort(unique(ddiv$SAISON))
  
  ## Boucle sur l'ensemble des indices renseignés
  for(i in index){
    dres <- data.frame(Hivernage = NA, Migration = NA, Repro = NA, Annee = NA, CLASS = class)

    ## Boucle sur l'ensemble des saisons biologiques
    for(s in saison){
      catlog(c(paste0(" - Calcul des indices de ", i, " pour la saison ", s, "\n")), fileLog)
      ## Création des tables de contingence pour calcul des indices de diversité
      ddiv2 <- subset(ddiv, ddiv$SAISON == s)
      ddiv2 <- ddiv2 %>% group_by_at(c("SP", CLASS)) %>% summarise(MEAN = sum(TOTAL_COUNT)/length(unique(ddiv2$DATE)))
      ddiv2 <- tidyr::spread(ddiv2, SP, MEAN, fill = 0)
      assign(paste0("ddiv2", s), ddiv2)
      ddiv3 <- dplyr::select(ddiv2, -c(CLASS))
      
      ## Calcul des indices de diversité
      dres[s] <- diversity(ddiv3, index = i)
    }
    
    ## Même processus pour l'année entière
    catlog(c(paste0(" - Calcul des indices de ", i, " pour l'ensemble de l'année\n")), fileLog)
    ddivtot <- ddiv %>% group_by_at(c("SP", CLASS)) %>% summarise(MEAN = sum(TOTAL_COUNT)/length(unique(ddiv$DATE)))
    ddivtot <- tidyr::spread(ddivtot, SP, MEAN, fill = 0)
    ddiv2Annee <- ddivtot
    ddivtot <- dplyr::select(ddivtot, -c(CLASS))
    dres$Annee <- diversity(ddivtot, i = i)
  
    ## Si on renseigne mod = TRUE, calcule l'exponentielle de l'indice de Shannon
    if(i == "shannon" & mod == TRUE) dres[1:4] <- exp(dres[1:4])
  
    ## Création de la table avec les indices
    catlog(c(paste0(" - Affichage de la table de résultats\n")), fileLog)
    breaks <- quantile(dres[,(1:4)], probs = seq(.05, .95, .05), na.rm = TRUE) ##LIGNE DE L'ERREUR
    colors <- round(seq(255, 40, length.out = length(breaks) + 1), 0) %>% 
      {paste0("rgb(255,", ., ",", ., ")")}
    dt_res <- datatable(dres) %>% formatStyle(names(dres[1:4]), backgroundColor = styleInterval(breaks, colors))
    print(dt_res)
  
    dres <- gather(dres, key = "Saison", value = "Indice", "Hivernage", "Migration", "Repro")
    
    ## Plot
    catlog(c(" - Affichage et sauvegarde du barplot des indices\n\n"), fileLog)
    if(CLASS == "USN"){
      gg <- ggplot(data = dres, aes(x = reorder(CLASS, -Annee),
                                    y = Indice, color = Saison, fill = Saison)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = alpha(c("cornflowerblue", "gold", "red", "black"), 0.4)) +
        scale_color_manual(values = alpha(c("cornflowerblue", "gold", "red", "black"), 1)) +
        labs(title = paste0("Indice de ", i, " par ", CLASS)) +
        geom_point(aes(y = Annee), group = 1, color = "black")
      print(gg)
    }
    else {
      gg <- ggplot(data = dres, aes(x = CLASS,
                                    y = Indice, color = Saison, fill = Saison)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = alpha(c("cornflowerblue", "gold", "red", "black"), 0.4)) +
        scale_color_manual(values = alpha(c("cornflowerblue", "gold", "red", "black"), 1)) +
        labs(title = paste0("Indice de ", i, " par ", CLASS)) +
        geom_point(aes(y = Annee), group = 1, color = "black")
      print(gg)
    }

    if(saveFig) ggsave(paste0("output/Diversity/barplot_", i, "_", CLASS, ".png"))
  
    ## Même processus pour l'ensemble de l'année
    d_an <- dres %>% gather(Saison, Indice, Annee) %>% distinct()
    dres <- dplyr::select(dres, -Annee)
    dres <- rbind(dres, d_an)
    if(i == "shannon" & mod == TRUE) {
      write.csv2(dres, file = paste0("output/Diversity/expshannon_", CLASS, ".csv"), row.names = FALSE)}
    else write.csv2(dres, file = paste0("output/Diversity/", i, "_", CLASS, ".csv"), row.names = FALSE)
  }
  
  catlog(c(" - Calcul et sauvegarde des tableaux de comparaisons de Hutcheson\n"), fileLog)
  ## Calcul de l'indice de Hutcheson qui permet de déterminer si les USN sont vraiment différentes ou pas
  dhutchHiv <- hutch(ddiv2Hivernage)
  write.csv2(dhutchHiv, file = paste0("output/Diversity/dhutchHiv_", CLASS, ".csv"), na = "")
  
  dhutchMig <- hutch(ddiv2Migration)
  write.csv2(dhutchMig, file = paste0("output/Diversity/dhutchMig_", CLASS, ".csv"), na = "")
  
  dhutchRep <- hutch(ddiv2Repro)
  write.csv2(dhutchRep, paste0(file = "output/Diversity/dhutchRep_", CLASS, ".csv"), na = "")
  
  dhutchAnn <- hutch(ddiv2Annee)
  write.csv2(dhutchAnn, paste0(file = "output/Diversity/dhutchAnn_", CLASS, ".csv"), na = "")
  
  if(CLASS == "USN") USN_clustering(dataHiv = ddiv2Hivernage, dataMig = ddiv2Migration,
                                    dataRep = ddiv2Repro, dataAnn = ddiv2Annee,
                                    saveFig = FALSE, Log = fileLog)
  
  return(dres)
}


hutch <- function(data){
  usn <- sort(unique(data$USN))
  data2 <- as.data.frame(t(data))
  colnames(data2) <- data2[1,]
  data2 <- data2[-1,]
  data2[] <- lapply(data2, as.numeric)
  dhutch <- data.frame(matrix(ncol = length(usn), nrow = length(usn)))
  rownames(dhutch) <- usn
  colnames(dhutch) <- usn
  for(u in usn){
    for(v in usn){    
      h <- Hutcheson_t_test(data2[,u], data2[,v])
      dhutch[u,v] <- h$p.value
    }
  }
  
  dhutch[dhutch >= 0.05] <- NA
  
  return(dhutch)
}



USN_clustering <- function(dataHiv, dataMig, dataRep, dataAnn, saveFig = saveFig, Log = fileLog){
  ## Pour chaque saison
  catlog(c(" - Dendrogramme de similitude des USN\n"), Log)
  for(data in list(dataHiv, dataMig, dataRep, dataAnn)){
    commu <- ifelse(identical(data, dataHiv), "hivernantes",
                ifelse(identical(data, dataMig), "de passage",
                       ifelse(identical(data, dataRep), "estivantes", "annuelles")))
    ## On calcule l'indice de Jaccard de diversité beta
    catlog(c("   - Communautés", commu, "\n"), Log)
    beta <- betadiver(data, "j")
    ## On le convertit en distance de Jaccard
    beta <- 1-beta
    ## On trace un dendrogramme à l'aide de ces distances de Jaccard et de la méthode de Ward
    arbre <- hclust(beta, "ward.D2")
    gg <- ggdendrogram(arbre)
    print(gg)
    inertie <- sort(arbre$height, decreasing = TRUE)
    plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
    gg <- fviz_dend(arbre, k = 4, show_labels = TRUE, rect = TRUE) +
          labs(title = paste0("Dendrogramme des unités de comptage par similarité des communautés ", commu),
               x = "USN", y = "Distance de Jaccard")
    print(gg)
    
    ## On confronte cette classification hiérarchique avec une classification non-hiérarchisée (K-means)
    catlog(c(" - Confrontation avec classification par K-means\n"), Log)
    kmeans.re <- kmeans(beta, centers = 4, nstart = 20)
    kmeans.re
    ## Affichage du résultat de la classif dans le log
    k <- print(kmeans.re$cluster)
    catlog(c(paste0(as.character(sort(unique(data$USN))), sep = "\t")), Log)
    catlog(c('\n'), Log)
    catlog(c(paste0(k, sep = "\t")), Log)
    catlog(c('\n\n'), Log)

    if(saveFig) ggsave(paste0("output/USN/Clustering_", commu, ".png"))
  }
}