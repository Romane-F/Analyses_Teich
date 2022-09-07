### Fonction pilote qui va appeler toutes les autres



### FONCTION : Analyses_Teich

### Entrée : file : character. Nom du fichier de données (txt).
###          fileDataClean : character. Nom du fichier de données nettoyées (à charger ou sauvegarder) (csv).
###          habitats : character. Nom du fichier d'habitats sans indiquer l'extension (shp).
###          importData : character. "brut" ou "clean" : fichier de données à importer.
###          saveFig : bool. Si TRUE, sauvegarde les figures dans le dossier output.
###          firstYear, lastYear : integer. Bornes de la période d'étude.
###          taxo : character. Groupe taxonomique à étudier. 20/05/2022 : Seul "Oiseaux" fonctionnel.
###          selectedSpecies : character. Sous-sélection d'espèces à étudier. Par défaut, toutes. 

### Sortie : d : tableau de données nettoyées.

Analyses_Teich <- function(file = 'data_tot.txt', fileDataClean = 'data_clean_tot.csv',
                           habitats = 'habitats_ok',
                           importData = 'brut', saveFig = TRUE, firstYear = 1981,
                           lastYear = NULL, taxo = "Oiseaux", selectedSpecies = '',
                           CTIstartRep = firstYear, CTIendRep = lastYear,
                           CTIstartHiv = firstYear, CTIendHiv = lastYear,
                           CTIsaison = "", CTIhors_sp = "", CTIclass = TRUE,
                           CTIsplitHiv = NULL, CTIsplitRep = NULL, fileSTI = "STI.csv",
                           ANOVAfacteurs = "all",
                           DIVindex  = "all", DIVmod = FALSE, DIVall_species = FALSE){
  
  #file = 'data_tot.txt'
  #fileDataClean = 'data_clean_tot.csv'
  #habitats = 'habitats_ok'
  #importData = 'brut'
  #saveFig = TRUE
  #firstYear = NULL
  #lastYear = NULL
  #taxo = "Oiseaux"
  #selectedSpecies = ''
  
# ==============================================================================
#                             IMPORTATION DES DONNEES
# ==============================================================================
  
  start <- Sys.time()
  fileLog <- paste("log",format(start, "%Y-%m-%d_%HH%M"),".txt",sep="")
  
  ## Chargement des données
  catlog(c("\n===================================\n            IMPORTATION\n=================================== \n\nfile :",
           file,"\nclean file :", fileDataClean, "\n"),fileLog)
  
  file <- paste("data/", file, sep = "")
  d <- read_delim(file, "\t", escape_double = FALSE, trim_ws = TRUE, skip = 0)
  color_table <- read.csv("library/color_code.csv", sep=";", na.strings = "")
  dsp <- read.csv2("library/sp.csv")
  dates <- read.csv2("library/liste_comptages.csv")
  
  catlog(c("Nombre initial de lignes :", nrow(d), "\n"), fileLog)
  
  while(importData %nin% c('brut', 'clean')){
    importData <- readline("Méthode d'importation non reconnue. Entrer 'brut' ou 'clean' puis [ENTRER]")
  }
  
  if(importData == 'brut'){
    d <- preparation_data(d = d, fileDataClean = fileDataClean, fileLog = fileLog, habitats = habitats,
                          color_table = color_table, firstYear = firstYear, lastYear = lastYear,
                          taxo = taxo, selectedSpecies = selectedSpecies)
  } else {
    if(importData == 'clean'){
      d <- read.csv2(paste0("data/", fileDataClean), na.strings = "")
    }
  }
  
  if(is.null(firstYear)) {
    firstYear = min(d$YEAR)
  }
  if(is.null(lastYear)) {
    lastYear = max(d$YEAR)
  }
  
  ## Met les colonnes DATE de 'd' et de 'dates' au même format
  if(class(d$DATE) == "Date"){
    dates$DATE <- as.Date.character(dates$DATE, format = "%d/%m/%Y")
  } else if(class(d$DATE) == "character"){
    dates$DATE <- as.character(as.Date.character(dates$DATE, format = "%d/%m/%Y"))
  }
  
# ==============================================================================
#                                 CALCUL DU CTI
# ==============================================================================
  
  #CTI_hiv <- CTI(data = d, startRep = CTIstartRep, endRep = CTIendRep, startHiv = CTIstartHiv, endHiv = CTIendHiv,
  #    saison = CTIsaison, hors_sp = CTIhors_sp, class = CTIclass, splitHiv = CTIsplitHiv, splitRep = CTIsplitRep,
  #    saveFig = saveFig, fileSTI = fileSTI, fileLog = fileLog)
  
# ==============================================================================
#                                CALCUL DES ANOVA
# ==============================================================================
  
  dtot <- ANOVA_poisson(data = d, dates = dates, facteurs = ANOVAfacteurs, saveFig = saveFig, fileLog = fileLog)

# ==============================================================================
#                        CALCUL DES INDICES DE DIVERSITE
# ==============================================================================
  
  dres <- Div_index(data = d, dates = dates, index = DIVindex, CLASS = "USN", mod = DIVmod, all_species = DIVall_species,
                    fileLog = fileLog, saveFig = TRUE)
  dres <- Div_index(data = d, dates = dates, index = DIVindex, CLASS = "YEAR", mod = DIVmod, all_species = DIVall_species,
                    fileLog = fileLog, saveFig = TRUE)
  
  end <- Sys.time()
  catlog(c("Durée d'exécution :", end - start, "\n"), fileLog)
  return(d)
  
}
