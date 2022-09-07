# ==============================================================================
#                                  SCRIPT START
# ==============================================================================



`%nin%` <- Negate(`%in%`)

### Fonction : catlog, écrit et sauvegarde le log du run.

catlog <- function(txt,fileLog) {
  cat(txt)
  if(!is.null(fileLog)) cat(txt,file=paste0("output_log/",fileLog),append = TRUE)
}



### Fonction : correct_coeff. En entrée un vecteur de type caractère

correct_coeff <- function(v){
  w <- as.numeric(str_extract(v, "[0-9]{1,}"))
  x <- ifelse(w >= 1 & w <= 4, v,
              ifelse(w >= 20 & w <= 60, "C4",
                     ifelse(w >= 61 & w <= 80, "C3",
                            ifelse(w >= 81 & w <= 95, "C2",
                                   ifelse(w >= 96 & w <= 120, "C1", NA)))))
  return(x)
}



moyennes_PM <- function(v){
  mat <- str_match(v, "(PM[\\+-]?[0-5]?\\.?[0-9]{0,2})[à\\b-]?(PM[\\+-]?[0-5]?\\.?[0-9]{0,2})?")
  dftest <- as.data.frame(mat)
  colnames(dftest) <- c("PM_EXTRACT", "PM1", "PM2")
  dftest$PM1 <- gsub("^PM$", 0, dftest$PM1)
  dftest$PM2 <- gsub("^PM$", 0, dftest$PM2)
  dftest$PM1 <- gsub("PM", "", dftest$PM1)
  dftest$PM2 <- gsub("PM", "", dftest$PM2)
  dftest$PM1 <- as.numeric(dftest$PM1)
  dftest$PM2 <- as.numeric(dftest$PM2)
  dftest$MEAN <- rowMeans(dftest[,c("PM1", "PM2")], na.rm=TRUE)
  
  return(dftest)
}



### Fonction : extract_commentaires
extract_commentaires <- function(d, new, fun = str_extract, t){
  v1 = d$DAILY_TEXT_COMMENT_REM
  v2 = d$COMMENT
  v3 = d$PRIVATE_COMMENT
  new <- if(sum(!is.na(new)) == 0) fun(v1, t) else ifelse(is.na(new), fun(v1, t), new)
  new <- ifelse(is.na(new), fun(v2, t), new)
  new <- ifelse(is.na(new), fun(v3, t), new)
  return(new)
}



### Fonction : preparation_data

preparation_data <- function(d = d, fileDataClean = "data_clean_tot", fileLog = fileLog, habitats = "habitats_ok",
                             color_table = color_table, firstYear = NULL, lastYear = 2021, taxo = "Oiseaux",
                             selectedSpecies = ""){
  
  ## Paramètres pour tests
  #file <- "data_tot.txt"
  #habitats <- "habitats.shp"
  #color_code <- "color_code.csv"
  #firstYear <- NULL
  #lastYear <- 2021
  #taxo = "Oiseaux"
  #selectedSpecies <- "patrimoniales"

  
  
# ==============================================================================
#                              PHASE D'IMPORTATION
# ==============================================================================
  
  
  
#  start <- Sys.time()
#  fileLog <- paste("log",format(start, "%Y-%m-%d_%HH%M"),".txt",sep="")
#  
#  ## Chargement des données
#  catlog(c("\n===================================\n            IMPORTATION\n=================================== \n\nfile : ",
#             file,"\n\n"),fileLog)
#  
#  file <- paste("data/", file, sep = "")
#  dini <- read_delim(file, "\t", escape_double = FALSE, trim_ws = TRUE, skip = 0)
#  color_table <- read.csv("data/color_code.csv", sep=";", na.strings = "")
#
#  catlog(c("Nombre initial de lignes :", nrow(d), "\n"), fileLog)
  
  
  ## Suppression de la 1ère ligne : traduction des titres des champs
  if(is.na(as.numeric(d$ID_SIGHTING[1]))) d <- d[-c(1),]
  
  ## Gestion des formats des colonnes pour faciliter la manipulation
  catlog(c("\n===================================\n\n - Ajout de colonnes :\n-----------------------------------\n"),fileLog)
  
  colnum <- c("ID_SIGHTING","ID_SPECIES","SYS_ORDER","ID_PLACE","COUNTY","COORD_LAT","COORD_LON","ALTITUDE","TOTAL_COUNT","ATLAS_CODE")
  d[colnum] <- sapply(d[colnum], as.numeric)
  d$DATE <- as.Date.character(d$DATE, format = "%d.%m.%Y")
  
  catlog(c("    -> HOUR : la tranche horaire de l'observation isolée\n"),fileLog)
  d$HOUR <-  as.numeric(substring(d$TIMING, 1, 2))
  
  catlog(c("    -> HOUR_START : la tranche horaire de début de la liste\n"),fileLog)
  d$HOUR_START <- ifelse(d$TIME_START != "00:00", as.numeric(substring(d$TIME_START, 1, 2)), NA)
  
  catlog(c("    -> HOUR_STOP : la tranche horaire de fin de la liste\n"),fileLog)
  d$HOUR_STOP <- ifelse(d$TIME_STOP != "00:00", as.numeric(substring(d$TIME_STOP, 1, 2)), NA)
  
  catlog(c("    -> YEAR : l'année de l'observation\n"),fileLog)
  d$YEAR <- year(d$DATE)
  d$UPDATE_DATE <- as.Date.character(d$UPDATE_DATE, format = "%d.%m.%Y %H:%M")
  
  catlog(c("    -> MONTH : le mois de l'observation\n"),fileLog)
  d$MONTH <- month(d$DATE)

  ## Création de trois nouvelles colonnes DEMIJOURNEE, MAREE et COEFF
  catlog(c("    -> DEMIJOURNEE : AM ou PM\n"),fileLog)
  d$DEMIJOURNEE <- NA
  
  catlog(c("    -> MAREE\n"),fileLog)
  d$MAREE <- NA
  
  catlog(c("    -> COEFF : le coefficient de marée\n\n"),fileLog)
  d$COEFF <- NA
  
  
  
  ## Définition des colonnes a conserver
  selectedColumns <- c("ID_SIGHTING", "ID_SPECIES", "NAME_SPECIES", "SP", "TAXONOMY_NAME", "FAMILY_NAME", "SYS_ORDER",
                    "DATE", "YEAR", "MONTH", "HOUR", "HOUR_START", "HOUR_STOP", "DEMIJOURNEE", "USN", "ID_PLACE", "PLACE", "MUNICIPALITY",
                    "COUNTY", "COUNTRY", "COORD_LAT", "COORD_LON", "ESTIMATION_CODE", "TOTAL_COUNT", "DETAIL", "ATLAS_CODE", "BEHAVIOUR",
                    "PROTOCOL", "MAREE", "COEFF", "TEMP", "ENSOL", "PRECIP", "VENT_FORCE", "VENT_DIRECTION", "BRUME", "ETAT_EAU")



# ==============================================================================
#                               PHASE DE SELECTION
# ==============================================================================
  
  
  
## ATTENTION : En cours. Affiner la sélection jusqu'a l'emprise précise de la réserve 
## ATTENTION - ALTERNATIVE : Terminer par la sélection spatiale sur QGIS
## RESOLU : Sélection des données de la réserve sur la base des identifiants des USN
  
  
  
  ## Sélection des données de la commune
  
  catlog(c("\n==================================================\n\n - Selection :\n--------------------------------------------------\n"),fileLog)
  
  catlog(c("-- Sélection : données appartenant à la commune du Teich (33) - France\n"),fileLog)
  de <- subset(d, !(d$MUNICIPALITY == "Teich (Le)" & d$COUNTY == 33 & d$COUNTRY == "France"))
  
  if(nrow(de) > 0) {
    catlog(c("   ->", nrow(de), "données écartées\n"), fileLog)
  } else {
    catlog(c("   -> OK\n"),fileLog)
  }
           
  d <- subset(d, (d$MUNICIPALITY == "Teich (Le)" & d$COUNTY == 33 & d$COUNTRY == "France") | !is.na(d$PROTOCOL))
  catlog(c("   -> Nombre actuel de lignes :", nrow(d), "\n\n"), fileLog)
  
  
  
  ## Recuperation des identifiants des unités de comptage et sélection sur ceux-ci
  catlog(c("-- Sélection : données identifiées par une unité de comptage (USN)\n"),fileLog)
  catlog(c("       - Création de colonne : USN\n"),fileLog)
  d$USN <- NA
  d$USN <- str_extract(d$PLACE, "USN[0-9]{2}")
  
  d$USN <- ifelse(d$USN == "USN09" | d$USN == "USN10", "USN08", d$USN)
  d$USN <- ifelse(d$USN == "USN11", "USN12", d$USN)

  ## ATTENTION : Ici le problème ne se pose pas, mais pour l'avenir, différencier les ID_LOC si deux stations au nom différent ont la même USN
  #table_loc <- distinct(dplyr::select(d, c("PLACE", "USN")))
  
  
  catlog(c("       - Sélection\n"),fileLog)
  de <- subset(d, (is.na(d$USN) & d$PROTOCOL != "WATERBIRD") | d$ID_PLACE == 248400)
    
  if(nrow(de) > 0) {
    catlog(c("         ->", nrow(de), "données écartées\n"), fileLog)
  } else {
    catlog(c("         -> OK\n"),fileLog)
  }
  
  d <- subset(d, !is.na(d$USN) | (d$PROTOCOL == "WATERBIRD" & d$ID_PLACE != 248400))
  catlog(c("         -> Nombre actuel de lignes :", nrow(d), "\n\n"), fileLog)
  
  d$USN[is.na(d$USN)] <- "USN00"


  
  ## Suppression des données refusées
  ## ATTENTION : Ultérieurement, voir au cas par cas. Pour l'instant, toutes les données refusées, incomplètes ou probablement incorrectes sont refusées.
  catlog(c("-- Sélection : données vérifiées par Faune Aquitaine\n"),fileLog)
  de <- subset(d, !(is.na(d$ADMIN_HIDDEN)))
  
  if(nrow(de) > 0) {
    catlog(c("   ->", nrow(de), "données écartées\n"), fileLog)
  } else {
    catlog(c("   -> OK\n"),fileLog)
  }
  
  d <- subset(d, is.na(d$ADMIN_HIDDEN))
  catlog(c("   -> Nombre actuel de lignes :", nrow(d), "\n\n"), fileLog)
  
  
  
  ## Sélection des données sur la date
  catlog(c("-- Sélection :
              - données identifiées par une date d'observation\n"), fileLog)
  
  if(is.null(firstYear)) {
    firstYear = min(d$YEAR)
  }
  if(is.null(lastYear)) {
    lastYear = max(d$YEAR)
  }
  
  catlog(c("              - date de la donnée entre", firstYear, "et", lastYear, "\n"),fileLog)
  de <- subset(d, is.na(d$YEAR) | d$YEAR < firstYear | d$YEAR > lastYear)
  
  if(nrow(de) > 0) {
    catlog(c("   ->", nrow(de), "données écartées\n"), fileLog)
  } else {
    catlog(c("   -> OK\n"),fileLog)
  }
  
  d <- subset(d, !(is.na(d$YEAR)) & d$YEAR >= firstYear & d$YEAR <= lastYear)
  catlog(c("   -> Nombre actuel de lignes :", nrow(d), "\n\n"), fileLog)
  
  
  
  ## Suppression des entrées sans oiseau et des données autres que oiseau
  catlog(c("-- Sélection : données recensant au moins 1 individu du groupe taxonomique", taxo, "\n"),fileLog)
  de <- subset(d, d$TOTAL_COUNT <= 0 | d$TAXONOMY_NAME != taxo)
  
  if(nrow(de) > 0) {
    catlog(c("   ->", nrow(de), "données écartées\n"), fileLog)
  } else {
    catlog(c("   -> OK\n"),fileLog)
  }
  
  d <- subset(d, d$TOTAL_COUNT > 0 & d$TAXONOMY_NAME == taxo)
  catlog(c("   -> Nombre actuel de lignes :", nrow(d), "\n\n"), fileLog)
  
  
  
  ## Suppression des observations sans comptage
  catlog(c("-- Sélection : données où le nombre d'individus est estimé\n"),fileLog)
  de <- subset(d, d$ESTIMATION_CODE == "×")
  
  if(nrow(de) > 0) {
    catlog(c("   ->", nrow(de), "données écartées\n"), fileLog)
  } else {
    catlog(c("   -> OK\n"),fileLog)
  }
  
  d <- subset(d, d$TOTAL_COUNT > 0 & d$TAXONOMY_NAME == taxo)
  catlog(c("   -> Nombre actuel de lignes :", nrow(d), "\n\n"), fileLog)
  
  
  
  ## Suppression des doublons
  #Trier par date décroissante de dernière mise à jour
  #Supprimer les doublons
  catlog(c("-- Sélection : suppression des doublons\n"),fileLog)
  d <- d[order(d$UPDATE_DATE, decreasing = TRUE),]
  de <- subset(d, duplicated(d$ID_SIGHTING) == TRUE)
  
  if(nrow(de) > 0) {
    catlog(c("   ->", nrow(de), "données écartées\n"), fileLog)
  } else {
    catlog(c("   -> OK\n"),fileLog)
  }
  
  d <- subset(d, duplicated(d$ID_SIGHTING) == FALSE)
  catlog(c("   -> Nombre actuel de lignes :", nrow(d), "\n\n"), fileLog)
  
  
  
  ## Sélection des espèces a l'identification certaine (on enlève les "Genre sp.")
  catlog(c("-- Sélection : suppression des espèces indéterminées\n"), fileLog)
  sp <- unique(d$LATIN_SPECIES[grep("sp\\.", d$LATIN_SPECIES, ignore.case = TRUE)])
  
  de <- subset(d, d$LATIN_SPECIES %in% sp | d$LATIN_SPECIES == "Larus argentatus / cachinnans / michahellis")
  
  if(nrow(de) > 0) {
    catlog(c("   ->", nrow(de), "données écartées\n"), fileLog)
  } else {
    catlog(c("   -> OK\n"),fileLog)
  }
  
  d <- subset(d, d$LATIN_SPECIES %nin% sp)
  catlog(c("   -> Nombre actuel de lignes :", nrow(d), "\n\n"), fileLog)
  
  ## Regroupement des sous-espèces
  catlog(c("-- Regroupement des sous-espèces\n\n"), fileLog)
  ssp <- read.csv("library/ssp.csv", sep=";", na.strings = "")
  
  d$SP <- NA
  d$SP <- word(d$LATIN_SPECIES, 1, 2, sep = " ")
  
  
  
  ## On sélectionne les données des espèces d'intérêt (ici les espèces patrimoniales)
  ## ATTENTION : Pour l'instant, ne fonctionne qu'avec selectedSpecies = "patrimoniales". L'objectif est d'arriver a élargir les cas pour que ca fonctionne
  ## également avec une liste de familles ou d'espèces
  
  nat <- c("Limosa lapponica", "Calidris canutus", "Calidris minuta", "Anas clypeata", "Tringa nebularia", "Tringa erythropus",
           "Tringa totanus", "Actitis hypoleucos", "Numenius arquata", "Numenius phaeopus", "Pluvialis squatarola", "Calidris alpina")
  internat <- c("Recurvirostra avosetta", "Limosa limosa", "Anas acuta", "Charadrius hiaticula", "Platalea leucorodia", "Calidris alpina")
  
  if(selectedSpecies == "patrimoniales"){
    catlog(c("-- Sélection : espèces patrimoniales\n"),fileLog)
    de <- subset(d, d$SP %nin% nat & d$SP %nin% internat)
    
    if(nrow(de) > 0) {
      catlog(c("   ->", nrow(de), "données écartées\n"), fileLog)
    } else {
      catlog(c("   -> OK\n"),fileLog)
    }
    
    d <- subset(d, d$SP %in% nat | d$SP %in% internat)
    catlog(c("   -> Nombre actuel de lignes :", nrow(d), "\n\n"), fileLog)
  }
  
  
  
# ==============================================================================
#          PHASE D'EXTRACTION DES INFOS CONTENUES DANS LES COMMENTAIRES
# ==============================================================================
  
  catlog(c("\n==================================================\n\n - Vérification et extraction :\n--------------------------------------------------\n"),fileLog)
  
  ## Vérification des codes ATLAS
  catlog(c("-- Vérification : Codes Atlas\n"),fileLog)
  atlas <- c(2:19, 30, 40, 50)
  check <- subset(d, d$ATLAS_CODE %nin% atlas & !(is.na(d$ATLAS_CODE)))
  
  if(nrow(check) > 0) {
    catlog(c("   ->", nrow(check), "Codes Atlas invalides, remplacés par NA\n\n"), fileLog)
  } else {
    catlog(c("   -> OK\n\n"),fileLog)
  }
  
  d$ATLAS_CODE <- ifelse(d$ATLAS_CODE %in% atlas, d$ATLAS_CODE, NA)


  
  ## Récuperation des coefficients de marée et de l'état de la marée
  # Codes Marée sous la forme MARX avec X entre 1 et 4
  # Codes Coeff sous la forme CX avec X entre 1 et 4
  
  d$PM_EXTRACT <- str_extract(d$DAILY_TEXT_COMMENT_REM,
                              "\\bPM\\s*[\\+-]?\\s*[0-5]?\\s*[,hH]?\\s*[0-9]{0,2}\\s*[àa-]?\\s*P?M?\\s*[+-]?\\s*[+-]?\\s*[0-5]?\\s*[,hH]?\\s*[0-9]{0,2}")
  d$PM_EXTRACT <- ifelse(is.na(d$PM_EXTRACT),
                         str_extract(d$COMMENT,
                                     "\\bPM\\s*[\\+-]?\\s*[0-5]?\\s*[,hH]?\\s*[0-9]{0,2}\\s*[àa-]?\\s*P?M?\\s*[+-]?\\s*[+-]?\\s*[0-5]?\\s*[,hH]?\\s*[0-9]{0,2}"),
                         d$PM_EXTRACT)
  d$PM_EXTRACT <- ifelse(is.na(d$PM_EXTRACT),
                         str_extract(d$PRIVATE_COMMENT,
                                     "\\bPM\\s*[\\+-]?\\s*[0-5]?\\s*[,hH]?\\s*[0-9]{0,2}\\s*[àa-]?\\s*P?M?\\s*[+-]?\\s*[+-]?\\s*[0-5]?\\s*[,hH]?\\s*[0-9]{0,2}"),
                         d$PM_EXTRACT)
  
  d$PM_EXTRACT <- gsub(",|h|H", "\\.", d$PM_EXTRACT)
  d$PM_EXTRACT <- gsub("\\.\\.", "\\.", d$PM_EXTRACT)
  d$PM_EXTRACT <- gsub(" ", "", d$PM_EXTRACT)
  d$PM_EXTRACT <- gsub("\\.$", "", d$PM_EXTRACT)
  d$PM_EXTRACT <- gsub("(?<=[0-9])([\\+-])", "PM\\1", d$PM_EXTRACT, perl = TRUE)
  d$PM_EXTRACT <- gsub("(?<=à)([\\+-])", "PM\\1", d$PM_EXTRACT, perl = TRUE)
  
  dfPM <- moyennes_PM(d$PM_EXTRACT)
  
  dfPM <- dplyr::select(dfPM, c("PM_EXTRACT", "MEAN"))
  dfPM <- distinct(dfPM)
  
  d$PM_EXTRACT <- str_extract(d$PM_EXTRACT, "(PM[\\+-]?[0-5]?\\.?[0-9]{0,2})[à\\b-]?(PM[\\+-]?[0-5]?\\.?[0-9]{0,2})?")
  
  d <- merge(d, dfPM, by = c("PM_EXTRACT"), sort = FALSE)
  
  
  ## A cause des interférences entre PM (horaire) et PM (marée haute), on commence par extraire les données de tranche horaire
  catlog(c("-- Stratification des données par demi-journée, marée et coefficient de marée\n"),fileLog)
  catlog(c("   - Récupération des demi-journées à partir de l'heure d'observation\n"),fileLog)
  recup_dj <- sum(!(is.na(d$DEMIJOURNEE)))
  d$DEMIJOURNEE <- ifelse(d$HOUR >= 13 | d$HOUR_START >= 13, "PM",
                          ifelse(d$HOUR < 13 | d$HOUR_STOP < 13, "AM", d$DEMIJOURNEE))
  catlog(c("     ->", sum(!(is.na(d$DEMIJOURNEE))) - recup_dj, "valeurs obtenues\n"), fileLog)
  recup_dj <- sum(!(is.na(d$DEMIJOURNEE)))
  
  
  
  catlog(c("   - Récupération des demi-journées à partir des mentions AM dans les commentaires\n"),fileLog)
  DJ <- str_extract(d$DAILY_TEXT_COMMENT_REM, "\\bAM\\b")
  DJ <- ifelse(is.na(DJ), str_extract(d$COMMENT, "\\bAM\\b"), DJ)
  DJ <- ifelse(is.na(DJ), str_extract(d$PRIVATE_COMMENT, "\\bAM\\b"), DJ)
  d$DEMIJOURNEE <- ifelse(is.na(d$DEMIJOURNEE), DJ, d$DEMIJOURNEE)
  catlog(c("     ->", sum(!(is.na(d$DEMIJOURNEE))) - recup_dj, "valeurs obtenues\n"), fileLog)
  recup_dj <- sum(!(is.na(d$DEMIJOURNEE)))
  
  
  
  ## On récupère les PM potentiellement horaires en les définissant ainsi :
  ## 1. Ils ne sont pas suivis par un + ou un -
  ## 2. Ils ne sont pas directement suivis par un caractère alphanumérique
  ## Puis on les affecte a DEMIJOURNEE seulement si le champ est deja vide (auquel cas on considère que PM correspond a la marée)
  catlog(c("   - Récupération des demi-journées à partir des mentions PM dans les commentaires\n"),fileLog)
  d$RECUP_DJ <- str_extract(d$DAILY_TEXT_COMMENT_REM, "PM(?!.?\\+)(?!.?-)(?![:alnum:])")
  d$RECUP_DJ <- ifelse(is.na(d$RECUP_DJ), str_extract(d$COMMENT, "PM(?!.?\\+)(?!.?-)(?![:alnum:])"), d$RECUP_DJ)
  d$RECUP_DJ <- ifelse(is.na(d$RECUP_DJ), str_extract(d$PRIVATE_COMMENT, "PM(?!.?\\+)(?!.?-)(?![:alnum:])"), d$RECUP_DJ)
  catlog(c("     ->", sum(is.na(d$DEMIJOURNEE) & !(is.na(d$RECUP_DJ))), "valeurs obtenues\n"), fileLog)
  recup_dj <- sum(is.na(d$DEMIJOURNEE) & !(is.na(d$RECUP_DJ))) + sum(!(is.na(d$DEMIJOURNEE)))
  
  
  
  ## On extrait dans les champs de commentaires les infos de type MARX avec X un nombre quel qu'il soit.
  catlog(c("   - Récupération de la marée à partir des mentions MAR dans les commentaires\n"),fileLog)
  recup_m <- sum(!(is.na(d$MAREE)))
  maree <- str_extract(d$DAILY_TEXT_COMMENT_REM, "MAR[0-9]+")
  maree <- ifelse(is.na(maree), str_extract(d$COMMENT, "MAR[0-9]+"), maree)
  maree <- ifelse(is.na(maree), str_extract(d$PRIVATE_COMMENT, "MAR[0-9]+"), maree)
  d$MAREE <- maree
  
  catlog(c("     ->", sum(!(is.na(d$MAREE))) - recup_m, "valeurs obtenues\n"), fileLog)
  recup_m <- sum(!(is.na(d$MAREE)))
  
  
  
  ## On sélectionne uniquement dans ce champ les valeurs de MAREE qui contiennent exactement 4 caractères (permet d'éviter les "MAR25" par exemple)
  d$MAREE <- ifelse(nchar(d$MAREE) != 4 | is.na(nchar(d$MAREE)), NA, d$MAREE)
  catlog(c("     ->", recup_m - sum(!(is.na(d$MAREE))), "valeurs erronées écartées\n"), fileLog)
  recup_m <- sum(!(is.na(d$MAREE)))
  
  
  
  ## Si l'info de marée n'est pas donnée par MARX, et si DEMIJOURNEE n'est pas vide, on vide RECUP_DJ (car le PM qu'il contient peut être
  ## celui de la marée)
  catlog(c("   - Correction des demi-journées\n"),fileLog)
  ndj <- sum(!(is.na(d$RECUP_DJ)) & is.na(d$MAREE) & is.na(d$DEMIJOURNEE))
  d$RECUP_DJ <- ifelse(is.na(d$MAREE) | !(is.na(d$DEMIJOURNEE)), NA, d$RECUP_DJ)
  catlog(c("     ->", ndj, "valeurs erronées écartées\n"), fileLog)
  recup_dj <- recup_dj - ndj
  
  
  
  ## Si l'info de marée n'est pas donnée par MARX et si les commentaires contiennent PM et "matin", on met "AM" dans RECUP_DJ
  catlog(c("   - Récupération des demi-journées à partir de la mention MATIN\n"),fileLog)
  recup_recup_dj <- sum(is.na(d$DEMIJOURNEE) & !(is.na(d$RECUP_DJ)))
  DJ <- str_extract(d$DAILY_TEXT_COMMENT_REM, "(?<![a-z]\\s?)[M,m]atin")
  DJ <- ifelse(is.na(DJ),str_extract(d$COMMENT, "(?<![a-z]\\s?)[M,m]atin"), DJ)
  DJ <- ifelse(is.na(DJ),str_extract(d$PRIVATE_COMMENT, "(?<![a-z]\\s?)[M,m]atin"), DJ)
  d$RECUP_DJ <- ifelse(is.na(d$RECUP_DJ) & !(is.na(DJ)), "AM", d$RECUP_DJ)
  catlog(c("     ->", sum(is.na(d$DEMIJOURNEE) & !(is.na(d$RECUP_DJ))) - recup_recup_dj, "valeurs obtenues\n"), fileLog)
  
  

  ## On considère que ce qui reste dans RECUP_DJ est effectivement relatif aux horaires. On le bascule dans DEMIJOURNEE
  d$DEMIJOURNEE <- ifelse(is.na(d$DEMIJOURNEE), d$RECUP_DJ, d$DEMIJOURNEE)
  
  
  
  ## On récupère les valeurs de marée à partir des indications PM+ ou PM- X, ainsi que des BM+ ou BM- X.
  ## On sélectionne tous les BM éventuellement suivis d'espaces, de + ou - et de chiffres.
  catlog(c("   - Récupération de la marée à partir des mentions BM dans les commentaires\n"),fileLog)
  mareeBasse <- str_extract(d$DAILY_TEXT_COMMENT_REM, "\\bBM\\s{0,}[\\+-]{0,}\\s{0,}[0-9]*")
  mareeBasse <- ifelse(is.na(mareeBasse), str_extract(d$COMMENT, "\\bBM\\s{0,}[\\+-]{0,}\\s{0,}[0-9]*"), mareeBasse)
  mareeBasse <- ifelse(is.na(mareeBasse), str_extract(d$PRIVATE_COMMENT, "\\bBM\\s{0,}[\\+-]{0,}\\s{0,}[0-9]*"), mareeBasse)
  
  ## On se débarasse des espaces qui vont nous gêner plus tard et on injecte dans d
  mareeBasse <- gsub(" ", "", mareeBasse)
  d$RECUP_MAREE_BASSE <- mareeBasse
  catlog(c("     ->", sum(is.na(d$MAREE) & !(is.na(d$RECUP_MAREE_BASSE))), "valeurs obtenues\n"), fileLog)
  recup_m <- recup_m + sum(is.na(d$MAREE) & !(is.na(d$RECUP_MAREE_BASSE)))
  
  
  
#  ##Idem avec les PM (première méthode imparfaite conservée pour archive de la méthode)
#  catlog(c("   - Récupération de la marée à partir des mentions PM dans les commentaires\n"),fileLog)
#  mareeHaute <- str_extract(d$DAILY_TEXT_COMMENT_REM, "\\bPM\\s{0,}[\\+-]{0,}\\s{0,}[0-9]*")
#  mareeHaute <- ifelse(is.na(mareeHaute), str_extract(d$COMMENT, "\\bPM\\s{0,}[\\+-]{0,}\\s{0,}[0-9]*"), mareeHaute)
#  mareeHaute <- ifelse(is.na(mareeHaute), str_extract(d$PRIVATE_COMMENT, "\\bPM\\s{0,}[\\+-]{0,}\\s{0,}[0-9]*"), mareeHaute)
#  mareeHaute <- gsub(" ", "", mareeHaute)
#  d$RECUP_MAREE_HAUTE <- ifelse(is.na(d$RECUP_MAREE_BASSE), mareeHaute, NA)
#  catlog(c("     ->", sum(is.na(d$MAREE) & is.na(d$RECUP_MAREE_BASSE) & !(is.na(d$RECUP_MAREE_HAUTE))), "valeurs obtenues\n"), fileLog)
#  recup_m <- recup_m + sum(is.na(d$MAREE) & is.na(d$RECUP_MAREE_BASSE) & !(is.na(d$RECUP_MAREE_HAUTE)))
  
#  ## Etape supplémentaire pour les PM : on exclut ceux qui peuvent être confondus avec une info de demi-journée
#  catlog(c("   - Correction de la donnée de marée\n"),fileLog)
#  nmh <- sum(!(is.na(d$RECUP_MAREE_HAUTE)) & is.na(d$MAREE))
#  d$RECUP_MAREE_HAUTE <- ifelse(d$RECUP_MAREE_HAUTE == "PM" & d$DEMIJOURNEE == "PM", NA, d$RECUP_MAREE_HAUTE)
#  catlog(c("     ->", nmh - sum(!(is.na(d$RECUP_MAREE_HAUTE)) & is.na(d$MAREE)), "valeurs erronées écartées\n"), fileLog)
#  recup_m <- recup_m - (nmh - sum(!(is.na(d$RECUP_MAREE_HAUTE)) & is.na(d$MAREE)))
  
  
  
  ## Etape supplémentaire pour les PM : on exclut ceux qui peuvent être confondus avec une info de demi-journee
  ## ATTENTION: LOG A CORRIGER
  catlog(c("   - Correction de la donnée de marée\n"),fileLog)
  nmh <- sum(!(is.na(d$MEAN)) & is.na(d$MAREE))
  d$MEAN <- ifelse(d$PM_EXTRACT == "PM" & d$DEMIJOURNEE == "PM", NA, d$MEAN)
  catlog(c("     ->", nmh - sum(!(is.na(d$MEAN)) & is.na(d$MAREE)), "valeurs erronées écartées\n"), fileLog)
  recup_m <- recup_m - (nmh - sum(!(is.na(d$MEAN)) & is.na(d$MAREE)))
  
  
  
  ## On affecte la valeur 0 aux PM et BM purs, pour ne pas les perdre lorsqu'on convertira en numérique
  catlog(c("   - Conversion des marées sous forme PM en classes MAR\n\n"),fileLog)
  d$RECUP_MAREE_BASSE <- ifelse(d$RECUP_MAREE_BASSE == "BM", 0, d$RECUP_MAREE_BASSE)
#  d$RECUP_MAREE_HAUTE <- ifelse(d$RECUP_MAREE_HAUTE == "PM", 0, d$RECUP_MAREE_HAUTE)
  
  ## On convertit au format numérique apres avoir garde uniquement les +X et -X
  d$RECUP_MAREE_BASSE <- as.numeric(gsub("BM", "", d$RECUP_MAREE_BASSE))
#  d$RECUP_MAREE_HAUTE <- as.numeric(gsub("PM", "", d$RECUP_MAREE_HAUTE))
  
  ## On affecte les classes de MARX aux tranches restantes
  d$RECUP_MAREE_BASSE <- ifelse(d$RECUP_MAREE_BASSE > -2 & d$RECUP_MAREE_BASSE < 2, "MAR4",
                                ifelse(d$RECUP_MAREE_BASSE > -4 & d$RECUP_MAREE_BASSE <= -2 |
                                       d$RECUP_MAREE_BASSE >= 2 & d$RECUP_MAREE_BASSE < 4, "MAR3", NA))
  
  d$RECUP_MAREE_HAUTE <- ifelse(d$MEAN >= -1 & d$MEAN <= 1, "MAR1",
                                ifelse(d$MEAN >= -2 & d$MEAN < -1 |
                                       d$MEAN > 1 & d$MEAN <= 2, "MAR2",
                                       ifelse(d$MEAN >= -4 & d$MEAN < -2 |
                                              d$MEAN > 2 & d$MEAN <= 4, "MAR3",
                                              ifelse(d$MEAN == -4 | d$MEAN == 4, "MAR4",
                                                     ifelse(d$MEAN >= 4, "MAR1", NA)))))
  
  ## On injecte les valeurs de marées ainsi recupérées dans MAREE, sans écraser celles qu'on avait déjà (on privilégie l'info MARX directement
  ## dans les données à celle déduite des PM+X et PM-X)
  d$MAREE <- ifelse(is.na(d$MAREE), ifelse(is.na(d$RECUP_MAREE_BASSE), d$RECUP_MAREE_HAUTE, d$RECUP_MAREE_BASSE), d$MAREE)
  
  catlog(c("TOTAL : Demi-journée récupérée pour", sum(!(is.na(d$DEMIJOURNEE))), "données
        Marée récupérée pour", sum(!(is.na(d$MAREE))), "données\n\n"), fileLog)
  
  
  
  ## On s'occupe ici de la récupération des données de température, car la suite du script implique de supprimer le symbole °.
  catlog(c("   - Récupération des températures dans les commentaires\n"), fileLog)
  d$TEMP <- NA
  d$DAILY_TEXT_COMMENT_REM <- gsub("Température", "t", d$DAILY_TEXT_COMMENT_REM, ignore.case = TRUE)
  d$TEMP <- ifelse(str_detect(d$COMMENT, "beau et chaud") == TRUE, 25, d$TEMP)
  d$TEMP <- extract_commentaires(d, new = d$TEMP, t = regex("\\bt\\s*-?\\s*[0-9]{1,2}", ignore_case = TRUE))
  d$TEMP <- extract_commentaires(d, new = d$TEMP, t = regex("(?<![0-9]\\s{0,10})-?[0-9]{1,2}°", ignore_case = TRUE))
  d$TEMP <- extract_commentaires(d, new = d$TEMP, t = regex("T°\\s*-?\\s*[0-9]{1,2}", ignore_case = TRUE))
  catlog(c("     ->", sum(!is.na(d$TEMP)), "valeurs obtenues\n"), fileLog)
  
  d$TEMP <- gsub("°|T| |t", "", d$TEMP)
  d$TEMP <- as.numeric(d$TEMP)
  d$TEMP <- ifelse(d$TEMP > 50, NA, d$TEMP)
  
  
  
  ## Extraction des données d'ensoleillement
  catlog(c("   - Récupération des couvertures nuageuses dans les commentaires\n"), fileLog)
  ensol0 <- c("beau\\b", "clair", "dégagé", "ensoleillé")
  ensol1 <- c("ciel de traîne", "couvert\\+soleil", "couverture nuageuse légère", "entrée mariti", "nuageux", "se couvrant", "soleil\\+nuages")
  d2 <- d
  
  ##Couverture nuageuse faible
  d$ENSOL <- NA
  d$ENSOL0 <- NA
  d$ENSOL0 <- extract_commentaires(d, new = d$ENSOL0, fun = str_detect, t = regex(str_c("(", str_c(ensol0, collapse = "|"), ")"), ignore_case = TRUE))
  d$ENSOL <- ifelse(d$ENSOL0 == TRUE, 0, NA)

  ##Couverture nuageuse moyenne
  d$ENSOL1 <- NA
  d$ENSOL1 <- extract_commentaires(d, new = d$ENSOL1, fun = str_detect, t = regex(str_c("(", str_c(ensol1, collapse = "|"), ")"), ignore_case = TRUE))
  d$ENSOL <- ifelse(d$ENSOL1 == TRUE & is.na(d$ENSOL), 1, d$ENSOL)
  
  ##Couverture nuageuse élevée
  d$ENSOL2 <- NA
  d$ENSOL2 <- extract_commentaires(d, new = d$ENSOL2, fun = str_detect, t = regex("couvert", ignore_case = TRUE))
  d$ENSOL <- ifelse(d$ENSOL2 == TRUE & is.na(d$ENSOL), 2, d$ENSOL)
  
  ##Couverture nuageuse faible bis
  d$ENSOL0 <- NA
  d$ENSOL0 <- extract_commentaires(d, new = d$ENSOL0, fun = str_detect, t = regex("soleil", ignore_case = TRUE))
  d$ENSOL <- ifelse(d$ENSOL0 == TRUE & is.na(d$ENSOL), 0, d$ENSOL)
  
  catlog(c("     ->", sum(!is.na(d$ENSOL)), "valeurs obtenues\n"), fileLog)
  
  
  
  ##Extraction des données de brume
  catlog(c("   - Récupération de la brume dans les commentaires\n"), fileLog)
  d$BRUME <- NA
  d$BRUME <- extract_commentaires(d, new = d$BRUME, fun = str_detect, t = regex("brume", ignore_case = TRUE))
  
  
  
  ##Extraction de l'état des plans d'eau
  catlog(c("   - Récupération de l'état des plans d'eau dans les commentaires\n"), fileLog)
  crues <- c("crue", "crûe", "submersion", "surcôte majeure")
  d$ETAT_EAU <- NA
  d$ETAT_EAU <- extract_commentaires(d, new = d$ETAT_EAU, fun = str_detect, t = regex(str_c("(", str_c(crues, collapse = "|"), ")"), ignore_case = TRUE))
  d$ETAT_EAU <- ifelse(d$ETAT_EAU, "Crue", NA)
  d$ETAT_EAU <- str_to_title(extract_commentaires(d, new = d$ETAT_EAU, t = regex("(gel)|(assec)", ignore_case = TRUE)))
  
  
  
  ##Extraction des précipitations
  catlog(c("   - Récupération des précipitations dans les commentaires\n"), fileLog)
  precip1 <- c("bruine", "pluie fine")
  precip3 <- c("averses", "giboulées", "neige épaisse", "ondées", "orage", "grains")
  
  ##Pas de pluie
  d$PRECIP <- NA
  d$PRECIP0 <- NA
  d$PRECIP0 <- extract_commentaires(d, new = d$PRECIP0, fun = str_detect, t = regex("Temps à grains", ignore_case = TRUE))
  d$PRECIP <- ifelse(d$PRECIP0 == TRUE, 0, NA)
  
  ##Pluie légère
  d$PRECIP1 <- NA
  d$PRECIP1 <- extract_commentaires(d, new = d$PRECIP1, fun = str_detect, t = regex(str_c("(", str_c(precip1, collapse = "|"), ")"), ignore_case = TRUE))
  d$PRECIP <- ifelse(d$PRECIP1 == TRUE & is.na(d$PRECIP), 1, d$PRECIP)
  
  ##Pluie modérée
  d$PRECIP2 <- NA
  d$PRECIP2 <- extract_commentaires(d, new = d$PRECIP2, fun = str_detect, t = regex("(pluvieux)|(pluie)", ignore_case = TRUE))
  d$PRECIP <- ifelse(d$PRECIP2 == TRUE & is.na(d$PRECIP), 2, d$PRECIP)
  
  ##Pluie forte
  d$PRECIP3 <- NA
  d$PRECIP3 <- extract_commentaires(d, new = d$PRECIP3, fun = str_detect, t = regex(str_c("(", str_c(precip3, collapse = "|"), ")"), ignore_case = TRUE))
  d$PRECIP <- ifelse(d$PRECIP3 == TRUE & is.na(d$PRECIP), 3, d$PRECIP)
  
  ##Pas de pluie bis
  d$PRECIP <- ifelse(is.na(d$PRECIP) & !is.na(d$ENSOL), 0, d$PRECIP)
  
  catlog(c("     ->", sum(!is.na(d$PRECIP)), "valeurs obtenues\n"), fileLog)
  
  
  ##Extraction du vent
  catlog(c("   - Récupération du vent dans les commentaires\n"), fileLog)
  d$VENT_DIRECTION <- NA
  d$VENT_DIRECTION <- extract_commentaires(d, new = d$VENT_DIRECTION, t = "(?<!\\.)\\b[NESW]{1,3}\\b(?![\\('/:°\\.])")
  
  catlog(c("     ->", sum(!is.na(d$VENT_DIRECTION)), "valeurs de direction obtenues\n"), fileLog)
  
  d$VENT_FORCE <- NA
  d$BEAUFORT <- NA
  d$BEAUFORT <- extract_commentaires(d, new = d$BEAUFORT, t = "(?<=(((?<!\\.)\\b[NESW]{1,3}\\b|[Vv]ent)\\s{0,10}))[0-9]{1,2}(?![°0-9])")
  d$BEAUFORT <- extract_commentaires(d, new = d$BEAUFORT, t = "(?<=/)0(?![0-9])")
  d$BEAUFORT <- ifelse(d$BEAUFORT == 0 & !is.na(d$VENT_DIRECTION), 1, d$BEAUFORT)
  d$BEAUFORT <- as.numeric(d$BEAUFORT)
  
  d$VENT_FORCE <- ifelse(d$BEAUFORT == 0, "Pas de vent",
                         ifelse(d$BEAUFORT <= 3, "Vent faible",
                                ifelse(d$BEAUFORT <= 5, "Vent modéré",
                                       ifelse(d$BEAUFORT <= 7, "Vent fort",
                                              ifelse(d$BEAUFORT <= 12, "Vent violent", NA)))))
  
  vent0 <- c("pas vent", "vent nul", "pas de vent")
  vent1 <- c("vent faible", "léger")
  vent3 <- c("coup de vent", "vent fort")
  vent4 <- c("fortes rafales", "tempête")
  
  ## Pas de vent
  d$VENT0 <- NA
  d$VENT0 <- extract_commentaires(d, new = d$VENT0, fun = str_detect, t = regex(str_c("(", str_c(vent0, collapse = "|"), ")"), ignore_case = TRUE))
  d$VENT_FORCE <- ifelse(d$VENT0 == TRUE & is.na(d$VENT_FORCE), "Pas de vent", d$VENT_FORCE)
  
  ## Vent faible
  d$VENT1 <- NA
  d$VENT1 <- extract_commentaires(d, new = d$VENT1, fun = str_detect, t = regex(str_c("(", str_c(vent1, collapse = "|"), ")"), ignore_case = TRUE))
  d$VENT_FORCE <- ifelse(d$VENT1 == TRUE & is.na(d$VENT_FORCE), "Vent faible", d$VENT_FORCE)
  
  ## Vent modéré
  d$VENT2 <- NA
  d$VENT2 <- extract_commentaires(d, new = d$VENT2, fun = str_detect, t = regex("Vent modéré", ignore_case = TRUE))
  d$VENT_FORCE <- ifelse(d$VENT2 == TRUE & is.na(d$VENT_FORCE), "Vent modéré", d$VENT_FORCE)
  
  ## Vent fort
  d$VENT3 <- NA
  d$VENT3 <- extract_commentaires(d, new = d$VENT3, fun = str_detect, t = regex(str_c("(", str_c(vent3, collapse = "|"), ")"), ignore_case = TRUE))
  d$VENT_FORCE <- ifelse(d$VENT3 == TRUE & is.na(d$VENT_FORCE), "Vent fort", d$VENT_FORCE)
  
  ## Vent violent
  d$VENT4 <- NA
  d$VENT4 <- extract_commentaires(d, new = d$VENT4, fun = str_detect, t = regex(str_c("(", str_c(vent4, collapse = "|"), ")"), ignore_case = TRUE))
  d$VENT_FORCE <- ifelse(d$VENT4 == TRUE & is.na(d$VENT_FORCE), "Vent violent", d$VENT_FORCE)
  
  ## Vent modéré bis
  ## Pas de vent
  d$VENT2 <- NA
  d$VENT2 <- extract_commentaires(d, new = d$VENT2, fun = str_detect, t = regex("vent", ignore_case = TRUE))
  d$VENT_FORCE <- ifelse(d$VENT2 == TRUE & is.na(d$VENT_FORCE), "Vent modéré", d$VENT_FORCE)
  
  catlog(c("     ->", sum(!is.na(d$VENT_FORCE)), "valeurs de puissance obtenues\n"), fileLog)
  
  
  
  ## On remplace quelques caractères qui vont gêner la recherche pour uniformiser le jeu de données
  catlog(c("   - Récupération des coefficients bruts et classés de 1 à 4 dans les commentaires\n"), fileLog)
  commentaires <- c("DAILY_TEXT_COMMENT_REM", "COMMENT", "PRIVATE_COMMENT") 
  d[commentaires] <- lapply(d[commentaires], gsub, pattern = "Â°", replacement = "")
  d[commentaires] <- lapply(d[commentaires], gsub, pattern = "coeff | coefficient", replacement = "C", ignore.case = TRUE)
  
  ## On extrait dans les champs de commentaires les infos de type CX, C.X, C X etc, avec X un nombre quel qu'il soit.
  coeff <- str_extract(d$DAILY_TEXT_COMMENT_REM, "\\bC\\s*\\.*\\s*[0-9]+")
  coeff <- ifelse(is.na(coeff), str_extract(d$COMMENT, "\\bC\\s*\\.*\\s*[0-9]+"), coeff)
  coeff <- ifelse(is.na(coeff), str_extract(d$PRIVATE_COMMENT, "\\bC\\s*\\.*\\s*[0-9]+"), coeff)
  
  ## On supprime les points et les espaces
  coeff <- gsub("\\.", "", coeff)
  coeff <- gsub("\\s", "", coeff)
  
  ## On applique la fonction correct_coeff pour recuperer les coefficients renseignes tels quels et les convertir dans les classes definies
  ## Ex : C83 devient C2, C119 devient C1 et C12 devient NA. C2 reste C2.
  catlog(c("   - Correction des coefficients bruts dans les classes appropriées\n"),fileLog)
  coeff_OK <- correct_coeff(coeff)
  
  ## On affecte au champ COEFF les bonnes valeurs de coeff.
  d$COEFF <- coeff_OK
  catlog(c("     ->", sum(!(is.na(d$COEFF))), "valeurs obtenues\n\n"), fileLog)
  

  d <- dplyr::select(d, selectedColumns)
  
  
  
  catlog(c("\n==================================================\n\n  IMPORTATION : Carte des habitats de la réserve\n--------------------------------------------------\n"),fileLog)
  habitats <- st_read(dsn = paste0('library/', habitats,'.shp'), layer = habitats)
  
  #Encoding(habitats$habitat) <- "UTF_8"    #Unmute en cas de problème d'encodage
  habitats <- dplyr::select(habitats, c("Habitat", "geometry"))
  habitats <- merge(habitats, color_table)

  ## Plot de la carte de la réserve
  catlog(c("   - Affichage : Données sur la carte des habitats de la réserve\n"),fileLog)
  colScale <- habitats$color_code
  colScale <- colScale[!duplicated(colScale)]
  
  gg <- ggplot() + geom_sf(data = habitats, aes(fill = Habitat), alpha = 0.75) +
    scale_fill_manual(values = colScale)
  gg <- gg + geom_point(data = d, aes(x = COORD_LON, y = COORD_LAT))
  print(gg)
  
  
  ### SELECTION : HOMOGENEISATION DES PROTOCOLES PAR RECONSTRUCTION
  
  info_marees <- read.csv("library/info_marees.csv")
  info_marees$DATE <- as.Date(info_marees$DATE, format = '%d/%m/%Y')
  info_marees$YEAR <- year(info_marees$DATE)
  info_marees$PM1 <- as.character(info_marees$PM1)
  info_marees$PM2 <- as.character(info_marees$PM2)
  for(i in 1:4) info_marees$PM1 <- ifelse(nchar(info_marees$PM1) < 4, paste0("0", info_marees$PM1), info_marees$PM1)
  
  info_marees$PM1 <- gsub("^(.{2})(.*)$", "\\1:\\2", info_marees$PM1)
  info_marees$PM2 <- gsub("^(.{2})(.*)$", "\\1:\\2", info_marees$PM2)
  
  info_marees$PM1 <- strptime(info_marees$PM1, format = "%H:%M")
  info_marees$PM2 <- strptime(info_marees$PM2, format = "%H:%M")
  
  d <- merge(d, info_marees, c('DATE', 'YEAR'), all.x = TRUE)
  
  d$MAREE <- ifelse(d$YEAR >= 1980 & d$YEAR <= 2008, "MAR1", d$MAREE)

  d$CHOIX_PM <- ifelse(is.na(d$COEFF) & d$YEAR >= 1980 & d$YEAR <= 2011,
                    ## Si été
                    ifelse(d$MONTH >= 4 & d$MONTH <= 9,
                           ## Si on prend la marée du matin
                           ifelse(d$PM1 >= strptime("06:30", format = "%H:%M") & d$PM2 >= strptime("17:30", format = "%H:%M"),
                                  1,
                                  ## Sinon
                                  ifelse(d$PM1 <= strptime("06:30", format = "%H:%M") & d$PM2 <= strptime("17:30", format = "%H:%M"),
                                         2,
                                         NA)),
                           ## Si hiver, si on prend la marée du matin
                           ifelse(d$PM1 >= strptime("07:30", format = "%H:%M") & d$PM2 >= strptime("17:30", format = "%H:%M"),
                                  1,
                                  ifelse(d$PM1 <= strptime("06:30", format = "%H:%M") & d$PM2 <= strptime("17:30", format = "%H:%M"),
                                         2,
                                         NA))),
                    NA)
  
  d$COEFF <- ifelse(is.na(d$COEFF), ifelse(d$CHOIX_PM == 1, d$COEFF1, ifelse(d$CHOIX_PM == 2, d$COEFF2, d$COEFF)), d$COEFF)
  d$DEMIJOURNEE <- ifelse(is.na(d$DEMIJOURNEE), ifelse(d$CHOIX_PM == 1, 'AM', ifelse(d$CHOIX_PM == 2, 'PM', d$DEMIJOURNEE)), d$DEMIJOURNEE)
  
  dates_pot <- dplyr::select(subset(d, d$YEAR <= 2011 & !is.na(d$COEFF)), c('DATE', 'YEAR', 'MONTH', 'COEFF'))
  dates_pot <- distinct(dates_pot)
  dates_pot$MDAY <- day(dates_pot$DATE)
  dates_pot <- subset(dates_pot, dates_pot$COEFF >= 70)
  
  dates_ok <- data.frame(YEAR = dates_pot$YEAR, MONTH = dates_pot$MONTH, C1 = NA, C2 = NA, C3 = NA, N_DATES = NA)
  dates_ok <- distinct(dates_ok)
  
  year <- sort(unique(dates_ok$YEAR))
  month <- sort(unique(dates_ok$MONTH))
  
  for(y in year){
    for(m in month){
      dates_ok$N_DATES[dates_ok$YEAR == y & dates_ok$MONTH == m] <- length(dates_pot$COEFF[dates_pot$YEAR == y & dates_pot$MONTH == m])
      a_rep <- dates_ok$C2[dates_ok$YEAR == y & dates_ok$MONTH == m]
      date_limi <- dates_pot$MDAY[dates_pot$COEFF == max(dates_pot$COEFF[dates_pot$YEAR == y & dates_pot$MONTH == m
                                                                         & dates_pot$MDAY >= 12 & dates_pot$MDAY <= 18]) &
                                    dates_pot$YEAR == y & dates_pot$MONTH == m & dates_pot$MDAY >= 12 & dates_pot$MDAY <= 18]
      a_rep <- ifelse(length(date_limi) == 0, a_rep, date_limi)
      
      if(length(dates_ok$N_DATES[dates_ok$YEAR == y & dates_ok$MONTH == m]) > 0){
        i = 4
        #browser()
        while(is.na(a_rep)){
          #browser()
          date_limi <- dates_pot$MDAY[dates_pot$COEFF == max(dates_pot$COEFF[dates_pot$YEAR == y & dates_pot$MONTH == m
                                                                             & dates_pot$MDAY >= 15-i & dates_pot$MDAY <= 15+i]) &
                                        dates_pot$YEAR == y & dates_pot$MONTH == m & dates_pot$MDAY >= 15-i & dates_pot$MDAY <= 15+i]
          a_rep <- ifelse(length(date_limi) == 0, a_rep, date_limi)
          i <- i+1
        }
      }
      dates_ok$C2[dates_ok$YEAR == y & dates_ok$MONTH == m] <- a_rep
    }
  }
  
  dates_ok$N_DATES <- dates_ok$N_DATES - 1
  for(y in year){
    for(m in month){
      a_rep <- dates_ok$C1[dates_ok$YEAR == y & dates_ok$MONTH == m]
      date_C1 <- dates_pot$MDAY[dates_pot$COEFF == max(dates_pot$COEFF[dates_pot$YEAR == y & dates_pot$MONTH == m
                                                                       & dates_pot$MDAY <= dates_ok$C2[dates_ok$YEAR == y & dates_ok$MONTH == m] - 5]) &
                                  dates_pot$YEAR == y & dates_pot$MONTH == m & dates_pot$MDAY <= dates_ok$C2[dates_ok$YEAR == y & dates_ok$MONTH == m] - 5]
      a_rep <- ifelse(length(date_C1) == 0, a_rep, date_C1)
      
      dates_ok$C1[dates_ok$YEAR == y & dates_ok$MONTH == m] <- a_rep
    }
  }
  
  dates_ok$N_DATES <- dates_ok$N_DATES - 1
  for(y in year){
    for(m in month){
      a_rep <- dates_ok$C3[dates_ok$YEAR == y & dates_ok$MONTH == m]
      date_C3 <- dates_pot$MDAY[dates_pot$COEFF == max(dates_pot$COEFF[dates_pot$YEAR == y & dates_pot$MONTH == m
                                                                       & dates_pot$MDAY >= dates_ok$C2[dates_ok$YEAR == y & dates_ok$MONTH == m] + 5]) &
                                  dates_pot$YEAR == y & dates_pot$MONTH == m & dates_pot$MDAY >= dates_ok$C2[dates_ok$YEAR == y & dates_ok$MONTH == m] + 5]
      a_rep <- ifelse(length(date_C3) == 0, a_rep, date_C3)
      
      dates_ok$C3[dates_ok$YEAR == y & dates_ok$MONTH == m] <- a_rep
    }
  }
  
  dates_final <- pivot_longer(dates_ok, c('C1', 'C2', 'C3'))
  colnames(dates_final)[c(4,5)] <- c("COMPTAGE", "DATE")
  
  dates_final$DATE <- ifelse(!is.na(dates_final$DATE), paste0(dates_final$YEAR, "-", dates_final$MONTH, "-", dates_final$DATE), NA)
  dates_final$DATE <- as.Date(dates_final$DATE)
  
  dates_final <- subset(dates_final, !is.na(dates_final$DATE))
  
  
  liste_comptages <- read.csv("C:/Users/roman/Documents/ENSAT/3A/Stage/Script_R/library/liste_comptages.csv", sep="")
  liste_comptages$DATE <- as.Date(liste_comptages$DATE, format = '%d/%m/%Y')
  limicoles <- c('Charadriidae', 'Scolopacidae', 'Recurvirostridae', 'Burhinidae', 'Haematopodidae')
  
  d$MDAY <- day(d$DATE)
  
  d$COMPTAGE <- ifelse((((d$DATE %in% dates_final$DATE & d$FAMILY_NAME %in% limicoles) | d$MDAY == 15) & d$YEAR <= 2008) | d$DATE %in% liste_comptages$DATE,
                       'OUI', 'NON')
  
  d$COEFF <- correct_coeff(d$COEFF)
  
  ## Une fois le nettoyage termine, on exporte le nouveau tableau en csv
  catlog(c("\nEcriture du fichier CSV\n"),fileLog)
  write.csv2(d, paste0("data/", fileDataClean), row.names=FALSE,na="",quote=FALSE)
  
  d <<- d
  return(d)

  
#  require(rgeos)
#  require(sp)
#  
#  ##create spdf
#  coords=expand.grid(seq(150,151,0.1),seq(-31,-30,0.1))
#  spdf=data.frame("lng"=coords[,1],"lat"=coords[,2])
#  coordinates(spdf) = ~lng+lat
#  coordinates(d) = ~ COORD_LON + COORD_LAT
#  proj4string(d)<- CRS("+init=epsg:2154")
#  plot(d)
#  
#  ##get difference
#  out = gDifference(d,habitats)
#  points(out,col="red",pch=16)
  

}


## Analyses en deux temps : avec toute la base de donnÃ©es, et une analyse de la fluctuation des effectifs Ã  partir des comptages
 # rÃ©alisÃ©s depuis ??? (dont les protocoles sont stables et donc les analyses sont plus robustes)
## Se renseigner sur les protocoles STOC_SITES et WATERBIRD

## Dans un deuxiÃ¨me temps, s'intÃ©resser aux dÃ©tails (en vol, migration)

## Estimation code a explorer, pour l'instant sera probablement simplement ignore et on considerera que > ou ~ vaut =
## Extraction des coefficients de marÃ©e et de l'Ã©tat de la marÃ©e

## SÃ©paration des donnÃ©es en classes de saisons selon les mois (saison froide 10-03 et saison chaude 04-09 par ex
##                                                              ou hivernage 11-02, migration 03, repro 04-08 et migration 09-10 par ex)

## Idees d'indicateurs a produire :
## Nombre d'espÃ¨ces par unitÃ© de comptage

