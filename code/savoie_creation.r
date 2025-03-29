# - - - Functions - - - #

# euclidean distance function
dist <- function(lat1, lon1, lat2, lon2) {
  distance <- sqrt((lat2 - lat1)^2 + (lon2 - lon1)^2)
  return(distance)
}



# - - - Main - - - #

path_meteo <- "~/Cours/M1/S2/DM/DM_Landslides/datasets/savoie/MENSQ_1900-2023.csv"
path_landslide <- "~/Cours/M1/S2/DM/DM_Landslides/datasets/savoie/mvt_dptList_73.csv"

meteo <- read.csv(path_meteo, sep = ";", stringsAsFactors=FALSE)
landslide <- read.csv(path_landslide, sep = ";", stringsAsFactors=FALSE)

# removing landslides with no date
landslide$dateDebut <- as.Date(landslide$dateDebut, format="%Y-%m-%d")
landslide <- landslide[!is.na(landslide$dateDebut), ]  

# converting dateDebut to AAAAMM for merging with meteo format
landslide$AAAAMM <- format(landslide$dateDebut, "%Y%m")
landslide$dateDebut <- NULL # we do not need dateDebut anymore since stored in AAAAMM
  
# merging
landslide_meteo <- data.frame()

for( i in 1:nrow(landslide) ){
  print(i)
  curr_date <- landslide$AAAAMM[i]
  curr_match <- meteo[meteo$AAAAMM == curr_date, ]
  
  # we merge with the meteo data that has same date and the minimal distance to the landslide (closest meteo station)
  nb_l = nrow(curr_match)
  
  if( nb_l > 0 ){
    best_match = curr_match[1, ]
    min_dist = dist(landslide$latitudeDoublePrec[i], landslide$longitudeDoublePrec[i], curr_match$LAT[1], curr_match$LON[1])
    
    # if more than 1 measure for the date, selecting min distance to the landslide
    if( nb_l > 1 ){
      for( j in 2:nrow(curr_match) ){
        curr_dist = dist(landslide$latitudeDoublePrec[i], landslide$longitudeDoublePrec[i], curr_match$LAT[j], curr_match$LON[j])
  
        if( curr_dist < min_dist ){
          best_match = curr_match[j, ]
          min_dist = curr_dist
        }
      }
    }
    
    new_l <- cbind(landslide[i, ], best_match) # adding meteo match to the landslide line
    landslide_meteo <- rbind(landslide_meteo, new_l)
  }
}

# removing 1 AAAAMM from the dataset because we keep the one from landslide and X because added at merging and will not use
landslide_meteo$AAAAMM <- NULL
landslide_meteo$X <- NULL

path_merged <- "~/Cours/M1/S2/DM/DM_Landslides/datasets/savoie/landslide_meteo.csv"
write.csv(landslide_meteo, path_merged)
