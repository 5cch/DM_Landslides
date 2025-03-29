# - - - Libraries - - - #

library(arules) # for association rules
library(pheatmap) # for heatmaps
library(ggplot2) # for plots
library(sf) # for savoy area map
library(dplyr) # to easily apply filters on data
library(rpart) # for decision trees
library(rpart.plot) # for decision trees plots
library(caret) # for KNN



# - - - Functions - - - #

# Adds the mean of RR over all registered years for each post in sav_data and a column storing whether the RR is above the corresponding mean_RR or not
add_mean_RR <- function(sav_data, meteo_data){
  mean_RR <- meteo_data %>% group_by(NUM_POSTE) %>% summarise(mean_RR = mean(RR, na.rm=TRUE))
  sav_data <- sav_data %>% left_join(mean_RR, by="NUM_POSTE")
  
  for( i in 1:nrow(sav_data) ){
    if( sav_data$RR[i] > sav_data$mean_RR[i] ){
      sav_data$above_mean_RR[i] = 1
    }
    else{
      sav_data$above_mean_RR[i] = 0
    }
  }
  
  return(sav_data)
}


# Returns the month number associated to dateDebut
convert_to_month <- function(dateDebut){
  month <- substr(dateDebut, 5, 6)
  
  if( is.na(month) ){
    return(NA)
  }
  else{
    return(as.numeric(month))
  }
}


# Returns the year associated to dateDebut
convert_to_year <- function(dateDebut){
  year <- substr(dateDebut, 1, 4)
  
  if( is.na(year) ){
    return(NA)
  }
  else{
    return(as.numeric(year))
  }
}


# Returns the dataset without the unused columns
clear_dataset <- function(sav_data){
  sav_data$QRR <- NULL
  sav_data$NBRR <- NULL
  sav_data$QRRAB <- NULL
  sav_data$RRABDAT <- NULL
  sav_data$NBPMERM <- NULL
  sav_data$NBTX <- NULL
  sav_data$NBTN <- NULL
  sav_data$NBTAMPLI <- NULL
  sav_data$NBTM <- NULL
  sav_data$NBTMM <- NULL
  sav_data$NBUN <- NULL
  sav_data$NBUX <- NULL
  sav_data$NBUM <- NULL
  sav_data$NBTSVM <- NULL
  sav_data$NBFXI <- NULL
  sav_data$NBFXY <- NULL
  sav_data$NBFFM <- NULL
  sav_data$NBINST <- NULL
  sav_data$NBGLOT <- NULL
  sav_data$NBDIFT <- NULL
  sav_data$NBDIRT <- NULL
  sav_data$NBHNEIGEF <- NULL
  
  sav_data$NOM_USUEL <- NULL
  sav_data$xsaisi <- NULL
  sav_data$ysaisi <- NULL
  sav_data$idMvt <- NULL
  sav_data$num_insee <- NULL
  sav_data$libellePrec <- NULL
  sav_data$prec_xy <- NULL
  sav_data$conf <- NULL
  sav_data$lieu_dit <- NULL
  
  sav_data$LAT <- NULL
  sav_data$LON <- NULL
  sav_data$commune <- NULL
  sav_data$epsg <- NULL
  sav_data$PrecDate <- NULL
  
  sav_data$X <- NULL
  
  return(sav_data)
}


# Returns a new data frame with all the columns scaled
scale_dataset <- function(sav_data){
  sav_data_scaled <- sav_data
  
  sav_data_scaled$typeMvt <- scale(sav_data$typeMvt)
  sav_data_scaled$longitudeDoublePrec <- scale(sav_data$longitudeDoublePrec)
  sav_data_scaled$latitudeDoublePrec <- scale(sav_data$latitudeDoublePrec)
  sav_data_scaled$ALTI <- scale(sav_data$ALTI)
  sav_data_scaled$RR <- scale(sav_data$RR)
  sav_data_scaled$RRAB <- scale(sav_data$RRAB)
  sav_data_scaled$NBJRR1 <- scale(sav_data$NBJRR1)
  sav_data_scaled$NBJRR5 <- scale(sav_data$NBJRR5)
  sav_data_scaled$NBJRR10 <- scale(sav_data$NBJRR10)
  sav_data_scaled$NBJRR30 <- scale(sav_data$NBJRR30)
  sav_data_scaled$NBJRR50 <- scale(sav_data$NBJRR50)
  sav_data_scaled$NBJRR100 <- scale(sav_data$NBJRR100)
  sav_data_scaled$month <- scale(sav_data$month)
  sav_data_scaled$year <- scale(sav_data$year)
  sav_data_scaled$mean_RR <- scale(sav_data$mean_RR)
  sav_data_scaled$above_mean_RR <- scale(sav_data$above_mean_RR)
  
  return(sav_data_scaled)
}


# Returns a new data frame with all the columns normalized according to the train min and max values, then applied to the test
norm_dataset <- function(train, test){
  
  min_type = min(train$typeMvt)
  max_type = max(train$typeMvt)
  
  min_long = min(train$longitudeDoublePrec)
  max_long = max(train$longitudeDoublePrec)
  
  min_lat = min(train$latitudeDoublePrec)
  max_lat = max(train$latitudeDoublePrec)
  
  min_rr = min(train$RR)
  max_rr = max(train$RR)
  
  min_rrab = min(train$RRAB)
  max_rrab = max(train$RRAB)
  
  min_1 = min(train$NBJRR1)
  max_1 = max(train$NBJRR1)
  
  min_5 = min(train$NBJRR5)
  max_5 = max(train$NBJRR5)
  
  min_10 = min(train$NBJRR10)
  max_10 = max(train$NBJRR10)
  
  min_30 = min(train$NBJRR30)
  max_30 = max(train$NBJRR30)
  
  min_50 = min(train$NBJRR50)
  max_50 = max(train$NBJRR50)
  
  min_100 = min(train$NBJRR100)
  max_100 = max(train$NBJRR100)
  
  min_month = min(train$month)
  max_month = max(train$month)
  
  min_sea = min(train$season)
  max_sea = max(train$season)
  
  min_year = min(train$year)
  max_year = max(train$year)
  
  min_mean_rr = min(train$mean_RR)
  max_mean_rr = max(train$mean_RR)
  
  min_alti = min(train$ALTI)
  max_alti = max(train$ALTI)
  
  
  train$typeMvt <- (train$typeMvt - min_type) / (max_type - min_type)
  train$longitudeDoublePrec <- (train$longitudeDoublePrec - min_long) / (max_long - min_long)
  train$latitudeDoublePrec <- (train$latitudeDoublePrec - min_lat) / (max_lat - min_lat)
  train$RR <- (train$RR - min_rr) / (max_rr - min_rr)
  train$RRAB <- (train$RRAB - min_rrab) / (max_rr - min_rrab)
  train$month <- (train$month - min_month) / (max_month - min_month)
  train$season <- (train$season - min_sea) / (max_sea - min_sea)
  train$year <- (train$year - min_year) / (max_year - min_year)
  train$mean_RR <- (train$mean_RR - min_mean_rr) / (max_mean_rr - min_mean_rr)
  train$ALTI <- (train$ALTI - min_alti) / (max_alti - min_alti)
  train$NBJRR1 <- (train$NBJRR1 - min_1) / (max_1 - min_1)
  train$NBJRR5 <- (train$NBJRR5 - min_5) / (max_5 - min_5)
  train$NBJRR10 <- (train$NBJRR10 - min_10) / (max_10 - min_10)
  train$NBJRR30 <- (train$NBJRR30 - min_30) / (max_30 - min_30)
  train$NBJRR50 <- (train$NBJRR50 - min_50) / (max_50 - min_50)
  train$NBJRR100 <- (train$NBJRR100 - min_100) / (max_100 - min_100)
  
  
  test$typeMvt <- (test$typeMvt - min_type) / (max_type - min_type)
  test$longitudeDoublePrec <- (test$longitudeDoublePrec - min_long) / (max_long - min_long)
  test$latitudeDoublePrec <- (test$latitudeDoublePrec - min_lat) / (max_lat - min_lat)
  test$RR <- (test$RR - min_rr) / (max_rr - min_rr)
  test$RRAB <- (test$RRAB - min_rrab) / (max_rr - min_rrab)
  test$month <- (test$month - min_month) / (max_month - min_month)
  test$season <- (test$season - min_sea) / (max_sea - min_sea)
  test$year <- (test$year - min_year) / (max_year - min_year)
  test$mean_RR <- (test$mean_RR - min_mean_rr) / (max_mean_rr - min_mean_rr)
  test$ALTI <- (test$ALTI - min_alti) / (max_alti - min_alti)
  test$NBJRR1 <- (test$NBJRR1 - min_1) / (max_1 - min_1)
  test$NBJRR5 <- (test$NBJRR5 - min_5) / (max_5 - min_5)
  test$NBJRR10 <- (test$NBJRR10 - min_10) / (max_10 - min_10)
  test$NBJRR30 <- (test$NBJRR30 - min_30) / (max_30 - min_30)
  test$NBJRR50 <- (test$NBJRR50 - min_50) / (max_50 - min_50)
  test$NBJRR100 <- (test$NBJRR100 - min_100) / (max_100 - min_100)
  
  
  train_test <- rbind(train, test)
  return(train_test)
}


# Plots the landslide points colored by the associated rainfall on the savoie map
plot_map_rr <- function(sav_data){
  map_full <- map_data("france")
  # savoie coordinates map
  savoie_sf <- st_as_sf(sav_data, coords = c("longitudeDoublePrec", "latitudeDoublePrec"), crs=4326)
  
  plot <- ggplot(data=savoie_sf) +
    geom_polygon(data=map_full, aes(x=long, y=lat, group=group), fill="lightgray", color="black") +
    geom_sf(aes(color=RR)) +
    labs(x="Longitude", y="Latitude", color="RR") +
    scale_color_continuous(low="gray", high="darkblue")+
    coord_sf(xlim = c(5.5, 7.2), ylim = c(45, 46))

  print(plot)
}


# Plots the landslide points colored by the associated altitude on the savoie map
plot_map_alti <- function(sav_data){
  map_full <- map_data("france")
  # savoie coordinates map
  savoie_sf <- st_as_sf(sav_data, coords = c("longitudeDoublePrec", "latitudeDoublePrec"), crs=4326)
  
  plot <- ggplot(data = savoie_sf) +
    geom_polygon(data=map_full, aes(x=long, y=lat, group=group), fill="lightgray", color="black") +
    geom_sf(aes(color=ALTI)) +
    labs(x="Longitude", y="Latitude", color="ALTI") +
    scale_color_continuous(low="gray", high="darkblue")+
    coord_sf(xlim = c(5.5, 7.2), ylim = c(45, 46))
  
  print(plot)
}


# Plots the landslide points colored by the associated month on the savoie map
plot_map_month <- function(sav_data){
  map_full <- map_data("france")
  # savoie coordinates map
  savoie_sf <- st_as_sf(sav_data, coords = c("longitudeDoublePrec", "latitudeDoublePrec"), crs=4326)
  
  plot <- ggplot(data = savoie_sf) +
    geom_polygon(data=map_full, aes(x=long, y=lat, group=group), fill="lightgray", color="black") +
    geom_sf(aes(color=as.factor(month))) +
    labs(x="Longitude", y="Latitude", color="Month") +
    scale_color_manual(values = c("dodgerblue", "lightblue", "lightgreen", "green", "darkgreen", "yellow", "orange", "red", "darkviolet", "violet", "pink", "darkblue")) +
    coord_sf(xlim = c(5.5, 7.2), ylim = c(45, 46))
  
  print(plot)
}


# Plots the landslide points colored by their season on the savoie map
plot_map_season <- function(sav_data){
  map_full <- map_data("france")
  # savoie coordinates map
  savoie_sf <- st_as_sf(sav_data, coords = c("longitudeDoublePrec", "latitudeDoublePrec"), crs=4326)
  
  plot <- ggplot(data = savoie_sf) +
    geom_polygon(data=map_full, aes(x=long, y=lat, group=group), fill="lightgray", color="black") +
    geom_sf(aes(color=as.factor(season))) +
    labs(x="Longitude", y="Latitude", color="Season") +
    scale_color_manual(values = c("white", "green", "yellow","dodgerblue")) +
    coord_sf(xlim = c(5.5, 7.2), ylim = c(45, 46))
  
  print(plot)
}


# Plots the landslide points colored by the associated year on the savoie map
plot_map_year <- function(sav_data){
  map_full <- map_data("france")
  # savoie coordinates map
  savoie_sf <- st_as_sf(sav_data, coords = c("longitudeDoublePrec", "latitudeDoublePrec"), crs=4326)
  
  plot <- ggplot(data = savoie_sf) +
    geom_polygon(data=map_full, aes(x=long, y=lat, group=group), fill="lightgray", color="black") +
    geom_sf(aes(color=year)) +
    labs(x="Longitude", y="Latitude", color="Year") +
    scale_color_continuous(low="gray", high="darkblue")+
    coord_sf(xlim = c(5.5, 7.2), ylim = c(45, 46))
  
  print(plot)
}


# Plots the landslide points colored by the associated NBJRR100 on the savoie map
plot_map_NBJRR100 <- function(sav_data){
  map_full <- map_data("france")
  # savoie coordinates map
  savoie_sf <- st_as_sf(sav_data, coords = c("longitudeDoublePrec", "latitudeDoublePrec"), crs=4326)
  
  plot <- ggplot(data = savoie_sf) +
    geom_polygon(data=map_full, aes(x=long, y=lat, group=group), fill="lightgray", color="black") +
    geom_sf(aes(color=as.factor(NBJRR100))) +
    labs(x="Longitude", y="Latitude", color="NBRR100") +
    scale_color_manual(values = c("black", "cyan")) +
    coord_sf(xlim = c(5.5, 7.2), ylim = c(45, 46))
  
  print(plot)
}


# Plots the landslide points colored by by whether their associated RR is above their mean RR or not
plot_map_above_mean_RR <- function(sav_data){
  map_full <- map_data("france")
  # savoie coordinates map
  savoie_sf <- st_as_sf(sav_data, coords = c("longitudeDoublePrec", "latitudeDoublePrec"), crs=4326)
  
  plot <- ggplot(data = savoie_sf) +
    geom_polygon(data=map_full, aes(x=long, y=lat, group=group), fill="lightgray", color="black") +
    geom_sf(aes(color=as.factor(above_mean_RR))) +
    labs(x="Longitude", y="Latitude", color="RR above mean") +
    scale_color_manual(values = c("black", "cyan")) +
    coord_sf(xlim = c(5.5, 7.2), ylim = c(45, 46))
  
  print(plot)
}


# Plots stations that are then divided between the closest landslides
plot_map_stations <- function(sav_data){
  map_full <- map_data("france")
  unq_stations <- sav_data %>% distinct(NUM_POSTE, .keep_all=TRUE) # keeping the rows of stations with distinct NUM_POSTE
  
  # savoie coordinates map
  savoie_sf <- st_as_sf(unq_stations, coords = c("longitudeDoublePrec", "latitudeDoublePrec"), crs=4326)  # Ensure LON comes first
  
  plot <- ggplot(data = savoie_sf) +
    geom_polygon(data=map_full, aes(x=long, y=lat, group=group), fill="lightgray", color="black") +
    geom_sf() +
    labs(x="Longitude", y="Latitude") +
    coord_sf(xlim = c(5.5, 7.2), ylim = c(45, 46))
  
  print(plot)
}


# Returns the given dataset with the corresponding season added for each month
add_season <- function(data){
  data_season <- data
  
  for( i in 1:nrow(data) ){
    if( data$month[i] == 12 | data$month[i] < 3 ){
      data_season$season[i] <- 1 # Winter
    }
    else if( data$month[i] < 6 ){
      data_season$season[i] <- 2 # Spring
    }
    else if( data$month[i] < 8 ){
      data_season$season[i] <- 3 # Summer
    }
    else{
      data_season$season[i] <- 4 # Autumn
    }
  }
  
  return(data_season)
}


# returns the dataset with RR convertes to the ranges < 100 ; between 100 and 200 ; above 300
rr_to_range <- function(sav_data){
  
  for( i in 1:nrow(sav_data) ){
    
    if( sav_data$RR[i] < 100 ){
      sav_data$RR[i] = "Low"
    }
    else if( sav_data$RR[i] < 200 ){
      sav_data$RR[i] = "Medium"
    }
    else{
      sav_data$RR[i] = "High"
    }
  }
  
  return(sav_data)
}






# - - - Main - - - #

# for execution time
exec_start <- Sys.time()

# must change the paths according to the place of the repository on the machine
path_merged = "~/Cours/M1/S2/DM/DM_Landslides/datasets/savoie/landslide_meteo.csv"
path_meteo = "~/Cours/M1/S2/DM/DM_Landslides/datasets/savoie/MENSQ_1900-2023.csv"
sav_data <- read.csv(path_merged)
meteo_data <- read.csv(path_meteo, sep = ";")



# - - Data Cleaning - - #

# removing columns with too many unknown values (>200)
nb_na <- colSums(is.na(sav_data))
col_na <- names(nb_na[nb_na > 200])
for (name in col_na) {
  sav_data[[name]] <- NULL
}

# checking the number of different posts divided between the movements and visualizing them on the map
nb_posts = length(unique(sav_data$NUM_POSTE))
cat("Distinct stations :", nb_posts, "\n")
plot_map_stations(sav_data)

# removing columns with no use
sav_data <- clear_dataset(sav_data)

# standardizing date values (associating to the corresponding month and year)
nb_l = nrow(sav_data)
for(i in 1:nb_l){
  curr_date <- sav_data$AAAAMM[i]
  sav_data$month[i] <- convert_to_month(curr_date)
  sav_data$year[i] <- convert_to_year(curr_date)
}
sav_data$AAAAMM <- NULL # since stored separately in month and year now

sav_data <- sav_data[sav_data$year <= 2000, ] # we had only 2 landslides stored after 2000, we remove them

# we replace the NA values by the mean (41 missing) ; round bc those columns are not continuous, int
sav_data$RR[is.na(sav_data$RR)] <- round(mean(sav_data$RR, na.rm = TRUE))
sav_data$RRAB[is.na(sav_data$RRAB)] <- round(mean(sav_data$RRAB, na.rm = TRUE))
sav_data$NBJRR1[is.na(sav_data$NBJRR1)] <- round(mean(sav_data$NBJRR1, na.rm = TRUE))
sav_data$NBJRR5[is.na(sav_data$NBJRR5)] <- round(mean(sav_data$NBJRR5, na.rm = TRUE))
sav_data$NBJRR10[is.na(sav_data$NBJRR10)] <- round(mean(sav_data$NBJRR10, na.rm = TRUE))
sav_data$NBJRR30[is.na(sav_data$NBJRR30)] <- round(mean(sav_data$NBJRR30, na.rm = TRUE))
sav_data$NBJRR50[is.na(sav_data$NBJRR50)] <- round(mean(sav_data$NBJRR50, na.rm = TRUE))
sav_data$NBJRR100[is.na(sav_data$NBJRR100)] <- round(mean(sav_data$NBJRR100, na.rm = TRUE))

# removing the main outlier
sav_data <- sav_data[-376, ]


# adding the mean RR over all years for the post of each landslide
sav_data <- add_mean_RR(sav_data, meteo_data)

sav_data <- add_season(sav_data) # added to study seasons

# translating text to english
nb_l = nrow(sav_data)
for(i in 1:nb_l){
  if(sav_data$libelleType[i] == "Chute de blocs / Eboulement"){
    sav_data$libelleType[i] <- "Rockfall"
  }
  else if(sav_data$libelleType[i] == "Coulée"){
    sav_data$libelleType[i] <- "Flow"
  }
  else if(sav_data$libelleType[i] == "Effondrement / Affaissement"){
    sav_data$libelleType[i] <- "Collapse"
  }
  else if(sav_data$libelleType[i] == "Erosion de berges"){
    sav_data$libelleType[i] <- "Bank erosion"
  }
  else if(sav_data$libelleType[i] == "Glissement"){
    sav_data$libelleType[i] <- "Landslide"
  }
}

# features unused removed
sav_data$fiabiliteType <- NULL
sav_data$libelleFiabilite <- NULL
sav_data$libelleDate <- NULL
sav_data$NUM_POSTE <- NULL

sav_data_range <- rr_to_range(sav_data) # for Apriori later on, we groups the RRs under ranges

print(summary(sav_data))

# creating a fully numerical dataset
sav_data_num <- sav_data
sav_data_num$libelleType <- NULL # bc we already have the numerical correspondance in TypeMvt

# scaling for PCA
sav_data_scaled <- scale_dataset(sav_data_num)



# - - Data Visualization - - #

# correlations heatmap
corr <- cor(sav_data_num)
pheatmap(corr, display_numbers=TRUE)


# PCA
# PCA all columns
sav_data_scaled$fiabiliteType <- NULL
sav_data_scaled$season <- NULL

sav_data_scaled.pca <- prcomp(sav_data_scaled)
biplot(sav_data_scaled.pca)

# looking at only one of the NBJRR in the following PCA because no need to study the link between them
# NBJRR5 PCA
sav_data_scaled.pca_5 <- prcomp(sav_data_scaled[, c("typeMvt", "longitudeDoublePrec", "latitudeDoublePrec", "ALTI", "RR", "RRAB", "NBJRR5")])
biplot(sav_data_scaled.pca_5)

# NBJRR50 PCA
sav_data_scaled.pca_50 <- prcomp(sav_data_scaled[, c("typeMvt", "longitudeDoublePrec", "latitudeDoublePrec", "ALTI", "RR", "RRAB", "NBJRR50")])
biplot(sav_data_scaled.pca_50)

# NBJRR100 PCA
sav_data_scaled.pca_100 <- prcomp(sav_data_scaled[, c("typeMvt", "longitudeDoublePrec", "latitudeDoublePrec", "ALTI", "RR", "RRAB", "NBJRR100")])
biplot(sav_data_scaled.pca_100)


# maps
plot_map_rr(sav_data)
plot_map_alti(sav_data)
plot_map_month(sav_data)
plot_map_season(sav_data)
plot_map_year(sav_data)
plot_map_NBJRR100(sav_data)
plot_map_above_mean_RR(sav_data)


# bar plots
# movement type per year
plot <- ggplot(sav_data, aes(x=year, fill=libelleType)) +
  geom_bar() +
  labs(x="Year", y="Count", fill="Movement Type")
print(plot)

# movement type per month
plot <- ggplot(sav_data, aes(x=month, fill=libelleType)) +
  geom_bar() +
  labs(x="Month", y="Count", fill="Movement Type") +
  scale_x_discrete(limits=factor(1:12))
print(plot)

# movement type depending on above mean RR or not
plot <- ggplot(sav_data, aes(x=above_mean_RR, fill=libelleType)) +
  geom_bar() +
  labs(x="Above mean RR", y="Count", fill="Movement Type")
print(plot)

# above mean RR or not depending on month
plot <- ggplot(sav_data, aes(x=month, fill=as.factor(above_mean_RR))) +
  geom_bar() +
  labs(x="Month", y="Count", fill="Above mean RR") +
  scale_x_discrete(limits=factor(1:12))
print(plot)

# movement type per month for above mean RR
sav_data_above_RR <- sav_data %>% filter(above_mean_RR == 1)
plot <- ggplot(sav_data_above_RR, aes(x=month, fill=libelleType)) +
  geom_bar() +
  labs(x="Month", y="Count", fill="Movement type") +
  scale_x_discrete(limits=factor(1:12))
print(plot)



# - - Association Rules - - #

# monthly
print("Apriori monthly :")
# conversion to factor to be able to apply apriori
sav_data_factors <- sav_data_range[, c("month", "libelleType", "RR", "RRAB", "NBJRR100")]
sav_data_factors$month <- as.factor(sav_data_range$month)
sav_data_factors$libelleType <- as.factor(sav_data_range$libelleType)
sav_data_factors$RR <- as.factor(sav_data_range$RR)
sav_data_factors$RRAB <- as.factor(sav_data_range$RRAB)
sav_data_factors$NBJRR100 <- as.factor(sav_data_range$NBJRR100)

rules <- apriori(sav_data_factors, parameter=list(supp=0.01, conf=0.5))

print("Association rules :")
inspect(sort(subset(rules, subset=lift > 5), by="confidence"))



# seasonal
print("Apriori seasonal :")
# conversion to factor to be able to apply apriori
sav_data_factors <- sav_data_range[, c("season", "libelleType", "RR", "RRAB", "NBJRR100")]
sav_data_factors$season <- as.factor(sav_data_range$season)
sav_data_factors$libelleType <- as.factor(sav_data_range$libelleType)
sav_data_factors$RR <- as.factor(sav_data_range$RR)
sav_data_factors$RRAB <- as.factor(sav_data_range$RRAB)
sav_data_factors$NBJRR100 <- as.factor(sav_data_range$NBJRR100)

rules <- apriori(sav_data_factors, parameter=list(supp=0.01, conf=0.5))

print("Association rules :")
inspect(sort(subset(rules, subset=lift > 5), by="confidence"))



# setting a seed for the following random-based experiments
set.seed(42)


# - - Clustering - - #
pca_data <- as.data.frame(sav_data_scaled.pca_100$x) # we will plot cluster results on pca

# cluster with RR + typeMvt
k_means_rr_type <- kmeans(sav_data_scaled[, c("RR", "typeMvt")], centers=5)
pca_data$rr_type <- as.factor(k_means_rr_type$cluster)

plot <- ggplot(pca_data, aes(x=PC1, y=PC2, color=rr_type)) +
  geom_point() +
  labs(x="PC 1", y="PC 2", color="Cluster") +
  scale_color_manual(values = c("red", "black", "cyan", "green", "darkgray"))
print(plot)

# cluster with RRAB + typeMvt
k_means_rrab_type <- kmeans(sav_data_scaled[, c("RRAB", "typeMvt")], centers=3)
pca_data$rrab_type <- as.factor(k_means_rrab_type$cluster)

plot <- ggplot(pca_data, aes(x=PC1, y=PC2, color=rrab_type)) +
  geom_point() +
  labs(x="PC 1", y="PC 2", color="Cluster") +
  scale_color_manual(values = c("red", "black", "cyan", "green", "darkgray"))
print(plot)

# cluster with month + typeMvt
k_means_month_type <- kmeans(sav_data_scaled[, c("month", "typeMvt")], centers=4)
pca_data$month_type <- as.factor(k_means_month_type$cluster)

plot <- ggplot(pca_data, aes(x=PC1, y=PC2, color=month_type)) +
  geom_point() +
  labs(x="PC 1", y="PC 2", color="Cluster") +
  scale_color_manual(values = c("red", "black", "cyan", "green"))
print(plot)

# cluster full dataset
k_means_full <- kmeans(sav_data_scaled, centers=4)
pca_data$cluster_full <- as.factor(k_means_full$cluster)

plot <- ggplot(pca_data, aes(x=PC1, y=PC2, color=cluster_full)) +
  geom_point() +
  labs(x="PC 1", y="PC 2", color="Cluster") +
  scale_color_manual(values = c("red", "black", "cyan", "green"))
print(plot)



# - - Predictive Models - - #
set.seed(42)


# train and test sets
nb_l = nrow(sav_data)
split <- sample(1:nb_l, size=0.8*nb_l) # 80% in train 20% in test
train <- sav_data[split, ]
test <- sav_data[-split, ]

# number of data in each class, needed to compute the correct random guessing probability
cat("\n")
print("- - - Counts of data for each class - - -")
print("Type")
cat(table(test$typeMvt), "\n")
print("Month")
cat(table(test$month), "\n")
print("Season")
cat(table(test$season), "\n")
cat("\n")


# Decision tree

print("- - - Decision Trees - - -")

# movement type prediction based on latitudeDoublePrec + longitudeDoublePrec + RR + month
print("Type pred")
exec_start_tmp <- Sys.time() # for the model computation time
tree <- rpart(libelleType ~ latitudeDoublePrec + longitudeDoublePrec + RR + month, data=train, method="class")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp

# test accuracy check
type_pred <- predict(tree, test, type="class")
acc <- sum(type_pred == test$libelleType) / length(test$libelleType)
cat("acc =", acc, "; exec =", t, "s\n")

rpart.plot(tree)


# landslide month prediction based on typeMvt + RR
print("Month pred")
exec_start_tmp <- Sys.time() # for the model computation time
tree <- rpart(month ~ typeMvt + RR, data=train, method="class")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp

# test accuracy check
month_pred <- predict(tree, test, type="class")
acc <- sum(month_pred == test$month) / length(test$month)
cat("acc =", acc, "; exec =", t, "s\n")

rpart.plot(tree)


# landslide season prediction based on latitudeDoublePrec + longitudeDoublePrec + typeMvt + RR
print("Season pred")
exec_start_tmp <- Sys.time() # for the model computation time
tree <- rpart(season ~ latitudeDoublePrec + longitudeDoublePrec + typeMvt + RR, data=train, method="class")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp

# test accuracy check
season_pred <- predict(tree, test, type="class")
acc <- sum(season_pred == test$season) / length(test$season)
cat("acc =", acc, "; exec =", t, "s\n")

rpart.plot(tree)



# KNN

cat("\n")
print("- - - KNNs - - -")

# train and test sets
sav_data_norm <- norm_dataset(train, test) # normalizing  according to the train set values then applying on the test set

sav_data_norm$typeMvt <- factor(sav_data_norm$typeMvt)
sav_data_norm$month <- factor(sav_data_norm$month)
sav_data_norm$season <- factor(sav_data_norm$season)

# train and test sets
train <- sav_data_norm[split, ]
test <- sav_data_norm[-split, ]


# movement type prediction based on longitudeDoublePrec + latitudeDoublePrec + RR + month
print("Type pred")

# tuning k
exec_start_tmp <- Sys.time() # for the model computation time
knn <- train(typeMvt ~ longitudeDoublePrec + latitudeDoublePrec + RR + month, data=train, method="knn")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp
cat("best k =", knn$bestTune$k, "\n")

# test accuracy check
type_pred <- predict(knn, test)
acc <- sum(type_pred == test$typeMvt) / length(test$typeMvt)
cat("acc =", acc, "; exec =", t, "s\n")


# month prediction based on typeMvt + RR
print("Month pred")

# tuning k
exec_start_tmp <- Sys.time() # for the model computation time
knn <- train(month ~ typeMvt + RR, data=train, method="knn")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp
cat("best k =", knn$bestTune$k, "\n")

# test accuracy check
month_pred <- predict(knn, test)
acc <- sum(month_pred == test$month) / length(test$month)
cat("acc =", acc, "; exec =", t, "s\n")


# season prediction based on latitudeDoublePrec + longitudeDoublePrec + typeMvt + RR
print("Season pred")

# tuning k
exec_start_tmp <- Sys.time() # for the model computation time
knn <- train(season ~ latitudeDoublePrec + longitudeDoublePrec + typeMvt + RR, data=train, method="knn")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp
cat("best k =", knn$bestTune$k, "\n")

# test accuracy check
season_pred <- predict(knn, test)
acc <- sum(season_pred == test$season) / length(test$season)
cat("acc =", acc, "; exec =", t, "s\n")



# for execution time
exec_end <- Sys.time()
t <- exec_end - exec_start
cat("Code executed in", t, "s\n")
