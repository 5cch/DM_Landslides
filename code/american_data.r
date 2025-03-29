# - - - Libraries - - - #

library(arules) # for association rules
library(pheatmap) # for heatmaps
library(ggplot2) # for plots
library(dplyr) # to easily apply filters on data
library(rpart) # for decision trees
library(rpart.plot) # for decision trees plots
library(caret) # for KNN



# - - - Functions - - - #

# Returns the month number associated to curr_date
convert_to_month <- function(curr_date){
  month = strsplit(curr_date, "/")[[1]][1]

  if( is.na(month) ){
    return(NA)
  }
  else{
    return(as.numeric(month))
  }
}


# Returns the year associated to curr_date
convert_to_year <- function(curr_date){
  year = strsplit(curr_date, "/")[[1]][3]
  
  year = as.numeric(year)
  
  return(as.numeric(2000+year))
}


# Returns a new data frame with all the chr columns converted to num
convert_to_numerical <- function(am_data){
  am_data_num <- am_data
  
  am_data_num$continent_code <- as.numeric(as.factor(am_data$continent_code))
  am_data_num$country_code <- as.numeric(as.factor(am_data$country_code))
  am_data_num$landslide_type <- as.numeric(as.factor(am_data$landslide_type))
  am_data_num$landslide_size <- as.numeric(as.factor(am_data$landslide_size))
  am_data_num$trigger <- as.numeric(as.factor(am_data$trigger))
  am_data_num$province <- as.numeric(as.factor(am_data$province))
  
  return(am_data_num)
}


# Returns a new data frame with all the columns normalized according to the train min and max values, then applied to the test
# Note : lowers every score of about 0.01 so not used in the end
norm_dataset <- function(train, test){
  
  min_pop = min(train$population)
  max_pop = max(train$population)
  
  min_inj = min(train$injuries)
  max_inj = max(train$injuries)
  
  min_fat = min(train$fatalities)
  max_fat = max(train$fatalities)
  
  min_month = min(train$month)
  max_month = max(train$month)
  
  min_sea = min(train$season)
  max_sea = max(train$season)
  
  min_year = min(train$year)
  max_year = max(train$year)
  
  min_dist = min(train$distance)
  max_dist = max(train$distance)
  
  min_long = min(train$longitude)
  max_long = max(train$longitude)
  
  min_lat = min(train$latitude)
  max_lat = max(train$latitude)
  
  min_type = min(train$landslide_type)
  max_type = max(train$landslide_type)
  
  min_prov = min(train$province)
  max_prov = max(train$province)
  
  min_cont = min(train$continent_code)
  max_cont = max(train$continent_code)
  
  min_trig = min(train$trigger)
  max_trig = max(train$trigger)
  
  min_size = min(train$landslide_size)
  max_size = max(train$landslide_size)
  
  min_coun = min(train$country_code)
  max_coun = max(train$country_code)
  
  
  train$population <- (train$population - min_pop) / (max_pop - min_pop)
  train$injuries <- (train$injuries - min_inj) / (max_inj - min_inj)
  train$fatalities <- (train$fatalities - min_fat) / (max_fat - min_fat)
  
  train$month <- (train$month - min_month) / (max_month - min_month)
  train$season <- (train$season - min_sea) / (max_sea - min_sea)
  train$year <- (train$year - min_year) / (max_year - min_year)
  
  train$distance <- (train$distance - min_dist) / (max_dist - min_dist)
  train$longitude <- (train$longitude - min_long) / (max_long - min_long)
  train$latitude <- (train$latitude - min_lat) / (max_lat - min_lat)
  
  train$landslide_type <- (train$landslide_type - min_type) / (max_type - min_type)
  train$province <- (train$province - min_prov) / (max_prov - min_prov)
  train$continent_code <- (train$continent_code - min_cont) / (max_cont - min_cont)
  train$trigger <- (train$trigger - min_trig) / (max_trig - min_trig)
  train$landslide_size <- (train$landslide_size - min_size) / (max_size - min_size)
  train$country_code <- (train$country_code - min_coun) / (max_coun - min_coun)

  
  test$population <- (test$population - min_pop) / (max_pop - min_pop)
  test$injuries <- (test$injuries - min_inj) / (max_inj - min_inj)
  test$fatalities <- (test$fatalities - min_fat) / (max_fat - min_fat)
  
  test$month <- (test$month - min_month) / (max_month - min_month)
  test$season <- (test$season - min_sea) / (max_sea - min_sea)
  test$year <- (test$year - min_year) / (max_year - min_year)
  
  test$distance <- (test$distance - min_dist) / (max_dist - min_dist)
  test$longitude <- (test$longitude - min_long) / (max_long - min_long)
  test$latitude <- (test$latitude - min_lat) / (max_lat - min_lat)
  
  test$landslide_type <- (test$landslide_type - min_type) / (max_type - min_type)
  test$province <- (test$province - min_prov) / (max_prov - min_prov)
  test$continent_code <- (test$continent_code - min_cont) / (max_cont - min_cont)
  test$trigger <- (test$trigger - min_trig) / (max_trig - min_trig)
  test$landslide_size <- (test$landslide_size - min_size) / (max_size - min_size)
  test$country_code <- (test$country_code - min_coun) / (max_coun - min_coun)

  
  train_test <- rbind(train, test)
  return(train_test)
}


# Returns a new data frame with all the columns scaled
scale_dataset <- function(am_data){
  am_data_scaled <- am_data
  
  am_data_scaled$population <- scale(am_data$population)
  am_data_scaled$injuries <- scale(am_data$injuries)
  am_data_scaled$fatalities <- scale(am_data$fatalities)
  
  am_data_scaled$month <- scale(am_data$month)
  am_data_scaled$season <- scale(am_data$season)
  am_data_scaled$year <- scale(am_data$year)
  
  am_data_scaled$distance <- scale(am_data$distance)
  am_data_scaled$longitude <- scale(am_data$longitude)
  am_data_scaled$latitude <- scale(am_data$latitude)
  
  am_data_scaled$landslide_type <- scale(am_data$landslide_type)
  am_data_scaled$province <- scale(am_data$province)
  am_data_scaled$continent_code <- scale(am_data$continent_code)
  am_data_scaled$trigger <- scale(am_data$trigger)
  am_data_scaled$landslide_size <- scale(am_data$landslide_size)
  
  return(am_data_scaled)
}


# Plots the landslide points colored by their trigger on the american map
plot_map_trigger <- function(am_data){
  map_full <- map_data("world")
  area <- map_full %>% filter(long > -130, long < -35, lat > -12, lat < 50) # limiting map to our data's area
  
  plot <- ggplot() +
    geom_polygon(data=area, aes(x=long, y=lat, group=group), fill="gray", color="black") +
    geom_point(data=am_data, aes(x=longitude, y=latitude, color=trigger)) +
    coord_fixed(1.3) +
    labs(x="Longitude", y="Latitude", color="Trigger") +
    scale_color_manual(values = c("red", "darkred", "orange", "yellow", "darkblue", "green", "darkgreen", "dodgerblue", "white", "lightblue", "purple"))
  
  print(plot)
}


# Plots the landslide points colored by their type on the american map
plot_map_type <- function(am_data){
  map_full <- map_data("world")
  area <- map_full %>% filter(long > -130, long < -35, lat > -12, lat < 50) # limiting map to our data's area
  
  plot <- ggplot() +
    geom_polygon(data=area, aes(x=long, y=lat, group=group), fill="gray", color="black") +
    geom_point(data=am_data, aes(x=longitude, y=latitude, color=landslide_type)) +
    coord_fixed(1.3) +
    labs(x="Longitude", y="Latitude", color="Type") +
    scale_color_manual(values = c("red", "darkblue", "orange", "yellow", "dodgerblue", "chocolate", "green", "darkgreen", "white", "purple"))
  
  print(plot)
}


# Plots the landslide points colored by their month on the american map
plot_map_month <- function(am_data){
  map_full <- map_data("world")
  area <- map_full %>% filter(long > -130, long < -35, lat > -12, lat < 50) # limiting map to our data's area
  
  plot <- ggplot() +
    geom_polygon(data=area, aes(x=long, y=lat, group=group), fill="gray", color="black") +
    geom_point(data=am_data, aes(x=longitude, y=latitude, color=as.factor(month))) +
    coord_fixed(1.3) +
    labs(x="Longitude", y="Latitude", color="Month") +
    scale_color_manual(values = c("dodgerblue", "lightblue", "lightgreen", "green", "darkgreen", "yellow", "orange", "red", "darkviolet", "violet", "pink", "darkblue"))
    # seasons scale :
    # scale_color_manual(values = c("dodgerblue", "dodgerblue", "green", "green", "green", "white", "white", "white", "red", "red", "red", "dodgerblue"))
  
  print(plot)
}


# Plots the landslide points colored by their season on the american map
plot_map_season <- function(am_data){
  map_full <- map_data("world")
  area <- map_full %>% filter(long > -130, long < -35, lat > -12, lat < 50) # limiting map to our data's area
  
  plot <- ggplot() +
    geom_polygon(data=area, aes(x=long, y=lat, group=group), fill="gray", color="black") +
    geom_point(data=am_data, aes(x=longitude, y=latitude, color=as.factor(season))) +
    coord_fixed(1.3) +
    labs(x="Longitude", y="Latitude", color="Season") +
    scale_color_manual(values = c("white", "green", "yellow","dodgerblue"))
  
  print(plot)
}


# Plots the landslide points colored by their size on the american map
plot_map_size <- function(am_data){
  map_full <- map_data("world")
  area <- map_full %>% filter(long > -130, long < -35, lat > -12, lat < 50) # limiting map to our data's area
  
  plot <- ggplot() +
    geom_polygon(data=area, aes(x=long, y=lat, group=group), fill="gray", color="black") +
    geom_point(data=am_data, aes(x=longitude, y=latitude, color=landslide_size)) +
    coord_fixed(1.3) +
    labs(x="Longitude", y="Latitude", color="Size") +
    scale_color_manual(values = c("red", "orange", "yellow", "green"))

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






# - - - Main - - - #

# for execution time
exec_start <- Sys.time()

# must change the paths according to the place of the repository on the machine
path_data = "~/Cours/M1/S2/DM/DM_Landslides/datasets/america/landslides.csv"
am_data <- read.csv(path_data)



# - - Data Cleaning - - #

# dropping duplicate/unuseful columns
am_data$country_name <- NULL # will only use country code, both columns contain the same information
am_data$geolocation <- NULL # contains the same information as latitude + longitude columns
am_data$hazard_type <- NULL # has only one value ("landslide")
am_data$storm_name <- NULL # we will not analyze the storms in particular, and triggered by storm or not is already indicated in "trigger"
am_data$source_link <- NULL # we will not look for anomalies linked to the reliability of the source
am_data$source_name <- NULL # we will not look for anomalies linked to the reliability of the source
am_data$id <- NULL # the id does not provide informations on the landslide
am_data$city.town <- NULL # we make no use of it

colnames(am_data)[5] <- "province" # renaming with no dot

# "NA" used for North America is mistaken for a missing value so change of value
am_data$continent_code[is.na(am_data$continent_code)] <- "NorthAmerica"

# we place NA as the value for all the values that have been left empty
for(i in 1:ncol(am_data)){
  am_data[[i]][am_data[[i]] == ""] <- NA
}

print("NA values :")
print(colSums(is.na(am_data)))

am_data$time <- NULL # too many missing values
am_data$location_description <- NULL # too many missing values

# standardizing date values (associating to the corresponding month and year)
nb_l = nrow(am_data)
for(i in 1:nb_l){
  curr_date <- am_data$date[i]
  am_data$month[i] <- convert_to_month(curr_date)
  am_data$year[i] <- convert_to_year(curr_date)
}

# standardizing the landslide types, triggers and sizes' names
for(i in 1:nb_l){
  if( is.na(am_data$landslide_type[i]) ){
    am_data$landslide_type[i] <- NA
  }
  else if(am_data$landslide_type[i] == "landslide"){
    am_data$landslide_type[i] <- "Landslide"
  }
  else if(am_data$landslide_type[i] == "mudslide"){
    am_data$landslide_type[i] <- "Mudslide"
  }

  if( is.na(am_data$trigger[i]) | am_data$trigger[i] == "unknown" | am_data$trigger[i] == "Unknown"){
    am_data$trigger[i] <- NA
  }
  else if( am_data$trigger[i] == "downpour" | am_data$trigger[i] == "Downpour" | am_data$trigger[i] == "Continuous rain"){
    am_data$trigger[i] <- "Rain"
  }

  if( is.na(am_data$landslide_size[i]) ){
    am_data$landslide_size[i] <- NA
  }
  else if(am_data$landslide_size[i] == "large"){
    am_data$landslide_size[i] <- "Large"
  }
  else if(am_data$landslide_size[i] == "medium"){
    am_data$landslide_size[i] <- "Medium"
  }
  else if(am_data$landslide_size[i] == "small"){
    am_data$landslide_size[i] <- "Small"
  }
  else if(am_data$landslide_size[i] == "Very_large"){
    am_data$landslide_size[i] <- "Huge"
  }
}

# handling missing values
# we drop the landslides that had no coordinates, distance, date or type/size, trigger
am_data <- am_data[!is.na(am_data$latitude), ]
am_data <- am_data[!is.na(am_data$distance), ]
am_data <- am_data[!is.na(am_data$date), ]
am_data <- am_data[!is.na(am_data$landslide_type), ]
am_data <- am_data[!is.na(am_data$landslide_size), ]
am_data <- am_data[!is.na(am_data$trigger), ]
am_data <- am_data[!is.na(am_data$province), ]

# when the injury data is missing, we consider that it is because no injuries were reported, the same goes for fatalities
am_data$injuries[is.na(am_data$injuries)] <- 0
am_data$fatalities[is.na(am_data$fatalities)] <- 0

am_data$date <- NULL # we do not use day
am_data<- add_season(am_data) # added to study seasons

# # Some outliers, in the end not removed because it did not increase performances
# am_data <- am_data[-396, ] # outlier : landslide due to construction and huge population (if not removed, point 401 on PCA)
# am_data <- am_data[-174, ] # again huge population
# am_data <- am_data[-1388, ] # lots of injuries and fatalities

# converting to numerical data for texts
am_data_num <- convert_to_numerical(am_data)

# scaling for PCA
am_data_scaled <- scale_dataset(am_data_num)

print("Final dataset symmary :")
print(summary(am_data))


# - - Association Rules - - #

# monthly
print("Apriori monthly")
# conversion to factor to be able to apply apriori
am_data_factors <- am_data[, c("month", "trigger", "landslide_type", "landslide_size", "province")]
am_data_factors$month <- as.factor(am_data$month)
am_data_factors$trigger <- as.factor(am_data$trigger)
am_data_factors$landslide_type <- as.factor(am_data$landslide_type)
am_data_factors$landslide_size <- as.factor(am_data$landslide_size)
am_data_factors$province <- as.factor(am_data$province)

rules <- apriori(am_data_factors, parameter=list(supp=0.01, conf=0.5))

print("Association rules :")
inspect(sort(subset(rules, subset=lift > 5), by="confidence"))


# seasonal
print("Apriori seasonal")
# conversion to factor to be able to apply apriori
am_data_factors <- am_data[, c("season", "trigger", "landslide_type", "landslide_size", "province")]
am_data_factors$season <- as.factor(am_data$season)
am_data_factors$trigger <- as.factor(am_data$trigger)
am_data_factors$landslide_type <- as.factor(am_data$landslide_type)
am_data_factors$landslide_size <- as.factor(am_data$landslide_size)
am_data_factors$province <- as.factor(am_data$province)

rules <- apriori(am_data_factors, parameter=list(supp=0.01, conf=0.5))

print("Association rules :")
inspect(sort(subset(rules, subset=lift > 5), by="confidence"))



# - - Data Visualization - - #

# correlations heatmap
corr <- cor(am_data_num)
pheatmap(corr, display_numbers=TRUE)

# PCA
am_data_scaled$country_code <- NULL
am_data_scaled$season <- NULL

am_data_pca <- prcomp(am_data_scaled)
biplot(am_data_pca)


# maps
plot_map_trigger(am_data)
plot_map_type(am_data)
plot_map_month(am_data)
plot_map_size(am_data)
plot_map_season(am_data)

# bar plots
# landslide type per state in the US
am_data_us <- am_data %>% filter(country_code == "US") # US data only
plot <- ggplot(am_data_us, aes(x=province, fill=landslide_type)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle = 90)) +
  labs(x="State", y="Count", fill="Landslide Type")
print(plot)

# landslide type per year
plot <- ggplot(am_data, aes(x=year, fill=landslide_type)) +
  geom_bar() +
  labs(x="Year", y="Count", fill="Landslide Type")
print(plot)

# landslide type per month
plot <- ggplot(am_data, aes(x=month, fill=landslide_type)) +
  geom_bar() +
  labs(x="Month", y="Count", fill="Landslide Type") +
  scale_x_discrete(limits=factor(1:12))
print(plot)

# landslide size per year
plot <- ggplot(am_data, aes(x=year, fill=landslide_size)) +
  geom_bar() +
  labs(x="Year", y="Count", fill="Landslide Size")
print(plot)

# landslide size per season
plot <- ggplot(am_data, aes(x=season, fill=landslide_size)) +
  geom_bar() +
  labs(x="Season", y="Count", fill="Landslide Size")
print(plot)



# setting a seed for the following random-based experiments
set.seed(42)

# - - Clustering - - #

# for clusters on PCA
am_data_pca <- as.data.frame(am_data_pca$x)

# cluster with all the scalsed data
k_means_full <- kmeans(am_data_scaled, centers=4)
am_data_pca$cluster_full <- factor(k_means_full$cluster)
plot <- ggplot(am_data_pca, aes(x=PC1, y=PC2, color=cluster_full)) +
  geom_point() +
  labs(x="PC 1", y="PC 2", color="Cluster")
print(plot)

# cluster with population + fatalities
k_means_pop <- kmeans(am_data_scaled[, c("population", "fatalities")], centers=2)
plot <- ggplot(am_data_scaled, aes(x=population, y=fatalities, color=factor(k_means_pop$cluster))) +
  geom_point() +
  labs(color="Cluster", x="Population", y="Fatalities")
print(plot)

am_data_pca$cluster_pop <- factor(k_means_pop$cluster)
plot <- ggplot(am_data_pca, aes(x=PC1, y=PC2, color=cluster_pop)) +
  geom_point() +
  labs(x="PC 1", y="PC 2", color="Cluster")
print(plot)

# cluster with distance + landslide_type + month
k_means_month <- kmeans(am_data_scaled[, c("distance", "landslide_type", "month")], centers=4)
plot <- ggplot(am_data_scaled, aes(x=landslide_type, y=distance, color=factor(k_means_month$cluster))) +
  geom_point() +
  labs(x="Landslide Type", y="Distance", color="Cluster")
print(plot)

am_data_pca$cluster_month <- factor(k_means_month$cluster)

plot <- ggplot(am_data_pca, aes(x=PC1, y=PC2, color=cluster_month)) +
  geom_point() +
  labs(x="PC 1", y="PC 2", color="Cluster")
print(plot)

# cluster with trigger + landslide_type
k_means_trigger <- kmeans(am_data_scaled[, c("trigger", "landslide_type")], centers=4)
plot <- ggplot(am_data_scaled, aes(x=landslide_type, y=trigger, color=factor(k_means_trigger$cluster))) +
  geom_point() +
  labs(color="Cluster", x="Landslide Type", y="Trigger")
print(plot)

am_data_pca$cluster_trigger <- factor(k_means_trigger$cluster)

plot <- ggplot(am_data_pca, aes(x=PC1, y=PC2, color=cluster_trigger)) +
  geom_point() +
  labs(x="PC 1", y="PC 2", color="Cluster")
print(plot)






# - - Predictive Models - - #
set.seed(42)


# train and test sets
nb_l = nrow(am_data_num)
split <- sample(1:nb_l, size=0.8*nb_l) # 80% in train 20% in test
train <- am_data_num[split, ]
test <- am_data_num[-split, ]

# number of data in each class, needed to compute the correct random guessing probability
cat("\n")
print("- - - Counts of data for each class - - -")
print("Size")
cat(table(test$landslide_size), "\n")
print("Month")
cat(table(test$month), "\n")
print("Season")
cat(table(test$season), "\n")
cat("\n")


# Decision tree

print("- - - Decision Trees - - -")

# landslide size prediction based on latitude + longitude + month
print("Size pred")
exec_start_tmp <- Sys.time() # for the model computation time
tree <- rpart(landslide_size ~ latitude + longitude + month, data=train, method="class")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp

# test accuracy check
size_pred <- predict(tree, test, type="class")
acc <- sum(size_pred == test$landslide_size) / length(test$landslide_size)
cat("acc =", acc, "; exec =", t, "s\n")

rpart.plot(tree)


# month prediction based on latitude + longitude + province
print("Month pred")
exec_start_tmp <- Sys.time() # for the model computation time
tree <- rpart(month ~ latitude + longitude + province, data=train, method="class")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp

# test accuracy check
month_pred <- predict(tree, test, type="class")
acc <- sum(month_pred == test$month) / length(test$month)
cat("acc =", acc, "; exec =", t, "s\n")

rpart.plot(tree, box.palette=0)


# season prediction based on latitude + longitude
print("Season pred")
exec_start_tmp <- Sys.time() # for the model computation time
tree <- rpart(season ~ latitude + longitude, data=train, method="class")
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
# to uncomment for US analysis (since 909 datas from US)
# am_data_num_us <- am_data_num %>% filter(country_code == 25) # US
# am_data_norm <- norm_dataset(am_data_num_us)
# am_data_norm$continent_code <- NULL
# am_data_norm$country_code <- NULL


# # normalization, lowers the size prediction score of about 0.05 while increasing season of also 0.05
am_data_norm <- norm_dataset(train, test) # to comment for US analysis
# train and test sets
train <- am_data_norm[split, ]
test <- am_data_norm[-split, ]


# distance prediction knn based on latitude + longitude
print("Distance pred")

# tuning k
exec_start_tmp <- Sys.time() # for the model computation time
knn <- train(distance ~ latitude + longitude, data=train, method="knn")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp
cat("best k =", knn$bestTune$k, "\n")

# test accuracy check
dist_pred <- predict(knn, test)
mae <- mean(abs(dist_pred - test$distance))
cat("MAE =", mae, "\n")
cat("std =", sd(test$distance), "\n")
cat("exec =", t, "s\n")


# landslide size prediction based on latitude + longitude + month
print("Size pred")

# factors for knn to perform classification
train$landslide_size <- factor(train$landslide_size)
test$landslide_size <- factor(test$landslide_size)

# tuning k
exec_start_tmp <- Sys.time() # for the model computation time
knn <- train(landslide_size ~ latitude + longitude + month, data=train, method="knn")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp
cat("best k =", knn$bestTune$k, "\n")

# test accuracy check
size_pred <- predict(knn, test)
acc <- sum(size_pred == test$landslide_size) / length(test$landslide_size)
cat("acc =", acc, "; exec =", t, "s\n")


# month prediction based on latitude + longitude + province
print("Month pred")

# factors for knn to perform classification
train$month <- factor(train$month)
test$month <- factor(test$month)

# tuning k
exec_start_tmp <- Sys.time() # for the model computation time
knn <- train(month ~ latitude + longitude + province, data=train, method="knn")
exec_end_tmp <- Sys.time() # for the model computation time
t <- exec_end_tmp - exec_start_tmp
cat("best k =", knn$bestTune$k, "\n")

# test accuracy check
month_pred <- predict(knn, test)
acc <- sum(month_pred == test$month) / length(test$month)
cat("acc =", acc, "; exec =", t, "s\n")


# season prediction based on latitude + longitude + province
print("Season pred")

# factors for knn to perform classification
train$season <- factor(train$season)
test$season <- factor(test$season)

# tuning k
exec_start_tmp <- Sys.time() # for the model computation time
knn <- train(season ~ latitude + longitude + province, data=train, method="knn")
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
