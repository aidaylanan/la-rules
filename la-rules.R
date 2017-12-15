library(rvest)
library(stringr)
library(readr)
library(ggplot2)
library(pls)
library(sp)
library(maps)
library(ggmap)
library(maptools)
library(geojsonio)
library(dplyr)
library(knitr)
library(kableExtra)
library(hexbin)
library(magick)
library(devtools)
devtools::install_github("tomwenseleers/export")
library(export)


#import the listings data set
listings <- read_csv("~/Downloads/Dataset/listings.csv")  


#import/scrape the LA times Regions classification
la_times <- read_html("http://maps.latimes.com/neighborhoods/neighborhood/list/")
neighbourhoods_full <- la_times %>% html_nodes("table") %>% html_table()
neighbourhoods <- neighbourhoods_full[[1]]
colnames(neighbourhoods) <- c("neighbourhood_cleansed", "Region")

listingsUpdated <- left_join(listings, neighbourhoods, by = "neighbourhood_cleansed")


listingsUpdated <- listingsUpdated %>% 
  select("room_type", "bathrooms", "beds",
         "security_deposit", "guests_included",
         "number_of_reviews", "cleaning_fee",
         "review_scores_rating", "price",
         "neighbourhood_cleansed", "latitude",
         "longitude", "Region")


# Using Regex to remove the $ sign  ---------------------------------------
pattern <- "\\$|,"
listingsUpdated$price <- as.numeric(str_replace_all(listingsUpdated$price, pattern,""))
listingsUpdated$cleaning_fee <- as.numeric(str_replace_all(listingsUpdated$cleaning_fee, pattern,""))
listingsUpdated$security_deposit <- as.numeric(str_replace_all(listingsUpdated$security_deposit, pattern,""))

summary(listingsUpdated)


# Potential Problems ------------------------------------------------------
##Outliers
subset(listings$minimum_nights, listings$minimum_nights == 365) 

subset(listings$maximum_nights, listings$maximum_nights > 9999) 




# Using Regex to extract rules for each listing ---------------------------

#General Rules
pattern <- "[Nn][Oo]\\s\\w*"
rules <- str_match_all(listings$house_rules, pattern)

houseRules <- c()      
for(i in 1:length(rules)){
  if(length(rules[[i]]) >= 1){
    for(j in 1:length(rules[[i]])){
      houseRules <- c(houseRules,rules[[i]][[j]]) 
    }
  }
}
as.data.frame(table(houseRules)) %>%
  group_by(houseRules) %>%
  arrange(desc(Freq)) %>%
  head(10)

no_something <- ifelse(lapply(str_match(listings$house_rules, "[Nn][Oo]\\s\\w*"),is.na), FALSE, TRUE)

#No smoking or no cigarettes store in no_smoke
pattern <- "[Nn][Oo]\\s[Ss][Mm][Oo][Kk]\\w*|[Nn][Oo]\\s[Cc][Ii]\\w*"   
no_smoke <- ifelse(lapply(str_match(listings$house_rules, pattern),is.na), FALSE, TRUE)

#No pets or animals
pattern <- "[Nn][Oo]\\s[Pp][Ee][Tt]\\w*|[Nn][Oo]\\s[Aa][Nn][Ii][Mm]\\w*" 
no_pet <- ifelse(lapply(str_match(listings$house_rules, pattern),is.na), FALSE, TRUE)

#No party or drinking/alcohol
patternPart1 <- "[Nn][Oo]\\s[Pp][Aa][Rr][Tt]\\w*|[Nn][Oo]\\s[Gg][Uu][Ee][Ss]\\w*|"
patternPart2 <- "[Nn][Oo]\\s[Oo][Vv][Ee][Rr]\\w*|[Nn][Oo]\\s[Vv][Ii][Ss]\\w*|"
patternPart3 <- "[Nn][Oo]\\s[Dd][Rr][Ii]\\w*|[Nn][Oo]\\s[Aa][Ll]\\w*"
no_party <- ifelse(lapply(
  str_match(listings$house_rules, paste(patternPart1, patternPart2, patternPart3, sep = "")),is.na),
  FALSE, TRUE)

#No drugs no marijuana stored in no_drug and illegal
pattern <- "[Nn][Oo]\\s[Dd][Rr][Uu][Gg]\\w|[Nn][Oo]\\s[Mm][Aa][Rr][Ii][Jj]\\w*"
no_drugs <- ifelse(lapply(str_match(listings$house_rules, pattern),is.na), FALSE, TRUE)

#No shoes
pattern <- "[Nn][Oo]\\s[Ss][[Hh]][[Oo]][[Ee]]\\w*"
no_shoes <- ifelse(lapply(str_match(listings$house_rules, pattern),is.na), FALSE, TRUE)


#No loud or no noise stored in no_loud
pattern <- "[Nn][Oo]\\s[Ll][Oo]\\w*|[Nn][Oo]\\s[Nn][Oo][Ii]\\w*"
no_loud <- ifelse(lapply(str_match(listings$house_rules, pattern),is.na), FALSE, TRUE)


# Define new variable strictness ------------------------------------------
strictness <- as.integer(no_smoke) + as.integer(no_pet) + as.integer(no_party) +
  as.integer(no_drugs) + as.integer(no_shoes) + as.integer(no_loud)

#Distribution of strictness
table(strictness)


# Extract amenities provided by hosts -------------------------------------
pattern <- "Internet"
internet <- as.vector(str_match(listings$amenities, pattern))
pattern <- "Wireless\\sInternet"
wireless <- as.vector(str_match(listings$amenities, pattern))

for(i in 1:length(internet)){
  if(!is.na(wireless[i]))
    internet[i] <- wireless[i]
}
internet[is.na(internet)] <- "No Internet"

table(internet)

# Using Regex to extract amenities ----------------------------------------
pattern <- "Air conditioning"
pattern <- ".*?[,}]"
count_amentities <- str_match_all(listings$amenities, pattern)

num_amentities <- rep(NA,length(count_amentities))
for(i in 1:length(count_amentities)){
  num_amentities[i] <- length(count_amentities[[i]])
}

listingsUpdatedDF <- data.frame(listingsUpdated, no_smoke,
                                no_pet, no_shoes, no_party,
                                no_drugs, no_loud, internet,
                                num_amentities, strictness) 

#listingsUpdatedDF <- na.omit(listingsUpdatedDF)



# Further exploring strictness --------------------------------------------
listingsRules <- cbind(id = listings$id, listingsUpdated, as.data.frame(no_smoke),
                       as.data.frame(no_pet), as.data.frame(no_party),
                       as.data.frame(no_shoes), as.data.frame(no_loud))

numRules <- listingsRules %>%
  group_by(id) %>% 
  summarise(., numRules = sum(no_smoke, no_pet, no_shoes, no_party, no_loud))

listingsRules <- left_join(listingsRules, numRules, by = "id")

listingsWithRules <- listingsRules %>% filter(numRules != 0)

listingsWithRules$rules <- NULL


for(i in 1:nrow(listingsWithRules)){
  rules <- c()
  for(j in 15:19){
    if(listingsWithRules[i,j]){
      rules <- c(rules, colnames(listingsWithRules)[j])
      listingsWithRules$rules[i] <- paste0(rules, collapse = ", ")
    }
  }
}

ruleFreqTable <- as.data.frame(table(listingsWithRules$rules)) %>%
  arrange(desc(Freq)) %>%
  head(10)

colnames(ruleFreqTable)[1] <- "ruleComb"

for(i in 1:nrow(listingsWithRules)){
  if(listingsWithRules$rules[i] %in% as.character(ruleFreqTable$ruleComb)){
    listingsWithRules$rulesCleaned[i] <- listingsWithRules$rules[i]
  } else {
    listingsWithRules$rulesCleaned[i] <- "other"
  }
}

ruleCleanedFreqTable <- as.data.frame(table(listingsWithRules$rulesCleaned)) %>%
  arrange(desc(Freq))

colnames(ruleCleanedFreqTable)[1] <- "ruleComb"

temp <- listingsWithRules %>% 
  dplyr::group_by(numRules, rulesCleaned) %>% 
  do(data.frame(numObs = nrow(.)))

listingsWithRules <- left_join(listingsWithRules, temp, by = c("numRules", "rulesCleaned"))

temp <- listingsWithRules %>% 
  dplyr::group_by(numRules) %>% 
  do(data.frame(numObsPerDay = nrow(.)))

listingsWithRules <- left_join(listingsWithRules, temp, by = c("numRules"))

plot <- ggplot(listingsWithRules, aes(x = numRules)) +
  geom_bar(aes(fill = rulesCleaned, order = rulesCleaned)) +
  theme_bw() + 
  xlab("Number of rules per listing") +
  ylab("Number of listings") +
  scale_fill_brewer(name = "Rules", palette = "Spectral",
                    labels = c("No Loud", "No Party", "No Pet",
                               "No Shoes", "No Smoke", "No Smoke and Loud",
                               "No Smoke and Party", "No Smoke and Pet",
                               "No Smoke, Pet and Party", "No Smoke, Pet, Party and Loud",
                               "Other"))
plot

# Models to find siginificant variables -----------------------------------
#Price based on only the new variable strictness
lm_m1 <- lm(price ~ strictness, data = listingsUpdatedDF)
summary(lm_m1)

#Price
lm_m2 <- lm(price~., data = listingsUpdatedDF)
lm_m2 <- update(lm_m2, .~.-strictness -neighbourhood_cleansed) 
summary(lm_m2)
anova(lm_m2)

#Rating 
lm_m3 <- lm(review_scores_rating~., data = listingsUpdatedDF)
lm_m3 <- update(lm_m3, .~.-strictness -neighbourhood_cleansed) #remove 
summary(lm_m3)
anova(lm_m3)

#Price only strictness levels(TRUE)
lm_m4 <- update(lm_m2, .~.-strictness -neighbourhood_cleansed -latitude
                -longitude -room_type -bathrooms - beds -security_deposit
                -num_amentities -number_of_reviews -cleaning_fee -internet
                -guests_included -Region -review_scores_rating )

summary(lm_m4)
anova(lm_m4)

#Rating only strictness levels(TRUE)
lm_m5 <- update(lm_m3, .~.-strictness -neighbourhood_cleansed -latitude
                -longitude -room_type -bathrooms - beds -security_deposit
                -num_amentities -number_of_reviews -cleaning_fee -internet
                -guests_included -Region -price)
summary(lm_m4)
anova(lm_m4)


# Plots for variables -----------------------------------------------------
ggplot(listingsUpdatedDF, aes(x = review_scores_rating, y = price, color = beds)) +
  geom_point(alpha = 0.2)

ggplot(listingsUpdatedDF, aes(x = no_loud, y = review_scores_rating, fill = no_party)) +
  geom_boxplot() 


ggplot(listingsUpdatedDF, aes(x = no_smoke, y = review_scores_rating, fill = no_party)) +
  geom_boxplot() +
  ylim(80,100)

ggplot(listingsUpdatedDF, aes(x = num_amentities, y = review_scores_rating, color = no_smoke)) +
  geom_point(alpha = 0.2)

table(listingsUpdatedDF$Region)

ggplot(listingsUpdatedDF, aes(x = no_smoke, y = review_scores_rating)) +
  geom_boxplot() +
  facet_wrap(~Region) +
  ylim(80,100)

ggplot(listingsUpdatedDF, aes(x = Region, y = strictness)) +
  geom_boxplot() 


# Propotions of Rules by Region -------------------------------------------
listingsUpdatedDF %>%
  group_by(Region) %>%
  summarise(mean_strictness = mean(strictness), 
            mean_price = mean(price),
            #mean_rating = mean(review_scores_rating,na.rm = TRUE),
            no_smoke = sum(ifelse(no_smoke,1,0)/n()),
            no_party = sum(ifelse(no_party,1,0)/n()),
            no_loud = sum(ifelse(no_loud,1,0)/n()),
            no_drugs = sum(ifelse(no_drugs,1,0)/n()),
            no_shoes = sum(ifelse(no_shoes,1,0)/n()))  %>%
  arrange(desc(mean_strictness)) %>%
  kable(., format = "latex",
        booktabs = TRUE,
        digits = 2,
        col.names = c("Region", "Mean Strictness", "Mean Price", "No Smoking", "No Party",
                      "No Noise", "No Drugs", "No Shoes")) %>% 
  kable_styling(latex_options = "hold_position", full_width = F, font_size = 9) %>% 
  landscape() %>% 
  kable_as_image(filename = "~/Desktop/table", file_format = "png", density = 350)

#Unfortunate there’s only 1 observation in Angeles Forest after all NA removed.


# Exploratory maps --------------------------------------------------------
dataJson <- geojson_read(
  "~/Downloads/neighbourhoods.geojson", what = "sp")

mapImage <- ggmap(get_googlemap(c(lon = -118.2437, lat = 34.0522), scale = 2/1, 
                                zoom = 9, maptype = "roadmap"), extent = "panel")
dataDF <- fortify(dataJson)
names(dataDF)[4:5] <- c("lon", "lat")

#Region Boundaries
mapImage + 
  geom_polygon(aes(long,lat, group = group), data = dataDF, 
               colour = "green") 

#Price and Strictness
mapImage +
  geom_polygon(data = dataDF, aes(long, lat, group = group),colour = "Orange", fill = NA) +
  geom_point(data = listingsUpdated, aes(longitude,latitude, color = strictness,alpha = price))

#No smoking rule and Price
mapImage +
  geom_polygon(data = dataDF, aes(long, lat, group = group),colour = "Orange", fill = NA) +
  geom_point(data = listingsUpdated, aes(longitude,latitude, color = no_smoke, alpha = price))

#Review scores and Price
mapImage +
  geom_polygon(data = dataDF, aes(long, lat, group = group),colour = "Orange", fill = NA) +
  geom_point(data = listingsUpdated, aes(longitude,latitude, color = review_scores_rating, alpha = price))

#Regions with color
mapImage +
  geom_polygon(data = dataDF, aes(long, lat, group = group),colour = "Orange", fill = NA) +
  geom_point(data = listingsUpdated, aes(longitude,latitude, color = Region), alpha = 1) 

#Strictness using with colored boxes 
mapImage +
  stat_summary_2d(data = listingsUpdated, aes(longitude,latitude, z = strictness), bins = 60) +
  geom_polygon(data = dataDF, aes(long, lat, group = group),colour = "Orange", fill = NA)

#Strictness using with colored hexagons 
mapImage +
  coord_cartesian() +
  stat_summary_hex(data = listingsUpdated, aes(longitude,latitude, z = strictness), bins = 60) +
  geom_polygon(data = dataDF, aes(long, lat, group = group),colour = "Orange", fill = NA)


# Using a different county geojson file -----------------------------------

dataJsonTwo <- geojson_read(
  "~/Downloads/la-county-regions-current.geojson", what = "sp")

mapImage <- ggmap(get_googlemap(c(lon = -118.2437, lat = 34.0522), scale = 2, 
                                zoom = 9, maptype = "roadmap"), extent = "panel")

dataDF2 <- fortify(dataJsonTwo)
dataDF2$id <- as.numeric(dataDF2$id) + 1
regionNames <- data.frame(id = 1:16, name = dataJsonTwo$name)
dataDF3 <- left_join(dataDF2, regionNames, by = "id")


ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

#first map of just regions with individual listings within those regions
mapOne <- mapImage +
  geom_point(data = listingsUpdated, aes(longitude,latitude, color = Region),
             alpha = .4, size = .4) +
  geom_polygon(data = dataDF2, aes(long, lat, group = group),
               colour = "orange2", size = .8, fill = NA, alpha = .5) +
  theme_bw() +
  guides(colour = guide_legend(
    title = "Regions of Los Angeles", override.aes = list(alpha = 1, size = 3))) +
  ditch_the_axes

mapOne

#graph2png(mapOne, width=9, aspectr=sqrt(2))

#second map with filled in regions based on mean strictness within
#first find mean strictness within each region
meanStrictness <- listingsUpdatedDF %>% 
  dplyr::group_by(Region) %>% 
  dplyr::summarise(mean_strictness = mean(strictness)) %>% 
  arrange(desc(mean_strictness)) 

names(meanStrictness)[1] <- "name"

#join meanStrictness with dataDF3
dataDF4 <- left_join(dataDF3, meanStrictness, by = "name")

#graph! 
mapImage +
  geom_polygon(data = dataDF4, aes(long, lat,  fill = (mean_strictness)^(1/5), group = group),
               colour = "White", alpha = .65, size = .8) +
  theme_bw() +
  scale_fill_gradientn(name = "Mean Strictness\n of Region", 
                       guide = "colourbar", 
                       colors = terrain.colors(n = 3),
                       breaks = c(.9, 1),
                       labels = c("Less strict", "More strict")) +
  ditch_the_axes +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

mapImage

#to save the graph
#ggsave("~/Desktop/mapImageSeven.png", bg = "transparent")

mapThree <- mapImage +
  geom_point(data = listingsUpdated, aes(longitude,latitude, alpha = strictness, color = strictness)) +
  geom_polygon(data = dataDF2, aes(long, lat, group = group), colour = "orange2", size = .8, fill = NA) +
  scale_color_gradientn(name = "Strictness", colors = c("lightblue", "black")) +
  guides(alpha = FALSE) +
  theme_bw() +
  ditch_the_axes + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

mapThree


#ggsave("~/Desktop/mapImageSix.png", bg = "transparent")
