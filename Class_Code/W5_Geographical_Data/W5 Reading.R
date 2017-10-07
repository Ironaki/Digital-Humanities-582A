#####A bunch of useful code
rm(list=ls())
library(stringr)
library(ggplot2)
library(tidyr)
library(plotly)
library(ggmap)
library(maps)
library(mapdata)
library(plyr)
library(geosphere)
library(dplyr)
library(data.table)

# Code for encoding setting on Windows
Sys.setlocale("LC_ALL", "English")
Sys.setlocale("LC_ALL", "Japanese")
Sys.setlocale("LC_ALL", "Chinese")
Sys.getlocale()

### CH 5.2 Dot maps
# Notice we get a new "ggmap" package
# Pull a map of Japan from Google

japan.map <- get_map(location = "Japan", zoom = 5, maptype = "satellite")
castles <- c("Hikone Castle", "Himeji Castle")
geo.castles <- geocode(castles)
geo.castles
# MAP 1 (Maps are getting more sophisticated)
ggmap(japan.map) +
        geom_point(data = geo.castles, aes(x = lon, y = lat), color = "red")

# Add annual harvest to the map
Annual_harvest <- c(200000,150000)
geo.castles <- cbind.data.frame(geo.castles, Annual_harvest)
# MAP 2
ggmap(japan.map) +
        geom_point(data = geo.castles, aes(x = lon, y = lat, size = Annual_harvest), color = "orange")
# MAP 3
ggmap(japan.map) +
        geom_point(data = geo.castles, aes(x = lon, y = lat, size = Annual_harvest), color = "orange") +
        xlim(c(125,145)) + ylim(c(30,42))

# A simple map with xlim
world.map <- borders(database = "world")
ggplot() +
        world.map+
        coord_quickmap(xlim = c(30,50))

# The book uses world map, but I use Eu.map 
Eu.map <- borders(database = "world", colour = "gray20", fill = "gray60")
ggplot() + Eu.map + 
        coord_map(projection = "gilbert", ylim = c(25,50), xlim = c(-15,40)) +
        xlab("") + ylab("") +
        ggtitle("The Mediterranean")

# Two versions of ITA map
ITA.map <- borders(database = "world", regions = "italy")
ITA.map.internal <- borders(database = "italy")

# The following creates a database
world <- map_data(map = "world")
country.names <- sort(unique(world$region))

#JPN KOR NKR
neasia.map <- borders(data = "world2", regions = c("japan", "south korea", "north korea"))
ggplot() + neasia.map + coord_quickmap()
        

JPN.map <- borders(data = "world2",regions = "japan",colour = "grey20",  fill = "lightblue")
KOR.map <- borders(data="world2", region="south korea",
                      colour="gray20", fill="lightblue")
PKR.map <- borders(data="world2", region="north korea",
                      colour="black", fill="red")
CHN.map <- borders(data="world2", region=c("china","taiwan"),
                     colour="grey90", fill="grey80")
ggplot() + CHN.map + JPN.map + KOR.map + PKR.map +
        coord_map(projection = "gilbert", xlim = c(120,145), ylim = c(20,45))+
        xlab("") + ylab("")


## CH 5.3

# Map the olives production
olives <- read.csv("http://history.emory.edu/RAVINA/HIST_582/Data/oxrep_oliveoil_wine.csv",
                   header = TRUE, stringsAsFactors = FALSE)
world.map  <- borders("world", colour = "grey65", fill = "grey60")
ggplot() + world.map + geom_point(data = olives, aes(x = loclong, y = loclat)) +
        coord_map(projection = "stereographic", xlim = c(-15, 45), ylim = c(28,60))+
        xlab("") + ylab("") +
        ggtitle("Ancient Olive Oil and Wine Press Locations")


# Map of olives production according to the presses
# Use "rep" to repeat according to the presses
olives.expanded <- olives[rep(seq_len(nrow(olives)), olives$presses),]
ggplot() + world.map +
        geom_jitter(data = olives.expanded, aes(x=loclong, y=loclat), width = 1, height = 1,size = 0.2) +
        coord_map(projection = "stereographic", xlim = c(-15, 45), ylim = c(30,60)) +
        xlab("") + ylab("") +
        ggtitle("Ancient Olive Oil and Wine Press Locations")

# Here is a density map
ggplot() + world.map +
        geom_density_2d(data = olives, aes(x=loclong, y=loclat)) +
        coord_map(projection = "stereographic", xlim = c(-15, 45), ylim = c(30,60)) +
        xlab("") + ylab("") +
        ggtitle("Ancient Olive Oil and Wine Press Locations")

# This one is just a combination of the two previous maps
ggplot() + world.map +
        geom_density_2d(data = olives, aes(x=loclong, y=loclat)) +
        geom_jitter(data = olives.expanded, aes(x=loclong, y=loclat), width = 1, height = 1,size = 0.2) +
        coord_map(projection = "stereographic", xlim = c(-15, 45), ylim = c(30,60)) +
        xlab("") + ylab("") +
        ggtitle("Ancient Olive Oil and Wine Press Locations")


## CH 5.4 Borders and Regions
data(worldHiresMapEnv)

# The function "map_data" creates dataframe.
JPN.wiremap <- map_data(map="world2", region="japan")
NE_asia.wiremap <- map_data(map="world2", region=c('north korea','south korea','japan'))
N_america.wiremap <- map_data(map="world", region=c('usa','canada','mexico'))
CHN.map <- map_data(map = "world2", region = "china")

italy.wiremap <- map_data(map="world", region="italy")
italy.wiremap.internal <- map_data(region="italy")

world <- map_data(map="world2")
country.names <- sort(unique(world$region))

# The example with Poland
POL.wiremap <- map_data(map = "world", region = "poland")
POL.map <- ggplot() + 
        geom_polygon(data = POL.wiremap, aes(x = long, y = lat, group = group), fill = "white", color ="black") +
        coord_map("orthographic")

# Add two cities in Poland
POL.cities <- c("Poznan","Warsaw")
POL.points <- geocode(POL.cities)
POL.points$names <-  POL.cities
POL.map + geom_point(data = POL.points, aes(x = lon, y = lat)) +
        geom_text(data = POL.points, aes(x = lon, y = lat, label = names, vjust = -1.5)) +
        geom_path(data = POL.points, aes(x = lon, y = lat))
        
world.wire <- map_data("world")
ggplot() + geom_polygon(data = world.wire, aes(x = long, y = lat, group = group), color = "black", fill = "white") +
        coord_map(projection = "stereographic", ylim = c(30,50), xlim = c(-15,35))


# Create Ruritania (An imaginary nation within POL)
cities <- c("Poznan","Wroclaw","Krakow","Warsaw","Bydgoszcz")
new.points <- geocode(cities)
new.points$order <- c(1:5)
new.points$group <- 2
new.points$region <- "Ruritania"
new.points$subregion <- NA
colnames(new.points)[1] <- "long"
POL.RUT.map <- rbind(POL.wiremap, new.points)

ggplot() + geom_polygon(data = POL.RUT.map, aes(x = long, y = lat, group = group, fill = region), color = "black") +
        coord_map("orthographic")

## CH 5.6 Choropleth Maps
NE_Asia.wiremap.df <- map_data(map="world2", region=c('north korea','south korea','japan'))
Pop_density.df <- data.frame("region" = c("North Korea", "South Korea", "Japan"),"pop_desity" = c(209,518,348))

NE_Asia.wiremap.df$pop_density <- mapvalues(NE_Asia.wiremap.df$region, from = Pop_density.df$region , to = Pop_density.df$pop_desity)
NE_Asia.wiremap.df$pop_density <- as.numeric(NE_Asia.wiremap.df$pop_density)
ggplot() + geom_polygon(data = NE_Asia.wiremap.df, aes(x = long, y = lat, group = group, fill = pop_density), color = "black") +
        coord_map("mercator") +
        scale_fill_gradient(limits = c(200,600),low = "white", high = "red", guide = guide_legend(title = "People per \nsq. km.\n")) +
        xlab("") + ylab("") +
        theme(legend.title.align = 0.5)

## CH 3.6 Spatial Data
p1 <- geocode("New York")
p2 <- geocode("Boston")
distHaversine(p1,p2)
daylength(p1$lat, "1995-12-25")

mapdist("Boston", "New York", mode="walking")
           
a = 3                     

