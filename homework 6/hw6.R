#References: 
#https://www.google.com/destination?q=the+town+of+Bude+in+Western+England&site=search&output=search&dest_mid=/m/01stbg&sa=X&ved=0ahUKEwja1sqIrYvaAhVOHqwKHW6CD74Q6tEBCEooBDAB

library(ggmap)
library(tidyverse)
######################################################################################################################################################
# Find the town of Bude in Western England. This is a town that is well-known as a beach resort. 
# You're going to make two maps of Bude – a road map and a watercolor map.

#Bude
gc_Bude <- geocode("Bude, England, UK")
data_Bude <- as.data.frame(gc_Bude)
map_Bude_road <- get_map(data_Bude, maptype = "roadmap", zoom=13)
map_Bude_water <- get_map(data_Bude, maptype = "watercolor",zoom=13)

#Bude_Road Map
Bude_RoadMap <- ggmap(map_Bude_road) +
  geom_point(
    aes(x = lon, y = lat),
    data = data_Bude, color = "red", size = 3
  )
Bude_RoadMap


#Bude_Water Map
Bude_WaterMap <- ggmap(map_Bude_water) +
  geom_point(
    aes(x = lon, y = lat),
    data = data_Bude, color = "red", size = 3
  )
#Bude_WaterMap


#############################################################################################################################
# On both maps, mark the map with vacation spots you might like: 
# surfing beaches and the cricket grounds (one of the most stunning 
# the the country and very local). Pick two local beaches (this will
# require some googling).
#############################################################################################################################
#Pick two local beaches
#pick top two list on google traval
#Beach1: Sandy Mouth Beach
gc_SandyMouth <- geocode("Kilkhampton, Bude EX23 9EG, UK")
data_beach1 <- as.data.frame(gc_SandyMouth)
map_SandyMouth <- get_map(data_beach1)

SandyMouth_RoadMap <- ggmap(map_SandyMouth) +
  geom_point(
    aes(x = lon, y = lat),
    data = data_beach1, color = "green", size = 4
  )
SandyMouth_RoadMap
#Beach2:Summerleaze Beach

gc_Summerleaze <- geocode("Summerleaze Cres, Bude EX23 8HN, UK")
data_beach2 <- as.data.frame(gc_Summerleaze)
map_Summerleaze <- get_map(data_beach2)

Summerleaze_RoadMap <- ggmap(map_Summerleaze) +
  geom_point(
    aes(x = lon, y = lat),
    data = data_beach2, color = "green", size = 4
  )
Summerleaze_RoadMap
#############################################################################################################################
#the cricket grounds (one of the most stunning 
#the the country and very local)
#pick top one on google travel list

#Cricket ground: Bude North Cornwall Cricket Club
gc_NorthCornwall <- geocode("Crooklets Cricket Ground,Bude, EX23 8ND, UK")
data_cricket <- as.data.frame(gc_NorthCornwall)
map_NorthCornwall <- get_map(data_cricket)

Cricket_RoadMap <- ggmap(map_NorthCornwall) +
  geom_point(
    aes(x = lon, y = lat),
    data = data_cricket, color = "yellow", size = 4
  )


#############################################################################################################################
#Finally find a pub that is convenient to the cricket grounds. Mark the route from the grounds to the pub.
#pick the nearest one I searched from google map

#Nearby Pub : Bar 35, Belle Vue Ln, Bude EX23 8BR, UK
gc_Bar35 <- geocode("Bar 35, Belle Vue Ln, Bude EX23 8BR, UK")
data_pub <- as.data.frame(gc_Bar35)

from <- "Bar 35, Belle Vue Ln, Bude EX23 8BR, UK"
to <- "Bude North Cornwall Cricket Club"
route_df <- route(from, to, structure = "route")

route_data <- data.frame(x = route_df$lon, y= route_df$lat)

Route_CricketBar <- ggmap(map_Bude_road) +  
  geom_path(
    aes(x = lon, y = lat), colour = "red", size = 1.5,
    data = route_df, lineend = "round"
  )

############################################################################################################################
# Two hotels added by Kevin Chan.

# The Falcon Hotel
gc_falcon <- geocode("The Falcon Hotel, Bude EX23 8SD, UK")
data_falcon <- as.data.frame(gc_falcon)
map_falcon <- get_map(data_falcon)

Falcon_RoadMap <- ggmap(map_falcon) +
  geom_point(
    aes(x = lon, y = lat),
    data = data_falcon, color = "Purple", size = 4
  )

# Edgcumbe Hotel
gc_edgcumbe <- geocode("Edgcumbe Hotel, Bude EX23 8HJ, UK")
data_edgcumbe <- as.data.frame(gc_edgcumbe)
map_edgcumbe <- get_map(data_edgcumbe)

Edgcumbe_RoadMap <- ggmap(map_edgcumbe) +
  geom_point(
    aes(x = lon, y = lat),
    data = data_edgcumbe, color = "Orange", size = 4
  )

#############################################################################################################################
#visualization:

#create a data frame containing all locations
lon_all <- as.numeric(c(data_Bude[1], data_cricket[1], data_beach1[1],data_beach2[1],data_pub[1],data_falcon[1],data_edgcumbe[1]))
lat_all <- as.numeric(c(data_Bude[2], data_cricket[2],data_beach1[2],data_beach2[2], data_pub[2],data_falcon[2],data_edgcumbe[2]))
allData <- data.frame(lon=lon_all, lat=lat_all)


all_spots_Roadmap <- ggmap(map_Bude_road) +
  geom_point(aes(x = lon, y = lat), data = data_Bude, color = "Brown", size = 3)+
  geom_point(aes(x = lon, y = lat), data = data_cricket, color = "Red", size = 3)+
  geom_point(aes(x = lon, y = lat), data = data_beach1, color = "yellow" , size = 3)+
  geom_point(aes(x = lon, y = lat), data = data_beach2,color = "Green" , size = 3)+
  geom_point(aes(x = lon, y = lat), data = data_pub, color = "Blue" , size = 3)+
  # Added Hotels
  geom_point(aes(x = lon, y = lat), data = data_falcon, color = "Purple", size = 3)+
  geom_point(aes(x = lon, y = lat), data = data_edgcumbe, color = "Orange", size = 3)+
  geom_text(aes(x = lon, y = lat, label="Bude"), data = data_Bude, vjust=1, hjust=1,size=3)+
  geom_text(aes(x = lon, y = lat, label="Cricket Club"), data_cricket, vjust=1, hjust=1,size=3)+
  geom_text(aes(x = lon, y = lat, label="Sandy Mouth Beach"), data = data_beach1, vjust=1, hjust=1,size=3)+
  geom_text(aes(x = lon, y = lat, label="Summerleaze Beach"), data = data_beach2, vjust=1, hjust=1,size=3)+
  geom_text(aes(x = lon, y = lat, label="Bar 35"), data = data_pub, vjust=0, hjust=0,size=3)+
  # Added Hotels Text
  geom_text(aes(x = lon, y = lat, label="The Falcon Hotel"), data = data_falcon, vjust=1, hjust=1, size=3)+
  geom_text(aes(x = lon, y = lat, label="Edgcumbe Hotel"), data = data_edgcumbe, vjust=1, hjust=1, size=3)+
  geom_path(aes(x = lon, y = lat), colour = "red", size = 1,data = route_df, lineend = "round")

all_spots_Watermap <- ggmap(map_Bude_water) +
  geom_point(aes(x = lon, y = lat), data = data_Bude, color = "Brown", size = 3)+
  geom_point(aes(x = lon, y = lat), data = data_cricket, color = "Red", size = 3)+
  geom_point(aes(x = lon, y = lat), data = data_beach1, color = "yellow" , size = 3)+
  geom_point(aes(x = lon, y = lat), data = data_beach2,color = "Green" , size = 3)+
  geom_point(aes(x = lon, y = lat), data = data_pub, color = "Blue" , size = 3)+
  # Added Hotels
  geom_point(aes(x = lon, y = lat), data = data_falcon, color = "Purple", size = 3)+
  geom_point(aes(x = lon, y = lat), data = data_edgcumbe, color = "Orange", size = 3)+
  geom_text(aes(x = lon, y = lat, label="Bude"), data = data_Bude, vjust=1, hjust=1,size=3)+
  geom_text(aes(x = lon, y = lat, label="Cricket Club"), data_cricket, vjust=1, hjust=1,size=3)+
  geom_text(aes(x = lon, y = lat, label="Sandy Mouth Beach"), data = data_beach1, vjust=1, hjust=1,size=3)+
  geom_text(aes(x = lon, y = lat, label="Summerleaze Beach"), data = data_beach2, vjust=1, hjust=1,size=3)+
  geom_text(aes(x = lon, y = lat, label="Bar 35"), data = data_pub, vjust=0, hjust=0,size=3)+
  # Added Hotels Text
  geom_text(aes(x = lon, y = lat, label="The Falcon Hotel"), data = data_falcon, vjust=1, hjust=1, size=3)+
  geom_text(aes(x = lon, y = lat, label="Edgcumbe Hotel"), data = data_edgcumbe, vjust=1, hjust=1, size=3)+
  geom_path(aes(x = lon, y = lat), colour = "red", size = 1,data = route_df, lineend = "round")


Bude_RoadMap
Bude_WaterMap
SandyMouth_RoadMap
Summerleaze_RoadMap
Cricket_RoadMap
Route_CricketBar
# Added Hotels Road Maps
Falcon_RoadMap
Edgcumbe_RoadMap
all_spots_Roadmap
all_spots_Watermap
