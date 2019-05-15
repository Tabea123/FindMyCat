# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# packages that are required
devtools::use_package("gtools")

## Create a data frame of all 243 situations
# There are five different sites each with three possibles types of content
# 3^5
sites <- c("North", "East", "South", "West", "Current")
content <- c("Wall", "Empty", "Evidence")
#get all permutations and  make a dataframe with all situations and sites
situations <- data.frame(permutations(n=3,r=5,v=content,repeats.allowed=T))
colnames(situations) <- sites

## Create the first population of 200 individuals
solutions_population <- list()

for (j in 1:200){
  # generating a sequence of 243 random Movements
  Move <- character(243)
  for(i in 1:243){
    Move[i] <- sample(c("North", "East", "South", "West", "Stay", "Pick-Up"), 1)
  }
  # storing the Movements next to the situations
  individual_solution <- data.frame(situations, Move)
  # creating a dataframe with all 200 individuals
  solutions_population[[j]] <- individual_solution
}


## Create the territory
# size (can be given via input later)
xstart <- 0
xend <- 10
ystart <- 0
yend <- 10

latitude <- ystart:yend
longitude <- xstart:xend

# create a dataframe that includes all coordinates of the grid
coordinates <- data.frame(latitude = rep(latitude, 11), longitude = rep(longitude, each = 11))

#  position of the evidence (e.g., footprints) on the grid
# can be given via input by the user later
footprints_latitude <- c(6, 1, 2, 5, 5, 6)
footprints_longitude <- c(5, 8, 6, 6, 4, 3)

df_footprints <- data.frame(latitude = footprints_latitude,
                            longitude = footprints_longitude)

# add the right type of content (wall, evidence or nothing) on the corresponding
# coordinates on the grid
content_of_coordinates <- character(length(coordinates$latitude))
for(i in 1:length(coordinates$latitude)){
  for(j in 1:length(df_footprints$latitude)){
    if(coordinates$latitude[i] == df_footprints$latitude[j] && coordinates$longitude[i] == df_footprints$longitude[j]){
      content_of_coordinates[i] <- content[3]
    }
  }
  if(coordinates$latitude[i] <= ystart || coordinates$latitude[i] >= yend ||
     coordinates$longitude[i] <= xstart || coordinates$longitude[i] >= xend){
    content_of_coordinates[i] <- content[1]
  }
}

content_of_coordinates[content_of_coordinates == ""] <- content[2] # why doesn't this work if i just add else in the for-loop

# creating the grid
grid <- cbind(coordinates, content_of_coordinates)

# create a function that retrieves the number in the handbook of the current situation
# default is the start at 1,1
Handbook_latitude <- c(1, 0, 1, 2, 1)
Handbook_longitude <- c(1, 1, 2, 1, 0)
Handbook <- function(latitude = Handbook_latitude, longitude = Handbook_longitude){
  which(situations$Current == grid[grid$latitude == Handbook_latitude[1] & grid$longitude == Handbook_longitude[1],]$content_of_coordinates &
          situations$North == grid[grid$latitude == Handbook_latitude[2] & grid$longitude == Handbook_longitude[2],]$content_of_coordinates &
          situations$East == grid[grid$latitude == Handbook_latitude[3] & grid$longitude == Handbook_longitude[3],]$content_of_coordinates &
          situations$South == grid[grid$latitude == Handbook_latitude[4] & grid$longitude == Handbook_longitude[4],]$content_of_coordinates &
          situations$West == grid[grid$latitude == Handbook_latitude[5] & grid$longitude == Handbook_longitude[5],]$content_of_coordinates)
}

x <- Handbook()
# whats in the handbook for these coordinates?
individual_solution[x,]
# let's assume the move said east - > change of coordinates when move east
Handbook_longitude <- Handbook_longitude +1
# whats in the hanbook for these cooridnates?
y <- Handbook()
individual_solution[y,]

# question: what happens if the robot stays or picks-up
# in the handbook the movement on this site will always be the same, which means that
# the robot will be stuck at that site??

# if he crashes into a wall, he bounces back
if(individual_solution[169,]$Move == "North" & individual_solution[169,]$North == "Wall"){
  individual_solution[169,]$Move <- "Stay"
} else if (individual_solution[169,]$Move == "East" & individual_solution[169,]$East == "Wall"){
  individual_solution[169,]$Move <- "Stay"
} else if (individual_solution[169,]$Move == "South" & individual_solution[169,]$South == "Wall"){
  individual_solution[169,]$Move <- "Stay"
} else if(individual_solution[169,]$Move == "West" & individual_solution[169,]$West == "Wall"){
  individual_solution[169,]$Move <- "Stay"
}


