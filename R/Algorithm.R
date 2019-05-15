# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# packages that are required
devtools::use_package("gtools")


#' Creating a population of x individuals with y moves
#'
#' @param individuals
#' @param situations
#' @param moves
#'
#' @return A population that contains x individual solutions for the y different
#'         situations
#' @export
#'
#' @examples
create_population <- function(individuals, situations, moves){

  if(is.numeric(individuals) == FALSE){
    stop ("argument 'individuals' must be numeric") # individuals has to be numeric
  }

  if(is.data.frame(situations) == FALSE){
    stop ("add a data.frame of possible situations")
  }

  if(!any(is.character(moves))){
    stop ("please indicate the possible moves as characters") # moves has to be characters
  }

solutions_population <- list()

for (j in 1:individuals){
  # generating a sequence of as much random Movements as there are situations
  Move <- character(nrow(situations))

  for(i in 1:nrow(situations)){
    Move[i] <- sample(moves, 1)
  }

  # storing the movements next to the situations
  individual_solution <- data.frame(situations, Move)

  # creating a dataframe with all 200 individuals
  solutions_population[[j]] <- individual_solution
}
return(solutions_population)
}


# There are five different sites each with three possibles types of content
sites <- c("North", "East", "South", "West", "Current")
content <- c("Wall", "Empty", "Evidence")

situations <- data.frame(permutations(n = length(content), r = length(sites),
                                      v = content, repeats.allowed = T))
colnames(situations) <- sites

first_population <- create_population(individuals = 200, situations = situations,
                                      moves = c("North", "East", "South",
                                                "West", "Stay", "Pick-Up"))

## Create the territory
create_territory <- function(coordinates, evidence_latitude, evidence_longitude){

  if(!any(is.numeric(coordinates))){
    stop ("coordinates have to be numeric") # coordinates have to be numeric
  }

  longitude <- coordinates[1]:coordinates[2]
  latitude <- coordinates[3]:coordinates[4]

  # create a dataframe that includes all coordinates of the grid
  df_coordinates <- data.frame(latitude  = rep(latitude, length(latitude)),
                               longitude = rep(longitude, each = length(longitude)))

  # create a dataframe that includes the evidence
  df_evidence <- data.frame(evidence_latitude, evidence_longitude)

  # add the right type of content (wall, evidence or nothing) on the corresponding
  # coordinates on the grid
  content_of_coordinates <- character(nrow(df_coordinates))
  for(i in 1:nrow(df_coordinates)){
    for(j in 1:nrow(df_evidence)){
      if(df_coordinates$latitude[i] == df_evidence$evidence_latitude[j] &&
         df_coordinates$longitude[i] == df_evidence$evidence_longitude[j]){
        content_of_coordinates[i] <- "Evidence"
      }
    }
    if(df_coordinates$latitude[i]  <= coordinates[3] ||
       df_coordinates$latitude[i]  >= coordinates[4] ||
       df_coordinates$longitude[i] <= coordinates[1] ||
       df_coordinates$longitude[i] >= coordinates[2]){
      content_of_coordinates[i] <- "Wall"
    }
  }

  content_of_coordinates[content_of_coordinates == ""] <- "Empty" # why doesn't this work if i just add else in the for-loop

  return(list(df_coordinates, content_of_coordinates))
}

evidence_latitude <- c(6, 1, 2, 5, 5, 6)
evidence_longitude <- c(5, 8, 6, 6, 4, 3)

coordinates <- create_territory(coordinates = c(0, 10, 0, 10),
                                evidence_latitude = evidence_latitude,
                                evidence_longitude = evidence_longitude)[[1]]





# size of territory and positions of evidence should be given via input
# by the user with the shiny app


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


