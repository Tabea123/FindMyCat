# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# packages that are required
devtools::use_package("gtools")

## Create the first population with random movements

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

## Create the grid

#' Function to create the grid in the right size with the evidence positions
#' included
#'
#' @param coordinates_grid
#' @param evidence_latitude
#' @param evidence_longitude
#'
#' @return
#' @export
#'
#' @examples
create_grid <- function(coordinates_grid, evidence_latitude, evidence_longitude){

  if(!any(is.numeric(coordinates_grid))){
    stop ("The coordinates have to be numeric") # coordinates_grid have to be numeric
  }

  if(!any(is.numeric(evidence_latitude))){
    stop ("The coordinates of the evidence have to be numeric") # coordinates_grid have to be numeric
  }

  if(!any(is.numeric(evidence_longitude))){
    stop ("The coordinates of the evidence have to be numeric") # coordinates_grid have to be numeric
  }

  if(length(coordinates_grid) != 4){
    stop ("Give four coordinates of the grid boundaries in the following manner:
          x start, x end, y start, yend")
  }

  if(length(evidence_longitude) != length(evidence_latitude)){
    stop ("Give for each piece of evidence the corresponding longitude and latitude")
  }

  longitude <- coordinates_grid[1]:coordinates_grid[2]
  latitude <-  coordinates_grid[3]:coordinates_grid[4]

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

      if(df_coordinates$latitude[i]  == df_evidence$evidence_latitude[j] &&
         df_coordinates$longitude[i] == df_evidence$evidence_longitude[j]){
        content_of_coordinates[i] <- "Evidence"
      }
    }
    if(df_coordinates$latitude[i]  <= coordinates_grid[3] ||
       df_coordinates$latitude[i]  >= coordinates_grid[4] ||
       df_coordinates$longitude[i] <= coordinates_grid[1] ||
       df_coordinates$longitude[i] >= coordinates_grid[2]){
      content_of_coordinates[i] <- "Wall"
    }
  }
  content_of_coordinates[content_of_coordinates == ""] <- "Empty" # why doesn't this work if i just add else in the for-loop

  grid <- cbind(df_coordinates, content_of_coordinates)
  return(grid)
}

grid <- create_grid(coordinates_grid = c(0, 10, 0, 10),
                    evidence_latitude = c(6, 1, 2, 5, 5, 6),
                    evidence_longitude = c(5, 8, 6, 6, 4, 3))

df_coordinates <- create_grid(coordinates_grid = c(0, 10, 0, 10),
                                evidence_latitude = c(6, 1, 2, 5, 5, 6),
                                evidence_longitude = c(5, 8, 6, 6, 4, 3))[,1:2]

content_of_coordiantes <- create_grid(coordinates_grid = c(0, 10, 0, 10),
                              evidence_latitude = c(6, 1, 2, 5, 5, 6),
                              evidence_longitude = c(5, 8, 6, 6, 4, 3))[,3]

# size of territory and positions of evidence should be given via input
# by the user with the shiny app

## Look up the strategy in the handbook to move through the grid

# create a function that retrieves the number in the handbook of the current situation
# default for latitude and longitude are the start position (C, N, E, S, W)
lookup_handbook <- function(situation, grid, latitude = c(1, 0, 1, 2, 1),
                            longitude  = c(1, 1, 2, 1, 0), content_of_coordiantes){
  which(situations$Current == grid[grid$latitude == latitude[1] &
                                     grid$longitude == longitude[1],]$content_of_coordinates &
          situations$North == grid[grid$latitude == latitude[2] &
                                     grid$longitude == longitude[2],]$content_of_coordinates &
          situations$East  == grid[grid$latitude == latitude[3] &
                                     grid$longitude == longitude[3],]$content_of_coordinates &
          situations$South == grid[grid$latitude == latitude[4] &
                                     grid$longitude == longitude[4],]$content_of_coordinates &
          situations$West  == grid[grid$latitude == latitude[5] &
                                     grid$longitude == longitude[5],]$content_of_coordinates)
}


handbook_number <- lookup_handbook(situation = situations, grid = grid,
                content_of_coordiantes = content_of_coordiantes)


move <- function(population, handbook_number){

  latitude <- c(1, 0, 1, 2, 1)
  longitude <- c(1, 1, 2, 1, 0)
  new_position <- data.frame(matrix(nrow = 200, ncol = 10))

  for (i in 1:length(population)){
    next_move <- population[[i]][handbook_number,]$Move

    if (next_move == "North"){
      new_position[i, 1:5] <- latitude + 1
      new_position[i, 6:10] <- longitude
    } else if (next_move == "East"){
      new_position[i, 1:5] <- latitude
      new_position[i, 6:10] <- longitude + 1
    } else if (next_move == "South"){
      new_position[i, 1:5] <- latitude - 1
      new_position[i, 6:10] <- longitude
    } else if (next_move == "West"){
      new_position[i, 1:5] <- latitude
      new_position[i, 6:10] <- longitude + 1
    }
  }

    # if he crashes into a wall, he bounces back
    if(population[[i]][handbook_number,]$Move == "North" &
       population[[i]][handbook_number,]$North == "Wall"){
       population[[i]][handbook_number,]$Move <- "Stay"
    } else if (population[[i]][handbook_number,]$Move == "East" &
               population[[i]][handbook_number,]$East == "Wall"){
               population[[i]][handbook_number,]$Move <- "Stay"
    } else if (population[[i]][handbook_number,]$Move == "South" &
               population[[i]][handbook_number,]$South == "Wall"){
               population[[i]][handbook_number,]$Move <- "Stay"
    } else if(population[[i]][handbook_number,]$Move == "West" &
              population[[i]][handbook_number,]$West == "Wall"){
              population[[i]][handbook_number,]$Move <- "Stay"
    }

  for (i in 1:length(population)){
    next_move <- population[[i]][handbook_number,]$Move

    if (next_move == "Stay" || next_move == "Pick-Up"){
      new_position[i, 1:5] <- latitude
      new_position[i, 6:10] <- longitude
    }
  }

    # if he picks up sth the environment changes
    if(population[[i]][handbook_number,]$Move == "Pick-Up" & population[[i]]$Current == "Evidence"){
      population[[i]]$Current <- "Empty"
    }

  latitude <- new_position[,1:5]
  longitude <- new_position[,6:10]

 return(c(latitude, longitude))
}




