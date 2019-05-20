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
#'
#' @return A population that contains x individual solutions
#' @export
#'
#' @examples
create_population <- function(individuals){

  if(is.numeric(individuals) == FALSE){
    stop ("argument 'individuals' must be numeric") # individuals has to be numeric
  }

solutions_population <- list()

# There are five different sites each with three possibles types of content
sites <- c("Current", "North", "East", "South", "West")
content <- c("Wall", "Empty", "Evidence")
situations <- data.frame(permutations(n = length(content), r = length(sites),
                                      v = content, repeats.allowed = T))
colnames(situations) <- sites
moves <- c("North", "East", "South", "West", "Stay", "Pick-Up")
Move <- character(nrow(situations))

for (j in 1:individuals){
  # generating a sequence of as much random movements as there are situations
  Move <- sample(moves, nrow(situations), replace = T)

  # storing the movements next to the situations
  individual_solution <- data.frame(situations, Move)

  # creating a dataframe with all 200 individuals
  solutions_population[[j]] <- individual_solution
}
return(solutions_population)
}

first_population <- create_population(individuals = 200)

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
    # coordinates_grid have to be numeric
    stop ("The coordinates have to be numeric")
  }

  if(!any(is.numeric(evidence_latitude))){
    # coordinates_grid have to be numeric
    stop ("The coordinates of the evidence have to be numeric")
  }

  if(!any(is.numeric(evidence_longitude))){
    # coordinates_grid have to be numeric
    stop ("The coordinates of the evidence have to be numeric")
  }

  if(length(coordinates_grid) != 4){
    stop ("Give four coordinates of the grid boundaries in the following manner:
          x start, x end, y start, y end")
  }

  if(coordinates_grid[2] < coordinates_grid[1] | coordinates_grid[4] < coordinates_grid[3]){
    stop ("Give four coordinates of the grid boundaries in the following manner:
          x start, x end, y start, y end. Ends have to be higher than starts.")
  }

  if(length(evidence_longitude) != length(evidence_latitude)){
    stop ("Give for each piece of evidence the corresponding longitude and latitude")
  }

  if(any(evidence_latitude <= coordinates_grid[1] | evidence_latitude >= coordinates_grid[2])){
    warning ("Evidence outside the grid will not be considered")
  }

  if(any(evidence_longitude <= coordinates_grid[3] | evidence_latitude >= coordinates_grid[4])){
    warning ("Evidence outside the grid will not be considered")
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
  content_of_coordinates[content_of_coordinates == ""] <- "Empty"

  grid <- cbind(df_coordinates, content_of_coordinates)
  return(grid)
}

grid <- create_grid(coordinates_grid = c(0, 10, 0, 10),
                   evidence_latitude = c(6, 1, 2, 5, 5, 6),
                  evidence_longitude = c(5, 8, 6, 6, 4, 3))

# size of territory and positions of evidence should be given via input
# by the user with the shiny app


#' A function that retrieves the number in the handbook of the current situation
#'
#' @param situation
#' @param grid
#' @param latitude
#' @param longitude
#'
#' @return number in the handbook that correpsonds to robots situation
#' @export
#'
#' @examples default for latitude and longitude are the start position
lookup_handbook <- function(grid, latitude = c(1, 0, 1, 2, 1),
                            longitude  = c(1, 1, 2, 1, 0)){

  if(length(latitude) != 5){
    stop ("Give five latitude coordinates (y-axis) of the grid in the following manner:
          Current, North, East, South, West")
  }

  if(length(longitude) != 5){
    stop ("Give five longitude coordinates (x-axis) of the grid in the following manner:
          Current, North, East, South, West")
  }

  if(class(longitude) != "numeric"){
    stop ("Coordinates have to be numeric. ")
  }

  if(class(latitude) != "numeric"){
    stop ("Coordinates have to be numeric. ")
  }

  if(class(situation) != "data.frame"){
    stop ("Give a data.frame of all possible situations")
  }

  if(class(grid) != "data.frame"){
    stop ("Give a data.frame containing latitude, longitude,
          and content on the respective coordinates")
  }

  if(length(grid) != 3){
    stop ("Give a data.frame of length 3 containing latitude, longitude,
          and content on the respective coordinates")
  }

  # There are five different sites each with three possibles types of content
  sites <- c("Current", "North", "East", "South", "West")
  content <- c("Wall", "Empty", "Evidence")
  situation <- data.frame(permutations(n = length(content), r = length(sites),
                                        v = content, repeats.allowed = T))

    which(situation[,1] == grid[grid[,1] == latitude[1] &
                                grid[,2] == longitude[1],]$content_of_coordinates &
          situation[,2] == grid[grid[,1] == latitude[2] &
                                grid[,2] == longitude[2],]$content_of_coordinates &
          situation[,3] == grid[grid[,1] == latitude[3] &
                                grid[,2] == longitude[3],]$content_of_coordinates &
          situation[,4] == grid[grid[,1] == latitude[4] &
                                grid[,2] == longitude[4],]$content_of_coordinates &
          situation[,5] == grid[grid[,1] == latitude[5] &
                                grid[,2] == longitude[5],]$content_of_coordinates)
}

# error messages are missing


# create a function that moves the robot through the grid according to his handbook

move_score <- function(population, handbook_number, latitude, longitude, score){

  new_position <- data.frame(matrix(nrow = 200, ncol = 10))

  for (i in 1:length(population)){

    next_move     <- population[[i]][handbook_number[i],]$Move
    content_current <- population[[i]][handbook_number[i],]$Current
    content_north <- population[[i]][handbook_number[i],]$North
    content_east  <- population[[i]][handbook_number[i],]$East
    content_south <- population[[i]][handbook_number[i],]$South
    content_west  <- population[[i]][handbook_number[i],]$West

    if (next_move == "North" & content_north != "Wall"){
      new_position[i, 1:5]  <- as.numeric(latitude[i,]) - 1
      new_position[i, 6:10] <- as.numeric(longitude[i,])
    } else if (next_move == "East" & content_east != "Wall"){
      new_position[i, 1:5]  <- as.numeric(latitude[i,])
      new_position[i, 6:10] <- as.numeric(longitude[i,]) + 1
    } else if (next_move == "South" & content_south != "Wall"){
      new_position[i, 1:5]  <- as.numeric(latitude[i,]) + 1
      new_position[i, 6:10] <- as.numeric(longitude[i,])
    } else if (next_move == "West" & content_west != "Wall"){
      new_position[i, 1:5]  <- as.numeric(latitude[i,])
      new_position[i, 6:10] <- as.numeric(longitude[i,]) + 1
    } else  if (next_move == "North" & content_north == "Wall"){
      new_position[i, 1:5]  <- as.numeric(latitude[i,])
      new_position[i, 6:10] <- as.numeric(longitude[i,])
      score[i] <- score[i] - 5
    } else if (next_move == "East" & content_east == "Wall"){
      new_position[i, 1:5]  <- as.numeric(latitude[i,])
      new_position[i, 6:10] <- as.numeric(longitude[i,])
      score[i] <- score[i] - 5
    } else if (next_move == "South" & content_south == "Wall"){
      new_position[i, 1:5]  <- as.numeric(latitude[i,])
      new_position[i, 6:10] <- as.numeric(longitude[i,])
      score[i] <- score[i] - 5
    } else if(next_move == "West" & content_west == "Wall"){
      new_position[i, 1:5]  <- as.numeric(latitude[i,])
      new_position[i, 6:10] <- as.numeric(longitude[i,])
      score[i] <- score[i] - 5
    }

    if (next_move == "Stay" || next_move == "Pick-Up"){
      new_position[i, 1:5]  <- as.numeric(latitude[i,])
      new_position[i, 6:10] <- as.numeric(longitude[i,])
    }
  }

  for (i in 1:length(population)){
    # if he picks up sth the environment changes
    if(next_move == "Pick-Up" & content_current == "Evidence"){
      population[[i]][handbook_number,]$Current <- "Empty"
      score[i] <- score[i] + 10
    }
  }

  for (i in 1:length(population)){
    # if he picks up but there is nothing, he is fined
    if(next_move == "Pick-Up" & content_current == "Empty"){
      score[i] <- score[i] - 1
    }
  }

  latitude <- new_position[,1:5]
  longitude <- new_position[,6:10]
  return(data.frame(latitude, longitude, score))
}

handbook_number <- numeric(length(first_population))
for (i in 1:length(first_population)){
  handbook_number[i] <- lookup_handbook(situation = situations, grid = grid)
}

# creating a data.frame with the current coordinates of the robot
latitude <-  data.frame(matrix(rep(c(1, 0, 1, 2, 1), each = 200), ncol = 5, nrow = 200))
longitude <-  data.frame(matrix(rep(c(1, 1, 2, 1, 0), each = 200), ncol = 5, nrow = 200))

score <- numeric(length(first_population))

# let the robot walk 200 steps
for (i in 1:200){
new_coordinates <- move_score(population = first_population,
                              handbook_number = handbook_number,
                              latitude = latitude, longitude = longitude,
                              score = score)


latitude <- new_coordinates[,1:5]
longitude <- new_coordinates[,6:10]
score <- as.numeric(new_coordinates$score)

handbook_number <- numeric(nrow(longitude))
for (j in 1:nrow(longitude)){
  handbook_number[j] <- as.numeric(lookup_handbook(
    situation = situations, grid = grid, latitude = as.numeric(latitude[j,]),
    longitude = as.numeric(longitude[j,])))
}
}

# After 200 steps in one grid configuration, change grid
grid <- NULL

evidence_latitude <- sample(1:9, 11, replace = T)
evidence_longitude <- sample(1:9, 11, replace = T)
grid <- create_grid(coordinates_grid = c(0, 10, 0, 10),
                    evidence_latitude = c(0, 10, 3, 5, 8),
                    evidence_longitude = c(9, 4, 5, 7, 1))

# Do this 100 times
