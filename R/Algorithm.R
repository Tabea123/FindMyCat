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
moves <- c("North", "East", "South", "West", "Stay", "Pick-Up") # an erster stelle kein stay
Move <- character(nrow(situations))

for (j in 1:individuals){
  # generating a sequence of as much random movements as there are situations
  Move <- sample(moves, nrow(situations), replace = T)

  if(Move[1] == "Stay"){
    first_move <- c("North", "East", "South", "West", "Pick-Up")
    Move <- sample(first_move, 1, replace = F)
  }

  # storing the movements next to the situations
  individual_solution <- data.frame(situations, Move)

  # creating a dataframe with all 200 individuals
  solutions_population[[j]] <- individual_solution
}
return(solutions_population)
}

first_population <- create_population(individuals = 200)

## Create the grid
create_grid <- function(coordinates_grid, n_evidence){

  if(!any(is.numeric(coordinates_grid))){
    # coordinates_grid have to be numeric
    stop ("The coordinates have to be numeric")
  }

  if(length(coordinates_grid) != 2){
    stop ("Give the length of the grid on the y-axis and x-axis")
  }

  if(is.numeric(n_evidence) == FALSE){
    # number of evidence have to be numeric
    stop ("Indicate how much evidence (numeric) you want to place in the grid.")
  }

  if(n_evidence <= 0){
    stop ("Number evidence has to be more than 0.")
  }

# Create an empty grid and add the walls
grid <- matrix(NA, coordinates_grid[1], coordinates_grid[2])
grid[1:nrow(grid),1:ncol(grid)] <- "Empty"
# create evidence randomly and add it to the grid
evidence <- sample(1:coordinates_grid[1]^2, n_evidence, replace = F)
grid[evidence] <- "Evidence"
grid <- rbind(grid, matrix("Wall", nrow = 1, ncol = coordinates_grid[2]))
grid <- rbind(matrix("Wall", nrow = 1, ncol = coordinates_grid[2]), grid)
grid <- cbind(grid, matrix("Wall", ncol = 1, nrow = coordinates_grid[2]+2))
grid <- cbind(matrix("Wall", ncol = 1, nrow = coordinates_grid[2]+2), grid)
return(grid)
}

grid <- create_grid(coordinates_grid = c(10, 10), n_evidence = 11)


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
lookup_handbook <- function(grid, latitude = 2, longitude = 2){

  if(length(latitude) != 1){
    stop ("Give the current position of the robot on the y-axis.")
  }
  if(length(longitude) != 1){
    stop ("Give the current position of the robot on the x-axis.")
  }
  if(longitude <= 1 || longitude >= ncol(grid)){
    stop ("The current position of the robot can't be outside the grid or on a wall.")
  }
  if(latitude <= 1 || latitude >= nrow(grid)){
    stop ("The current position of the robot can't be outside the grid or on a wall.")
  }
  if(class(longitude) != "numeric"){
    stop ("Coordinates have to be numeric.")
  }
  if(class(latitude) != "numeric"){
    stop ("Coordinates have to be numeric.")
  }

  # There are five different sites each with three possibles types of content
  sites <- c("Current", "North", "East", "South", "West")
  content <- c("Wall", "Empty", "Evidence")
  situation <- data.frame(permutations(n = length(content), r = length(sites),
                                        v = content, repeats.allowed = T))

  # coordintes of the current field and north east south and west
  latitude <- c(latitude, latitude - 1, latitude, latitude + 1, latitude)
  longitude <- c(longitude, longitude, longitude + 1, longitude, longitude - 1)

  # looking up which number in the handbook corresponds to the current situation
  number <- which(situation[,1] == grid[latitude[1], longitude[1]] &
          situation[,2] == grid[latitude[2], longitude[2]] &
          situation[,3] == grid[latitude[3], longitude[3]] &
          situation[,4] == grid[latitude[4], longitude[4]] &
          situation[,5] == grid[latitude[5], longitude[5]])

  return(number)
}

# lookup the position of the robot for each of the strategies
situation <- numeric(length(first_population))
for (i in 1:length(first_population)){
  situation[i] <- as.numeric(lookup_handbook(grid = grid))
}


# roxgen is missing
# create a function that moves the robot through the grid according to his handbook
for(i in 1:2){
  x <- (lookup_handbook(grid = grid, latitude = latitude, longitude = longitude))
  print(first_population[[1]][x,]$Move)
  latitude <- latitude + 1
  longitude <- longitude + 1
}

move_score <- function(individual, grid, latitude, longitude, steps, score){

  for(i in 1:steps){
  # current situation the robot finds itself in
  situation <- as.numeric(lookup_handbook(grid, latitude, longitude))
  # next move that will be performed according to the handbook
  next_move <- individual[situation,]$Move

  # needed for the moving and scoring:
  # coordintes of the current field and north east south and west
  yaxis <- c(latitude, latitude - 1, latitude, latitude + 1, latitude)
  xaxis <- c(longitude, longitude, longitude + 1, longitude, longitude - 1)
  # content on the current field and the fields around the robot
  content_current <- grid[yaxis[1], xaxis[1]]
  content_north <- grid[yaxis[2], xaxis[2]]
  content_east <- grid[yaxis[3], xaxis[3]]
  content_south <- grid[yaxis[4], xaxis[4]]
  content_west <- grid[yaxis[5], xaxis[5]]


  # change of the current position of the robot according to the move that
  # corresponds to his current position
  if (next_move == "North" & content_north != "Wall"){
    latitude  <- latitude - 1
    longitude <- longitude
  } else if (next_move == "East" & content_east != "Wall"){
    latitude <- latitude
    longitude <- longitude + 1
  } else if (next_move == "South" & content_south != "Wall"){
    latitude <- latitude + 1
    longitude <- longitude
  } else if (next_move == "West" & content_west != "Wall"){
    latitude <- latitude
    longitude <- longitude + 1

    # if he moves into a wall he bounces back to his old position and is fined 5 points
  } else  if (next_move == "North" & content_north == "Wall"){
    latitude <- latitude
    longitude <- longitude
    score <- score - 5
  } else if (next_move == "East" & content_east == "Wall"){
    latitude <- latitude
    longitude <- longitude
    score <- score - 5
  } else if (next_move == "South" & content_south == "Wall"){
    latitude <- latitude
    longitude <- longitude
    score <- score - 5
  } else if(next_move == "West" & content_west == "Wall"){
    latitude <- latitude
    longitude <- longitude
    score <- score - 5
  }

  # if he picks-up his location doesnt change
  else if (next_move == "Stay"){
    latitude <- latitude
    longitude <- longitude
  }
  # if he picks up sth his location doesnt change but the environment changes
  else if(next_move == "Pick-Up" & content_current == "Evidence"){
    latitude <- latitude
    longitude <- longitude
    grid[xaxis[1], yaxis[1]] <- "Empty"
    score <- score + 10
  }
  # if he picks up but there is nothing, he is fined
  else if(next_move == "Pick-Up" & content_current == "Empty"){
    latitude <- latitude
    longitude <- longitude
    score <- score - 1

  }
  }
  return(data.frame(latitude, longitude, score))
}

# run this function for all individuals
for(i in 1:length(first_population)){
print(move_score(first_population[[i]], grid, latitude = 2, longitude = 2,
                 steps = 100, score = 0))
}


# Do the following 100 times
# After 200 steps in one grid configuration, change grid

grid <- NULL
grid <- create_grid(coordinates_grid = c(0, 10, 0, 10), n_evidence = 8)

# let the robot walk again
