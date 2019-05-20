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

  # storing the movements next to the situations
  individual_solution <- data.frame(situations, Move)

  # creating a dataframe with all 200 individuals
  solutions_population[[j]] <- individual_solution
}
return(solutions_population)
}

first_population <- create_population(individuals = 20)

## Create the grid
create_grid <- function(coordinates_grid, n_evidence){

  if(!any(is.numeric(coordinates_grid))){
    # coordinates_grid have to be numeric
    stop ("The coordinates have to be numeric")
  }

  if(length(coordinates_grid) != 4){
    stop ("Give four coordinates of the grid boundaries in the following manner:
          x start, x end, y start, y end")
  }

  if(coordinates_grid[2] < coordinates_grid[1] | coordinates_grid[4] < coordinates_grid[3]){
    stop ("Give four coordinates of the grid boundaries in the following manner:
          x start, x end, y start, y end. Ends have to be higher than starts.")
  }

  if(is.numeric(n_evidence) == FALSE){
    # number of evidence have to be numeric
    stop ("Indicate how much evidence (numeric) you want to place in the grid.")
  }

  if(n_evidence <= 0){
    stop ("Number evidence has to be more than 0.")
  }

# Create an empty grid and add the walls
grid <- matrix(NA, coordinates_grid[2] + 2, coordinates_grid[4] + 2)
grid[,1] <- grid[1,] <- grid[,12] <- grid[12,] <-  "Wall"
grid[2:11,2:11] <- "Empty"
# create evidence randomly and add it to the grid
evidence <- sample(1:coordinates_grid[4]^2-1, n_evidence, replace = F)
grid[evidence] <- "Evidence"
return(grid)
}

grid <- create_grid(coordinates_grid = c(0, 10, 0, 10), n_evidence = 11)

# size of territory should be given via input by the user with the shiny app


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

situation <- numeric(length(first_population))
for (i in 1:length(first_population)){
  situation[i] <- as.numeric(lookup_handbook(grid = grid))
}


# roxgen is missing
# create a function that moves the robot through the grid according to his handbook
move_score <- function(individual, grid, situation, latitude, longitude, steps, score){

  next_move     <- individual[situation,]$Move

  latitude <- c(latitude, latitude - 1, latitude, latitude + 1, latitude)
  longitude <- c(longitude, longitude, longitude + 1, longitude, longitude - 1)
  content_current <- grid[latitude[1], longitude[1]]
  content_north <- grid[latitude[2], longitude[2]]
  content_east <- grid[latitude[3], longitude[3]]
  content_south <- grid[latitude[4], longitude[4]]
  content_west <- grid[latitude[5], longitude[5]]

  for(i in 1:steps){
  # change of the current position of the robot according to the move that
  # corresponds to his current position
  if (next_move == "North" & content_north != "Wall"){
    latitude[1]  <- latitude[1] - 1
    longitude[1] <- longitude[1]
  } else if (next_move == "East" & content_east != "Wall"){
    latitude[1] <- latitude[1]
    longitude[1] <- longitude[1] + 1
  } else if (next_move == "South" & content_south != "Wall"){
    latitude[1] <- latitude[1] + 1
    longitude[1] <- longitude[1]
  } else if (next_move == "West" & content_west != "Wall"){
    latitude[1] <- latitude[1]
    longitude[1] <- longitude[1] + 1

    # if he moves into a wall he bounces back to his old position and is fined 5 points
  } else  if (next_move == "North" & content_north == "Wall"){
    latitude[1] <- latitude[1]
    longitude[1] <- longitude[1]
    score <- score - 5
  } else if (next_move == "East" & content_east == "Wall"){
    latitude[1] <- latitude[1]
    longitude[1] <- longitude[1]
    score <- score - 5
  } else if (next_move == "South" & content_south == "Wall"){
    latidue <- latitude[1]
    longitude[1] <- longitude[1]
    score <- score - 5
  } else if(next_move == "West" & content_west == "Wall"){
    latitude[1] <- latitude[1]
    longitude[1] <- longitude[1]
    score <- score - 5
  }

  # if he picks-up his location doesnt change
  else if (next_move == "Stay"){
    latitude[1] <- latitude[1]
    longitude[1] <- longitude[1]
  }
  # if he picks up sth his location doesnt change but the environment changes
  else if(next_move == "Pick-Up" & content_current == "Evidence"){
    latitude[1] <- latitude[1]
    longitude[1] <- longitude[1]
    grid[latitude[1], longitude[1]] <- "Empty" # das grid aendert sich nur für den
    score <- score + 10
  }
  # if he picks up but there is nothing, he is fined
  else if(next_move == "Pick-Up" & content_current == "Empty"){
    latitude[1] <- latitude[1]
    longitude[1] <- longitude[1]
    score <- score - 1

  }
  }
  return(data.frame(latitude = latitude[1], longitude = longitude[1], score))
}
move_score2(first_population[[1]], grid, situation[1], latitude = 2, longitude = 2,
            steps = 10, score = 0)


# creating a data.frame with the current coordinates of the robot
latitude <-  data.frame(matrix(rep(c(1, 0, 1, 2, 1), each = length(first_population)),
                               ncol = 5, nrow = length(first_population)))
longitude <-  data.frame(matrix(rep(c(1, 1, 2, 1, 0), each = length(first_population)),
                                ncol = 5, nrow = length(first_population)))

score <- numeric(length(first_population))

# let the robot walk 10 steps

# umdrehen: erst läuft ein individuum x schritte und nimmt auch das grid mit wegen pick-up
# und dann lässt man diese funktion für alle individuen laufen
for (i in 1:100){
new_coordinates <- move_score(population = first_population,
                              situation = situation,
                              latitude = latitude, longitude = longitude,
                              score = score)

print(new_coordinates)
latitude <- new_coordinates[,1:5]
longitude <- new_coordinates[,6:10]
score <- as.numeric(new_coordinates$score)

for (j in 1:nrow(longitude)){
  handbook_number[j] <- as.numeric(lookup_handbook(grid, as.numeric(latitude[j,]),
                                                   as.numeric(longitude[j,])))
}
print(handbook_number)
}

# Do the following 100 times
# After 200 steps in one grid configuration, change grid

grid <- NULL
grid <- create_grid(coordinates_grid = c(0, 10, 0, 10), n_evidence = 8)

# let the robot walk again
for (i in 1:10){
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
      grid = grid, latitude = as.numeric(latitude[j,]),
      longitude = as.numeric(longitude[j,])))
  }
}
