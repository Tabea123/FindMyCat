# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# packages that are required
devtools::use_package("gtools")

## Create the first population with random movements
# Creating a population of x individuals with y moves
# A population that contains x individual solutions


#' Creating a population of x individuals
#'
#' @param individuals
#'
#' @return
#' @export
#'
#' @examples
create_population <- function(individuals){

  # funktioniert nicht :(
  if (!requireNamespace("gtools", quietly = TRUE)) {
    stop("Package \"gtools\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(is.numeric(individuals) == FALSE){
    stop ("argument 'individuals' must be numeric") # individuals has to be numeric
  }

  if(individuals <= 5){
    stop ("The genepool of your population is too small.
          The population needs to have more than five individuals")
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

first_population <- create_population(individuals = 20) # this functopn takes 0.007 secs

## Create the grid
# maybe delete n_evidence
create_grid <- function(grid_size, n_evidence){

  if(!any(is.numeric(grid_size))){
    # grid_size have to be numeric
    stop ("The coordinates have to be numeric")
  }

  if(length(grid_size) != 2){
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
grid <- matrix(NA, grid_size[1], grid_size[2])
grid[0:nrow(grid),0:ncol(grid)] <- "Empty"
# create evidence randomly and add it to the grid
evidence <- sample(0:grid_size[1]^2, n_evidence, replace = F)
grid[evidence] <- "Evidence"
grid <- rbind(grid, matrix("Wall", nrow = 1, ncol = grid_size[2]))
grid <- rbind(matrix("Wall", nrow = 1, ncol = grid_size[2]), grid)
grid <- cbind(grid, matrix("Wall", ncol = 1, nrow = grid_size[2]+2))
grid <- cbind(matrix("Wall", ncol = 1, nrow = grid_size[2]+2), grid)

return(grid)
}

grid <- create_grid(grid_size = c(10, 10), n_evidence = 11) # this function takes 0.001 secs


#' A function that retrieves the number in the handbook of the current situation

lookup_situation <- function(grid, latitude = 2, longitude = 2){

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
  yaxis <- c(latitude, latitude - 1, latitude, latitude + 1, latitude)
  xaxis <- c(longitude, longitude, longitude + 1, longitude, longitude - 1)

  # looking up which number in the handbook corresponds to the current situation
  number <- which(situation[,1] == grid[yaxis[1], xaxis[1]] &
          situation[,2] == grid[yaxis[2], xaxis[2]] &
          situation[,3] == grid[yaxis[3], xaxis[3]] &
          situation[,4] == grid[yaxis[4], xaxis[4]] &
          situation[,5] == grid[yaxis[5], xaxis[5]])

  return(number)
}

# This function takes 0.0009 sec

# roxgen is missing
# create a function that moves the robot through the grid according to his handbook
move_score <- function(individual, grid, latitude = 2, longitude = 2, steps, score = 0){

  for(i in 1:steps){
  # current situation the robot finds itself in
  situation <- as.numeric(lookup_situation(grid, latitude, longitude))
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
    longitude <- longitude - 1

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

    # if he picks-up his location doesnt change
  } else if (next_move == "Stay"){
    latitude <- latitude
    longitude <- longitude

    # if he picks up sth his location doesnt change but the environment changes
  } else if(next_move == "Pick-Up" & content_current == "Evidence"){
    latitude <- latitude
    longitude <- longitude
    grid[xaxis[1], yaxis[1]] <- "Empty"
    score <- score + 10
    # if he picks up but there is nothing, he is fined
  } else if(next_move == "Pick-Up" & content_current == "Empty"){
    latitude <- latitude
    longitude <- longitude
    score <- score - 1
  }
  }

  return(data.frame(latitude, longitude, score))
}

move_score(first_population[[5]], grid, steps = 100, score = 0) # This function takes 0.08 sec

life <- function(population, steps, repetitions, grid_size, n_evidence){
  scores <- matrix(0, nrow = length(population), ncol = repetitions)
  for(j in 1:repetitions){
    for(i in 1:length(population)){
      scores[i,j] <- move_score(population[[i]], grid, steps = steps)$score
    }
    grid <- create_grid(grid_size = grid_size, n_evidence = n_evidence)
  }
return(scores)
}

all_scores <- life(first_population, 50, 30, grid_size = c(10, 10),
                   n_evidence = 11)

evolve <- function(population, all_scores){
  mean_scores <- apply(all_scores, 1, mean)

  best_scores <- which(mean_scores == sort(mean_scores, decreasing = TRUE)[1] |
          mean_scores == sort(mean_scores, decreasing = TRUE)[2])

  parent1 <- population[[best_scores[1]]]
  parent2 <- population[[best_scores[2]]]

  new_population <- list()
  for(i in 1:length(population)){

  # recombination of parental genetic material at a random section
  genetic_recombination <- sample(1:nrow(parent1), 1)
  genetic_material1 <- parent1[1:genetic_recombination,]
  genetic_material2 <- parent2[(genetic_recombination+1):nrow(parent2),]
  new_population[[i]] <- rbind(genetic_material1, genetic_material2)

  # mutation
  mutation <- sample(1:nrow(parent1), 1)
  moves1 <- c("North", "East", "South", "West", "Stay")
  moves2 <- c("North", "East", "South", "West", "Stay", "Pick-Up")

  if(mutation == 1){
  # stay can't be the first move
  new_population[[i]][mutation,]$Move <- sample(moves1, 1)
  } else {
  new_population[[i]][mutation,]$Move <- sample(moves2, 1)
  }

  }
  return(new_population)
}


evolution <- function(population_size, grid_size, n_evidence, steps, repetitions, generations){
  # create first population
  population <- create_population(population_size)
  mean_population <- numeric(5)
  for(i in 1:generations){
  # create grid
  grid <- create_grid(grid_size, n_evidence)
  # let this population walk in one grid for x steps then change the grid
  # repeat this procedure for y repetitions
  all_scores <- life(population, steps, repetitions, grid_size, n_evidence)
  mean_scores <- apply(all_scores, 1, mean)
  mean_population[i] <- mean(mean_scores)
  # chose the two best strategies and recombine them with mutations to a new population
  population <- evolve(population, all_scores)
  }
  return(mean_population)
}
