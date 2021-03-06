# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# packages that are required
usethis::use_package("gtools")


#' Creat A Grid
#'
#' \code{create_grid} creates a grid from the given set of values.
#'
#' @usage create_grid(grid_size, n_evidence)
#'
#' @param grid_size a numeric vector of the form c(nrows, ncolumns)
#' which gives the size of the grid in x and y direction.
#' @param n_evidence a number specifing the amount of evidence in the grid.
#'
#' @details The user can specify how big the grid is and how much evidence will
#' be placed in the grid. The function first creates a grid that is completely
#' empty. The evidence is sampled and added to the grid at random. In a last step
#' a wall is being built around the grid.
#'
#' @return A matrix containing a grid.
#' @export
#'
#' @examples
#' create_grid(grid_size = c(10, 10), n_evidence = 5)
create_grid <- function(grid_size, n_evidence){

  if(!any(is.numeric(grid_size))){
    stop ("Grid coordinates of grid boundaries have to be numeric.")
  }
  if(length(grid_size) != 2){
    stop ("Give the length of the grid on the y-axis and x-axis.")
  }
  if(is.numeric(n_evidence) == FALSE){
    stop ("Indicate how much evidence (numeric) you want to place in the grid.")
  }
  if(n_evidence <= 0){
    stop ("Number evidence has to be more than 0.")
  }
  if(n_evidence > grid_size[1]*grid_size[2]){
    stop ("There can not be more evidence than fields in the grid.")
  }

  # create an empty grid
  # the size is specified by the user
  grid <- matrix(NA, grid_size[1], grid_size[2])
  grid[0:nrow(grid),0:ncol(grid)] <- "Empty"

  # sample evidence randomly and add it to the grid
  evidence <- sample(0:(grid_size[1]*grid_size[2]), n_evidence, replace = F)
  grid[evidence] <- "Evidence"

  # in a last step the walls are added to the grid
  # this is done with cbind() and rbind() to avoid overwriting evidence
  grid <- rbind(grid, matrix("Wall", nrow = 1, ncol = grid_size[2]))
  grid <- rbind(matrix("Wall", nrow = 1, ncol = grid_size[2]), grid)
  grid <- cbind(grid, matrix("Wall", ncol = 1, nrow = grid_size[1]+2))
  grid <- cbind(matrix("Wall", ncol = 1, nrow = grid_size[1]+2), grid)

  return(grid)
}


#' Create A Population
#'
#' \code{create_poulation} A function to create the primary population of
#' candidate strategies.
#'
#' @usage create_population(individuals)
#'
#' @param individuals a number indicating how many individuals are in one population.
#'
#' @details There are five sites the robot can see: the current field and the
#' fields north, east, south, and west of the robot. Each of the sites can
#' contain a wall, evidence or nothing. Consequently, the robot can possibly be
#' in 243 situations. In each situation, the robot can perform one of the following
#' six actions: move north, east, south, and west, stay in one field, or pick up
#' something. An individual is one specific strategy to move through the grid.
#' \code{create_poulation} creates for each individual in the population a random
#' strategy table:
#' 243 moves are randomly sampled and assigned to each of the situations.
#'
#' @return A list containing a population of x data.frames of solutions.
#' @export
#'
#' @examples
#' create_population(100)
create_population <- function(individuals){

  if (!require("gtools")) {
    stop("Package \"gtools\" needed for this function to work. Please install it.")
  }
  if(!is.numeric(individuals)){
    stop ("Argument 'individuals' must be numeric.")
  }
  if(individuals <= 5){
    stop ("The genepool of your population is too small.
          The population needs to have more than five individuals.")
  }

  # There are five different sites each with three possibles types of content
  sites   <- c("Current", "North", "East", "South", "West")
  content <- c("Wall", "Empty", "Evidence")
  # There are 243 possible situations in total (permutation of sites and content)
  situations <- data.frame(permutations(n = length(content), r = length(sites),
                                        v = content, repeats.allowed = T))
  colnames(situations) <- sites

  # In each situation, the robot can perform one of the following six actions:
  # move north, east, south, and west, stay in one field, or pick up something.
  moves <- c("North", "East", "South", "West", "Stay", "Pick-Up")
  Move  <- character(nrow(situations))

  population_of_strategies <- list()

  # for the initial population, strategy tables are created by storing
  # randomly sampled movements next to situations
  for (j in 1:individuals){
    # a random sequence of 243 movements is created
    Move <- sample(moves, nrow(situations), replace = T)

    # the movements are stored next to the situations
    individual_strategy <- data.frame(situations, Move)

    # creating a dataframe with all 200 individuals
    population_of_strategies[[j]] <- individual_strategy
  }
  return(population_of_strategies)
}


#' Retrieve from the Strategy Table the Number of the Current Situation
#'
#' \code{lookup_situation} retrieves the robots current situation in his strategy
#' table.
#'
#' @param individual one individual strategy taken form the population made with
#' \code{create_poulation}.
#' @param grid a matrix made with \code{create_grid}.
#' @param latitude a number indicating the current position of the robot on the
#' y-axis.
#' @param longitude a number indicating the current position of the robot on the
#' x-axis.
#'
#' @details To decide which move to perform next, the robot looks up his current
#' situation in his strategy table. There he finds the corresponding action.
#'
#' @return A number from the robots' strategy table.
#' @export
#'
#' @examples
#' population1 <- create_population(50)
#' my_grid <- create_grid(c(5, 5), 8)
#' lookup_situation(individual = population1[[2]], grid = my_grid, latitude = 9,
#' longitude = 5)
lookup_situation <- function(individual, grid, latitude = 2, longitude = 2){

  if(class(individual) != "data.frame"){
    stop ("Provide a data.frame containing the individual strategy table.")
  }
  if(class(grid) != "matrix"){
    stop ("Provide a matrix containing the grid.")
  }
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

  # extracting the possible situation from the individual strategy table
  situation <- individual[,1:5]

  # coordintes of the current field and north east south and west
  yaxis <- c(latitude, latitude - 1, latitude, latitude + 1, latitude)
  xaxis <- c(longitude, longitude, longitude + 1, longitude, longitude - 1)

  # looking up which number in the strategy table corresponds to the current
  # situation
  number <- which(situation[,1] == grid[yaxis[1], xaxis[1]] &
                  situation[,2] == grid[yaxis[2], xaxis[2]] &
                  situation[,3] == grid[yaxis[3], xaxis[3]] &
                  situation[,4] == grid[yaxis[4], xaxis[4]] &
                  situation[,5] == grid[yaxis[5], xaxis[5]])

  return(as.numeric(number))
}


#' Move the Robot and Score his Actions
#'
#' \code{move_score} is used to move the robot through the grid according to his
#' strategy table and to score his actions.
#'
#' @usage move_score(individual, grid, latitude, longitude, steps)
#'
#' @param individual one individual strategy taken form the population made with
#' \code{create_poulation}.
#' @param grid an object of class matrix made with \code{create_grid}.
#' @param latitude a number indicating the current position of the robot on the
#' y-axis.
#' @param longitude a number indicating the current position of the robot on the
#' x-axis.
#' @param steps a number indicating how many moves the robot should walk in the
#' grid.
#'
#' @details The robot begins at his starting position (latitude = 2, longitude = 2).
#' The robot then follows one strategy for X actions. The number of actions is
#' indicated by the user with the argument \code{steps}. If there is no wall in the
#' direction of his next movement, the robot walks. If his next action is to stay,
#' the robot stops moving.
#'
#' The score of the strategy is the number of bonus- and minuspoints the
#' robot accumulates in a session.
#' If the robot is in the same site as a piece of evidence and picks it up,
#' he gets ten points.
#' If he bends down to pick up in a site where there is no evidence, he is fined
#' one point.
#' If he crashes into a wall, he is fined five points and bounces back into the
#' current site.
#'
#' @return The robots position after taking all steps and the resulting score.
#' @export
#'
#' @examples
#' population <- create_population(100)
#' grid <- create_grid(c(10, 12), 11)
#' move_score(population[[1]], grid = grid, latitude = 5, longitude = 5, steps = 100)
move_score <- function(individual, grid, latitude = 2, longitude = 2, steps){

  if(class(individual) != "data.frame"){
    stop ("Provide a data.frame containing the individual strategy table.")
  }
  if(class(grid) != "matrix"){
    stop ("Provide a matrix containing the grid.")
  }
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
  if(class(steps) != "numeric"){
    stop ("Indicate how many steps the robot walks in one grid configuration.")
  }

  score <- 0

  for(i in 1:steps){
  # current situation of the robot
  situation <- lookup_situation(individual, grid, latitude, longitude)

  # next move that will be performed according to the strategy table
  next_move <- as.character(individual[situation,]$Move)

  # the first move cannot be stay; otherwise the robot gets caught up in an endless
  # loop and makes no minus- or bonuspoints
  # this skews the fitness of this strategy, making it score better than it is
  if(i == 1 & next_move == "Stay"){
    first_move <- c("North", "East", "South", "West", "Pick-Up")
    next_move  <- sample(first_move, 1, replace = F)
    individual[situation,]$Move <- next_move
  }

  # needed for the moving and scoring:
  # coordintes of the current field and north east south and west
  yaxis <- c(latitude, latitude - 1, latitude, latitude + 1, latitude)
  xaxis <- c(longitude, longitude, longitude + 1, longitude, longitude - 1)
  # content on the current field and the fields around the robot
  content_current <- grid[yaxis[1], xaxis[1]]
  content_north   <- grid[yaxis[2], xaxis[2]]
  content_east    <- grid[yaxis[3], xaxis[3]]
  content_south   <- grid[yaxis[4], xaxis[4]]
  content_west    <- grid[yaxis[5], xaxis[5]]


  # change of the current position of the robot according to the move that
  # corresponds to his current position
  if (next_move == "North" & content_north != "Wall"){
    latitude  <- latitude - 1
    longitude <- longitude
  } else if (next_move == "East" & content_east != "Wall"){
    latitude  <- latitude
    longitude <- longitude + 1
  } else if (next_move == "South" & content_south != "Wall"){
    latitude  <- latitude + 1
    longitude <- longitude
  } else if (next_move == "West" & content_west != "Wall"){
    latitude  <- latitude
    longitude <- longitude - 1

    # if he moves into a wall he bounces back to his old position and is fined
    # 5 points
  } else  if (next_move == "North" & content_north == "Wall"){
    latitude  <- latitude
    longitude <- longitude
    score <- score - 5
  } else if (next_move == "East" & content_east == "Wall"){
    latitude  <- latitude
    longitude <- longitude
    score <- score - 5
  } else if (next_move == "South" & content_south == "Wall"){
    latitude  <- latitude
    longitude <- longitude
    score <- score - 5
  } else if(next_move == "West" & content_west == "Wall"){
    latitude  <- latitude
    longitude <- longitude
    score <- score - 5

    # if he stays his location doesnt change
  } else if (next_move == "Stay"){
    latitude  <- latitude
    longitude <- longitude

    # if he picks up a piece of evidence succesfully his location doesnt change
    # but the environment changes because his current site is then empty
    # he also receives 10 bonuspoints
  } else if(next_move == "Pick-Up" & content_current == "Evidence"){
    latitude  <- latitude
    longitude <- longitude
    grid[latitude, longitude] <- "Empty"
    score <- score + 10
    # if he picks up but there is nothing, he is fined 1 point
  } else if(next_move == "Pick-Up" & content_current == "Empty"){
    latitude  <- latitude
    longitude <- longitude
    score <- score - 1
  }
  }
  return(data.frame(latitude, longitude, score))
}


#' One Life Cycle of a Population
#'
#' \code{life_cycle} is used to simulate one life cycle of a population.
#'
#' @usage life_cycle(population, grid_size, n_evidence, steps, sessions)
#'
#' @param population a list containing a population made with \code{create_population}.
#' @param grid_size a numeric vector of the form c(nrow, ncol)
#' which gives the size of the grid.
#' @param n_evidence a number specifing the amount of evidence in the grid.
#' @param steps a number indicating how many moves the robot should walk in one
#' grid configuration.
#' @param sessions a number indicating how many times the grid configuration is
#' changed.
#'
#' @details The fitness of an individual strategy is determined by seeing how well
#' the strategy works in X different sessions.
#'
#' In one session, the robot walks X steps in a grid and is scored on his actions
#' (see function \code{move_score}).
#' A new session is initiated with changing the configuration of the grid. Consequently,
#' the robot walks again and is scored. The number of sessions is indicated by
#' the user with the argument \code{sessions}.
#'
#' This procedure is done for every individual strategy in the population.
#'
#' @return The score for each individual strategy in each session.
#' @export
#'
#' @examples
#' first_population <- create_population(50)
#' life_cycle(population = first_population, grid_size = c(10, 10), n_evidence = 10,
#' steps = 100, sessions = 200)
life_cycle <- function(population, grid_size, n_evidence, steps, sessions){

  if(class(population) != "list" && class(population[[1]]) != "data.frame"){
    stop ("Provide a population containing a list of individual strategy tables.")
  }
  if(class(sessions) != "numeric"){
    stop ("Indicate how many times the grid configuration should change.")
  }
  if(sessions <= 1){
    stop ("There has to be more than one session.")
  }

  # an empty matrix of scores
  # each row represents one individual, each columns represents one session with one
  # grid configuration
  scores <- matrix(0, nrow = length(population), ncol = sessions)

  # loops over all individuals
  # repeats move_score and change of grid as many times as indicates by the user
  # with sessions
  for(j in 1:sessions){
  for(k in 1:length(population)){
      grid <- create_grid(grid_size, n_evidence)
      scores[k,j] <- move_score(population[[k]], grid, steps = steps)$score
    }
  }
  return(scores)
}


#' Creating the next Population
#'
#' \code{next_generation} is used to apply genetic recombination and mutation
#' to the current population of strategies to create a new population.
#'
#' @usage next_generation(population, all_scores)
#'
#' @param population a list containing an initial population made with
#' \code{create_population}.
#' @param all_scores a matrix containing all scores for each individual and each
#' repetition made with \code{life_cycle}.
#'
#' @details Evolution works in the following way:
#' The strategies with the two highest scores are chosen as the parent individuals.
#' The two parents are mated to create offspring.
#' A position at which to split the two parent stategies is randomly chosen.
#' The offspring receives the genetic material from parent A before that position
#' and from parent B after that position.
#' Note: This is not done vice versa; otherwise the same situation can possibly
#' exist twice in one strategy table.
#'
#' With a small probability, 1 to 5 mutations occur in a random row of the strategy
#' table. A mutation replaces the move that is inherited by the parent with a
#' random movement.
#'
#' The function creates as much offspring as there were individuals in the previous
#' population to fill up the new population.
#'
#' @return a new population of the same size as the previous population.
#' @export
#'
#' @examples
#' first_population <- create_population(100)
#' scores <- life_cycle(first_population, c(5, 5), 9, 100, 200)
#' next_generation(first_population, scores)
next_generation <- function(population, all_scores){

  if(class(population) != "list" && class(population[[1]]) != "data.frame"){
    stop ("Provide a population containing a list of individual strategy tables.")
  }
  if(class(all_scores) != "matrix"){
    stop ("Provide a matrix containing the scores of each strategy in the different sessions.")
  }

  # calculation of mean scores per individual over the different sessions
  mean_scores <- apply(all_scores, 1, mean)

  # chose the two individuals with the best scores as the parents of the new population
  best_scores <- which(mean_scores == sort(mean_scores, decreasing = TRUE)[1] |
                       mean_scores == sort(mean_scores, decreasing = TRUE)[2])

  parent1 <- population[[best_scores[1]]]
  parent2 <- population[[best_scores[2]]]

  new_population <- list()

  for(i in 1:length(population)){

    # recombination of parental genetic material at a random section
    genetic_recombination <- sample(1:nrow(parent1), 1)
    genetic_material1     <- parent1[1:genetic_recombination,]
    genetic_material2     <- parent2[(genetic_recombination+1):nrow(parent2),]

    # offspring is created by combining the parental genetic material
    new_population[[i]]   <- rbind(genetic_material1, genetic_material2)

    # with a small probability movements in offspring mutates
    possible_mutation     <- sample(c(TRUE, FALSE), 1, prob = c(0.2, 0.8))

     if(possible_mutation == TRUE){
      # sampling how many mutations should occur
      # I chose a maximum of 5 mutations arbitrarly
      n_mutations <- sample(1:5, 1)

      # randomly chosing rows that will be mutated
      mutation <- sample(1:nrow(parent1), n_mutations)
      moves    <- c("North", "East", "South", "West", "Stay", "Pick-Up")
      new_population[[i]][mutation,]$Move <- sample(moves, length(mutation))
     }

    # the row numbers are messed up because of the combination of parental material
    # changing the row numbers back to 1:243
    rownames(new_population[[i]]) <- 1:nrow(new_population[[i]])
  }
  return(new_population)
}


#' Evolution of the Best Strategy
#'
#' \code{evolution} is used to evolve the best strategy table.
#'
#' @usage evolution(population_size, grid_size, n_evidence, steps, sessions,
#' generations)
#'
#' @param population_size a number indicating the amount of individuals in each
#' population.
#' @param grid_size a numeric vector of the form c(nrows, ncolumns)
#' which gives the size of the grid in x and y direction.
#' @param n_evidence a number specifing the amount of evidence in the grid.
#' @param steps a number indicating how many moves the robot should walk in one
#' grid configuration.
#' @param sessions a number indicating how many times the grid configuration is
#' changed in one life cycle.
#' @param generations a number indicating how many populations should be evolved.
#'
#' @details \code{evolution} calls all subordinate functions.
#' First, the initial population of random strategies is generated with
#' \code{create_population}.
#' The fitness of each individual strategy is calculated with the function \code{life_cycle}.
#' Fitness is determined by seeing how well a strategy lets the robot do on X
#' different sessions. The number of \code{sessions} is determined
#' by the user. One session consists of putting the robot at his starting position
#' on a grid and then letting the robot move for X \code{steps}.
#' The score of the strategy in each session is the number of bonus- and minuspoints
#' the robot accumulates. The strategy’s fitness is its average score over the
#' different sessions, each of which has a different grid configuration.
#' A grid is generated with the arguments \code{grid_size} and \code{n_evidence}.
#' The position of the evidence is randomly chosen.
#'
#' With the function \code{next_generation} evolution is applied to the current
#' population to create a new population of strategies.
#' The two individuals from the current population with the highest scores are chosen
#' as the parents of the new generation. The two parents are mated to create offspring.
#' At a randomly chosen position the parental strategies are split. Offpring is
#' created by recombining the pieces of parental material. With a small probability,
#' mutations accur. Offspring is generated until the next population has the same
#' amount of individuals as the first generation.
#'
#' This procedure is repeated for as many \code{generations} as the user indicates.
#' \code{evolution} returns the best strategy table.
#'
#' @return the individual with the highest score.
#' @export
#'
#' @examples
#' evolution(100, c(10, 10), 11, 100, 50, 1000)
evolution <- function(population_size, grid_size, n_evidence, steps, sessions, generations){

  if(class(generations) != "numeric"){
    stop ("Indicate with a number how many generations you want the evolution to last.")
  }

  if(length(generations) > 1){
    stop ("Indicate with only one number how many generations you want the evolution to last.")
  }

  # create first populationg
  population <- create_population(population_size)

  for(i in 1:generations){

    # the function is very slow; printing will let the user know, what is currently
    # happening behind closed curtains
    print(paste("generation", i, "in progress"))

    # let this population walk in one grid for x steps then change the grid
    # repeat this procedure for y sessions
    all_scores <- life_cycle(population, grid_size, n_evidence, steps, sessions)

    # chose the two best strategies and recombine them with mutations to a new population
    population <- next_generation(population, all_scores)
  }

  # retrieving the scores from the last population
  all_scores <- life_cycle(population, grid_size, n_evidence, steps, sessions)

  # chosing the individual with the highest score
  x <- which.max(apply(all_scores, 1, mean))
  individual <- population[[x]]

  return(individual)
}

