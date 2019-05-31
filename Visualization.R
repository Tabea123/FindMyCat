usethis::use_package("ggplot2")
usethis::use_package("gganimate")
usethis::use_package("ggimage")

# Storing the coordinates

individual <- evolution(100, c(10, 10), 11, 100, 10, 15)

steps <- 30

grid_size <- c(10, 10)
evidence_latitude <- c(3, 6, 4, 2, 1)
evidence_longitude <- c(2, 4, 8, 1, 9)

#' Documentation of the Path
#'
#' \code{path} is used to move the robot through the grid according to his
#' strategy table and to document his path.
#'
#' @usage path(individual, grid_size, evidence_latitude, evidence_longitude, steps)
#'
#' @param individual one individual strategy taken form the population made with
#' \code{create_poulation}.
#' @param grid_size a numeric vector of the form c(nrows, ncolumns)
#' which gives the size of the grid in x and y direction.
#' @param evidence_latitude a string indicating the position of the evidence on
#' the x-axis.
#' @param evidence_longitude a string indicating the position of the evidence
#' on the y-axis.
#' @param steps a number indicating how many moves the robot should walk in the
#' grid.
#' @param animal the animal that is being searched.
#'
#' @details The robot begins his path at his starting position (latitude = 2, longitude = 2).
#' The robot then follows the given strategy \code{individual} for X \code{steps}.
#' If there is no wall in the direction of his next movement, the robot walks.
#' If he crashes into a wall, he is fined five points and bounces back into the
#' current site.
#' If his next action is to stay or to pick up, the robot stops moving.
#' The robots path is visualized with \code{ggplot}.
#'
#' @return a plot showing the path
#' @export
#'
#'
#'
#' @examples
#' population1 <- create_population(30)
#' visualize_path(individual = population1[[3]], grid_size = c(5, 7), evidence_latitude =
#' c(3, 2, 4), evidence_longitude = c(1, 6, 5), steps = 30, animal = bird)
visualize_path <- function(individual, grid_size, evidence_latitude,
                           evidence_longitude, steps, animal){

  if(class(individual) != "data.frame"){
    stop ("Provide a data.frame containing the individual strategy table.")
  }
  if(!any(is.numeric(grid_size))){
    stop ("Grid coordinates of grid boundaries have to be numeric.")
  }
  if(length(grid_size) != 2){
    stop ("Give the length of the grid on the y-axis and x-axis.")
  }
  if(is.numeric(evidence_latitude) == FALSE){
    stop ("Give the coordiantes of the evidence on the latitude as numbers.")
  }
  if(is.numeric(evidence_longitude) == FALSE){
    stop ("Give the coordiantes of the evidence on the latitude as numbers.")
  }
  if(length(evidence_latitude) <= 0 || length(evidence_longitude) <= 0){
    stop ("If there is no evidence, this function is useless!")
  }
  if(length(evidence_latitude) != length(evidence_longitude)){
    stop ("Always indicate the position of the evidence by giving the corresponding
          latitude and longitude.")
  }
  if(length(evidence_latitude) > grid_size[1]*grid_size[2]){
    stop ("There can not be more evidence than fields in the grid.")
  }
  if(class(steps) != "numeric"){
    stop ("Indicate how many steps the robot walks in one grid configuration.")
  }
  if(animal != cat | horse | bird | bear){
    stop ("The function can only display the footprints of cats, horses, birds, and bears.")
  }

# create an empty grid
# the size is specified by the user
grid <- matrix(NA, grid_size[1], grid_size[2])
grid[0:nrow(grid),0:ncol(grid)] <- "Empty"

# sample evidence randomly and add it to the grid
for(i in 1:length(evidence_latitude)){
grid[evidence_latitude[i], evidence_longitude[i]] <- "Evidence"
}

# in a last step the walls are added to the grid
# this is done with cbind() and rbind() to avoid overwriting evidence
grid <- rbind(grid, matrix("Wall", nrow = 1, ncol = grid_size[2]))
grid <- rbind(matrix("Wall", nrow = 1, ncol = grid_size[2]), grid)
grid <- cbind(grid, matrix("Wall", ncol = 1, nrow = grid_size[1]+2))
grid <- cbind(matrix("Wall", ncol = 1, nrow = grid_size[1]+2), grid)

# creating two empty vectors to store the coordinates
latitude <- numeric(steps)
longitude <- numeric(steps)
# starting point is (2,2)
latitude[1] <- 2
longitude[1] <- 2

for(i in 2:steps){

  # current situation the robot finds itself in
  situation <- lookup_situation(individual, grid, latitude[i-1], longitude[i-1])
  # next move that will be performed according to the handbook
  next_move <- individual[situation,]$Move

  # needed for the moving:
  # coordintes of the current field and north east south and west
  yaxis <- c(latitude[i-1], latitude[i-1] - 1, latitude[i-1], latitude[i-1] + 1, latitude[i-1])
  xaxis <- c(longitude[i-1], longitude[i-1], longitude[i-1] + 1, longitude[i-1], longitude[i-1] - 1)
  # content on the current field and the fields around the robot
  content_current <- grid[yaxis[1], xaxis[1]]
  content_north <- grid[yaxis[2], xaxis[2]]
  content_east <- grid[yaxis[3], xaxis[3]]
  content_south <- grid[yaxis[4], xaxis[4]]
  content_west <- grid[yaxis[5], xaxis[5]]


  # change of the current position of the robot according to the move that
  # corresponds to his current position
  if (next_move == "North" & content_north != "Wall"){
    latitude[i]  <- latitude[i-1] - 1
    longitude[i] <- longitude[i-1]
  } else if (next_move == "East" & content_east != "Wall"){
    latitude[i] <- latitude[i-1]
    longitude[i] <- longitude[i-1] + 1
  } else if (next_move == "South" & content_south != "Wall"){
    latitude[i] <- latitude[i-1] + 1
    longitude[i] <- longitude[i-1]
  } else if (next_move == "West" & content_west != "Wall"){
    latitude[i] <- latitude[i-1]
    longitude[i] <- longitude[i-1] - 1

    # if he moves into a wall he bounces back to his old position
  } else  if (next_move == "North" & content_north == "Wall"){
    latitude[i] <- latitude[i-1]
    longitude[i] <- longitude[i-1]

  } else if (next_move == "East" & content_east == "Wall"){
    latitude[i] <- latitude[i-1]
    longitude[i] <- longitude[i-1]

  } else if (next_move == "South" & content_south == "Wall"){
    latitude[i] <- latitude[i-1]
    longitude <- longitude[i-1]

  } else if(next_move == "West" & content_west == "Wall"){
    latitude[i] <- latitude[i-1]
    longitude[i] <- longitude[i-1]


    # if he picks-up his location doesnt change
  } else if (next_move == "Stay"){
    latitude[i] <- latitude[i-1]
    longitude[i] <- longitude[i-1]

    # if he picks up sth his location doesnt change but the environment changes
  } else if(next_move == "Pick-Up" & content_current == "Evidence"){
    latitude[i] <- latitude[i-1]
    longitude[i] <- longitude[i-1]
    grid[xaxis[1], yaxis[1]] <- "Empty"

    # if he picks up but there is nothing, his location doesnt change and the
    # environment doesnt change either
  } else if(next_move == "Pick-Up" & content_current == "Empty"){
    latitude[i] <- latitude[i-1]
    longitude[i] <- longitude[i-1]
  }
}

coordinates <- data.frame(latitude, longitude)
df_footprints <- data.frame(evidence_latitude, evidence_longitude)

# pictures for funsies
researcher <- "https://jeroenooms.github.io/images/frink.png"
cat <- "https://www.clipartqueen.com/image-files/paw-prints-clipart-dog-paw-prints.png"
horse <- "https://cdn.onlinewebfonts.com/svg/img_72745.png"
bird <- "http://www.clker.com/cliparts/a/f/b/e/13209639322102727323Bird%20Footprints.svg.hi.png"
bear <- "http://clipart-library.com/images/6cr54jKgi.gif"

# gganimate is used to show the movement of the researcher, each step is one point
# in time
# axis limits are given with the territory parameters
plot <- ggplot(coordinates, aes(longitude, latitude, size = 3)) +
  geom_image(aes(image = researcher), size = .05) +
  geom_image(aes(evidence_latitude, evidence_longitude, image = animal), size = 0.1,
             data = df_footprints, inherit.aes = FALSE) +
  labs(x = 'Longitude', y = 'Latitude') +
  xlim(1, grid_size[2]) +
  ylim(1, grid_size[1]) +
  transition_states(steps)

return(plot)
}
