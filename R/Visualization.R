usethis::use_package("ggplot2")
usethis::use_package("gganimate")
usethis::use_package("ggimage")

# Storing the coordinates

#' Visualization of the Path
#'
#' \code{visualize_path} is used to plot the robot's moves.
#'
#' @usage visualize_path(individual, grid_size, evidence_latitude, evidence_longitude,
#' steps, animal)
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
#' @param animal the animal that is being searched (see details).
#'
#' @details The robot begins his path at his starting position (latitude = 2, longitude = 2).
#' The robot then follows the given strategy \code{individual} for X \code{steps}.
#' If there is no wall in the direction of his next movement, the robot walks.
#' If he crashes into a wall, he bounces back into the
#' current site.
#' If his next action is to stay or to pick up, the robot stops moving.
#' The robot's path is visualized with \code{ggplot}.
#'
#' #' \code{visualize_path} can only display the following animal footprints: cat, horse,
#' bird or bear.
#'
#' @return an animated plot.
#' @export
#'
#' @examples
#' population1 <- create_population(30)
#' visualize_path(individual = population1[[3]], grid_size = c(5, 7),
#' evidence_latitude = c(3, 2, 4), evidence_longitude = c(1, 6, 5),
#' steps = 30, animal = "bird")
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
  if (!require("ggplot2")) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.")
  }
  if (!require("gganimate")) {
    stop("Package \"gganimate\" needed for this function to work. Please install it.")
  }
  if (!require("ggimage")) {
    stop("Package \"ggimage\" needed for this function to work. Please install it.")
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
  content_north   <- grid[yaxis[2], xaxis[2]]
  content_east    <- grid[yaxis[3], xaxis[3]]
  content_south   <- grid[yaxis[4], xaxis[4]]
  content_west    <- grid[yaxis[5], xaxis[5]]


  # change of the current position of the robot according to the move that
  # corresponds to his current position
  if (next_move == "North" & content_north != "Wall"){
    latitude[i]  <- latitude[i-1] - 1
    longitude[i] <- longitude[i-1]
  } else if (next_move == "East" & content_east != "Wall"){
    latitude[i]  <- latitude[i-1]
    longitude[i] <- longitude[i-1] + 1
  } else if (next_move == "South" & content_south != "Wall"){
    latitude[i]  <- latitude[i-1] + 1
    longitude[i] <- longitude[i-1]
  } else if (next_move == "West" & content_west != "Wall"){
    latitude[i]  <- latitude[i-1]
    longitude[i] <- longitude[i-1] - 1

    # if he moves into a wall he bounces back to his old position
  } else  if (next_move == "North" & content_north == "Wall"){
    latitude[i]  <- latitude[i-1]
    longitude[i] <- longitude[i-1]

  } else if (next_move == "East" & content_east == "Wall"){
    latitude[i] <- latitude[i-1]
    longitude[i] <- longitude[i-1]

  } else if (next_move == "South" & content_south == "Wall"){
    latitude[i] <- latitude[i-1]
    longitude   <- longitude[i-1]

  } else if(next_move == "West" & content_west == "Wall"){
    latitude[i]  <- latitude[i-1]
    longitude[i] <- longitude[i-1]


    # if he picks-up his location doesnt change
  } else if (next_move == "Stay"){
    latitude[i]  <- latitude[i-1]
    longitude[i] <- longitude[i-1]

    # if he picks up sth his location doesnt change but the environment changes
  } else if(next_move == "Pick-Up" & content_current == "Evidence"){
    latitude[i]  <- latitude[i-1]
    longitude[i] <- longitude[i-1]
    grid[xaxis[1], yaxis[1]] <- "Empty"

    # if he picks up but there is nothing, his location doesnt change and the
    # environment doesnt change either
  } else if(next_move == "Pick-Up" & content_current == "Empty"){
    latitude[i]  <- latitude[i-1]
    longitude[i] <- longitude[i-1]
  }
}

coordinates   <- data.frame(latitude, longitude)
df_footprints <- data.frame(evidence_latitude, evidence_longitude)

# pictures for funsies
researcher <- "https://jeroenooms.github.io/images/frink.png"
image <- NULL
if(animal == "cat"){
  image <- "https://www.clipartqueen.com/image-files/paw-prints-clipart-dog-paw-prints.png"
} else if(animal == "horse"){
  image <- "https://cdn.onlinewebfonts.com/svg/img_72745.png"
} else if(animal == "bird"){
  image <- "http://www.clker.com/cliparts/a/f/b/e/13209639322102727323Bird%20Footprints.svg.hi.png"
} else if(animal == "bear"){
  image <- "http://clipart-library.com/images/6cr54jKgi.gif"
}

tstates <- 1:steps

# gganimate is used to show the movement of the researcher, each step is one point
# in time
# axis limits are given with the territory parameters
plot <- ggplot(coordinates, aes(longitude, latitude, size = 3)) +
  geom_image(aes(image = researcher), size = .05) +
  geom_image(aes(evidence_latitude, evidence_longitude, image = image),
                 size = 0.05, data = df_footprints, inherit.aes = FALSE) +
  labs(x = 'Longitude', y = 'Latitude') +
  xlim(1, grid_size[2]) +
  ylim(1, grid_size[1]) +
  transition_states(tstates)

return(plot)
}
