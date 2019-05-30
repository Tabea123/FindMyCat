usethis::use_package("ggplot2")
usethis::use_package("gganimate")
usethis::use_package("ggimage")

# Storing the coordinates
individual <- evolution(100, c(10, 10), 11, 100, 10, 15)

steps <- 30

grid_size <- c(10, 10)
evidence_latitude <- c(3, 6, 4, 2, 1)
evidence_longitude <- c(2, 4, 8, 1, 9)

visualize_path <- function(individual, steps, grid_size, evidence_latitude,
                           evidence_longitude){

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

# starting point is (2,2)
latitude <- numeric(steps)
latitude[1] <- 2
longitude <- numeric(steps)
longitude[1] <- 2

for(i in 2:steps){

  # current situation the robot finds itself in
  situation <- lookup_situation(individual, grid, latitude[i-1], longitude[i-1])
  # next move that will be performed according to the handbook
  next_move <- individual[situation,]$Move

  # needed for the moving and scoring:
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

    # if he moves into a wall he bounces back to his old position and is fined 5 points
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

    # if he picks up but there is nothing, he is fined
  } else if(next_move == "Pick-Up" & content_current == "Empty"){
    latitude[i] <- latitude[i-1]
    longitude[i] <- longitude[i-1]

  }
}

return(data.frame(latitude, longitude))
}

# Visualization of the best route

step <- 1:30 # needed for transition_time

# plotting
coordinates <- data.frame(latitude, longitude)
df_footprints <- data.frame(evidence_latitude, evidence_longitude)

# later on the right image for the right animal has to be implemented
researcher <- "https://jeroenooms.github.io/images/frink.png"

cat <- "https://www.clipartqueen.com/image-files/paw-prints-clipart-dog-paw-prints.png"
horse <- "https://cdn.onlinewebfonts.com/svg/img_72745.png"
bird <- "http://www.clker.com/cliparts/a/f/b/e/13209639322102727323Bird%20Footprints.svg.hi.png"
bear <- "http://clipart-library.com/images/6cr54jKgi.gif"

# axis limits will be given by territory parameters
ggplot(coordinates, aes(longitude, latitude, size = 3)) +
  geom_image(aes(image = researcher), size = .05) +
  geom_image(aes(evidence_latitude, evidence_longitude, image = horse), size = 0.1,
             data = df_footprints, inherit.aes = FALSE) +
  labs(x = 'Longitude', y = 'Latitude') +
  xlim(1, ncol(grid)) +
  ylim(1, nrow(grid)) +
  transition_states(step)
