
steps <- 30

latitude <- numeric(30)
latitude[1] <- 2
longitude <- numeric(30)
longitude[1] <- 2

grid <- create_grid(c(5, 5), 10)

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

# Visualization of the best route

# packages
library(ggplot2)
library(gganimate)
library(ggimage)

step <- 1:200 # needed for transition_time

# plotting

# later on the right image for the right animal has to be implemented
image <- "https://jeroenooms.github.io/images/frink.png"
image2 <- "https://www.clipartqueen.com/image-files/paw-prints-clipart-dog-paw-prints.png"


# axis limits will be given by territory parameters
ggplot(individual_solution, aes(df.longitude, df.latitude, size = 3)) +
  geom_image(aes(image=image), size=.05) +
  geom_image(aes(latitude, longitude, image = image2), size = 0.1,
             data = df_footprints, inherit.aes = FALSE) +
  labs(title = 'Step: {frame_time}', x = 'Longitude', y = 'Latitude') +
  xlim(xstart, xend) +
  ylim(ystart, yend) +
  transition_time(step, range = c(1L, 100L))
