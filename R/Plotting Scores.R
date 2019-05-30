evolution <- function(population_size, grid_size, n_evidence, steps, sessions, generations){

  sammelcontainer <- list()

  # create first populationg
  population <- create_population(population_size)

  for(i in 1:generations){
    print(paste("generation", i, "in progress"))
    # let this population walk in one grid for x steps then change the grid
    # repeat this procedure for y sessions
    all_scores <- life_cycle(population, grid_size, n_evidence, steps, sessions)
    sammelcontainer[[i]] <- apply(all_scores, 1, mean)
    # chose the two best strategies and recombine them with mutations to a new population
    population <- next_generation(population, all_scores)
  }

  return(sammelcontainer)
}

scores <- evolution(population_size = 100, grid_size = c(5, 5),
                    n_evidence = 10, steps = 50, sessions = 10, generations = 70)

x <- numeric(50)
for(t in 1:50){
  x[t] <- mean(scores[[t]])
}

png("70g20se50st100i.png")
plot(x)
dev.off()
