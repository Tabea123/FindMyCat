Project Description
===================

Theoretical background
----------------------

The package ‘findmycat’ is an implementation of a genetic algorithm. The
program is based on chapter nine ‘Genetic Algorithms’ of the book
‘Complexity: A guided tour.’ by .\
The genetic algorithm in this package evolves a solution to a specific
task: successfully picking up evidence of an animal, such as footprints,
in a two-dimensional grid. Fields in the grid can either be empty or
contain evidence. The grid is surrounded by a wall.\
To run the algorithm, the task is carried out by a ‘robot’. The robot
knows of the content of his current field, the field north, east, south,
and west of him. To start off, an initial population of random,
candidate solutions to the task is generated. Each solution is one
individual. The solutions are strategy tables that the robot can use to
navigate through the grid. A strategy tables contains all 243 (3 types
of field content \* 5 possible locations) situations the robot could
find himself in. To every situation, the strategy table contains a move
that is performed next. Table [table1] illustrates the first three rows
of a potential strategy table.\

[] [table1]

| Current | North  | East     | South | West  |   | Move    |
|---------|--------|----------|-------|-------|---|---------|
| Empty   | Wall   | Evidence | Empty | Empty |   | East    |
| Empty   | Wall   | Empty    | Empty | Wall  |   | South   |
| Emtpy   | Empty  | Empty    | Empty | Empty |   | Pick-up |
| ...     | ...    | ...      | ...   | ...   |   | ...     |

The robot moves through the grid according to the strategy table. He
checks the current and adjacent fields, looks up his situation in the
strategy table and performs the move that is written down there. The
score, or so-called fitness, of each individual is calculated as
follows:

-   If the robot successfully picks up evidence in his current field, he
    gets a reward of ten points.

-   If he bends down to pick up in a site where there is no evidence, he
    is fined one point.

-   If he crashes into a wall, he is fined five points and bounces back
    into the current site.

This procedure of ‘moving and scoring’ is done for several sessions for
each individual. The evidence in a grid is placed randomly. After the
completion of one session, the grid configuration changes.\
The scores each individual acquires are used for the evolution of the
next generation. The two individuals with the highest fitness (scores)
are chosen to be the parents of the next generation. The selected
parents are being paired up to create offspring. Each pair produces
offspring by recombining parts of the parents, with some chance of
random mutations, and the offspring enter the new population. The
selected parents continue creating offspring until the new population is
full (i.e., has the same number of individuals as the initial
population). The new population now becomes the current population.\
Consequently, the procedure of ‘moving and scoring’ is applied again, to
find the two individuals with the highest fitness to evolve the next
generation. This is repeated for many generations in order to evolve the
best universal strategy to move through a grid without bumping into
walls but pick up evidence successfully.\

Requirements and Design
-----------------------

For my program I thought of the following scenario: David, the animal
researcher and TV producer, wants to film snow leopards. Snow leopards
are very rare, and very shy animals. They are hard to find in their
natural environment. Accordingly, it takes many months, to produce good
footage. David wants to find a quicker way to find the snow leopard. He
received hints from local experts where the leopard, or evidence of the
leopard, has been seen last. Now, he is looking for the most efficient
route to pick up that evidence. Instead of trying to think of a route
himself, he lets computer evolution figure it out for him. He utilizes a
genetic algorithm, to evolve a good strategy. For this purpose, a robot
is modeled that walks through a grid the same size as the territory the
researcher has to examine. The robot trains to pick up the same amount
of evidence the researcher received.\
To implement this program, it was initially my goal to set up a Shiny
App. While working on the algorithm I realized though, that R is too
slow. Therefore, I abolished the plan and compiled a package instead.
The package has two parts: the algorithm and a visualization of the
smartest route. Figure [flowchart] illustrates the overall flow of the
program from the user’s and the computer’s perspective.

![Flowchart illustrating the user’s and the software’s
perspective.](flowchart.png "fig:") [flowchart]

The algorithm is modeled with several functions. There is one
overarching function, called `evolution`, that calls six subordinate
functions. The subordinate functions serve the following purposes:\
First, an intital population is created with `create_population`. Then
the fitness of each individual in the population is determined. With
`create_grid` a grid is set up. The robot then moves through the grid
and is scored on his actions with `move_score`. The function
`lookup_situation` tells the robot which move he has to carry out in
which situation. This moving and scoring is repeated in several
sessions. In each session the grid configuration changes. The repeated
walking and scoring in different grid configurations is modeled by the
function `life_cycle`. Evolution from one to the next population is
achieved with the function `next_generation`. The user only feeds the
right arguments to the `evolution` function and all other functions are
being executed automatically.\
The package, though, includes all functions. All functions are being
exported so that the user can reproduce the different processes
manually.\
For the visualization, the developed, ideal strategy is used to show the
robots path through the grid. The user uses the function
`visualize_path`. The function takes in the strategy, the grid/territory
size, the position of the evidence on the latitude and longitude, the
number of steps the researchers wants to walk, as well as the animal
that is being searched. The function returns a plot made with `ggplot`.\
The input and output of each function can be checked in the man
directory or via the help page of the functions.

Project Implementation
======================

Screenshots and examples of the software
----------------------------------------

The function `evolution` is a very big function and therefor very slow.
While the user waits, the function `evolution` informs the user, which
generation is currently developed (Figure [generations]).

Figure [scores] plots the mean score of the individuals in the different
generations. In the beginning, the scores improve very quickly.
recommends to run the genetic algorithm with 200 individuals in each
population, 100 sessions of 200 actions each and this for 1,000
generations. Doing this in R will take very long though. Therefor, the
user is free to set the parameters arbitrarily.

Figure [plot] gives an idea of what the visualization looks like.

Step-by-step manual
===================

To make use of the package ‘findmycat’ follow these steps:

-   Install the **devtools** package.

-   Run `library(devtools)`.

-   Run `install_github(Tabea123/FindMyCat)`.

-   Run `library(findmycat)`.

-   Run `?evolution` to find out how to use the function.

-   Run `evolution`. Don’t forget to store the result in a variable.
    (e.g., `scores <- evolution(...)`)

-   Run `?path` to find out how to use the function.

-   Run `path`. Don’t forget to store the result in a variable. (e.g.,
    `path <- path(...)`)

-   Run `?visualize_path` to find out how to use the function.

-   Run `visualize_path`. Caution: To run this function you need a
    stable internet connection. Miau World!

![Improvement of the scores over 100
generations.](100g100se100st100i.png "fig:") [scores]

![This is what the user sees while the function `evolution`
runs.](generationinprocess.png "fig:") [generations]

![Visualization of the robot researcher walking in the
grid.](plot.png "fig:") [plot]
