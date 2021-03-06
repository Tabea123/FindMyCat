% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Algorithm.R
\name{move_score}
\alias{move_score}
\title{Move the Robot and Score his Actions}
\usage{
move_score(individual, grid, latitude, longitude, steps)
}
\arguments{
\item{individual}{one individual strategy taken form the population made with
\code{create_poulation}.}

\item{grid}{an object of class matrix made with \code{create_grid}.}

\item{latitude}{a number indicating the current position of the robot on the
y-axis.}

\item{longitude}{a number indicating the current position of the robot on the
x-axis.}

\item{steps}{a number indicating how many moves the robot should walk in the
grid.}
}
\value{
The robots position after taking all steps and the resulting score.
}
\description{
\code{move_score} is used to move the robot through the grid according to his
strategy table and to score his actions.
}
\details{
The robot begins at his starting position (latitude = 2, longitude = 2).
The robot then follows one strategy for X actions. The number of actions is
indicated by the user with the argument \code{steps}. If there is no wall in the
direction of his next movement, the robot walks. If his next action is to stay,
the robot stops moving.

The score of the strategy is the number of bonus- and minuspoints the
robot accumulates in a session.
If the robot is in the same site as a piece of evidence and picks it up,
he gets ten points.
If he bends down to pick up in a site where there is no evidence, he is fined
one point.
If he crashes into a wall, he is fined five points and bounces back into the
current site.
}
\examples{
population <- create_population(100)
grid <- create_grid(c(10, 12), 11)
move_score(population[[1]], grid = grid, latitude = 5, longitude = 5, steps = 100)
}
