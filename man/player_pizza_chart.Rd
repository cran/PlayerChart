% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/player_pizza_chart.R
\name{player_pizza_chart}
\alias{player_pizza_chart}
\title{Create a pizza chart for sports player.}
\usage{
player_pizza_chart(data, player_stats_col, name_col, player_name)
}
\arguments{
\item{data}{an input dataframe containing the player data.}

\item{player_stats_col}{a vector specifying the names of the columns from the dataframe that will be used to create slices in the pizza chart. Stats must be between 0 to 100.}

\item{name_col}{specifies the name of the column in the dataframe that contains the player names.}

\item{player_name}{specifies the name of the player (or the value of 'name_col' parameter) whose statistics will be visualized in the chart.}
}
\value{
an interactive pizza chart for a player.
}
\description{
Create a pizza chart for sports player.
}
\examples{

# Create a generic dataframe for the example
player_data <- data.frame(
  Name = c('Player1', 'Player2', 'Player3'),
  Crossing = c(70, 80, 85),
  Finishing = c(75, 85, 80),
  Volleys = c(65, 70, 75),
  FK_Accuracy = c(70, 75, 80)
)

player_pizza_chart(player_data,
player_stats_col=c('Crossing', 'Finishing', 'Volleys', 'FK_Accuracy'),
                   name_col = 'Name', player_name = 'Player1')
}
