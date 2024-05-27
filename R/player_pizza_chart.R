#' Create a pizza chart for sports player.
# Importing functions using the import statement
#' @import tidyr
#' @import ggtext
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes
#'
#' @param data an input dataframe containing the player data.
#' @param player_stats_col a vector specifying the names of the columns from the dataframe that will be used to create slices in the pizza chart. Stats must be between 0 to 100.
#' @param name_col specifies the name of the column in the dataframe that contains the player names.
#' @param player_name specifies the name of the player (or the value of 'name_col' parameter) whose statistics will be visualized in the chart.
#'
#' @return an interactive pizza chart for a player.
#' @export
#'
#' @examples
#'
#' # Create a generic dataframe for the example
#' player_data <- data.frame(
#'   Name = c('Player1', 'Player2', 'Player3'),
#'   Crossing = c(70, 80, 85),
#'   Finishing = c(75, 85, 80),
#'   Volleys = c(65, 70, 75),
#'   FK_Accuracy = c(70, 75, 80)
#' )
#'
#' player_pizza_chart(player_data,
#' player_stats_col=c('Crossing', 'Finishing', 'Volleys', 'FK_Accuracy'),
#'                    name_col = 'Name', player_name = 'Player1')

player_pizza_chart <- function(data, player_stats_col, name_col, player_name) {
  var1 <- NULL
  value <- NULL
  # Filter data for the specific player
  filtered_data <- data[data[[name_col]] == player_name, ]

  # Reshape the data into a longer format
  data_long <- filtered_data %>%
    dplyr::select(all_of(player_stats_col)) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "var1", values_to = "value")

  # Create the pizza chart
  chart <- ggplot(data_long, aes(x=var1, y=value)) +
    geom_bar(aes(y = 100), fill = "#131313", stat = "identity", width = 1, colour = "#797979", alpha = 0.3, show.legend = FALSE) +
    geom_bar(stat = "identity", width = 0.95, aes(fill = var1), colour = "#F3FEFC", alpha = 1, show.legend = FALSE) +
    coord_polar(clip = "off") +
    geom_hline(yintercept = c(25, 50, 75), colour = "#565656", linetype = "longdash", alpha = 0.5) +
    geom_label(aes(label = value, fill = var1), size = 3, color = "white", show.legend = FALSE) +
    labs(fill = "", title = player_name) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 7, vjust = 1, colour = 'black'),
      plot.title = element_markdown(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 2, 2, 2)
    )

  return(chart)
}
