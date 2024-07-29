#' rmvpt
#'
#' rmvpt allows users to remove points by clicking on them on an interactive graph.
#'
#' @param df A dataframe that will be processed.
#' @param x Independent variable name as a string for the plot.
#' @param y Dependent variable as a string for the plot.
#' @return Returns a dataframe with an additional column with the normalized values.
#' @export
#' @import shiny
#' @import plotly
#' @import dplyr


rmvpt <- function(df, x, y) {

  if (!(x %in% names(df)) | !(y %in% names(df))) { # Warning if columns selected are not in the dataframe
    stop("Specified columns not found in the dataframe")
  }

  app <- shinyApp( # Create Shiny app
    ui = fluidPage(
      titlePanel("Interactive Point Deselection"),
      plotlyOutput("scatterPlot"),
      actionButton("closeApp", "Close App and Return Updated Data")
    ),
    server = function(input, output, session) {
      # Reactive value to store selected points
      selected_points <- reactiveVal(rep(TRUE, nrow(df)))

      output$scatterPlot <- renderPlotly({
        # Get the current selected points
        current_data <- df[selected_points(), ]

        # Create the plot
        p <- ggplot(current_data, aes_string(x = x, y = y)) +
          geom_point() +
          labs(title = "Click on points to deselect them", x = x, y = y)

        ggplotly(p) %>%
          layout(dragmode = "select") %>%
          event_register("plotly_click")
      })

      observeEvent(event_data("plotly_click"), {
        click_data <- event_data("plotly_click")
        current_selected <- selected_points()

        if (!is.null(click_data)) {
          # Find the clicked point
          clicked_x <- click_data$x
          clicked_y <- click_data$y

          # Find the index of the clicked point in the original dataframe
          index <- which(df[[x]] == clicked_x & df[[y]] == clicked_y)

          # Deselect the clicked point
          current_selected[index] <- FALSE

          # Update the selected points
          selected_points(current_selected)
        }
      })

      # Close the app and return the updated dataset when the button is clicked
      observeEvent(input$closeApp, {
        stopApp(df[selected_points(), ])
      })
    }
  )

  # Run the app and return the updated dataset
  result <- runApp(app, launch.browser = TRUE)
  return(result)
}
