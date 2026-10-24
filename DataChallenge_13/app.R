#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(gapminder)
library(dplyr)
library(ggplot2)


ui = fluidPage(
  
  titlePanel("Shiny Gapminder"),
  
  sidebarLayout(
    sidebarPanel(
      # Year input
      sliderInput("year",
                  "Year",
                  min = 1952,
                  max = 2007,
                  step = 5,
                  value = 1952),
      
      # Selezione del continente
      selectInput("continent", "Continent", 
                  choices = list("Africa", "Americas", "Asia", "Europe", "Oceania", "All"), 
                  selected = "All"),
      # Selezione della variabile da avere sull'asse x
      radioButtons("xaxis", "X",
                   choices = list("Life expectancy" = "lifeExp", 
                                  "Population" = "pop", 
                                  "GDP per capita" = "gdpPercap"), 
                   selected = "gdpPercap"),
      
      # selezione della variabile da avere sull'asse y
      radioButtons("yaxis", "Y",
                   choices = list("Life expectancy" = "lifeExp", 
                                  "Population" = "pop", 
                                  "GDP per capita" = "gdpPercap"), 
                   selected = "lifeExp")
      
      
    ),
    
    # scatter plot output
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    # generate dataset
    if (input$continent == "All") {
      dataset = filter(gapminder, year == input$year)
    } else {
      dataset = filter(gapminder, year == input$year, continent == input$continent)
    }
    
    # Comincia a disegnare i l grafico
    p <- ggplot(dataset, aes_string(input$xaxis, input$yaxis, size = "pop", colour = "country")) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_colour_manual(values = country_colors) +
      scale_size(range = c(2, 20)) + # imposta il range delle dimensioni dei cerchi
      theme_minimal()
    
    # Scale logaritmiche solo se non e' l'aspettativa di vita
    if (input$xaxis != "lifeExp") {
      p <- p + scale_x_log10()
    }
    if (input$yaxis != "lifeExp") {
      p <- p + scale_y_log10()
    }
    
    # Titoli degli assi
    titoli_assi <- c(
      lifeExp = "Life expectancy",
      pop = "Population (log scale)",
      gdpPercap = "GDP per capita (log scale)"
    );
    p <- p +
      xlab(titoli_assi[input$xaxis]) +
      ylab(titoli_assi[input$yaxis]);
    
    p
  })
}

# Esegue l'applicazione
shinyApp(ui = ui, server = server)
