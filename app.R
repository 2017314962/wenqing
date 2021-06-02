# Load packages ----
library(shiny)
library(mapproj)
library(ggplot2)
library(readr)
library(maps)

# Load data ----
mydata <- read_csv("data/annual shipments data.csv")



# User interface ----
ui <- fluidPage(
  titlePanel("Annual Shipments Data of China"),
  sidebarLayout(
    sidebarPanel(
      helpText("The annual shipments of mobile phones
               in China from 2014 to 2020."),
      
      selectInput("var", 
                  label = "Choose a selection to display:",
                  choices = c("Shipments",
                              "Growth Rate"),
                  selected = "shipment")

    ),
   mainPanel(
    plotOutput(outputId = "bar")
    )
    
      
)
)


# Server logic ----
server <- function(input, output) {
  output$bar <- renderPlot({
     
    sp_data<- ggplot(mydata, aes(x = factor(Year), y = Shipments)) +
      geom_bar(stat = "identity",fill = "lightcoral",colour = "black",width = 0.8, alpha = 0.8) +
      geom_text(aes(label = Shipments), vjust = -0.2, size = 3.5) +
      ggtitle("Annual shipments data\nof China") +
      labs(x = "Year",y = "Shipments(one hundred million units)") +
      theme(
        axis.title.x = element_text(colour = "indianred2", size = 16, face = "bold"),
        axis.text.x  = element_text(colour = "gray51"),
        axis.title.y = element_text(colour = "indianred2", size = 14, face = "bold"),
        axis.text.y  = element_text(colour = "gray51"),
        plot.title = element_text(colour = "indianred2", size = 20, face = "bold")
      )
    gr_plot <- ggplot(mydata, aes(x = Year, y = GrowthRate)) +
      geom_line() +
      geom_point(size = 4, shape = 22, colour = "darkred", fill = "lightcoral") +
      geom_text(aes(label = scales::percent(GrowthRate,0.01)), vjust = -0.6, size = 3.8) +
      scale_y_continuous(
        breaks = c(-0.2, -0.1, 0.0, 0.1),
        labels = c("-20%", "-10%", "0%", "10%")
      ) +
      ggtitle("Annual shipments\ngrowth rate of China") +
      labs(x = "Year",y = "Growth Rate") + 
      theme(
        axis.title.x = element_text(colour = "indianred2", size = 16, face = "bold"),
        axis.text.x  = element_text(colour = "gray51"),
        axis.title.y = element_text(colour = "indianred2", size = 16, face = "bold"),
        axis.text.y  = element_text(colour = "gray51"),
        plot.title = element_text(colour = "indianred2", size = 20, face = "bold")
      ) 
    plot <- switch(input$var, "Shipments" = sp_plot, "Growth Rate" = gr_plot)
    print(plot)

  })

  
}

# Run app ----
shinyApp(ui, server)