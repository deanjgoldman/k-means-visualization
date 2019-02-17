# KMEANS APP

library(shiny)
library(dplyr)
library(ggplot2)

# irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
# iris$cluster <- as.factor(irisCluster$cluster)
# ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$cluster)) + geom_point()
# Define UI for application that draws a histogram

features <- names(iris)
features <- features[! features %in% c("Species")]

ui <- fluidPage(
   
   # Application title
   titlePanel("K-Means Visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("centroids",
                     "Number of centroids:",
                     min = 2,
                     max = 10,
                     value = 2),
         selectInput("km.x", "X-axis", c("Choose one" = "", features ), "Petal.Length"),
         selectInput("km.y", "Y-axis", c("Choose one" = "", features ), "Petal.Width")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("km.Plot"),
         verbatimTextOutput("code")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  get_kmeans_plot <- function(n, x, y, nstart){
    # Prepare dataset x and y values
    km.iris <- iris %>% select(x, y)
    names(km.iris) <- c("x", "y")
    # Compute kmeans clustering
    irisCluster <- kmeans(km.iris, n, nstart = 20)
    km.iris$cluster <- as.factor(irisCluster$cluster)
    # Compute center of clusters
    centers = km.iris %>%
      group_by(., cluster) %>%
      summarize(., center_x = mean(x), center_y = mean(y))
    # Return plot: Clusters with centers
    return(
      ggplot(km.iris, aes(x, y, color = km.iris$cluster)) +
        geom_point() + 
        geom_point(data=centers, aes(x = center_x, y = center_y,  color = cluster), size=5)
    )
  }
  
  
  
   output$km.Plot <- renderPlot({ get_kmeans_plot(input$centroids, input$km.x, input$km.y) })
   
   get_text <- function(n, x, y) {
     paste(paste0("irisCluster <- kmeans(km.iris, ", n, " nstart = 20)"),
            paste0("  centers = km.iris %>%)"),
            paste0("  group_by(., cluster) %>%"),
            paste0("  summarize(., center_x = mean(", x, "), center_y = mean(", y, "))"),
            paste0(" "),
            paste0("ggplot(km.iris, aes(", x, ", ", y, ", ", "color = km.iris$cluster )) +"),
            paste0("  geom_point() +"),
            paste0("  geom_point(data=centers, aes(x = center_x, y = center_y,  color = cluster), size=5)"),
            sep="\n")
   }
   
   output$code <- renderText({ get_text(input$centroids, input$km.x, input$km.y) })
}

# Run the application 
shinyApp(ui = ui, server = server)