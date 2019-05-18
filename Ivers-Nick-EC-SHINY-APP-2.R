
library(tidyverse)
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 
library(leaflet.extras)
library(dplyr)
library(curl)
library(readr)
spreadsheet <- curl("https://raw.githubusercontent.com/naivers/Ivers-Nick-EC-SHINY-APP/master/urban_garden_shiny.csv")
df <- read.csv(spreadsheet, sep = ",", header=TRUE, stringsAsFactors = FALSE)
df <- as.data.frame(df)
parasites <- as.data.frame(df$Infection)
parasites <- parasites*10
G <- c("", "Aptos", "Berryessa", "Chadwick", "Charles Street", "Chinatown", "Coyote Creek", "El Valle Verde", "Goodwill", "Grange", "Guadalupe Garden", "Homeless", "La Colina", "Laguna Seca", "MEarth", "Mi Jardin Verde", "Pacific Grove", "Pajaro", "Prusch", "Salinas", "Senior Center", "Trescony")


ui <- fluidPage(
  headerPanel("Do urban gardens act as refugia, or as parasitism 'hotspots' for pollinators?"),
  sidebarLayout(
    selectInput("garden", label = "Select a garden to evaluate infection rate...", 
                choices = G),
    mainPanel(
      column(8, leafletOutput(outputId = "map", width = "100%", height = 500)),
      column(4, textOutput("garden")),
      column(4, plotOutput("plot")),
      br(),
      tableOutput(outputId = "table")
    )
  )
)


server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lng= -121.7806606, lat = 37.00119929, zoom = 8.5) %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Base") %>% #Provider tiles ARE necessary  
      addCircleMarkers(data=df, layerId=~id, popup=df$Site, group = "Base") %>%
      addHeatmap(lng = ~long, lat = ~lat, data = df, intensity = (df$Infection)*10,
                       blur = 10, max = 10, radius = 25, group = "All-Parasites") %>%
      addHeatmap(lng = ~long, lat = ~lat, data = df, intensity = (df$Trypanosomatids)*10,
                 blur = 15, max = 10, radius = 25, group = "Trypanosomatids") %>%
      addHeatmap(lng = ~long, lat = ~lat, data = df, intensity = (df$Neogregarines)*10,
                 blur = 25, max = 10, radius = 25, group = "Neogregarines") %>%
      addHeatmap(lng = ~long, lat = ~lat, data = df, intensity = (df$Microsporidians)*10,
                 blur = 25, max = 10, radius = 25, group = "Microsporidians") %>%
      addHeatmap(lng = ~long, lat = ~lat, data = df, intensity = (df$Mites)*10,
                 blur = 25, max = 10, radius = 25, group = "Mites") %>%
      addLayersControl(
        overlayGroups = c("All-Parasites", "Trypanosomatids", "Neogregarines", "Microsporidians", "Mites"),
        options = layersControlOptions(collapsed = FALSE)
      )
    })
  
  output$garden <- renderText({
    paste0(       input$garden)
    
  })
  
  output$plot <- renderPlot({
    massage <- grep(pattern=input$garden, x=df$Site)
    massage <- df[massage, ]
    garden_rates <- cbind(massage$Infection, massage$Neogregarines, massage$Trypanosomatids, massage$Microsporidians,massage$Mites) 
    barplot(height=garden_rates, main="Infection data for \nselected garden", ylab = "Infection Rate", names.arg = c("All Parasites", "\n'Neo'\n", "\n'Tryp'\n", "\n'Micro'\n", "Mites"), las=2, cex.names=0.8)
    
  })   
  output$table <- renderTable(
    data <- as.data.frame(df)
  )
}


shinyApp(ui=ui, server=server)


