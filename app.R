library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)
library(leaflet)
library(rvest)
library(geojsonio)
library(ggrepel)
library(rsconnect)
library(shiny)

#read in pokemon data
pokemon_sheet <- read.csv("Pokemon.csv")
#can rank pokemon too 
#creates functions 'percentile' that gets the percentile of a column
new_cols <- pokemon_sheet %>%
  mutate(Attack.percentile = ntile(Attack,100),
         HP.percentile = ntile(HP,100),
         Defense.percentile = ntile(Defense,100),
         Sp.Attackpercentile = ntile(Sp..Atk,100),
         Sp.Defensepercentile = ntile(Sp..Def,100),
         Speed.percentile = ntile(Speed,100)) %>%
  select(Name,
         HP.percentile,
         Attack.percentile,
         Defense.percentile,
         Sp.Attackpercentile,
         Sp.Defensepercentile,
         Speed.percentile) 
# y <- x %>%
#   mutate(percentileattack2 = quantile(x$Attack, probs = c(0,.25,.5,.75,1)))
# ?quantile
# percentileattack = ecdf(min(pokemon_sheet$Attack):max(pokemon_sheet$Attack))
# percentilehp = ecdf(min(pokemon_sheet$HP):max(pokemon_sheet$HP))
# percentiledefense = ecdf(min(pokemon_sheet$Defense):max(pokemon_sheet$Defense))
# percentileSpatt = ecdf(min(pokemon_sheet$Sp..Atk):max(pokemon_sheet$Sp..Atk))
# percentileaSp.def = ecdf(min(pokemon_sheet$Sp..Def):max(pokemon_sheet$Sp..Def))
# percentileSpeed = ecdf(min(pokemon_sheet$Speed):max(pokemon_sheet$Speed))
#create new columns that have percentages of stats for the pokemons
# new_cols <- pokemon_sheet %>%
#   mutate(HP.percentile = percentilehp(HP),
#          Attack.percentile = percentileattack(Attack),
#          Defense.percentile = percentiledefense(Defense),
#          Sp.Attackpercentile = percentileSpatt(Sp..Atk),
#          Sp.Defensepercentile = percentileaSp.def(Sp..Def),
#          Speed.percentile = percentileSpeed(Speed)) %>%
#   select(Name,
#          HP.percentile,
#          Attack.percentile,
#          Defense.percentile,
#          Sp.Attackpercentile,
#          Sp.Defensepercentile,
#          Speed.percentile) 

#Map stuff
states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"

states <- geojson_read(states.url, what = "sp")


#getting the data
map <- states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-96, 37.8, 4)
bins <- c(0, 7, 14, 21, 28, Inf)
pokemon_sheet.Rand <- pokemon_sheet %>%
  mutate(RandState = sample.int(51, nrow(pokemon_sheet), replace = TRUE))

states.clean <- states@data[-c(8),]

states.clean <- states.clean %>%
  mutate(RandState = sample.int(51, nrow(states.clean), replace = FALSE))

states.clean1 <- left_join(states.clean,
                           pokemon_sheet.Rand,
                           by = "RandState")
counts <- states.clean1 %>%
  group_by(NAME) %>%
  tally()   
colors <-colorBin(palette = "YlOrRd", #whats the gradient I want
                  domain = counts$n, bins = bins) #this is the thing I'm basing my color gradient off of

# Define UI for application 
ui <- fluidPage(
  
  titlePanel("Pokemon"),
  textInput("text1", "What Pokemon Do you want to learn about?", "Enter pokemon here"),
  actionButton("Search", "Click here"),
  textOutput("text"),
  mainPanel(
    plotOutput(outputId = "plot1"),
    leafletOutput(outputId = "map1"),
    tableOutput(outputId = "list1")
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  pokemon_text <- eventReactive(input$Search, {
    
    str_c("https://pokemon.fandom.com/wiki/", input$text1) %>%
      read_html() %>%
      html_nodes(xpath = '//*[@id="mw-content-text"]/p[1]') %>%
      html_text() 
    
  })
  
  
  output$text <- renderText({
    pokemon_text()
    
  })
  
  
  
  output$plot1 <- renderPlot({
    req(input$Search)
    row <- which(grepl(input$text1, new_cols$Name)) #searches name row and returns the row position where it's found
    x <- new_cols[row,] #makes data frame with just that pokemon's stats
    #gathers data to one row
    x.gathered <- x %>%
      gather(key ="Stat",
             value = "percentile1",
             -Name)
    #finds max value stat and returns all information of the row that it's in
    max.stat <- x.gathered[which.max(x.gathered$percentile1),]
    #gets name of the stat that is the highest percentile
    name.of.stat <- max.stat[,"Stat"]
    #need to make new dataframe only with that stat. make it from new cols, not new cols gathered
    colnames <- c("Name", name.of.stat)
    #creates new dataset with only the specific columns
    new.frame <- new_cols[colnames]
    row2 <- which(grepl(input$text1, new.frame$Name)) #searches name row and returns the row position where it's found
    
    #makes a plot of percentile 
    ggplot(new.frame,
           aes(x = reorder(Name, new.frame[,2]), y = new.frame[,2])) + 
      geom_bar(stat = "identity", aes(fill = ifelse(Name == input$text1,
                                                    "purple",
                                                    "yellow"))) +
      ylab(name.of.stat) +
      xlab(NULL) +
      theme(legend.position="none",
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      ggtitle(isolate(input$text1))
  })
  output$map1 <- renderLeaflet({
    poke_state <- states.clean1[states.clean1$Name == input$text1,]
    poke_state1 <- poke_state$NAME[1]
    map %>%
      addPolygons(fillColor = ~colors(counts$n),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  layerId = ~states@data$NAME,
                  popup = paste(input$map1_shape_click$id),
                  highlight = 
                    highlightOptions(
                      weight = 5,
                      stroke = ifelse(input$map1_shape_click$id == poke_state1,
                                      TRUE,
                                      FALSE),
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE)
                  
      )
  })
  output$list1 <- renderTable({
    states.clean1 %>%
      filter(NAME == input$map1_shape_click$id) %>%
      select(Name)
  })
}    


# Run the application 
shinyApp(ui = ui, server = server)