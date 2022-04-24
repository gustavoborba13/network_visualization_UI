#Libraries 

library(dplyr)
library(visNetwork)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(igraph)
library(plotly)





## Load data ---------------------------

edges.viz <- read.csv("data/netviz-edges.csv", sep = "|")

nodes.viz <- read.csv("data/netviz-nodes.csv", sep = "|")

#checking length of each 
##################################### VISNETWORK APPROACH ###########################################
#nodes
nodes.net <- as.data.frame(nodes.viz)
colnames(nodes.net) <- c("id", "label", "type")

#Edges
edges.net <- as.data.frame(edges.viz)
colnames(edges.net) <- c("from", "to", "date")

#Shape by Type

v.shape <- ifelse(nodes.net$type == "s", "square", "circle")
nodes.net$shape = v.shape

#Color by Value(label)
continuous.color <- colorRampPalette(c('yellow',"orange" ,'dark red'))               #color pallet from yellow to dark red


color.background <- continuous.color(3)[cut(nodes.net$label,               
                                            breaks = 3)]


nodes.net$color.background = color.background                                        #new column with nodes background color

#border color
nodes.net$color.border = "#013848"        #Very dark cyan border color   

## legend ------------------------------
legend <- data.frame(
  shape = c("square", "dot", "square", "square", "square"), 
  label = c("Party", "Claim", "Low", "Mid", "High"),
  color = c("black", "black", continuous.color(3))
)


#titles = paste0("<p><b>", nodes.net$id ,"</b><br>Node !</p>")
titles.nodes = paste0("<p><b>", nodes.net$id ,
                      "</b><br>Value:", round(nodes.net$label,3))

nodes.net$title = titles.nodes

#Edges Information (Date)
titles.edges = paste0("<p><b>", edges.net$from, " - ", edges.net$to , 
                      "</b><br>Date: ", edges.net$date)

edges.net$title = titles.edges


#dates to date type
edges.net$date = as.Date(edges.net$date)

## X and Y node Coordinates
network.igraph <- graph_from_data_frame(edges.viz, directed = F)          #create igraph object
coordinates <- layout_nicely(network.igraph)                              #Calculate coordinates once for entire component
nodes1<- names(V(network.igraph))                                         #extract vertices (nodes) names(IDs) 
pos1 <- setNames(split(coordinates, seq(nrow(coordinates))), nodes1)      #list with each node's cooordinates

#nodes coordinates x and y 
nodes.net[c("x", "y")] = do.call(rbind, pos1[nodes.net$id])               #add coordinates x and y to nodes.net



#date filter
date.filtered.edges <- edges.net %>%
  slice(-c(which(edges.net$date >= "2015-05-29")))                  #filtering edges that are outside date range

date.filtered.nodes <- nodes.net %>%                              #filtering nodes that are not in  filtered edge data
  slice(c(which(nodes.net$id %in%  c(date.filtered.edges$from, date.filtered.edges$to))))






#Building the Visualization
visNetwork(nodes = date.filtered.nodes, edges = date.filtered.edges, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
  visIgraphLayout() %>%
  visNodes( 
    color = list(highlight = NA),
    borderWidthSelected = 3) %>%
  visEdges(color = "black", hoverWidth = 5, selectionWidth = 3.5, shadow = T)%>%
  visInteraction(dragNodes = TRUE, multiselect = T, navigationButtons = F, zoomView = T) %>%
  visOptions(highlightNearest = list(enabled = T, degree = nrow(edges.net), hover = T),
             selectedBy = list(variable = "type", style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black'), 
             nodesIdSelection = list(enabled = TRUE, useLabels = FALSE, style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black')) %>% 
  visLegend(useGroups = F,addNodes= legend, width = 0.08)%>%
  visLayout(randomSeed = 11, improvedLayout = T)


################################################# SHINY ########################################

#Shiny dashboard with slider input for date

ui <- fluidPage(
  sliderInput(
    inputId = "date",
    label = "Dates:",
    min = min(edges.net$date)+1,
    max = max(edges.net$date),
    value = max(edges.net$date),
    timeFormat = "%Y-%m-%d",
    step = 60,                                               #not exact dates, step is every 60 days
    animate = animationOptions(interval = 300),
    width = '95%'
  ),
  visNetworkOutput("mynetwork")
)



ui <- fluidPage(
  sliderTextInput(                                           #slider to chose date
    inputId = "date",
    label = "Dates:",
    choices = unique(edges.net$date[order(edges.net$date)]), #exact dates from edges.net without repeating date
    from_min = min(edges.net$date),                          #from oldest edge date
    to_max = max(edges.net$date),                            #to newest edge date
    selected = max(edges.net$date),                          # start with newest selected
    grid = TRUE,                                             # ticks and dates between the range showing
    width = '95%',
    animate = animationOptions(interval = 100)              #speed of steps when play button is pressed
  ),
  visNetworkOutput("mynetwork")
)




server <- function(input, output) {
  output$mynetwork <- renderVisNetwork({
    
    edges.net$date = as.Date(edges.net$date)
    
    #date filter
    date.filtered.edges <- edges.net %>%
      slice(-c(which(edges.net$date >= input$date)))                  #filtering edges that are outside date range
    
    date.filtered.nodes <- nodes.net %>%                              #filtering nodes that are not in  filtered edge data
      slice(c(which(nodes.net$id %in%  c(date.filtered.edges$from, date.filtered.edges$to))))

    
    visualization <- visNetwork(nodes = date.filtered.nodes, edges= date.filtered.edges, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
      visIgraphLayout(randomSeed = 4) %>%
      visNodes( 
        color = list(
          highlight = NA),
        borderWidthSelected = 3) %>%
      visEdges(color = "#0D0D0D", hoverWidth = 5, selectionWidth = 3.5, shadow = T)%>%
      visInteraction(dragNodes = TRUE, multiselect = T, navigationButtons = F, zoomView = T) %>%
      visOptions(highlightNearest = list(enabled = T, degree = nrow(edges.net), hover = T, hideColor = 'rgba(0,0,0,0)'),
                 selectedBy = list(variable = "type", style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black'), 
                 nodesIdSelection = list(enabled = TRUE, useLabels = FALSE,style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black')) %>% 
      visLegend(useGroups = F,addNodes= legend, width = 0.08)      
    
  })
}
shinyApp(ui, server)
