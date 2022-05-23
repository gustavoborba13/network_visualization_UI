#Libraries 
library(dplyr)
library(visNetwork)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(igraph)
library(plotly)

#read data
edges.viz <- read.csv("data-raw_trygg-hansa_nodes-edges/netviz-edges.csv", sep = "|")

nodes.viz <- read.csv("data-raw_trygg-hansa_nodes-edges/netviz-nodes.csv", sep = "|")


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
#continuous color

continuous.color <- colorRampPalette(c('yellow',"orange" ,'dark red'))

color.background <- continuous.color(nrow(nodes.net))[cut(nodes.net$label, breaks = nrow(nodes.net))]

nodes.net$color.background = color.background

#border color
nodes.net$color.border = "#013848"

#nodes size
nodes.net$size = 25

#titles = paste0("<p><b>", nodes.net$id ,"</b><br>Node !</p>")
titles.nodes = paste0("<p><b>", nodes.net$id ,
                      "</b><br>Value:", round(nodes.net$label,3))

nodes.net$title = titles.nodes

#Edges Information (Date)
titles.edges = paste0("<p><b>", edges.net$from, " - ", edges.net$to , 
                      "</b><br>Date: ", edges.net$date)

edges.net$title = titles.edges



#legend
legend <- data.frame(shape = c("square", "dot"), 
                     label = c("Party", "Claim"),
                     color = c("black","black"))

#dates to date type
edges.net$date = as.Date(edges.net$date)


visNetwork(nodes = nodes.net, edges = edges.net, main = "Network Visualization",  background = "beige") %>%
  visIgraphLayout(randomSeed = 4) %>%
  visNodes( 
    color = list(highlight = NA),
    borderWidthSelected = 3) %>%
  visEdges(color = "black", hoverWidth = 5, selectionWidth = 3.5, shadow = T)%>%
  visInteraction(dragNodes = TRUE, multiselect = T, navigationButtons = F, zoomView = T) %>%
  visOptions(highlightNearest = list(enabled = T, degree = nrow(edges.net), hover = T, hideColor = 'rgba(0,0,0,0)'),
             selectedBy = list(variable = "type", style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black'), 
             nodesIdSelection = list(enabled = TRUE, useLabels = FALSE, style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black')) %>% 
  visLegend(useGroups = F,addNodes= legend, width = 0.08)


#shiny

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Network", tabName="network-date", icon=icon("connectdevelop"))
  )
)
#body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "network-date",
            textInput(inputId = "num", label = "Type ID",
                      value = "", width = 100, placeholder = NULL),
            span(textOutput(outputId = "id"), style = "color:red"),
            visNetworkOutput("mynetwork"),
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
            )
            
    )
    
  )
)

#user interface

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Network Visualization"),
  sidebar,
  body
)

server <- function(input, output) {
  
  output$id <- renderText({                             #output text stating ID (if exists) or error message
    if (input$num %in% nodes.net$id){
      print(input$num)
    }else{
      print("ID NOT FOUND")                      #message if id is not found
    }
  })
  
  
  output$mynetwork <- renderVisNetwork({
    
    #date filter
    date.filtered.edges <- edges.net %>%
      slice(-c(which(edges.net$date >= input$date)))
    
    date.filtered.nodes <- nodes.net %>%
      slice(c(which(nodes.net$id %in%  c(date.filtered.edges$from, date.filtered.edges$to))))
    
    #opacity 
      #nodes
    node.opacity <- list()
    for (i in 1:nrow(nodes.net)){
      
      if (nodes.net$id[i] %in% c(date.filtered.edges$from, date.filtered.edges$to)){
        node.opacity[i] <- 1
      }
      else{
        node.opacity[i] <- 0
      }
    }
    
    nodes.net$opacity = node.opacity
    
      #edges
    edge.opacity <- list()
    for (i in 1:nrow(edges.net)){
      
      if (edges.net$date[i] < input$date){
        edge.opacity[i] <- 1
      }
      else{
        edge.opacity[i] <- 0
      }
    }
    
    
    edges.net$color.opacity = edge.opacity
    
    if (input$num %in% nodes.net$id){
      
      #Building the Visualization
      visualization <- visNetwork(nodes = nodes.net, edges = edges.net, main = "Network Visualization",background = "beige") %>%
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
                   nodesIdSelection = list(enabled = TRUE, useLabels = FALSE, selected = input$num ,style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black')) %>% 
        visLegend(useGroups = F,addNodes= legend, width = 0.08)
      
    }else{
      visualization <- visNetwork(nodes = nodes.net, edges= edges.net, main = "Network Visualization", background = "beige") %>%
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
    }
  })
  
}
shinyApp(ui, server)


