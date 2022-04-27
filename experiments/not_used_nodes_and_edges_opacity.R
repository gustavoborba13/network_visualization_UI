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


#opacity
# date.filtered.edges <- edges.net %>%
#   slice(-c(which(edges.net$date >= "2017-07-31")))
# 
# date.filtered.nodes <- nodes.net %>%
#   slice(c(which(nodes.net$id %in%  c(date.filtered.edges$from, date.filtered.edges$to))))
# 
# 
# node.opacity <- list()
# for (i in 1:nrow(nodes.net)){
#   
#   if (nodes.net$id[i] %in% c(date.filtered.edges$from, date.filtered.edges$to)){
#     node.opacity[i] <- 1
#   }
#   else{
#     node.opacity[i] <- 0
#   }
# }
# 
# nodes.net$opacity = node.opacity
# 
# 
# 
# edge.opacity <- list()
# for (i in 1:nrow(edges.net)){
#   
#   if (edges.net$date[i] < "2017-07-31"){
#     edge.opacity[i] <- 1
#   }
#   else{
#     edge.opacity[i] <- 0
#   }
# }
# 
# 
# edges.net$color.opacity = edge.opacity
# edges.net$color.color = "#0D0D0D"



#Building the Visualization
visNetwork(nodes = nodes.net, edges = edges.net, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
  visIgraphLayout(randomSeed = 4) %>%
  visNodes( 
    color = list(highlight = NA),
    borderWidthSelected = 3) %>%
  visEdges(hoverWidth = 5, selectionWidth = 3.5, shadow = T)%>%
  visInteraction(dragNodes = TRUE, multiselect = T, navigationButtons = F, zoomView = T) %>%
  visOptions(highlightNearest = list(enabled = T, degree = nrow(edges.net), hover = T, hideColor = 'rgba(0,0,0,0)'),
             selectedBy = list(variable = "type", style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black'), 
             nodesIdSelection = list(enabled = TRUE, useLabels = FALSE, style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black')) %>% 
  visLegend(useGroups = F,addNodes= legend, width = 0.08)


######################3 SHINY DASHBOARD ##################################3

ui <- fluidPage(
  sliderInput(
    inputId = "date",
    label = "Dates:",
    min = min(edges.net$date)+1,
    max = max(edges.net$date),
    value = max(edges.net$date),
    timeFormat = "%Y-%m-%d",
    step = 60,
    animate = animationOptions(interval = 300)
  ),
  visNetworkOutput("mynetwork")
)

server <- function(input, output) {
  output$mynetwork <- renderVisNetwork({
    
    date.filtered.edges <- edges.net %>%
      slice(-c(which(edges.net$date >= input$date)))
    
    date.filtered.nodes <- nodes.net %>%
      slice(c(which(nodes.net$id %in%  c(date.filtered.edges$from, date.filtered.edges$to))))
    
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
  
    #Building the Visualization
    visNetwork(nodes = nodes.net, edges = edges.net, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
      visIgraphLayout(randomSeed = 4) %>%
      visNodes( 
        color = list(highlight = NA),
        #opacity = 1,
        borderWidthSelected = 3) %>%
      visEdges(color = "#0D0D0D", hoverWidth = 5, selectionWidth = 3.5, shadow = T)%>%
      visInteraction(dragNodes = TRUE, multiselect = T, navigationButtons = F, zoomView = T) %>%
      visOptions(highlightNearest = list(enabled = T, degree = nrow(edges.net), hover = T, hideColor = 'rgba(0,0,0,0)'),
                 selectedBy = list(variable = "type", style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black'), 
                 nodesIdSelection = list(enabled = TRUE, useLabels = FALSE, style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black')) %>% 
      visLegend(useGroups = F,addNodes= legend, width = 0.08)
  })
}
shinyApp(ui, server)
