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

#nodes.net$color.background[which(visualization$x$nodes$id == input$num )] = NA

#border color
nodes.net$color.border = "#013848"
#nodes.net$color.border[which(visualization$x$nodes$id == input$num)] = "blue"

#nodes size
nodes.net$size = 25

#nodes.net$size[which(visualization$x$nodes$id == input$num)] = 55


#tooltip (html or character), when the mouse is above
#Node Information (Value)
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



#Building the Visualization
visualization <- visNetwork(nodes = nodes.net, edges = edges.net, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
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

visualization



######################3 SHINY DASHBOARD ##################################3

#sidebar
sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Network", tabName="network-date", icon=icon("calendar"))
  )
)

#body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "network-date",
            textInput(inputId = "num", label = "Type ID",
                      value = "", width = 100, placeholder = NULL),
            visNetworkOutput("mynetwork"),
            sliderInput(
              inputId = "date",
              label = "Dates:",
              min = min(edges.net$date)+1,
              max = max(edges.net$date),
              value = max(edges.net$date),
              timeFormat = "%Y-%m-%d",
              step = 60,
              animate = animationOptions(interval = 300),
              width = '95%'
            ),
            selectInput("Focus", "Focus on node :",
                        nodes.net$id)
            
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
  

  output$mynetwork <- renderVisNetwork({

    #date filter
    date.filtered.edges <- edges.net
    
    date.filtered.nodes <- nodes.net 
    
    if (input$num %in% nodes.net$id){
      
      #Building the Visualization
      visualization <- visNetwork(nodes = date.filtered.nodes, edges = date.filtered.edges, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
        visIgraphLayout(randomSeed = 4) %>%
        visNodes( 
          color = list(
            highlight = NA),
          borderWidthSelected = 3) %>%
        visEdges(color = "black", hoverWidth = 5, selectionWidth = 3.5, shadow = T)%>%
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
      visualization <- visNetwork(nodes = date.filtered.nodes, edges =date.filtered.edges, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
        visIgraphLayout(randomSeed = 4) %>%
        visNodes( 
          color = list(
            highlight = NA),
          borderWidthSelected = 3) %>%
        visEdges(color = "black", hoverWidth = 5, selectionWidth = 3.5, shadow = T)%>%
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
  
  myVisNetworkProxy <- visNetworkProxy("mynetwork")
  
  observe({
    #date filter
    date.filtered.edges <- edges.net %>%
      slice(-c(which(edges.net$date >= input$date)))
    
    date.filtered.nodes <- nodes.net %>%
      slice(c(which(nodes.net$id %in%  c(date.filtered.edges$from, date.filtered.edges$to))))
    
    hiddenNodes <- anti_join(nodes.net, date.filtered.nodes)
    visRemoveNodes(myVisNetworkProxy, id = hiddenNodes$id)
    visUpdateNodes(myVisNetworkProxy, nodes = date.filtered.nodes)
    
  })
  
  observe({
    visNetworkProxy("mynetwork") %>%
      visFocus(id = input$Focus, scale = 0.6, animation = list(duration = 1200, easingFunction = "easeInOutQuad"))
  })
  
}
shinyApp(ui, server)
