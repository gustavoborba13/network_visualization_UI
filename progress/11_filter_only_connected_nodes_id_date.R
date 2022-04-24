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


## X and Y node Coordinates
network.igraph <- graph_from_data_frame(edges.viz, directed = F)                       # create igraph object
coordinates <- layout_nicely(network.igraph)                                           # Calculate coordinates once for entire component
nodes1<- names(V(network.igraph))
pos1 <- setNames(split(coordinates, seq(nrow(coordinates))), nodes1)

#nodes coordinates x and y 
nodes.net[c("x", "y")] = do.call(rbind, pos1[nodes.net$id])

edges.viz[c("x", "y")] <- do.call(rbind, pos1[edges.viz$source_id])
edges.viz[c("xend", "yend")] <- do.call(rbind, pos1[edges.viz$target_id])

## connected nodes
# Create adjacency mappings
grp1 <- split(edges.viz, edges.viz$source_id)
d1 <- lapply(grp1, function(x) x$target_id)

grp2 <- split(edges.viz, edges.viz$target_id)
d2 <- lapply(grp2, function(x) x$source_id)

d <- c(d1, d2)                                           #list with nodes and al its connections
rm(d1, d2,grp1, grp2) #remove unnecessary objects from environment



###################  find nodes and its connections ######################

node <- "s3"  # Selected node, find all nodes in the same component
seen <- c()  # Will contain all nodes in the component
next_d <- node  # Nodes in next order of neighbours

while (length(next_d) > 0) {
  # Nodes in current level
  this_d <- next_d
  # Nodes in next level, i.e. neighbours of nodes in current level 
  next_d <- c()  
  
  # Go through nodes in current level
  for (v in this_d) {
    # If node hasn't been visited
    if (!(v %in% seen)) {
      # Add the node
      seen <- unique(c(seen, v))
      # And add its neighbours to search through next iteration 
      next_d <- unique(c(next_d, d[[v]]))
    }
  }
}

list.connected.edges <- edges.net[edges.viz$target_id %in% seen, ]

list.connected.nodes <- nodes.net %>%
  slice(c(which(nodes.net$id %in%  c(list.connected.edges$from, list.connected.edges$to))))




##################

axis <- list(
  title = "",
  showgrid = FALSE,
  showticklabels = FALSE,
  zeroline = FALSE)

plot_ly() %>%
  # edges
  add_segments(data = edges.viz,
               x = ~x,
               xend = ~xend,
               y = ~y,
               yend = ~yend,
               name = "Edges") %>%
  # nodes
  add_trace(x = coordinates[, 1],
            y = coordinates[, 2],
            mode = "markers",
            type = "scatter",
            name = "Nodes") %>%
  layout(xaxis = axis,
         yaxis = axis)



#Building the Visualization
visualization <- visNetwork(nodes = nodes.net, edges = edges.net, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
  visIgraphLayout() %>%
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

#############################################################

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
      slice(c(which(edges.net$date < input$date)))
    
    date.filtered.nodes <- nodes.net %>%
      slice(c(which(nodes.net$id %in%  c(date.filtered.edges$from, date.filtered.edges$to))))
    
    if (input$num %in% nodes.net$id){
      
      grp1 <- split(date.filtered.edges, date.filtered.edges$from)
      d1 <- lapply(grp1, function(x) x$to)
      
      grp2 <- split(date.filtered.edges, date.filtered.edges$to)
      d2 <- lapply(grp2, function(x) x$from)
      
      d <- c(d1, d2)                                           #list with nodes and al its connections
      rm(d1, d2,grp1, grp2) #remove unnecessary objects from environment
      
      node <- input$num  # Selected node, find all nodes in the same component
      seen <- c()  # Will contain all nodes in the component
      next_d <- node  # Nodes in next order of neighbours
      
      while (length(next_d) > 0) {
        # Nodes in current level
        this_d <- next_d
        # Nodes in next level, i.e. neighbours of nodes in current level 
        next_d <- c()  
        
        # Go through nodes in current level
        for (v in this_d) {
          # If node hasn't been visited
          if (!(v %in% seen)) {
            # Add the node
            seen <- unique(c(seen, v))
            # And add its neighbours to search through next iteration 
            next_d <- unique(c(next_d, d[[v]]))
          }
        }
      }
      
      
      list.connected.edges <- date.filtered.edges[c(date.filtered.edges$from,date.filtered.edges$to) %in% seen, ]
      
      list.connected.nodes <- nodes.net %>%
        slice(c(which(nodes.net$id %in%  c(list.connected.edges$from, list.connected.edges$to))))
      
      
      #Building the Visualization
      visualization <- visNetwork(nodes = list.connected.nodes, edges =date.filtered.edges, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
        visIgraphLayout() %>%
        visNodes( 
          color = list(
            highlight = NA),
          borderWidthSelected = 3) %>%
        visEdges(color = "#0D0D0D", hoverWidth = 5, selectionWidth = 3.5, shadow = T)%>%
        visInteraction(dragNodes = TRUE, multiselect = T, navigationButtons = F, zoomView = T) %>%
        visOptions(highlightNearest = list(enabled = T, degree = nrow(edges.net), hover = T),
                   selectedBy = list(variable = "type", style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black'), 
                   nodesIdSelection = list(enabled = TRUE, useLabels = FALSE, selected = input$num ,style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black')) %>% 
        visLegend(useGroups = F,addNodes= legend, width = 0.08)
      
    }else{
      visualization <- visNetwork(nodes = date.filtered.nodes, edges= date.filtered.edges, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
        visIgraphLayout() %>%
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
