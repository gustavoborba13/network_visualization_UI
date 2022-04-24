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


#Building the Visualization
visNetwork(nodes = nodes.net, edges = edges.net, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
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
  visLegend(useGroups = F,addNodes= legend, width = 0.08)


######################################### shiny ######################################################
 


ui <- fluidPage(
  textInput(inputId = "num", label = "Type ID",
            value = "", width = 100, placeholder = NULL),   #text input
  visNetworkOutput("mynetwork"),                            #visnet output
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
  )
  )

server <- function(input, output) {
  output$value <- renderText({ input$caption })
  output$mynetwork <- renderVisNetwork({
    #date filter
    date.filtered.edges <- edges.net %>%
      slice(c(which(edges.net$date <= input$date)))
    
    date.filtered.nodes <- nodes.net %>%
      slice(c(which(nodes.net$id %in%  c(date.filtered.edges$from, date.filtered.edges$to))))
    
    if (input$num %in% nodes.net$id){
      
      ## connected nodes
      # Create adjacency mappings
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
      visNetwork(nodes = list.connected.nodes, edges =list.connected.edges, main = "Trygg-Hansa", background = "beige") %>%
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
                   nodesIdSelection = list(enabled = TRUE, useLabels = FALSE, selected = node ,style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black')) %>% 
        visLegend(useGroups = F,addNodes= legend, width = 0.08)
      
      
      
    }else{
      visualization <- visNetwork(nodes = date.filtered.nodes, edges= date.filtered.edges, main = "Trygg-Hansa", background = "beige") %>%
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





