## Libraries ---------------------------
library(dplyr)
library(visNetwork)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(igraph)


#In this file I chose 2 icons from the Ionicons (around line 100)
#(Font Awesome icons can also be used in a simple way). 
#I opted for using those icons to make it easy to apply the color function 
#it is also possible to add figures, but I am not sure how to deal with colors 


## Load data ---------------------------

edges.viz <- read.csv("data/netviz-edges.csv", sep = "|")

nodes.viz <- read.csv("data/netviz-nodes.csv", sep = "|")


################################# VisNetwork Approach ######################################

## Nodes and Edges ---------------------

# Nodes 
nodes.net <- as.data.frame(nodes.viz)
colnames(nodes.net) <- c("id", "label", "type") #rename columns for visNetwork standards

# Edges 
edges.net <- as.data.frame(edges.viz)
colnames(edges.net) <- c("from", "to", "date")  #rename columns for visNetwork standards

## Colors -------------------------------

#color pallet from yellow to dark red
continuous.color <- colorRampPalette(c('yellow',"orange" ,'dark red'))  

#list of colors related to nodes values for node background 
color.background <- continuous.color(nrow(nodes.net))[cut(nodes.net$label, 
                                                          breaks = nrow(nodes.net))]
nodes.net$color.background <- color.background      #new column with nodes background color

#border color
nodes.net$color.border <- "#013848"   #Very dark cyan border color   

## legend ------------------------------
legend <- data.frame(
  shape = c("square", "dot", "square", "square", "square"),    #shapes shown in the legend
  label = c("Party", "Claim", "Low", "Mid", "High"),           #labels shown in the legend
  color = c("black", "black", continuous.color(3))             #legend for 3 main colors
)
## Titles -------------------------------

#titles - info shown when mouse hover a node or edge

#nodes information
titles.nodes <- paste0("<p><b>", nodes.net$id ,
                       "</b><br>Value:", round(nodes.net$label,3)) 
nodes.net$title <- titles.nodes                  #new column in nodes.net

#edges Information 
titles.edges <- paste0("<p><b>", edges.net$from, " - ", edges.net$to, 
                       "</b><br>Date: ", edges.net$date)
edges.net$title <- titles.edges                  #new column in edges.net

## Date Type format ---------------------

#dates to date type
edges.net$date <- as.Date(edges.net$date)  #date column as.Date value


## X and Y node Coordinates -------------

#create igraph object 
network.igraph <- graph_from_data_frame(edges.viz, directed = FALSE) 

#Calculate coordinates once for entire component
coordinates <- layout_nicely(network.igraph)    

#extract vertices (nodes) names(IDs)
nodes1<- names(V(network.igraph))                                         

#list with each node's coordinates
pos1 <- setNames(split(coordinates, seq(nrow(coordinates))), nodes1)      

#nodes coordinates x and y 
nodes.net[c("x", "y")] <- do.call(rbind, pos1[nodes.net$id]) #add x and y coords to nodes.net
##########################################################################################
####################################### CHAGING ICONS ####################################

icon.s <- "f2d8"
icon.c <- "f100"
icon.nodes <-  ifelse(nodes.net$type == "s", icon.s, icon.c)

nodes.net$shape <- "icon"
nodes.net$icon.face <- "Ionicons"
nodes.net$icon.code <- icon.nodes

nodes.net$icon.color <- color.background 

##############################################################################################
############################################################################################

visNetwork(nodes = nodes.net, edges = edges.net, main = "Network Visualization", background = "beige") %>%
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
  visLegend(useGroups = F,addNodes= legend, width = 0.08)%>%
  addIonicons()





