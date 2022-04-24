#Libraries 

library(dplyr)
library(visNetwork)


#
#
# node's color based on node's values
#
#

## Load data ---------------------------

edges.viz <- read.csv("data/netviz-edges.csv", sep = "|")

nodes.viz <- read.csv("data/netviz-nodes.csv", sep = "|")

#nodes
nodes.net <- as.data.frame(nodes.viz)
colnames(nodes.net) <- c("id", "label", "type")

#Edges
edges.net <- as.data.frame(edges.viz)
colnames(edges.net) <- c("from", "to", "date")

#shape
v.shape <- ifelse(nodes.net$type == "s", "square", "circle")
nodes.net$shape = v.shape


###########################################  color ########################################################

continuous.color <- colorRampPalette(c('yellow',"orange" ,'dark red'))               #color pallet from yellow to dark red


color.background <- continuous.color(3)[cut(nodes.net$label,               #for visualization with 3 colors 
                                            breaks = 3)]

color.background <- continuous.color(nrow(nodes.net))[cut(nodes.net$label, breaks = nrow(nodes.net))]   #for visualization with range of colors.


nodes.net$color.background = color.background                                        #new column with nodes background color

#border color
nodes.net$color.border = "#013848"        #Very dark cyan border color     



########################################### network visualization ########################################################
#Building the Visualization
visNetwork(nodes = nodes.net, edges = edges.net, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
  visIgraphLayout() %>%
  visNodes( 
    color = list(
      border = "#013848",
      highlight = NA),
    borderWidthSelected = 3) %>%
  visEdges(color = "black", hoverWidth = 5, selectionWidth = 3.5, shadow = T)%>%
  visInteraction(dragNodes = TRUE, multiselect = T, navigationButtons = F, zoomView = T) %>%
  visOptions(highlightNearest = list(enabled = T, degree = nrow(edges.net), hover = T, hideColor = 'rgba(0,0,0,0)' ),
             selectedBy = list(variable = "type", style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black'), 
             nodesIdSelection = list(enabled = TRUE, useLabels = FALSE, style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black')) %>% 
  visLegend(useGroups = F,addNodes= legend, width = 0.08)%>%
  visLayout(randomSeed = 11, improvedLayout = T)
