#Libraries 

library(dplyr)
library(visNetwork)

#
#
# square = party    , circle = claims
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

#Shape by Type

v.shape <- ifelse(nodes.net$type == "s", "square", "circle")
nodes.net$shape = v.shape

#color
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


#Building the Visualization
visNetwork(nodes = nodes.net, edges = edges.net) %>%
  visIgraphLayout() %>%
  visNodes( 
    color = list(
      highlight = "#FF8000"
    )) %>%
  visEdges(color = "black", hoverWidth = 5)%>%
  visInteraction(dragNodes = TRUE, multiselect = T, navigationButtons = F, zoomView = T) %>%
  visLegend(useGroups = F,addNodes= legend, width = 0.08)%>%
  visLayout(randomSeed = 11)































