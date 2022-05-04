#Libraries 

library(dplyr)
library(visNetwork)



#
#
# node's shape based on node's type
# type 's' = square , other types = circle
# square = party    , circle = claims
#
#

#read data
edges.viz <- read.csv("data/netviz-edges.csv", sep = "|")

nodes.viz <- read.csv("data/netviz-nodes.csv", sep = "|")

#checking length of each 
nrow(nodes.viz); length(unique(nodes.viz$node_id))

nrow(edges.viz); nrow(unique(edges.viz[,c("source_id", "target_id")]))

table(edges.viz['target_id'])

#undirect network
net.viz2 <- graph_from_data_frame(d=edges.viz, vertices=nodes.viz, directed= FALSE)

#explorations on network
E(net.viz2)       # The edges of the "net.viz" object

V(net.viz2)       # The vertices of the "net.viz" object

V(net.viz2)$value #vertices values

E(net.viz2)$edge_date #dates

V(net.viz2)$node_type #types

as_data_frame(net.viz2, what="vertices")

as_data_frame(net.viz2, what="edges")

##################################### VISNETWORK APPROACH ##################################

#nodes
nodes.net <- as.data.frame(nodes.viz)
colnames(nodes.net) <- c("id", "label", "type")

#Edges
edges.net <- as.data.frame(edges.viz)
colnames(edges.net) <- c("from", "to", "date")


############################################################ SHAPES ####################################################
# node type "s" = "square" shape, others = "circle" shape

v.shape <- ifelse(nodes.net$type == "s", "square", "circle")
nodes.net$shape = v.shape

########################################### network visualization ########################################################

visNetwork(nodes = nodes.net, edges = edges.net) %>%
  visIgraphLayout() %>%
  visNodes( 
           color = list(
             background = "#0085AF",
             border = "#013848",
             highlight = "#FF8000"
           )) %>%
  visEdges(color = "black")%>%
  visInteraction(dragNodes = TRUE, multiselect = T, navigationButtons = F, zoomView = T) %>%
  visOptions(highlightNearest = list(enabled = T, degree = nrow(edges.net), hover = T)) %>% 
  visLayout(randomSeed = 11)
