#Libraries 

library(dplyr)
library(visNetwork)



#
#
# titles that pop up when mouse hover over nodes and edges 
#
#



## Load data ---------------------------

edges.viz <- read.csv("data/netviz-edges.csv", sep = "|")

nodes.viz <- read.csv("data/netviz-nodes.csv", sep = "|")


#undirect network
net.viz2 <- graph_from_data_frame(d=edges.viz, vertices=nodes.viz, directed=F)


##################################### VISNETWORK APPROACH ###########################################

#nodes
nodes.net <- as.data.frame(nodes.viz)
colnames(nodes.net) <- c("id", "label", "type")

#Edges
edges.net <- as.data.frame(edges.viz)
colnames(edges.net) <- c("from", "to", "date")

#Shape by Type
## node type "s" = "square" shape, others = "circle" shape

v.shape <- ifelse(nodes.net$type == "s", "square", "circle")
nodes.net$shape = v.shape

#legend
legend <- data.frame(shape = c("square", "dot"), 
                     label = c("Party", "Claim"),
                     color = c("black","black"))

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




######################### tooltip (html or character), when the mouse is above ##################################



#nodes
titles.nodes = paste0("<p><b>", nodes.net$id ,
                      "</b><br>Value:", round(nodes.net$label,3))       #Node Information (Value)

nodes.net$title = titles.nodes

#edges
titles.edges = paste0("<p><b>", edges.net$from, " - ", edges.net$to , 
                      "</b><br>Date: ", edges.net$date)                 #Edges Information (Date)

edges.net$title = titles.edges

############################################################ vis network #####################

#Building the Visualization
visNetwork(nodes = nodes.net, edges = edges.net, main = "Network Visualization", submain = "Trygg-Hansa", background = "beige") %>%
  visIgraphLayout() %>%
  visNodes( 
    color = list(
      border = "#013848",
      highlight = NA
    )) %>%
  visEdges(color = "black", hoverWidth = 5)%>%
  visInteraction(dragNodes = TRUE, multiselect = T, navigationButtons = F, zoomView = T) %>%
  visOptions(highlightNearest = list(enabled = T, degree = nrow(edges.net), hover = T),
             selectedBy = list(variable = "type", style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black'), 
             nodesIdSelection = list(enabled = TRUE, useLabels = FALSE, style = 'width: 150px; height: 26px;
   background: #f8f8f8;
   color: black')) %>% 
  visLegend(useGroups = F,addNodes= legend, width = 0.08)%>%
  visLayout(randomSeed = 11)

