#the idea here is to exclude the select by tabs that existed in the previous versions of the project

#excluding the select by type was easy, I just had to delete the selectBy function from the visNetwork

#however, excluding the select by ID is tough because it affects the highlighting of the selected node

#I believe that it is possible to mimic that highlight function with a visNetworkProxy, but I will not have time to do so

#therefore, my way around it was to "hide" the tab in the visualization. by setting it's size to zero


server <- function(input, output) {
  output$id <- renderText({             #output text stating ID (if exists) or error message
    if (input$num %in% nodes.net$id){
      print(input$num)
    }else{
      print("ID NOT FOUND")             #message if id is not found
    }
  })
  
  
  output$mynetwork <- renderVisNetwork({
    
    ##date filter
    #filtering edges that are outside date range
    date.filtered.edges <- edges.net %>%
      slice(c(which(edges.net$date <= input$date)))
    
    #filtering nodes that are not in  filtered edge data
    date.filtered.nodes <- nodes.net %>%
      slice(c(which(nodes.net$id %in% c(date.filtered.edges$from, date.filtered.edges$to))))
    
    if (input$num %in% nodes.net$id){
      
      ## connected nodes
      # Create adjacency mappings
      grp1 <- split(date.filtered.edges, date.filtered.edges$from) 
      d1 <- lapply(grp1, function(x) x$to)
      
      grp2 <- split(date.filtered.edges, date.filtered.edges$to)
      d2 <- lapply(grp2, function(x) x$from)
      
      d <- c(d1, d2)            #list with nodes and all its connections
      rm(d1, d2,grp1, grp2)     #remove unnecessary objects from environment
      
      node <- input$num         #Selected node, find all nodes in the same component
      seen <- c()               #Will contain all nodes in the component
      next_d <- node            #Nodes in next order of neighbors
      
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
      
      #filtered list of connected edges that are inside date range
      list.connected.edges <- date.filtered.edges[c(date.filtered.edges$from,
                                                    date.filtered.edges$to) %in% seen,]
      
      #filtered list of connected nodes that are inside date range
      list.connected.nodes <- nodes.net %>%
        slice(c(which(nodes.net$id %in% c(list.connected.edges$from,
                                          list.connected.edges$to))))
      
      
      
      #Building the Visualization
      visNetwork(nodes = list.connected.nodes, 
                 edges =list.connected.edges, 
                 main = "Network", 
                 background = NA) %>%
        visIgraphLayout() %>%
        visNodes( 
          color = list(highlight = NA),
          borderWidthSelected = 3) %>%
        visEdges(color = "#0D0D0D", 
                 hoverWidth = 5, 
                 selectionWidth = 3.5, 
                 shadow =TRUE)%>%
        visInteraction(dragNodes = TRUE, 
                       multiselect =TRUE, 
                       navigationButtons = FALSE, 
                       zoomView =TRUE) %>%
        visOptions(highlightNearest = list(enabled =TRUE, 
                                           degree = nrow(edges.net), 
                                           hover =TRUE),
                   nodesIdSelection = list(enabled = TRUE, 
                                           useLabels = FALSE, 
                                           selected = node,
        #here is where i set width and height to zero. 
                                           style = 'width: 0px;      
                                                    height: 0px; 
                                                    background: NA; 
                                                    color: NA')) %>% 
        visLegend(useGroups = FALSE,
                  addNodes= legend, 
                  width = 0.08)
      
      
      
    }else{
      visualization <- visNetwork(nodes = date.filtered.nodes, 
                                  edges= date.filtered.edges, 
                                  main = "Network", 
                                  background = NA) %>%
        visIgraphLayout() %>%
        visNodes( 
          color = list(highlight = NA),
          borderWidthSelected = 3) %>%
        visEdges(color = "#0D0D0D",
                 hoverWidth = 5,
                 selectionWidth = 3.5,
                 shadow =TRUE)%>%
        visInteraction(dragNodes = TRUE,
                       multiselect =TRUE, 
                       navigationButtons = FALSE, 
                       zoomView =TRUE) %>%
        visOptions(highlightNearest = list(enabled =TRUE, 
                                           degree = nrow(edges.net), 
                                           hover =TRUE, 
                                           hideColor = 'rgba(0,0,0,0)'),
                   nodesIdSelection = list(enabled = TRUE, 
                                           useLabels = FALSE,
        #here is where i set width and height to zero.
                                           style = 'width: 0px; 
                                               height: 0px;
                                               background: #f8f8f8;
                                               color: black')) %>% 
        visLegend(useGroups = FALSE,
                  addNodes= legend, 
                  width = 0.08)
    }
    
  })
  
  
}

## Shiny App ----------------------------
shinyApp(ui, server)
