library(tidyverse)

#the idea is that the slider minimum date will be updated depending on the node selected
#in a way that the minimum date possible is the first date in which the selected node exists


#thought process 

which(edges.net$from == "s2")

edges.net$date[13]

edges.net$date[c(which(edges.net$from == "s2"))]

c(which(edges.net$from == "s2"), which(edges.net$to == "s2"))

c(which(edges.net$from == "c0"), which(edges.net$to == "c0"))


edges.net$date[c(which(edges.net$from == "e0"), which(edges.net$to == "e0"))]

#minimum date of an edge given a selected node ID
min(edges.net$date[c(which(edges.net$from == "e0"), which(edges.net$to == "e0"))])
###################################################################################################



date.choices <- sort(unique(edges.net$date[order(edges.net$date)]))  #these are all possible dates in the dataset


################################## Shiny Dashboard #########################################

## User Interface (UI) ------------------

#sidebar
sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Network",                   #name on sidebar
                       tabName="network-date", 
                       icon=icon("connectdevelop")) #sidebar icon
  )
)

#body
body <- dashboardBody(        #dashboard body, include the Network clicked on sidebar
  tabItems(
    tabItem(tabName = "network-date",
            textInput(inputId = "num",         #textInput for user to type ID code
                      label = "Type ID",
                      value = edges.net$from[which(edges.net$date == min(edges.net$date))][1],  #set initial ID for when the visualization is launched
                      width = 100, 
                      placeholder = NULL),    
            span(textOutput(outputId = "id"),  #textOutput stating the ID or error in RED
                 style = "color:red"),    
            visNetworkOutput("mynetwork"),     #visNetworkOutput based on ID and date range
            sliderTextInput(                   #slider to chose date
              inputId = "date",
              label = "Dates:",
              choices = unique(edges.net$date[order(edges.net$date)]), #unique dates from data
              from_min = min(edges.net$date),       #from oldest edge date
              to_max = max(edges.net$date),         #to newest edge date
              selected = max(edges.net$date),       # start with newest selected
              grid = TRUE,                          # ticks and dates showing in slider
              width = '95%',
              animate = animationOptions(interval = 100)
            )
            
    )
    
  )
)

#user interface
ui <- dashboardPage(
  skin = "red",                                           #company's main color
  dashboardHeader(title = "Network Visualization"),       #dashboard title 
  sidebar,
  body
)


## Server -------------------------------

server <- function(input, output, session) {
  output$id <- renderText({             #output text stating ID (if exists) or error message
    if (input$num %in% nodes.net$id){
      print(input$num)
    }else{
      print("ID NOT FOUND")             #message if id is not found
    }
  })
  
  
###############################################################################################################
  ######################################################## UPDATE SLIDER ####################################
  observeEvent(input$num, {
    data_subset <- 
      edges.net %>%
      filter(from == input$num | to == input$num)

    if (nrow(data_subset) > 0) {
      dates <- data_subset %>%
        pull(date)
      
      a <- which(date.choices == min(dates))
      b <- length(date.choices)
      
      updateSliderTextInput(session, "date", choices = date.choices[a:b], selected = max(edges.net$date))
    }
  })
 ############################################################################################################# 
##############################################################################################################
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
                                           style = 'width: 0px; 
                                                    height: 0px; 
                                                    background: #f8f8f8; 
                                                    color: black')) %>% 
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

