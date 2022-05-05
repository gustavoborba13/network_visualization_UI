# User Interface for a Network Visualization 

This Project was designed upon request, to be implemented by Trygg-Hansa. 

## Overview

In this project, we created a simple user interface that presents the visualization of a network. The data used is not real, but was created to represent the interactions between an insurance company's policies and claims in a way that facilitates the understanding of the data. 

The visualization includes colors, shapes, titles, and labels that allow easy interpretation. 

The user interface includes different inputs and outputs:
* text input: filters the network visualization output by an ID typed
* slider input: slider bar that filters the network visualization output by a date-range. 
  * The minimum date presented in the slider is dependent on the text input chosen by the user. The code can be found in the "progress" folder, in the 14_date_slider_range_depending_on_nodeID.R file
* text output: text information repeating the typed ID, or informing that the typed ID does not exist 
* network visualization output: the filtered network visualization of the data

## Future Work
Because of the time, we were not able to experiment all types of solution for the problems, but we believe that the use of reaction functions in the server, combined with some vis Proxy functions available can be of much use in the future developments of this project. 
* In the "experiments" folder, you will find the icon_as_nodes_shape.R file that has an example of how to change the nodes' shapes to icons using Ionicons.
* Regarding the idea of aggregating terminal nodes with same type, we believe that visCollapse() and visUncollapse() of visNetworkProxy objects may be the solution, but we were not able to work it out. 
* visUpdateNodes() and visRemoveNodes() may also be useful for future tasks. 

## Folders
The following folders and included in the repository:
* data: includes the raw data files (netviz-edges.csv and netviz-nodes.csv) that were used in this project.
* progress: includes numbered files that represent each step up to the final result of the project. 
* rsconnect/shinyapps.io/gustavo-borba: contains the connection information to a URL, where the project can be visualized. 
* experiments: inlcudes R files that were not used in the main project, either because execution failed, or a better solution was found. 
  * icon_as_nodes_shape.R can be found here.
* screenshots_of_progess: contain pictures of the development of the visualization and dashboard.

## R Packages Used
* dplyr
* visNetwork
* shiny
* shinyWidgets
* shinydashboard
* igraph

