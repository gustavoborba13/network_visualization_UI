# User Interface for a Network Visualization 

This Project was designed upon request, to be implemented by Trygg-Hansa. 

## Overview

In this project, we created a simple user interface that presents the visualization of a network. The data used is not real, but was created to represent the interactions between an insurance company's policies and claims in a way that facilitates the understanding of the data. 

The visualization includes colors, shapes, titles, and labels that allow easy interpretation. 

The user interface includes different inputs and outputs:
* text input: filters the network visualization output by an ID typed
* slider input: slider bar that filters the network visualization output by a date-range
* text output: text information repeating the typed ID, or informing that the typed ID does not exist 
* network visualization output: the filtered network visualization of the data


## Folders
The following folders and included in the repository:
* data: includes the raw data files (netviz-edges.csv and netviz-nodes.csv) that were used in this project.
* progress: includes numbered files that represent each step up to the final result of the project. 
* rsconnect/shinyapps.io/gustavo-borba: contains the connection information to a URL, where the project can be visualized. 

## R Packages Used
* dplyr
* visNetwork
* shiny
* shinyWidgets
* shinydashboard
* igraph

