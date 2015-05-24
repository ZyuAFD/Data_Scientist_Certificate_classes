---
title: "Dev Data Prod Proj"
author: "Ziwen Yu"
output: html_document
---
#### Introduction
This is a introduction of the shiny app for "Developing Data Products" course project.

The app contains two files (`ui.R` and `server.R`). It's purpose is to illustrate the fast greedy community detection algorithm using a random network. 

#### User Input
Users can specify the complecity of the network by selecting the number of nodes and number links. `SliderInput` is used for this interactive activity. The ranges of number of nodes and links are 1~30 and 1~100, respectively.

#### Calculation
The `igraph` package is loaded for network analysis. User defined network is first convert into igraph network object. The `fastgreedy.community` function in igraph is then applied to detect the communities in this network. 

A customized function `layout.modular` is defined to produce a clear plot with communities. 

#### Output
The communities detected by fast greedy algorithm is then displayed in the main panel. This result is instantly updated with the input from `SliderInput`.
