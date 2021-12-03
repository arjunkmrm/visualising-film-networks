library(shiny)
library(tidyverse)
library(visNetwork)
library(igraph)
library(quanteda)
library(markdown)



function(input, output) {
 
  source("global.R")
  load("global.RData")
  
  output$code.male <- renderPrint({ 
    grapher("male/characters", input$nodes.general, token_filter(pos = input$pos.general, "female", input$decade.general, token.all))[[3]][1:input$nodes.general, ]
  })
  
  output$code.female <- renderPrint({ 
    grapher("female/characters", input$nodes.general, token_filter(pos = input$pos.general, "male", input$decade.general, token.all))[[3]][1:input$nodes.general, ]
  })
  
  output$code.explore <- renderPrint({ 
    grapher(input$word.explore, input$nodes.explore, token_filter3(input$pos.explore, input$decade.explore[1], input$decade.explore[2], token.all))[[3]][1:input$nodes.explore, ]
  })
  
  output$network.male <- renderVisNetwork({
    visIgraph(grapher("male/characters", input$nodes.general, token_filter(pos = input$pos.general, "female", input$decade.general, token.all))[[1]]) %>% 
      visNodes(font = list(color = "white"))
  })
  output$network.female <- renderVisNetwork({
    visIgraph(grapher("female/characters", input$nodes.general, token_filter(pos = input$pos.general, "male", input$decade.general, token.all))[[1]]) %>% 
      visNodes(font = list(color = "white"))
  })
  output$network.explore <- renderVisNetwork({
    visIgraph(grapher(input$word.explore, input$nodes.explore, token_filter3(input$pos.explore, input$decade.explore[1], input$decade.explore[2], token.all))[[1]]) %>% 
      visNodes(font = list(color = "white"))
  })
}





