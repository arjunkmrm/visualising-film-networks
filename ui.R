library(shiny)
library(tidyverse)
library(visNetwork)
library(igraph)
library(quanteda)
library(markdown)
library(shinycssloaders)


source("global.R")
load("global.RData")
#options(spinner.type=3, color="#0dc5c1")

navbarPage(
  theme = bslib::bs_theme(bootswatch = "lux",
                          bg = "#202123",
                          fg = "#FFFFFF"),
  "Movie Plot Co-occurence Networks",
  tabPanel("General",
           fluidPage(
             # App title ----
             title = "Network visualisation",
             h5("Set network parameters"),
             fluidRow(
               column(3,
                      sliderInput("decade.general", "Select decade", min = 1940, max = 2010, value = c(1940), step = 10)
               ),
               column(3,
                      selectInput("pos.general", "Select part of speech", choices = c("verb", "adj", "noun"))
               ),
               column(3,
                      sliderInput('nodes.general', 'Number of immediately co-occurring nodes (green nodes)', 
                                  min=3, max=30, value=10, 
                                  step=1, round=0)
               ),
             ),
             fluidRow(
               column(12, p("Note: Green nodes in the network represent immediate co-occurences with the central blue node - here it's either 
                            male or female characters.
                            Orange nodes are immediate co-occurences of green nodes and hence secondary co-occurences of the central node.
                            The thickness of the edges denote the log likelihood of co-occurrence.
                            Scroll over the networks to examine individual nodes.
                            Scroll down to read the complete documentation", style = "font-si11pt")),
             ),
             fluidRow(
               column(3, h4("Male"), withSpinner(visNetworkOutput("network.male"), type = 3, color = "#FFFFFF", color.background = "#202123")),
               column(3, h6("Immediate co-occurrences ranked by log likelihood - male (green nodes)"), verbatimTextOutput("code.male")),
               column(3, h4("Female"), withSpinner(visNetworkOutput("network.female"), type = 3, color = "#FFFFFF", color.background = "#202123")),
               column(3, h6("Immediate co-occurrences ranked by log likelihood - female (green nodes)"), verbatimTextOutput("code.female"))
             ),
             fluidRow(
               column(9, includeMarkdown("introduction.Rmd")),
             )
           )
           
  ),
  tabPanel("Explore",
           fluidPage(
             # App title ----
             title = "Network visualisation",
             h5("Set network parameters"),
             fluidRow(
               column(3,
                      sliderInput("decade.explore", "Select decade range", min = 1940, max = 2010, value = c(1940, 1960), step = 10)
               ),
               column(3,
                      textInput("word.explore", "Enter word you want to explore along with the pos e.g. wealthy/adj; run/verb; father/noun", value = "marry/verb", width = NULL, placeholder = NULL)
               ),
               column(3,
                      sliderInput('nodes.explore', 'Number of immediately co-occurring nodes (green nodes)', 
                                  min=3, max=30, value=10, 
                                  step=1, round=0)
               ),
               column(3,
                      selectInput("pos.explore", "Select part of speech. Note that male/female character nodes are not removed for greater context", choices = c("all", "verb", "adj", "noun"))
               ),
             ),
             fluidRow(
               column(9, withSpinner(visNetworkOutput("network.explore"), type = 3, color = "#FFFFFF", color.background = "#202123")),
               column(3, h6("co-occuring words ranked by log likelihood"), verbatimTextOutput("code.explore"))
             )
           )
  )
)




