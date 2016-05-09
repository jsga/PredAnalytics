# Load packages
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dygraphs)
library(xts)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)




ui <- dashboardPage(skin = "purple",
                    ## HEADER
                    dashboardHeader(title = "Predictive analytics"),
                    ## SIDEBAR
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Predictions", tabName = "prediction", icon = icon("cubes")),
                        menuItem("Analysis", tabName = "analysis", icon = icon("bar-chart")),
                        menuItem("About", tabName = "about", icon = icon("info"))
                      )
                    ),
                    
                    ## DASHBOARD BODY
                    dashboardBody(
                      tabItems(
                        # Dashboard tab iterm
                        tabItem(tabName = "prediction",
                                fluidRow(
                                  box(width = 12,dygraphOutput("Time_series", height = 250)),
                                  box(title = 'Inputs',
                                      selectInput("Area", 
                                                  label = "Choose Area",
                                                  choices = list("Denmark West", "Denmark East", "Whole Denmark"),
                                                  selected = "Denmark West"),
                                      
                                      helpText("Please choose the coefficients of the fitted model."),
                                      
                                      sliderInput("AR", label = h6("Auto regressive"),
                                                  min = 0, max = 5, value = 2),
                                      sliderInput("MA", label = h6("Moving average"),
                                                  min = 0, max = 5 ,value = 2),
                                      sliderInput("Dummy", label = h6("Number of dummy seasonal variables"),
                                                  min = 1, max = 12 ,value = 6)
                                  ),
                                  valueBoxOutput("mape")
                                  
                                )),
                        
                        # Widget tab item
                        tabItem(tabName = "analysis",
                                h3(em("All generalizations are false, including this one."),'Mark Twain',align = "center"),
                                fluidRow(width=3,
                                         radioButtons("radio", label = h3("Area displayed"),
                                                      choices = list("DK1" = 1, "DK2" = 2), 
                                                      selected = 1)
                                         ,
                                         box(width = 12,  plotlyOutput("Analysis"))
                                ),
                                fluidRow(
                                  box(width = 5,dataTableOutput("table"))
                                )
                                
                        ),
                        # About tab iterm
                        tabItem(tabName = "about",
                                fluidRow(
                                      h3("This dashboard analyzes the wind power production.",align = "center"),
                                      h3("Contact: myemail [at] gmail.com.",align = "center")
                                ))
                      )
                    )
)
