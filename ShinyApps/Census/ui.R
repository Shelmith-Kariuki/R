## Date: 23/02/2019
## Project: Shiny Dashboard
## Author: Shelmith Kariuki

## Load the libraries
library(shiny)
library(shinydashboard)
library(shinyEffects)
census_data<-read.csv("Census.csv")

vars<-c("Education","MaritalStatus","Occupation","Relationship",
        "Race","Sex","NativeCountry","Income")

ui <- dashboardPage(skin = "yellow",
                    
  ## Create the header of the dashboard.
  
    dashboardHeader(
        title = span(tags$i(tags$b("Census Data Analytics")),style = "color: black;
                                    font-size: 28px"),titleWidth = 500),
  ## Create the side bar menu
    dashboardSidebar(
          sidebarMenu(

            menuItem(span(tags$i("Introduction"),style="color:white;font-size:23px"),
                  tabName = "introduction",icon = icon("home")),br(),
            menuItem(span(tags$i("Descriptives"),style="color:white;font-size:23px"),
                  tabName = "descriptives", icon = icon("bar-chart-o"),br(),
            menuSubItem(span(tags$i("Univariate"),style="color:white; font-size:18px"),
                        tabName = "univariates",icon =icon("table")),br(),
            menuSubItem(span(tags$i("Multivariate"),style="color:white; font-size:18px"),
                        tabName = "multivariates",icon =icon("table"))),
            menuItem(span(tags$i("Prediction Model"),style="color:white;font-size:23px"),
                  tabName = "predictive",icon = icon("list-alt")),br(),
            
            menuItem(span(tags$i("Created by:"),style="color:white;font-size:23px"),
                     tabName = "author",icon=icon("user"),
                     div(tags$img(src="Shel2.jpg",height="100%", width="100%", align="center")),
                         br(),
                     menuSubItem(span(tags$i("Github"),style="color:white;font-size:18px"),
                                 icon=icon("github"),href = "https://github.com/Shelmith-Kariuki"),br(),
                     menuSubItem(span(tags$i("Twitter"),style="color:white;font-size:18px"),
                                 icon=icon("twitter"),href = "https://twitter.com/Shel_Kariuki"),br(),
                     menuSubItem(span(tags$i("LinkedIn"),style="color:white;font-size:18px"),
                                 icon=icon("linkedin-in"),href = "https://www.linkedin.com/in/shelmith-kariuki-44351363")
                     ,br()))
                      
                    ),
  ## Create the body of the user interface
    dashboardBody(
      tabItems(
        tabItem("introduction",h3("Welcome to the Census Data Analytics Dashboard"),
                fluidRow(includeHTML("www/Intro.html")),br(),br(),
                fluidRow(div(tags$img(src = "Africa.jpeg",height="50%", width="50%",align="middle")))),
        tabItem("univariates",h3("This page shows summary statistics of the various variables"),
                
                fluidRow(
                  box(selectInput("var","Select variable",choices = vars,multiple = F))
                  ),
                fluidRow(
                  box(title = "Frequency Graph",solidHeader = T,status = "info",collapsible = T,
                      plotOutput("freq_graph",height = 500)),
                  box(title = "Frequency Table",solidHeader = T,status = "info",collapsible = T,
                      dataTableOutput("freq_table"))
                )
                ),
        tabItem("multivariates",h3("This page shows summary statistics of various variables, 
                                   grouped by other variables"),
                                   
                fluidRow(
                  box(selectInput("var2","Select main variable",choices = vars, multiple = F)),
                  box(selectInput("var3","Select grouping variables",choices = vars, multiple = F))
                ),
                
                fluidRow(
                  box(title = "Frequency Graph",solidHeader = T, status = "info",collapsible = T,
                      plotOutput("freq_graph_grp")),
                  box(title = "Frequency Table",solidHeader = T, status = "info",collapsible = T,
                      dataTableOutput("freq_table_grp"))
                )
                )
      )
    ))



