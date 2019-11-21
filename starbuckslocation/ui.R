library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(data.table)

shinyUI(
  dashboardPage( skin="green",
                 dashboardHeader(title="Starbucks Location Selection" ,titleWidth = 300),
                 dashboardSidebar(
                   sidebarMenu(
                     # menuItem("Home",tabName="Home",icon=icon("home")),
                     menuItem("Find Location",tabName = "Find Location",icon = icon("search"),
                              menuSubItem(icon=NULL,
                                          selectInput("check2_age", "Age Targets :", c(" " = "", list("<5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+")),
                                                      multiple=TRUE)),
                              menuSubItem(icon=NULL,
                                          selectInput("check2_crime", "Crimes:", c(" " = "", list("Safe(<55)", "Relatively Safe(56~129)", "Relatively Dangerous(130~214)", "Dangerous(>214)")))),
                              menuSubItem(icon=NULL,
                                          selectInput("check2_market","Market:", c(" " = "", list("Many", "A few","Don't care")))),
                              menuSubItem(icon=NULL,selectInput("check2_trans", "Transportation:", c(" " = "", list("Many","A few","Don't care")))),
                              menuSubItem(icon=NULL,selectInput("check2_ct", "Cinema/Theater:", c(" " = "", list("Many","A few","Don't care"))) ),
                              menuSubItem(icon=NULL,div(id = "action",actionButton("button2", "Reset")))
                     ),
                     menuItem("Visit My Site", icon = icon("send",lib='glyphicon'), 
                              href = "https://maybeshewillx7.github.io/XiaomengHuang/"))),
                 dashboardBody(
                   
                   tabItem("Find Location", 
                           fluidPage(
                             wellPanel("Map",leafletOutput("map"),
                                       fluidRow(column(1,actionButton("click_back_button",label="Go back to original view")))),
                             wellPanel("Data",dataTableOutput("table2")))
                   )
                 ))
)