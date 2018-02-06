#Importing libraries
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("leaflet")
library(leaflet)
#install.packages("DT")
library(DT)

header<-dashboardHeader(title="Healthcare Services vs Population in Malaysia", titleWidth = 450)

body<-dashboardBody(
  fluidRow(
    column(width = 8,
           box(width = NULL, solidHeader = TRUE,leafletOutput("StateMap", height=700))
    ),
    column(width=4,
           mainPanel(
             selectInput("dataset", "Choose a dataset:",choices = c("Hospitals", "Clinics")),
             
             uiOutput("stateSelect"),
             
             tabsetPanel(
               tabPanel("Pie Chart", plotOutput("Plot", height = "530", width = "100%")),
               tabPanel("Bar Chart", plotOutput("Bar", height = "530", width = "100%"))
             )
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)