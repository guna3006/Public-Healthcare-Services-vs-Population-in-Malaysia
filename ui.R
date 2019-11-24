#Importing libraries
#install.packages(c("digest", "shinydashboard", "leaflet", "DT"), ask=FALSE, checkBuilt=TRUE, dependencies=TRUE)
library(digest)
library(shinydashboard)
library(leaflet)
library(DT)

header<-dashboardHeader(title="Healthcare Centre Proposition Based On Location & Population In Malaysia", titleWidth = 900)

body<-dashboardBody(
  fluidRow(
    column(width=12,box("Problem Statement: Nowadays, the government hospitals are congested with the patients that waiting for treatment. As the Malaysian population is steadily increases every year, there is a need to upgrade the existing hospitals or newly build other hospitals. The critical chronic deceases such as heart failure, kidney problem etc. are increase in Malaysia. As population increases, the deceases are expected to increase as well causing congestion to current government hospitals. To justify the location to build new public healthcare services.",width=12,background = "red",style="font-size: 15px")),
    column(width=12,box("Objective: Mapped Malaysia population for every district with the marker to show hospitals location. Solution can be used by policy makers or related government agencies in order to know the current situation happening to every district on the healthcare needs. Based from that they may proceed with initial decision to upgrade or build another hospitals based from the population of particular district and the surrounding district.",width=12,background = "red",style="font-size: 15px")),
    column(width=12,box("Project Prepared by:","WQD170002-Gunasegarran Magadevan, WQD170074-Sivanesan Thayasegamanay, WQD170075-Mathavan Chandrasegaram, WQD180069-Yap Boon Hui",width=8,background = "light-blue",style="font-size: 15px")),
    column(width = 8,box(width = NULL, solidHeader = TRUE,leafletOutput("StateMap", height=700))),
    column(width=4,
           mainPanel(
             selectInput("dataset", "Choose a dataset:",choices = c("Hospitals", "Clinics")),
             
             uiOutput("stateSelect"),
             
             tabsetPanel(
               tabPanel("Pie Chart", plotOutput("Plot", height = "520", width = "100%")),
               tabPanel("Bar Chart", plotOutput("Bar", height = "520", width = "100%")),
               tabPanel("Box Chart", plotOutput("Box", height = "520", width = "100%"))
             )
           )
    ),
    column(width=12,box("Datasets 1. Malaysia District Population  : http://www.data.gov.my/data/ms_MY/dataset/population-by-sex-local-authority-area-and-state-malaysia-2010/resource/52a71815-220d-4633-aaeb-6de26eaf2164",width=8,background = "green",style="font-size: 15px")),
    column(width=12,box("Datasets 2. Government Clinics Datasets   : http://www.data.gov.my/data/dataset/b42c7865-ea9c-4534-9520-451abf2be6b6/resource/f664e25d-9ac8-4e7a-8037-1a0459c4cce0/download/clinicgov.csv",width=8,background = "green",style="font-size: 15px")),
    column(width=12,box("Datasets 3. Government Hospitals Datasets : http://www.data.gov.my/data/dataset/56ef0c05-3b81-4e0b-81db-6466f03bb2f2/resource/a5aa0b10-632b-41c5-b8bb-0093ca6963ea/download/hospital.csv",width=8,background = "green",style="font-size: 15px"))
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)