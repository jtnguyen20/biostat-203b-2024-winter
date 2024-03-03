#load packages
library(shiny)
library(ggplot2)
library(bigrquery)

#load data from local file
mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds")

#connect to bigquery 
#path to the service account token 
#satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
#bigquery authentication using service account
#bq_auth(path = satoken)
# connect to the bigquery database `biostat-203b-2024-winter.mimic4_v2_2`
#con_bq <- dbConnect(
  #bigrquery::bigquery(),
  #project = "biostat-203b-2024-winter",
  #dataset = "mimic4_v2_2",
  #billing = "biostat-203b-2024-winter"
#)
#con_bq

#load data from bigquery database
transfers_tble <- tbl(con_bq, "transfers") 

#define ui 
ui <- fluidPage(
  #application title
  titlePanel("Distribution of Patient Characteristics in MIMIC-IV ICU Cohort"),
  #create multiple tabs
  tabsetPanel(
    tabPanel("Patient Characteristics", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", 
                             "Choose variable of interest:",
                             choices = c(
                               "first_careunit",
                               "last_careunit",
                               "admission_type",
                               "admission_location",
                               "discharge_location",
                               "insurance",
                               "language",
                               "marital_status",
                               "race",
                               "gender",
                               "los_long"
                             ),
                             selected = "first_careunit")
               ),
               mainPanel(
                 plotOutput("charPlot")
               )
             )
    ),
    tabPanel("Patient ADT and ICU Stay Information",
             sidebarLayout(
               sidebarPanel(
                 numericInput("subject_id", 
                              "Enter subject_id:",
                              value = 10000032,
                              min = 10000032,
                              max = 19999987,
                              step = 1)
               ),
               mainPanel(
                 plotOutput("adtPlot")
               )
             )
    )
  )
)

#define server 
server <- function(input, output) {

  output$charPlot <- renderPlot({
    mimic_icu_cohort |>
      ggplot() +
      geom_bar(mapping = aes(y = .data[[input$variable]])) +
      labs(
        title = "Patient Count by Variable of Interest",
        y = input$variable
      ) 
  })
  
  output$adtPlot <- renderPlot({
    ggplot() +
      geom_segment(
        data = transfers_tble |> filter(subject_id == input$subject_id),
        mapping = aes(
          x = intime,
          xend = outtime, 
          y = "ADT",
          yend = "ADT",
          color = careunit,
          linewidth = str_detect(careunit, "(ICU|CCU)")
          )
        ) +
      labs(
        title = "Patient __",
        x = "Calendar Time",
        y = " "
        )
  })
}

#run the application 
shinyApp(ui = ui, server = server)