#load packages
library(shiny)
library(ggplot2)
library(bigrquery)

#load data from local file
mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds")

#connect to bigquery 
#path to the service account token 
satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
#bigquery authentication using service account
bq_auth(path = satoken)
# connect to the bigquery database `biostat-203b-2024-winter.mimic4_v2_2`
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)
con_bq

#load data from bigquery database
transfers_tble <- tbl(con_bq, "transfers") 
labevents_tble <- tbl(con_bq, "labevents")
d_icd_procedures_tble <- tbl(con_bq, "d_icd_procedures")
procedures_icd_tble <- tbl(con_bq, "procedures_icd") |>
  mutate(chartdate = as.POSIXct(chartdate)) |>
  left_join(d_icd_procedures_tble, by = c("icd_code", "icd_version"))
patients_tble <- tbl(con_bq, "patients")
admissions_tble <- tbl(con_bq, "admissions")
d_icd_diagnoses_tble <- tbl(con_bq, "d_icd_diagnoses")
diagnoses_icd_tble <- tbl(con_bq, "diagnoses_icd") |>
  left_join(d_icd_diagnoses_tble, by = c("icd_code", "icd_version")) 

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
                              "Enter subject ID:",
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
    plot <- reactive({
      ggplot() +
        geom_segment(
          data = transfers_tble |> 
            filter(subject_id == !!input$subject_id),
          mapping = aes(
            x = intime, xend = outtime, 
            y = 0.025, yend = 0.025,
            color = careunit,
            linewidth = str_detect(careunit, "ICU|CCU") |>
              ifelse(6, 2)
          )
        ) +
        geom_point(
          data = labevents_tble |> 
            filter(subject_id == !!input$subject_id),
          mapping = aes(x = charttime, y = 0),
          shape = 3,
          size = 3
        ) +
        geom_point(
          data = procedures_icd_tble |> 
            filter(subject_id == !!input$subject_id),
          mapping = aes(
            x = chartdate, 
            y = -0.025, 
            shape = long_title),
          size = 3
        ) +
        labs(
          title = str_c(
            "Patient ", 
            input$subject_id, 
            ", ",
            pull(patients_tble |> 
                   filter(subject_id == !!input$subject_id), gender),
            ", ",
            pull(patients_tble |> 
                   filter(subject_id == !!input$subject_id), anchor_age), 
            " years old",
            ", ", 
            pull(admissions_tble |> 
                   filter(subject_id == !!input$subject_id), race)
          ),
          subtitle = str_c(
            diagnoses_icd_tble |> 
              filter(subject_id == !!input$subject_id) |>
              filter(row_number() %in% 1) |>
              pull(long_title), 
            "\n",
            diagnoses_icd_tble |> 
              filter(subject_id == !!input$subject_id) |>
              filter(row_number() %in% 2) |>
              pull(long_title),
            "\n",
            diagnoses_icd_tble |> 
              filter(subject_id == !!input$subject_id) |>
              filter(row_number() %in% 3) |>
              pull(long_title)
          ),
          x = "Calendar Time",
          y = " ",
          shape = "Procedure",
          color = "Care Unit"
        ) +
        theme_light() +
        theme(
          legend.position = "bottom",
          aspect.ratio = 1/4
        ) +
        guides(
          shape = guide_legend(ncol = 1),
          color = guide_legend(ncol = 1),
          linewidth = "none"
        ) +
        scale_y_continuous(
          breaks = c(-0.025, 0, 0.025),
          labels = c("Procedure", "Lab", "ADT")
        ) +
        coord_cartesian(ylim = c(-0.04, 0.04))
    })
    print(plot())

  })
}

#run the application 
shinyApp(ui = ui, server = server)