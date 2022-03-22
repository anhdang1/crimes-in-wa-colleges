library("shiny")
library("rbokeh")
library("ggplot2")
library("dplyr")
library("plotly")
library(tidyverse)
library(bslib)
library("rbokeh")
library(lubridate)

#From 2001 - 2020
disciplinary_on_campus <- read.csv('https://raw.githubusercontent.com/anhdang1/crimes-in-wa-colleges/main/OPE%20CSS%20Custom%20Data%202022-03-13%20164937/Disciplinary_Actions_On_campus.csv')


#Rename columns
disciplinary_on_campus <- disciplinary_on_campus %>%
  plyr::rename(c("Illegal.weapons.possession" = "Illegal_weapons",
                 "Drug.law.violations" = "Drug_law",
                 "Liquor.law.violations" = "Liquor_law"))%>%
  mutate(uniq = paste(Survey.year, " ", Unitid))%>%
  select(uniq, Survey.year, Institution.name, Campus.Name,Illegal_weapons,Drug_law,Liquor_law)





disciplines_list <- colnames(disciplinary_on_campus)
disciplines_list <- disciplines_list[7:10]

count_range <- range(discip_oncampus_public$Survey.year)

#Data for disciplinary types
disciplinary_types <- disciplinary_on_campus %>% group_by(Survey.year) %>% summarize(Illegal_weapons = sum(Illegal_weapons),
                                                                                     Drug_law = sum(Drug_law),
                                                                                     Liquor_law = sum(Liquor_law))
#Sum total disciplinary actions 
discip_types_table <- disciplinary_types %>%
  pivot_longer(!Survey.year, names_to = "Disciplinary_actions", values_to = "Num_crimes")

custom_legend_titles <- reactiveValues("Illegal_weapons" = "Illegal Weapons", "Drug_law" = "Drug Law Action", "Liquor_law" = "Liquor Law Action")






ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  navbarPage("Crimes Across WA Colleges",
             tabPanel(
               "Introduction",
               #HTML('<div class= "header">Crimes Across WA Colleges</div>'),
               tags$div(class = "header", "Let's look at crime trends across Washington Colleges from 2001 to 2020!", style = "font-size:50px;"),
               HTML('<a href = "https://ope.ed.gov/campussafety/#/" >Data Drawn from Campus Safety and Security</a>'),
                         p("Crimes Across WA Colleges"),
                         mainPanel(#img(src = "/image/cat_dog.png", height = 300, width = 400, align = "center"),
                           textOutput("introduction"),
                           textOutput("Value")
                         )
               )
             ,
               

             tabPanel("Trends", 
                      # Side bar layout
                      sidebarLayout(
                        sidebarPanel(
                          # Allow user to input range
                          selectInput(
                            inputId = "user_category",
                            label = "Enter institutions",
                            choices = draft$Institution.name,
                            selected = "University of Washington-Seattle Campus",
                            multiple = TRUE),
                          sliderInput("Survey.year", "Choose year range:",
                                      min = count_range[1],
                                      max = count_range[2],
                                      value = count_range,
                                      sep = "",
                                      step = 1)
                        ),

                        # Main Panel
                        mainPanel(
                          plotlyOutput(outputId = "wa_colleges_plot"),
                        ) 
                      )),
             tabPanel("Disciplinary Types",
                      # Side bar layout
                      sidebarLayout(
                        sidebarPanel(
                          # Allow user to input range
                          selectInput(
                            inputId = "user_types",
                            label = "Select Disciplinary Types",
                            choices = list("Illegal Weapons" = "Illegal_weapons",
                                           "Drug Law Action" = "Drug_law",
                                           "Liquor Law Action" = "Liquor_law"),
                            selected = "Illegal_weapons"
                          )
                        ),
                        mainPanel(
                          plotlyOutput(outputId = "disiplinary_comparisonPlot"),

                        )
                      )),

             tabPanel(
               "Conclusion",
                         p("Welcome to The End!"),
                                   textOutput("conclusion")
                                   ),
  )
)



