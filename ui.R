library("shiny")
library("rbokeh")
library("ggplot2")
library("dplyr")
library("plotly")
library(tidyverse)
library(bslib)
library("rbokeh")
library(lubridate)
library(DT)


#From 2001 - 2020 - Draw data
disciplinary_on_campus <- read.csv('https://raw.githubusercontent.com/anhdang1/crimes-in-wa-colleges/main/OPE%20CSS%20Custom%20Data%202022-03-13%20164937/Disciplinary_Actions_On_campus.csv')

criminal_offenses <- read.csv("https://raw.githubusercontent.com/anhdang1/crimes-in-wa-colleges/main/OPE%20CSS%20Custom%20Data%202022-03-13%20164937/Criminal_Offenses_On_campus.csv")

#Rename columns
disciplinary_on_campus <- disciplinary_on_campus %>%
  plyr::rename(c("Illegal.weapons.possession" = "Illegal_weapons",
                 "Drug.law.violations" = "Drug_law",
                 "Liquor.law.violations" = "Liquor_law"))%>%
  mutate(uniq = paste(Survey.year, " ", Unitid))%>%
  select(uniq, Survey.year, Institution.name, Campus.Name,Illegal_weapons,Drug_law,Liquor_law)





disciplines_list <- colnames(disciplinary_on_campus)
disciplines_list <- disciplines_list[7:10]

count_range <- range(disciplinary_on_campus$Survey.year)
draft <- pivot_longer(disciplinary_on_campus, 5:7, names_to = "Disciplinary_actions", values_to = "Num_crimes")

#Data for disciplinary types
disciplinary_types <- disciplinary_on_campus %>% group_by(Survey.year) %>% summarize(Illegal_weapons = sum(Illegal_weapons),
                                                                                     Drug_law = sum(Drug_law),
                                                                                     Liquor_law = sum(Liquor_law))
#Sum total disciplinary actions 
discip_types_table <- disciplinary_types %>%
  pivot_longer(!Survey.year, names_to = "Disciplinary_actions", values_to = "Num_crimes")

custom_legend_titles <- reactiveValues("Illegal_weapons" = "Illegal Weapons", "Drug_law" = "Drug Law Action", "Liquor_law" = "Liquor Law Action")


#convert values to numeric
criminal_offenses_table <- as.numeric(criminal_offenses$Negligent.manslaughter,
                                      criminal_offenses$Rape,
                                      criminal_offenses$Robbery,
                                      criminal_offenses$Burglary,
                                      criminal_offenses$Motor.vehicle.theft,
                                      criminal_offenses$Arson
)
#drop NA values and mutate total crimes in criminal offense table
criminal_offenses_table <- criminal_offenses %>%
  drop_na(Negligent.manslaughter,Rape,Robbery,Burglary,Motor.vehicle.theft, Arson) %>%
  mutate(Total = Negligent.manslaughter + Rape + Robbery + Burglary + Motor.vehicle.theft + Arson)




ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  navbarPage("Crimes Across WA Colleges",
             tabPanel(
               "Introduction",
               #HTML('<div class= "header">Crimes Across WA Colleges</div>'),
               titlePanel(HTML('<h1 style="color:#722F37;"> Crime across Washington Colleges </h1>',
                               '<h4> Author: Anh Dang</h4>')),
               hr(),
               #tags$div(class = "header", "Crime across Washington Colleges", style = "font-size:30px;"),
               #HTML('<h4> Author: Anh Dang</h4>'),
               
               HTML('<a href = "https://ope.ed.gov/campussafety/#/" >Data Drawn from Campus Safety and Security</a>'),
                         p("Crimes Across WA Colleges From 2001 to 2020!"),
                         mainPanel(#img(src = "/image/cat_dog.png", height = 300, width = 400, align = "center"),
                           textOutput("introduction"),
                           # textOutput("Value")
                         )
               )
             ,
               
             
             tabPanel("Trends", 
                      titlePanel(HTML('<h1 style="color:#722F37;"> Disciplinary Actions Trends </h1>')),
                      hr(),
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
                          plotlyOutput(outputId = "wa_colleges_plot")
                        ) 
                      )),
             tabPanel("Disciplinary Types",
                      # Side bar layout
                      titlePanel(HTML('<h1 style="color:#722F37;"> Disciplinary Actions Types </h1>')),
                      hr(),
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
                          plotlyOutput(outputId = "disiplinary_comparisonPlot")

                        )
                      )),
             tabPanel("Crime Table",
                      fluidPage(
                        titlePanel(HTML('<h1 style="color:#722F37;"> Crime Data Table </h1>')),
                        hr(),
                        HTML('<h4 style="color:"darkorchid4";"> How to use this table:</h4>'),
                        HTML('<p style ="color::"darkorchid4";"> The table allow users to get the number of crimes on campus by filtering year and institution. University of Washington - Seattle Campus is found to be one of the campuses with the highest number of crimes.'),
                        fluidRow(
                          column(4,
                                 selectInput("year",
                                             "Year:",
                                             c("All",
                                               unique(as.character(criminal_offenses_table$Survey.year))))
                        ),
                        column(4,
                               selectInput("insti",
                                           "Institution:",
                                           c("All",
                                             unique(as.character(criminal_offenses_table$Institution.name)))))
                        ),
                        DT::dataTableOutput("crime_table")
                      )
                      ),
                        

  )
)



