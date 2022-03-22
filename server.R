library(ggplot2)
library(stringr)
library(pivottabler)
library(plotly)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)



#chart_script 
#From 2001 - 2020
disciplinary_on_campus <- read.csv('https://raw.githubusercontent.com/anhdang1/crime_wa_colleges/main/OPE%20CSS%20Custom%20Data%202022-03-13%20164937/Disciplinary_Actions_On_campus.csv')
disciplinary_public_property <- read.csv('https://raw.githubusercontent.com/anhdang1/crime_wa_colleges/main/OPE%20CSS%20Custom%20Data%202022-03-13%20164937/Disciplinary_Actions_Public_Property.csv')


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

custom_legend_titles <- reactiveValues("Illegal Weapons" = "Illegal_weapons",
                                       "Drug Law Action" = "Drug_law",
                                       "Liquor Law Action" = "Liquor_law")


#Pivot Table
draft <- pivot_longer(disciplinary_on_campus, 5:7, names_to = "Disciplinary_actions", values_to = "Num_crimes")

#Sum total disciplinary actions 
test <- disciplinary_on_campus %>% mutate(total_disciplinary = Illegal_weapons + Drug_law + Liquor_law ) %>% select(Survey.year, Institution.name, Campus.Name, total_disciplinary)

disciplinary_types <- disciplinary_on_campus %>% group_by(Survey.year) %>% summarize(Illegal_weapons = sum(Illegal_weapons),
                                                                                   Drug_law = sum(Drug_law),
                                                                                   Liquor_law = sum(Liquor_law))

discip_types_table <- disciplinary_types %>%
  pivot_longer(!Survey.year, names_to = "Disciplinary_actions", values_to = "Num_crimes")

custom_legend_titles <- reactiveValues("Illegal_weapons" = "Illegal Weapons", "Drug_law" = "Drug Law Action", "Liquor_law" = "Liquor Law Action")


server <- function(input, output) {
  
  output$wa_colleges_plot <- renderPlotly({
    
    # Allow user to filter by multiple year
    
    test <- test %>% dplyr::filter(Institution.name %in% input$user_category) %>% dplyr::filter(Survey.year <= input$Survey.year[2],
                                                                                                  Survey.year >= input$Survey.year[1])
    
    wa_colleges_plot <- ggplot(test,aes(x = Survey.year, y = total_disciplinary,
                                        color = Institution.name)) +
      geom_line() + 
      labs(title = "Number of Disciplinary Actions On Campus",
           subtitle = "Data drawn from Campus Safety",
           x = "Year",
           y = "Number of disiplinary actions",
           fill = "Institution.name") +
      theme(legend.position = "right",
            legend.title = element_text(face = "bold")) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    ggplotly(wa_colleges_plot)
    
    })

    output$disiplinary_comparisonPlot <- renderPlotly({
      disciplinary_types <- disciplinary_on_campus %>% group_by(Survey.year) %>% summarize(Illegal_weapons = sum(Illegal_weapons),
                                                                                           Drug_law = sum(Drug_law),
                                                                                           Liquor_law = sum(Liquor_law))
      discip_types_table <- disciplinary_types %>%
        pivot_longer(!Survey.year, names_to = "Disciplinary_actions", values_to = "Num_crimes")
      types_filter <- discip_types_table %>% filter(Disciplinary_actions %in% input$user_types)
      

      compare <- ggplot(data = types_filter) +
        geom_point(mapping= aes(x=Survey.year, y = Num_crimes, color = "red"), position = "dodge") +
        theme(legend.position="none")+
        labs(title = 'Number of Disciplinary Actions Over The Year', x='Year', y='Total crimes',
             fill = custom_legend_titles[[input$user_types]])
      ggplotly(compare)  })

  
}
  




