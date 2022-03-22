library(ggplot2)
library(stringr)
library(pivottabler)
library(plotly)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(DT)



#chart_script 
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

count_range <- range(discip_oncampus_public$Survey.year)

custom_legend_titles <- reactiveValues("Illegal Weapons" = "Illegal_weapons",
                                       "Drug Law Action" = "Drug_law",
                                       "Liquor Law Action" = "Liquor_law")


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
      labs(title = "Disciplinary Actions Across Campus",
           subtitle = "Data drawn from Campus Safety",
           x = "Year",
           y = "Number of Disiplinary Actions",
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
        labs(title = 'Disciplinary Actions By Types', x='Year', y='Total crimes',
             fill = custom_legend_titles[[input$user_types]])
      ggplotly(compare)  })
    
    #crime table
    output$crime_table <- DT::renderDataTable(DT::datatable({
      criminal_offenses_table <- criminal_offenses_table %>% select(Survey.year, Institution.name, Campus.Name, Total,
                                                                    Negligent.manslaughter,
                                                                    Rape,
                                                                    Robbery,
                                                                    Burglary,
                                                                    Motor.vehicle.theft,
                                                                    Arson) %>% arrange(desc(Total))
      if (input$year != "All"){
        criminal_offenses_table <- criminal_offenses_table[criminal_offenses_table$Survey.year %in% input$year,]
      }
      if (input$insti != "All"){
        criminal_offenses_table <- criminal_offenses_table[criminal_offenses_table$Institution.name %in% input$insti,]
      }
      criminal_offenses_table
      
    }))

  
}





