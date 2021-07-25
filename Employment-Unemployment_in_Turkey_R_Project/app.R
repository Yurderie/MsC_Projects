#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2movies)
library(lubridate)

load(url("https://github.com/pjournal/mef04g-rhapsody/blob/gh-pages/Project_Data/project_all_data.RData?raw=true"))

#Total Unemployment grafiðinin verisi
occ_group_overall_m <- occ_group_overall %>%
    filter(month != "Annual") %>%
    mutate(year = factor(year),
           month = factor(month, levels = c("January","February", "March", 
                                            "April", "May", "June", "July", "August", "September", "October", "November", "December")))
occ_group_overall_m <- occ_group_overall_m %>%
    mutate(date = factor(paste(year, month), levels = paste(year, month)))



#Unemployment by Gender grafiðinin verisi
monthly_jobsearch_male <- filter(job_search_male, !grepl("Annual", month))
monthly_jobsearch_female <- filter(job_search_female, !grepl("Annual", month))

unemp_gr_male<-monthly_jobsearch_male%>%
    transmute(date = paste(year, month, sep = " "), total_unemployed)

unemp_gr_female<-monthly_jobsearch_female%>%
    transmute(date = paste(year, month, sep = " "), total_unemployed)


unemp_merged <-merge(unemp_gr_female,unemp_gr_male, by="date", all=TRUE, sort = FALSE)

unemp_merged <- unemp_merged %>%
    rename(female=total_unemployed.x, male=total_unemployed.y)

unemp_merged$date <- as.character(unemp_merged$date)
unemp_merged$date <- factor(unemp_merged$date, level=unemp_merged$date)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Unemployment & Employment Rates in Turkey (2014-2020)"),
    tabsetPanel(
        tabPanel("Total Unemployment",

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(2,
            # dateRangeInput(inputId = 'dateselect',label = "Select a timeline : ",language = "en", format = "mm/yyyy",start = Sys.Date(), 
            #                end=Sys.Date(),startview = "year",separator = " - ")),
            
            selectInput(inputId = "month1",
                        label = "Select a month",
                        choices = c("January","February", "March", "April", "May",
                                    "June", "July", "August", "September", "October", "November", "December"),
                        multiple= FALSE
            ),
            selectInput(inputId = "month2",
                label = "Select a month",
                choices = c("January","February", "March", "April", "May",
                            "June", "July", "August", "September", "October", "November", "December"),
                multiple= FALSE,
                selected = "August"
            )),
        column(2,
            selectInput(inputId = "year1",
                        label = "Select a year",
                        choices = c(2014:2020),
                        multiple= FALSE),
            
            selectInput(inputId = "year2",
                        label = "Select a year",
                        choices = c(2014:2020),
                        multiple= FALSE,
                        selected = 2020
            ))),
        
        mainPanel(
           plotOutput("unempPlot"),
           br(),
           plotOutput("genderPlot")
        )
    ),
    tabPanel("Majors")))



server <- function(input, output) {
    
    

    output$unempPlot <- renderPlot({

       first_date <- paste(input$year1, input$month1, sep=" ")
       second_date <- paste(input$year2, input$month2, sep=" ")
       
       if (first_date %in% c("2020 September", "2020 October", "2020 November", "2020 December")) {
           first_date <- "2020 August"
       }
       
       if (second_date %in% c("2020 September", "2020 October", "2020 November", "2020 December")) {
           second_date <- "2020 August"
       }
       
       occ_group_overall_m <- occ_group_overall_m %>%
           mutate(date = factor(paste(year, month), levels = paste(year, month)))


       df <- occ_group_overall_m[match(first_date,occ_group_overall_m$date): match(second_date,occ_group_overall_m$date), c("date", "total_unemployed")]
      
       
       ggplot(df, aes(date, total_unemployed)) +
           geom_line(aes(group = 1)) +
           labs(
               x = "Date",
               y = "Unemployment (in thousands)") +
           theme(axis.ticks.x = element_blank(),
                 axis.text.x = element_text(angle = 90, size = 10,  hjust = 1))

    })
    
    output$genderPlot <- renderPlot({
        
        first_date <- paste(input$year1, input$month1, sep=" ")
        second_date <- paste(input$year2, input$month2, sep=" ")
        
        if (first_date %in% c("2020 September", "2020 October", "2020 November", "2020 December")) {
            first_date <- "2020 August"
        }
        
        if (second_date %in% c("2020 September", "2020 October", "2020 November", "2020 December")) {
            second_date <- "2020 August"
        }
        
        
        df <- unemp_merged[match(first_date,unemp_merged$date): match(second_date,unemp_merged$date), c("date", "female", "male")]
        
        ggplot(df, aes(x=factor(date))) +
            geom_line(aes(y=female, color="Female", group=1))+
            geom_line(aes(y=male, color="Male", group=1))+
            theme(axis.text.x = element_text(angle=90, size=10, hjust = 1),
                  plot.title = element_text(hjust = 0.5))+
            labs(title = "Unemployment by Gender Over the Years",
                 x="Date",
                 y="Unemployment (in thousands)",
                 color="Gender")})
}



# Run the application 
shinyApp(ui = ui, server = server)
