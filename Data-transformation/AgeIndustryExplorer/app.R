library(shiny)
library(tidyverse)
library(ggthemes)

gpg_industry <- read_csv('gpg_industry.csv')
gpg_industry <- gpg_industry %>% 
    select(-GPG_mean) %>% 
    group_by(Industry) %>% 
    mutate(numAgeGroups = n()) %>% 
    ungroup() %>% 
    filter(numAgeGroups == 6) %>% 
    mutate(payGap = ifelse((GPG_median > 5 | GPG_median < -5),"Gender pay gap > 5%","Gender pay gap < 5%"))
colnames(gpg_industry)
gpg_overall <- gpg_industry %>% 
    group_by(Age_range) %>% 
    summarise(GPG_median = median(GPG_median)) %>% 
    mutate(Industry = ".Overall",
           numAgeGroups = 6,
           payGap = "Overall") %>% 
    select(Age_range,Industry,GPG_median,numAgeGroups,payGap) %>% 
    rbind(gpg_industry)


a <- min(gpg_industry$GPG_median)
b <- max(gpg_industry$GPG_median)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("age",
                        "Age group:",
                        min = 1,
                        max = 6,
                        value = 1),
            tagList(
                tags$strong("Age Groups"),
                tags$p("Group 1: 18-21",
                       tags$br(),"Group 2: 22-29",
                       tags$br(),"Group 3: 30-39", 
                       tags$br(),"Group 4: 40-49", 
                       tags$br(),"Group 5: 50-59", 
                       tags$br(),"Group 6: 60+",
                       tags$br(),tags$br(),
                       tags$em("Data from Office of National Statistics, Gender Pay Gap Dataset (Revised 2018)")
                ) )
        ),
        # Show a plot of the generated distribution
        mainPanel(
            
            textOutput("selectedAge", inline = TRUE),
            plotOutput("industryPlot")
        ))) # end ui


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$selectedAge <- renderText({
        paste0("Age group: ",switch(input$age,
                                    "18-21",
                                    "22-29",
                                    "30-39",
                                    "40-49",
                                    "50-59",
                                    "60+")
        )
        
    })
    
    output$industryPlot <- renderPlot({
        # filter data by age range choice
        age_group <- gpg_overall %>% 
            filter(Age_range == switch(input$age,
                                       "18-21",
                                       "22-29",
                                       "30-39",
                                       "40-49",
                                       "50-59",
                                       "60+"))
        
        
        # draw the column chart
        age_group %>% 
            group_by(Industry) %>% 
            ggplot(aes(y = GPG_median, x = Industry))+
            geom_col(aes(fill = payGap)) + 
            theme(panel.background = element_blank()) +
            scale_fill_tableau(palette = "Tableau 10") +
            xlab("Industry") +
            ylab("Gender pay gap*") + 
            labs(caption = "Median female hourly wage expressed as % decrease compared to median male hourly wage") +
            expand_limits(y = c(a,b)) +
            coord_flip()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
