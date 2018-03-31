#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library (shinydashboard)
library (data.table)
library (ggrepel)
library (dplyr)
library (ggplot2)
library (wordcloud)
library (scales)
library(tidyverse)
library(tidytext)
library(highcharter)

if (names(dev.cur()) != "null device") dev.off()
pdf(NULL)

DIR <- "/home/teddy/fbtext/"
Vendor_list <- read.csv (paste0(DIR, "vendors.csv"),stringsAsFactors=FALSE)$x
Quarter_list <- read.csv (paste0(DIR, "quarters.csv"),stringsAsFactors=FALSE)$x
#prop_top5 <- fread (paste0(DIR, "prop_top5.csv"))

header <- dashboardHeader (title = "Telco vendors' Facebook", titleWidth = 300)
sidebar <- dashboardSidebar(
  
  selectInput(inputId = "vendorselect",
              label = "Select Vendor",
              choices = Vendor_list)
)



body <- dashboardBody(
  
  
  fluidRow (box (plotOutput("plot1", height = "400px"), width = 12, title = "Most frequent themes of selected vendor on Facebook per quarter", solidHeader = TRUE, collapsible = TRUE, status = "primary")),
  
  fluidRow (box (width = 12, title = "Most frequent themes on Facebook of selected vendors in selected Quarter", solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(column(selectInput (inputId = "quarterselect",
                                              label = "Select Quarter",
                                              choices = Quarter_list), width =3),
                          column(offset = 1,uiOutput("select_vendor2"), width =4)),
                 box (width = 4, plotOutput("cloud1"), title = "Top 5 words and phrases of selected vendor"),
                 box (width = 4, plotOutput("CompPlot"), title = "Word frequencies of the two vendors"),
                 box (width = 4, plotOutput("cloud2"),title = "Top 5 words and phrases of comparison vendor"))
  ),
  fluidRow (box (width = 12, title = "Sentiment score of selected vendor's Facebook comments", solidHeader = TRUE, collapsible = TRUE, status = "primary",
                 fluidRow(box (highchartOutput("hc"), width = 12), box (highchartOutput("hc2"), width = 12))
  )
  )

)


ui <- dashboardPage( 
  header, 
  sidebar, 
  body) 
server <- function(input, output) {
  
  DIR <- "/home/teddy/fbtext/"
  prop_top5 <- fread (paste0(DIR, "prop_top5.csv"))
  proportion <- fread (paste0(DIR, "proportion.csv"))
  comment_sent <- fread (paste0(DIR, "comment_sent.csv"))
  
  
  vendor2 <- reactiveVal(Vendor_list [2]) 
  
  output$select_vendor2 <- renderUI({
    selectInput(inputId = "vendor2",
                label = "Select Another Vendor to compare",
                choices = Vendor_list [Vendor_list != input$vendorselect])
  })
  
  observeEvent(input$vendor2, {
    vendor2(input$vendor2)             # rv$value <- newValue
  })
  
  output$plot1 <- renderPlot({prop_top5 %>% filter (Vendor == input$vendorselect)%>%
      ggplot (aes(created_Q, prop, color=created_Q, label = word)) + geom_jitter (size =0) + 
      geom_text_repel (size=5, segment.color = "white") + theme_bw (base_size = 24) + 
      theme(legend.position="none", panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(), axis.text.y=element_blank(), axis.line = element_blank()) +
      labs(title = sprintf ("Most frequent themes in %s Facebook posts", toupper(input$vendorselect))) +
      xlab("Quarters") + ylab  ("Frequency")
  })
  
  output$cloud1 <- renderPlot({
    prop_top5 %>% filter (Vendor == input$vendorselect) %>% filter (created_Q == input$quarterselect) %>%
      with(wordcloud(word, n, max.words = 100, min.freq = 0.2, scale = c(3,0.5), colors = c("#0091ff", "#f0650e")))  
  })
  output$cloud2 <- renderPlot({
    prop_top5 %>% filter (Vendor == vendor2()) %>% filter (created_Q == input$quarterselect) %>% 
      with(wordcloud(word, n, max.words = 100, min.freq = 0.2, scale = c(3,0.5),colors = c("#0091ff", "#f0650e")))  
  })
  
  output$CompPlot <- renderPlot ({
    proportion %>%
      filter (created_Q == input$quarterselect) %>%
      filter (Vendor %in% c(input$vendorselect, vendor2())) %>%
      select (-n) %>%     #drop n, in order  to allow spread and collapse (gather)
      spread (Vendor, prop) %>%    #spred prop per vendor
      gather (Vendor, prop, -created_Q, -word, -c(input$vendorselect, vendor2())) %>%
      ggplot ( aes(get(vendor2())/100, get(input$vendorselect)/100, 
                   color = abs (get(input$vendorselect)-get(vendor2())))) + 
      geom_abline (color = "gray40", lty =2) + 
      geom_jitter (alpha = 0.3, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
      scale_x_log10(labels = percent_format(), limits = c(0.001,0.1)) +
      scale_y_log10(labels = percent_format(), limits = c(0.001,0.1)) +
      scale_color_gradient(limits = c(0, 2), low = "#0091ff", high = "#f0650e") +
      theme_bw() + theme(legend.position="none") +
      labs(y = input$vendorselect, x = vendor2())
    
  })
  
  
  output$hc <- renderHighchart({
    hc <- comment_sent %>% 
      filter (Vendor == input$vendorselect) %>% 
      hchart("line", hcaes(x=as.Date(`as.Date(Comment_created_time)`), y = sentiment)) %>%        
      hc_xAxis(type = "datetime") %>%
      hc_title (text = sprintf("Comments' sentiments of %s", toupper(input$vendorselect)))
    hc <- hc_tooltip(hc, pointFormat = "Daily Sentiment Score: {point.y}")
    
    hc$x$type <- "stock"                              
    
    hc
    
  })
  
  output$hc2 <- renderHighchart({
    hc2 <- comment_sent %>% 
      filter (Vendor == vendor2()) %>% 
      hchart("line", hcaes(x=as.Date(`as.Date(Comment_created_time)`), y = sentiment)) %>%         
      hc_xAxis(type = "datetime") %>%
      hc_title (text = sprintf("Comments' sentiments of %s", toupper(vendor2())))
    hc2 <- hc_tooltip(hc2, pointFormat = "Daily Sentiment Score: {point.y}")
    
    hc2$x$type <- "stock"                              
    
    hc2
    
  })
  
}

shinyApp(ui, server)
