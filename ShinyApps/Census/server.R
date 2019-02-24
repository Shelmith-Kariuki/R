## Date: 23/02/2019
## Project: Shiny Dashboard

## Load the libraries
library(shiny)
library(shinydashboard)
library(shinyEffects)
library(dplyr)
library(DT)
library(ggplot2)

census_data<-read.csv("Census.csv")


vars<-c("Education","MaritalStatus","Occupation","Relationship",
        "Race","Sex","NativeCountry","Income")

server<-function(input, output){
  
  output$freq_table = renderDataTable({
    datatable(census_data %>% 
      dplyr::group_by_(input$var) %>% 
      dplyr::summarise(count = n()) %>% 
      dplyr::mutate(perc =round((count/sum(count))*100,0)),
      colnames = c(input$var,"Frequency","Percentage"))
  })
  
  output$freq_graph = renderPlot({
    census_data %>% 
      dplyr::group_by_(input$var) %>% 
      dplyr::summarise(count = n()) %>% 
      dplyr::mutate(perc =round((count/sum(count))*100,0))%>%
      ggplot(aes_string(x=input$var, y="perc"))+
      geom_bar(stat="identity", fill="maroon")+
      labs(title=paste("Distribution \n of \n",input$var),y="Percentage",x=input$var)+
      theme(legend.position = "right",
            legend.title = element_blank(),
            axis.line=element_blank(),
            legend.text = element_text(family = "Source Sans Pro", size = rel(1.1)),
            plot.caption = element_text(family = "Source Sans Pro", size = rel(1.1), vjust = -0.5),
            plot.title = element_text(family="Source Sans Pro Semibold", size = rel(1.4), 
                                      hjust = 0.5),
            plot.subtitle = element_text(family="Source Sans Pro Semibold", size = rel(1.3), 
                                         hjust = 0.5),
            axis.text = element_text(family = "Source Sans Pro", size = rel(1.2)),
            axis.title = element_text(family = "Source Sans Pro", size = rel(1.3)),
            strip.text.x = element_text(family = "Source Sans Pro", size = rel(1.4)),
            panel.background = element_rect(fill = NA))+coord_flip() 
    
  })

  output$freq_table_grp = renderDataTable({
    datatable(census_data %>% 
                dplyr::group_by_(.dots = c(input$var2,input$var3)) %>% 
                dplyr::summarise(count = n()) %>% 
                dplyr::ungroup() %>% 
                dplyr::group_by_(input$var2) %>% 
                dplyr::mutate(perc =round((count/sum(count))*100,0)),
              colnames = c(input$var2,input$var3,"Frequency","Percentage"))
  })
  
  output$freq_graph_grp = renderPlot({
    census_data %>% 
      dplyr::group_by_(.dots = c(input$var2,input$var3)) %>% 
      dplyr::summarise(count = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by_(input$var2) %>% 
      dplyr::mutate(perc =round((count/sum(count))*100,0)) %>% 
      ggplot(aes_string(x=input$var2, y="perc",fill=input$var3))+
      geom_bar(stat="identity",position = "stack")+
      labs(title=paste("Distribution \n of \n",input$var2,"by",input$var3),y="Percentage",x=input$var2)+
      theme(legend.position = "right",
            legend.title = element_blank(),
            axis.line=element_blank(),
            legend.text = element_text(family = "Source Sans Pro", size = rel(1.1)),
            plot.caption = element_text(family = "Source Sans Pro", size = rel(1.1), vjust = -0.5),
            plot.title = element_text(family="Source Sans Pro Semibold", size = rel(1.4), 
                                      hjust = 0.5),
            plot.subtitle = element_text(family="Source Sans Pro Semibold", size = rel(1.3), 
                                         hjust = 0.5),
            axis.text = element_text(family = "Source Sans Pro", size = rel(1.2)),
            axis.title = element_text(family = "Source Sans Pro", size = rel(1.3)),
            strip.text.x = element_text(family = "Source Sans Pro", size = rel(1.4)),
            panel.background = element_rect(fill = NA))+coord_flip()+
      scale_fill_brewer(palette = "Spectral")
    
  })
}
