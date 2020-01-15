
library(ggplot2)
library(dplyr)
library(ggthemes)
rusers<- read.csv("PersonalDevelopment/R/R Users in Africa.csv")

rusers_theme<-theme_hc()+theme(legend.position = "right", 
                           legend.direction = "vertical",
                           legend.title = element_blank(),
                           plot.title = element_text( size = rel(1.4), hjust = 0.5),
                           plot.subtitle = element_text(size = rel(1.4), hjust = 0.5),
                           #axis.text = element_text( size = rel(1.5)),
                           axis.text.x = element_text(size =rel(1.3),angle = 0),
                           axis.text.y = element_text(size =rel(1.3),angle = 0),
                           axis.title = element_text( size = rel(1.3)),
                           panel.background = element_rect(fill = NA))

summ1<-function(data, var,xlab,xtitle){
  
  ## Summary table
  summ_table<-data%>%
    dplyr::group_by_(var)%>%
    dplyr::filter(Country!="United States") %>% 
    dplyr::summarise(count = n())%>%
    filter(!is.na((!!as.symbol(var)))) %>%
    filter_(paste0(var,"!=''"))%>%
    mutate(perc = round((count/sum(count)*100),1))
  #print(summ_table)

  
  ## Frequency Graph
  bargraph<- ggplot(data=summ_table, aes_string(x=var, y = "perc")) + 
    geom_bar(stat = "identity",fill = "brown") + 
    rusers_theme+
    #theme(axis.text.x =element_text(angle = 25))+
    labs(title =paste("",xtitle, sep=" "),x = xlab, y = "Percentage")
  
  if(var=="Country"){
    bargraph=ggplot(data=summ_table, aes_string(x= paste0("reorder(",colnames(summ_table)[1],", perc)")  , y = "count")) + 
      geom_bar(stat = "identity",fill = "brown") + 
      rusers_theme+
      #theme(axis.text.x =element_text(angle = 25))+
      labs(title =paste("",xtitle, sep=" "),x = xlab, y = "Frequency")+
      scale_fill_brewer(palette="Blues")+
      geom_text(aes(label = count), family="Source Sans Pro Semibold",
                vjust = 0.5,hjust = -0.5, size = 4)+
      coord_flip()+ylim(c(0,max(summ_table$count + 10)))
  }else{
    bargraph=bargraph+
      geom_text(aes(label =count),vjust = -0.25, size = 4)
  }
  print(bargraph)
}

summ1(rusers, "Country","","R Users in Africa \n\n (as at 26th Feb, 2019)")