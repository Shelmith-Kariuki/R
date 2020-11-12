#' This function is used to to generate summary tables and export them into excel, in a format that the client can easily understand and work with.
#'
#' @param data  The data to be used
#' @param var The variable that holds the question
#' @param xlab The question labels for the main variables
#' @param gvars The variables to be used for crosstabs
#' @param grp_titles The question labels for the crosstab variables
#' @param wbname Name of the workbook where the results will be saved
#' @param sr1 The row that will contain the question
#' @param sr2 The row where the table will start
#' @param qt This indicates the question type
#'
#' @return An excel workbook saved in the path of your computer
#' @export
#'
#' @examples
r2xl <-function(data, var, xlab, gvars,grp_titles,wbname, sr1,sr2,qt){

  ##1. Create a dataframe that will hold the question and save it on the first rows of the sheet

  QA <- data.frame(A = xlab)

  colnames(QA)<- paste0("Question: ",QA$A)

  QA[1,]<-""

  writeDataTable(wb,sheetname1, x = QA, startCol = 1, startRow = sr1,
                 tableStyle = "TableStyleLight1",bandedRows=FALSE,withFilter=FALSE)

  writeDataTable(wb,sheetname2, x = QA, startCol = 1, startRow = sr1,
                 tableStyle = "TableStyleLight1",bandedRows=FALSE,withFilter=FALSE)

  ##2. Generate an overall summary table and save it on the excel sheet

  ###2.1 The table
  if(qt == "sc"){

    summ_table<-data%>%

      distinct_("UserId",var, .keep_all = T) %>%

      group_by_(var)%>%

      summarise(Count = n())%>%

      filter(!is.na((!!as.symbol(var)))) %>%

      filter_(paste0(var,"!=''"))%>%

      filter(!is.na(Count)) %>%

      mutate(Percentage = round(Count/sum(Count),2)) %>%

      select_(var, "Count","Percentage")
  }else{

    summ_table<-data%>%

      distinct_("UserId",var, .keep_all = T) %>%

      group_by_(var)%>%

      summarise(Count = n())%>%

      filter(!is.na((!!as.symbol(var)))) %>%

      filter_(paste0(var,"!=''"))%>%

      filter(!is.na(Count)) %>%

      mutate(Percentage = paste0(round(Count/length(unique(data$UserId)),2),"%")) %>%

      select_(var, "Count","Percentage")
  }
  ###2.2 Rename the column names
  colnames(summ_table) <- c(xlab,"Count","Percentage")

  ###2.3 Reformat the numbers, so that they appear as "%"
  class(summ_table$Percentage) <- c(class(summ_table$Percentage), "percentage")

  ###2.4. Save this dataset to both the count sheet, and the "%" sheet
  writeDataTable(wb,sheetname1, x = summ_table,startCol = 1, startRow = sr2)

  writeDataTable(wb,sheetname2, x = summ_table,startCol = 1, startRow = sr2)


  ##3. Generate a grouped summary table and save it on the excel sheet


  ### We are creating a function that will produce a table, for every cross tab variable.
  sj <- 1
  for(i in 1:length(gvars)){

    if(var != gvars[i]){

      ### 3.1 Generate the count data, and reshape it from long to wide

      summ_table1a<-data%>%

        distinct_("UserId",var, .keep_all = T) %>%

        group_by_(var,gvars[i])%>%

        summarise(Count = n())%>%

        ungroup()%>%

        filter(!is.na((!!as.symbol(var)))) %>%

        filter_(paste0(var,"!=''"))%>%

        filter(!is.na((!!as.symbol(gvars[i])))) %>%

        filter_(paste0(gvars[i],"!=''"))%>%

        filter(!is.na(Count)) %>%

        #arrange_("Count") %>%

        spread(gvars[i],"Count")

      ### 3.2 Rename the column names accordingly

      colnames(summ_table1a)[1]<-xlab

      colnames(summ_table1a)[-1] <- paste(grp_titles[i],colnames(summ_table1a)[-1],sep="_")

      ### 3.3 Write the data to the count excel sheet (sheetname1)

      writeDataTable(wb,sheetname1 , x = summ_table1a,startCol = sj*10, startRow = sr2)

      ### 3.4 Generate the "%" data and reshape it from long to wide

      if(qt=="sc"){
        summ_table1b<-data%>%

          distinct_("UserId",var, .keep_all = T) %>%

          group_by_(var,gvars[i])%>%

          summarise(Count = n())%>%

          ungroup()%>%

          filter(!is.na((!!as.symbol(var)))) %>%

          filter_(paste0(var,"!=''"))%>%

          filter(!is.na((!!as.symbol(gvars[i])))) %>%

          filter_(paste0(gvars[i],"!=''"))%>%

          filter(!is.na(Count)) %>%

          group_by_(gvars[i])%>%

          mutate(Percentage = paste0(round((Count/sum(Count))*100,0),"%")) %>%

          select(-Count)%>%

          spread(gvars[i],"Percentage")
      }else
      {
        summ_table1b<-data%>%

          distinct_("UserId",var, .keep_all = T) %>%

          group_by_(var,gvars[i])%>%

          summarise(Count = n())%>%

          ungroup()%>%

          filter(!is.na((!!as.symbol(var)))) %>%

          filter_(paste0(var,"!=''"))%>%

          filter(!is.na((!!as.symbol(gvars[i])))) %>%

          filter_(paste0(gvars[i],"!=''"))%>%

          filter(!is.na(Count)) %>%

          group_by_(gvars[i])%>%

          mutate(Percentage = paste0(round((Count/sum(Count))*100,0),"%")) %>%

          select(-Count)%>%

          spread(gvars[i],"Percentage")
      }

      ### 3.5 Rename the column names of this dataset
      colnames(summ_table1b)[1]<-xlab

      colnames(summ_table1b)[-1] <- paste(grp_titles[i],colnames(summ_table1b)[-1],sep="_")

      ### 3.6 Write the data to the "%" excel sheet (sheetname2)

      writeDataTable(wb,sheetname2 , x = summ_table1b,startCol = sj*10, startRow = sr2)

      sj<-sj+1

    }

  }
}
