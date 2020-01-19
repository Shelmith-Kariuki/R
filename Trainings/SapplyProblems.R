## Name: Shelmith 
## Date: 15/01/2020

## Generating a dataset of opinions
vec <- 1:5
Questions <- paste0("Opinion_",vec)
opinion_df <- data.frame(matrix("", ncol=5, nrow=200))
names(opinion_df) <- Questions


for(i in 1: length(Questions)){
  opinion_df[,Questions[i]] <- sample(c("Strongly Agree", "Agree","Neutral","Disagree", "Strongly Disagree"),size = 200,replace = TRUE) 
}

## Populating the dataset with likert scale values
## Factorizing all the variables using sapply (why is the function below not working? I do not understand.)
opinion_df <- data.frame(sapply(opinion_df, 
                                    function(x) factor(x,
                                                       levels = c("Strongly Disagree", "Disagree","Neutral","Agree", "Strongly Agree"),
                                                       labels = c("Strongly Disagree", "Disagree","Neutral","Agree", "Strongly Agree"))),
                             check.names = FALSE)

for(i in 1: length(Questions)){
  xsamp <- sample(c("Strongly Agree", "Agree","Neutral","Disagree", "Strongly Disagree"),size = 200,replace = TRUE)
  opinion_df[,Questions[i]] <- factor(x =  xsamp,                                          levels = c("Strongly Disagree", "Disagree","Neutral","Agree", "Strongly Agree")
  )
}
# check the result
levels(opinion_df$Opinion_1)

library(dplyr)
gene_stable_id <- c("ENSG00000201217","ENSG00000206959","ENSG00000221345",
                    "ENSG00000206711","ENSG00000252341","ENSG00000201990")

gene_stable_id_version<- c("ENSG00000201217.1","ENSG00000206959.1",
                           "ENSG00000221345.2","ENSG00000206711.1","ENSG00000252341.1",
                           "ENSG00000201990.1")
ncbi_gene_id<-NA

gene_name<-c("Y_RNA","Y_RNA","U3","Y_RNA","Y_RNA","Y_RNA")

gene_df <- data.frame(gene_stable_id,gene_stable_id_version,
                      ncbi_gene_id,gene_name)

gene_df <- gene_df %>% 
  group_by(gene_name) %>% 
  mutate(counter = seq_along(gene_stable_id)) %>% 
  ungroup() %>% 
  mutate(gene_name = paste(gene_name,counter,sep = "_")) %>% 
  select(-counter)


x <- data.frame(Var1 = c("cat", NA, "bat", "hat", NA))

x <- x %>% 
  group_by(Var1) %>% 
  mutate(counter = seq_along(Var1)) %>% 
  ungroup() %>% 
  mutate(Var1 = ifelse(is.na(Var1), paste0(Var1,counter),as.character(Var1))) %>% 
  select(-counter)