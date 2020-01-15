dfZ <- foreign::read.spss("~/Downloads/Geopoll MSU Zambia Agricultural Funding Evaluation Data_2019-11-19-2.sav",value.labels = TRUE,to.data.frame = TRUE)

## Select module B questions

## Most Desirable
vars1 <- c("SurveyId", "B1", "B2","B3","B4","B5","B6","B7")

MD <- dfZ[, c(vars1)]

MDS<-list()
patterns <- vars1[-1]
for(i in 1:length(patterns)){
MDS[[i]] <- MD %>% 
  gather("Variable", "Response",-SurveyId, na.rm = T) %>% 
  filter(Variable==patterns[i]) %>% 
  mutate(Variable = 1) %>% 
  spread(Response, Variable) 
names(MDS[[i]])[-1] <- paste(patterns[i],names(MDS[[i]])[-1],sep = "_")
}

MDS_df <- Reduce(function(x,y) merge(x,y, all=TRUE,by = "SurveyId"),MDS )

MDS_df <- data.frame(sapply(MDS_df, function(x) ifelse(is.na(x),0,x)))
rm(MD, MDS)


## Least Desirable
vars2 <- c("SurveyId","B1_1", "B2_1",  "B3_1", "B4_1","B5_1","B6_1","B7_1")

LD <- dfZ[, c(vars2)]

LDS<-list()
patterns <- vars2[-1]
#patterns2 <- gsub("_1","",patterns)
for(i in 1:length(patterns)){
  LDS[[i]] <- LD %>% 
    gather("Variable", "Response",-SurveyId, na.rm = T) %>% 
    filter(Variable==patterns[i]) %>% 
    mutate(Variable = -1) %>% 
    spread(Response, Variable) 
  names(LDS[[i]])[-1] <- paste(patterns[i],names(LDS[[i]])[-1],sep = "_")
}

LDS_df <- Reduce(function(x,y) merge(x,y, all=TRUE,by = "SurveyId"),LDS )
LDS_df <- data.frame(sapply(LDS_df, function(x) ifelse(is.na(x),0,x)))
rm(LD, LDS)

## Combine both

z <- full_join(MDS_df, LDS_df, by = "SurveyId")[,-1]

## Counter

Counter <- z %>% 
  gather("Alternative", "Category", na.rm = T) %>% 
  group_by(Alternative, Category) %>% 
  count() %>% 
  ungroup() %>% group_by(Alternative) %>% 
  mutate(count = n())

## Get the alternative names

alternativeNames <- unique(gsub("B1|B2|B3|B4|B5|B6|B7|_|_1","",colnames(z)))


nAlternatives = length(alternativeNames)

nBlocks = ncol(z) / nAlternatives #8
nAltsPerSet = 4
n = nrow(z)
nObservations = n * nBlocks 
itMaxDiffData = matrix(as.numeric(t(z)),ncol = nAlternatives,byrow = TRUE, dimnames = list(1:nObservations, alternativeNames))


dimnames = list(1:nObservations, alternativeNames)

## Computing the overall counts for the whole sample
counts = apply(itMaxDiffData, 2, mean, na.rm = TRUE)
ranks = nAlternatives + 1 - rank(counts)
cbind(Counts = counts, Ranks = ranks)## Okay

## Increase.spending.on.the.Farmer.Input.Support.Program..FISP. is the most
## desirble attribute, followed by Improve.roads.and.bridges.in.the.rural.areas.
## Develop.better.crop.varieties.and.crop.management.practices is the least desirable attribute.


## Computing individual-level counts
id = rep(1:n,rep(nBlocks,n))
individualCounts = aggregate(itMaxDiffData,list(id),mean, na.rm = TRUE)[,-1]
round(individualCounts[1,],2) #show data for first respondent

## Computing individual-level ranks from the counts
set.seed(789) # setting the random number seed to enhance comparability
indidualCountsNoTies = individualCounts + matrix(runif(n * nAlternatives)/100000, n) #adding random numbers to break ties
ranks = nAlternatives + 1 - apply(indidualCountsNoTies,1,rank) #ranks
rankProportions = t(apply(ranks,1,table) / n * 100)
round(rankProportions,1)

## The first column of numbers shows the proportion of people that have 
## their highest count for each of the different attributes 
## Note that "Improve.roads.and.bridges.in.the.rural.areas" comes in first place by this measure, 
## whereas with the aggregate analysis it was second 


## The following analysis produces the cumulative proportions
rankCumProportions = t(apply(rankProportions,1,cumsum))
round(rankCumProportions,1)
## A majority of people (16.5%) most desire Improve.roads.and.bridges.in.the.rural.areas.

## We can compute the average of the ranks as follows:
aveRank = rankProportions %*% (1:7)/100
cbind(aveRank, Rank = rank(aveRank))

## Improve.roads.and.bridges.in.the.rural.areas  is rank first.
