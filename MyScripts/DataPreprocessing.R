library(dplyr)
data %>% summarize(cfin1_distinct=n_distinct(cfin1))
data %>% select (3:16) %>% sapply(n_distinct)

summary(data)

year_seq<-1
data$year_seq<-NULL
for(i in seq_along(data$xmonth))
{
  month<-data$xmonth[i]
  if(i==1)
  {
    data$year_seq[i]<-paste(year_seq,"_",if(month<10) {paste("0",month)} else {month})
    
  }
  else
  {
    if(month>=data$xmonth[i-1])
    {
      data$year_seq[i]<-paste(year_seq,"_",if(month<10) {paste("0",month)} else {month})
    }
    else
    {
      year_seq<-year_seq+1
      data$year_seq[i]<-paste(year_seq,"_",if(month<10) {paste("0",month)} else {month})
    }
  }
}
data %>% select (3:17) %>% sapply(n_distinct)
# Pogrupowanie wg roku i miesiąca
data_years<-data %>% group_by(year_seq) %>% summarize(meanLength=mean(length))
ggplot(data=data_years,aes(x=year_seq,y=meanLength))+geom_point()

library(Hmisc)
data$year_seq<-NULL
corelationsData<-round(cor(data, method="pearson") ,2)
corelationsData<-cbind(CorelatedAttribute=corelationsData,rownames(corelationsData))
corelationsData<-as.data.frame(corelationsData)
corelationsData<-corelationsData %>% gather (AttributeName,Corelation,1:16)
require(graphics)
corelationsData$
pairs(corelationsData)
str(data)
sample(nrow(data), 5)
