# 1. Kiedy dyskretyzować?
# 2. Kiedy normalizować zmienne?

####Data preparation
dataInit<-read.csv("C:\\MOJE_PLIKI\\Sem3_ZaawEksplDanych\\PROJEKT_2\\sledzie.csv",stringsAsFactors = FALSE,na.strings = "?")
data<-tbl_df(dataInit)
data<-dataInit
dataSubset<-na.omit(dataInit)
ggplot(data=dataSubset,aes(x=X,y=length))+geom_line()
testpref<-data.frame(col1=c(1,2,NA,NA,4),col2=c(NA,1,2,3,4),col3=c(1,2,3,4,NA),col4=c(1,2,3,4,5))
testpref
testdf<-data.frame(col1=c(1,2,3,4,5),xmonth=c(7,7,12,1,2),col3=c(5,6,7,8,9),col4=c(1,2,3,4,5))
testdf

regsubsets(length ~ .,data = training_set, nvmax = 4)


#Dyskretyzacja
library(arules)
vect<-discretize(data$chel1,categories = 10)
vect

# Średnia ruchoma:
library("TTR")
SMA(c(1,2,3,4,5,6,7,8,9),n=3)


mynames<-colnames(testdf)
for(i in seq_along(mynames)) 
{
  if(checkColumns[i]>0)
  {
    #print(testdf[,i])
    print(paste("Analyze column",i))
    testdf[,i]<-replaceUnknown(testdf[,i])
  }
}

#Liczba obserwacji na miesiąc
data %>% group_by(xmonth) %>% summarize(countMonth=n())
###
mynames<-colnames(data)
mynames
checkColumns<-sapply(data,function(x){sum(is.na(x))})
checkColumns
for(i in seq_along(mynames)) 
{
  if(checkColumns[i]>0)
  {
    #print(testdf[,i])
    print(paste("Analyze column",i))
    data[,i]<-replaceUnknown(data[,i])
  }
}

replaceUnknown(data[,"chel1"])

replaceUnknown<-function(vector)
{
  len<-length(vector)
  for(i in seq_along(vector))
  {
    if(is.na(vector[i]))
    {
      #print(paste("NA found at position",i))
      prevElem<-findPrevElement(i,vector)
      nextElem<-findNextElement(i,vector)
      #print(paste("PREF: ",prevElem,"Nect",nextElem))
      if(is.na(prevElem)){
        if(is.na(nextElem))
        {
          vector[i]<-mean(vector,na.rm = T)
        }
        else
        {
          vector[i]<-nextElem
        }
      }
      else
      {
        if(is.na(nextElem))
        {
          vector[i]<-prevElem
        }
        else
        {
          vector[i]<-(prevElem+nextElem)/2
        }
      }

    }
  }
  vector
}

findNextElement<-function(from,vector){
  len<-length(vector)
  if(from==len)
  {
    NA
  }
  else if(is.na(vector[from+1]))
  {
    findNextElement(from+1,vector)
  }
  else
  {
    vector[from+1]
  }
}

findPrevElement<-function(from,vector){
  if(from==1)
  {
    NA
  }
  else if(is.na(vector[from-1]))
  {
    findPrevElement(from-1,vector)
  }
  else
  {
    vector[from-1]
  }
}

#Missing values
data$cfin1<-ifelse(is.na(data$cfin1),ave(data$cfin1,FUN=function(x) mean(x,na.rm=TRUE)),data$cfin1)

?ave
set.seed(23)
split=sample.split(df$length,SplitRatio = 0.8)
training_set<-subset(df,split==TRUE)
test_set<-subset(df,split==FALSE)


#different approach
diff<-round(range(df$X)[2]*0.8)
df_forSubset<- df %>% mutate(subset=ifelse(X>42065,1,0))
training_set<-subset(df_forSubset,subset==0)
test_set<-subset(df_forSubset,subset==1)

#Feature Scaling
trainint_set[,1]<-scale(trainint_set[,1])
test_set[,1]<-scale(test_set[,1])

#regression
regresor<-lm(formula =length ~ X,data = trainint_set )
summary(regresor)

#predict the test set result
y_pred=predict(regresor,newdata = test_set)

#compare by graph
library(ggplot2)
x_grid<-seq(min(df$X),max(df$X),0.1)
ggplot(data=data)+ 
  geom_point(aes(x=recr,y=length,color=recr)) +
  ggtitle("Długość a czas")

ggplot(data=data)+geom_bar(aes(x=cfin1),color="blue")
ggplot(data=data)+geom_bar(aes(x=cfin2),color="red")
ggplot(data=data)+geom_bar(aes(x=chel1),color="red")
ggplot(data=data)+geom_bar(aes(x=chel2),color="red")
ggplot(data=data)+geom_bar(aes(x=lcop1),color="red")
ggplot(data=data)+geom_bar(aes(x=lcop2),color="red")

ggplot(data=data)+geom_bar(aes(x=fbar),color="red")
ggplot(data=data)+geom_bar(aes(x=recr),color="red")
ggplot(data=data)+geom_bar(aes(x=cumf),color="red")
ggplot(data=data)+geom_bar(aes(x=totaln),color="red")

summary(data)
ggplot(data=data)+geom_line(aes(x=X,y=length),color="blue")+
  geom_line(aes(x=X,y=cfin1),color="green")+geom_line(aes(x=X,y=cfin2),color="red")
ggplot(data=data)+geom_line(aes(x=X,y=length),color="blue")+
  geom_line(aes(x=X,y=chel1),color="green")+geom_line(aes(x=X,y=chel2),color="red")
ggplot(data=data)+geom_line(aes(x=X,y=length),color="blue")+
  geom_line(aes(x=X,y=lcop1),color="green")+geom_line(aes(x=X,y=lcop2),color="red")
ggplot(data=data)+geom_line(aes(x=X,y=length),color="blue")+
  geom_line(aes(x=X,y=fbar),color="green")+geom_line(aes(x=X,y=cumf),color="red")
ggplot(data=data)+
  geom_line(aes(x=X,y=recr),color="green")+geom_line(aes(x=X,y=totaln),color="red")
ggplot(data=data)+geom_line(aes(x=X,y=length),color="blue")+
  geom_line(aes(x=X,y=sst),color="green")+geom_line(aes(x=X,y=sal),color="red")
ggplot(data=data)+geom_line(aes(x=X,y=length),color="blue")+geom_line(aes(x=X,y=nao),color="red")

ggplot(data=data)+geom_point(aes(x=X,y=length),color="blue")+geom_smooth()

ggplot(data=data,aes(x=X))+geom_line(aes(y=cfin2),color="red")
ggplot(data=data,aes(x=X))+geom_line(aes(y=chel1),color="blue")
ggplot(data=data,aes(x=X))+geom_line(aes(y=chel2),color="red")
ggplot(data=data,aes(x=X))+geom_line(aes(y=lcop1),color="blue")
ggplot(data=data,aes(x=X))+geom_line(aes(y=lcop2),color="red")

ggplot(data=data,aes(x=X))+geom_line(aes(y=fbar),color="blue")
ggplot(data=data,aes(x=X))+geom_line(aes(y=recr),color="red")
ggplot(data=data,aes(x=X))+geom_line(aes(y=cumf),color="blue")
ggplot(data=data,aes(x=X))+geom_point(aes(y=totaln,color=length))

ggplot(data=data,aes(x=X))+geom_line(aes(y=sst),color="red")
ggplot(data=data,aes(x=X))+geom_line(aes(y=sal),color="blue")
ggplot(data=data,aes(x=X))+geom_line(aes(y=nao),color="red")


ggplot(data=data)+geom_point(aes(x=cfin1,y=length))

#WARToSCi odstające:
#cfin1,cfin2,lcop1,lcop2,recr
