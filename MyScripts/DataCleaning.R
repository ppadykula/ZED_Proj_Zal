replaceUnknown<-function(vector)
{
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
rmseCalc <- function(error)
{
  sqrt(mean(error^2))
}


