library(amap)
hc <- hcluster(USArrests,method = "euclidean", link="complete")
distant = 7
index = 7
merge=hc$merge
height=hc$height
labels=hc$labels
indexarray=c(0)
index=-index


findD<-function(dis,ind)
{
  temp1=0
  temp2=0
  temp3=0
  indexOfind=(which(merge==ind,arr.ind = TRUE))
  if(dis>=0 && height[indexOfind[1]]<50)
  {
    indexarray=c(indexarray,ind)
    if(dis==0)
    {
      return(indexarray)
    }
    else
    {
      dis=dis-1
      if((dis-1)>=0 && merge[indexOfind[1],(3-indexOfind[2])]<0)
      {
        indexarray=c(indexarray,merge[indexOfind[1],(3-indexOfind[2])])
      }
      else
      {
        if((dis-2)>=0)
        {
          temp1= findD(dis,merge[merge[indexOfind[1],(3-indexOfind[2])],1])
          temp2= findD(dis,merge[merge[indexOfind[1],(3-indexOfind[2])],2])
        }
      } 
      temp3=findD(dis,indexOfind[1])
      return(c(temp1,temp2,temp3)) 
    }
  }
  return(indexarray)
}



if(distant>=1 && index>=-50 && index<=-1)
{
  x=findD(distant,index)
  y=unique(x)
  targetnames=c("0")
  for(i in 1:length(y))
  {
    if(y[i]<0)
    {
      if(y[i]!=index)
        targetnames=c(targetnames,labels[-y[i]])
    }
  }
  targetnames=targetnames[targetnames!="0"]
  print(sort(targetnames))
}

