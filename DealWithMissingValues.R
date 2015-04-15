x = read.csv("hw1input.csv",sep=",",header=T)

for (i in 1:ncol(x))
  {
    emprownum = which(is.na(x[,i]))
    aveval = sum(x[which(!is.na(x[,i])),i])/length(which(!is.na(x[,i])))
    if (length(emprownum)!=0 && emprownum[1]==1)
      x[1,i] = aveval
    if (length(emprownum)!=0 && emprownum[length(emprownum)]==nrow(x))
      x[nrow(x),i] = aveval
    emprownum = which(is.na(x[,i]))
    if (length(emprownum)!=0)
      for (j in 1:length(emprownum))
      {
        prerownum = emprownum[j]-1
        nextrownum = emprownum[j]+1
        while(is.na(x[nextrownum,i]))
        {
          nextrownum = nextrownum+1
        }
        x[emprownum[j],i] = x[prerownum,i]+(x[nextrownum,i]-x[prerownum,i])/(nextrownum-prerownum)
      }
  }

print(x)
