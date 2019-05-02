
#### This little routine uses the dataset DOSE and creates a new dataset newDOSE that includes only studies the have the same drug in different doses and, 
## optionally, placebo. Multi-drug studies of placebo are split by dividing the placebo group


newDOSE=c()# a new DOSE database
a=split(DOSE,DOSE$Study_No)

for (i in 1:length(unique(DOSE$Study_No)))# loop in all studies
{

  sa=split(a[[i]],a[[i]]$Drug)
  whereispla=!is.na(match(names(sa),c("placebo")))#finds the place of placebo
  oldStudy_No=a[[i]]$Study_No[1]
  uniquedrugs=length(table(names(sa)))

  if(sum(whereispla)==0)#in head-to-head studies
    {
      if(max(table(a[[i]]$Drug))>1)#keep those head-to-head that have at least 2 doses of the same drug
          {
            for(j in 1:length(sa)){#re-assign study IDs for the case of splitting studies
            sa[[j]]$Study_No=oldStudy_No*1000+j}
            sa=do.call("rbind", sa)
           }
      else{sa=NULL}
     }
  if(sum(whereispla)>0)#in Placebo-controlled
    {
        if(uniquedrugs==2)# studies PA and PA...A
        {
          sa=do.call("rbind", sa)}
        if(uniquedrugs>2)# studies PAB and PA...AB..BC..C
              {sa$placebo$No_randomised=round(sa$placebo$No_randomised/(uniquedrugs-1))#split the placebo sample size 
               sa$placebo$Responders=round(sa$placebo$Responders/(uniquedrugs-1))#split the placebo events 
              sa$placeborep=data.frame(matrix(rep(unlist(sa$placebo),(uniquedrugs-1)), nrow=(uniquedrugs-1), byrow=T))
              names(sa$placeborep)=names(sa$placebo)
              sa$placebo=NULL
                    for(j in 1:length(sa))#re-assign study IDs for the case of splitting studies
                      {sa[[j]]$Study_No=oldStudy_No*1000+j}
              sa$placeborep$Study_No=oldStudy_No*1000+c(1:(uniquedrugs-1))
              sa=do.call("rbind", sa)
              sa=sa[order(sa$Study_No),]}
    
  }
  newDOSE=rbind.data.frame(newDOSE,sa)
  
}

 newDOSE[,c(2:4,6,7,9,10,12,16:23,48:50)]<-lapply(newDOSE[,c(2:4,6,7,9,10,12,16:23,48:50)],as.numeric)
 rm(a,i,j,whereispla,uniquedrugs,oldStudy_No,sa)   
    
    
    
    
