##########################################
#FUNCTIONS NEEDED in the dose-response analysis
###########################################

#a simple function that excludes single-arm studies
exludesinglearmsdata.fun<-function(dataset,studyid)
{
  #the dataset to correct dataset
  # studyid: defining the name of the studyid column
  studyid=eval(substitute(studyid), dataset)
  singlearmstudies=names(table(studyid))[table(studyid)<2]
  dataset2=dataset[is.na(match(studyid,singlearmstudies)),]
  dataset2
  #returns a dataset with the same columns after excluding multiarms
}

# A FUNCTION THAT TAKES A DATABASE AND CREATES LOGRR AND THE SE FOR A DOSE-RESPONSE ANALYSIS

createRRreference.fun=function(r,n)
{
  
  logRR=c(0)
  selogRR=c(NA)
  
  for(i in 2:c(length(n)))
  {
    calculate=metabin(r[i],n[i],r[1],n[1],sm="RR")
    logRR=c(logRR,calculate$TE)
    selogRR=c(selogRR,calculate$seTE)
    
  }
  return(cbind(logRR=logRR,selogRR=selogRR))
}

# A FUNCTION THAT CREATES THE DATA IN DOSE-RESPONSE FORMAT
createdatasetdoseresponse.fun<-function(dataset,r,n,studyid,dose, nameoflogRR="logRR",nameofselogRR="selogRR"){
  
  #--------------------------------------------
  #this function takes a dataset and a) orders the within-study data by dose b)checks and excludes single arm studies
  # c) calculates RR and selogRR to be used in the dose-response model
  
  #the dataset: the dataset, one arm per row
  # r,n,studyid,dose: defining the events, sample size, studyid and dose columns
  # nameoflogRR="logRR",nameofselogRR="selogRR": define the names of the output columns to be added to the dataset
  
  # Needs the functions:exludesinglearmsdata.fun, createRRreference.fun
  #--------------------------------------------
  
  originaldimension=dim(dataset)[2]
  r=eval(substitute(r), dataset)
  n=eval(substitute(n), dataset)
  dose=eval(substitute(dose), dataset)
  studyid=eval(substitute(studyid), dataset)
  
  #Order within study data from the lowest to the highest dose
  dataset=dataset[with(dataset,order(studyid,dose)),]
  
  #Exclude single arm studies
  dataset=exludesinglearmsdata.fun(dataset, studyid)
  
  #create the variables
  ID=unique(studyid)
  logRR=c()
  selogRR=c()
  RRmat=c()
  
  for(i in ID){
    r1=c(r[studyid==i])
    n1=c(n[studyid==i])
    a=createRRreference.fun(r1,n1)
    
    RRmat=rbind(RRmat,a)
  }
  RRmat=as.data.frame(RRmat)
  names(RRmat)=c(nameoflogRR,nameofselogRR)
  
  #create the final dataset by adding the logRR and selogRR
  dataset=cbind.data.frame(dataset,RRmat)
  #dataset$logRR=RRmat[,1]
  #dataset$selogRR=RRmat[,2]
  #names(dataset)[originaldimension+1]=nameoflogRR
  #names(dataset)[originaldimension+2]=nameofselogRR
  #return
  dataset
}

######################

#this function clears the data out of problems that may make the code in dose-reponse not running

cleandosresdata.fun=function(dataset,studyid,logRR,r,n,dose,nametoexclude="toexclude")
{ 
  # this function takes a database and exclude studies that will cause problemsin fiting the dosres model for a particular outcome
  # It return the same database but in the end it has a variable that tells you which studies to exclude
  # Give a name to that column in the nametoexclude variable
  originaldimension=dim(dataset)[2]
  r=eval(substitute(r), dataset)
  logRR=eval(substitute(logRR), dataset)
  dose=eval(substitute(dose), dataset)
  n=eval(substitute(n), dataset)
  studyid=eval(substitute(studyid), dataset)
  #exlude missing logRR
  out0=unique(studyid[is.na(logRR)])
  #exclude studies with zero events
  out1=unique(studyid[r==0 & !is.na(r)])
  out2=unique(studyid[r==n & !is.na(r)])
  #exclude those studies with the same dose in all arms
  out3=unique(studyid)[tapply(dose,studyid,max)==tapply(dose,studyid,min)]
  toexclude=as.data.frame(studyid%in%c(out0,out1,out2,out3))
  names(toexclude)=c(nametoexclude)
  #exclude single arm studies
  dataset=cbind.data.frame(dataset,toexclude)
  dataset
}

