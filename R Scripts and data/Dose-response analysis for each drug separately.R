

################################################################################################
# Analysis of studies with the same Drug 
##############################################################################################
# Create the data
##################
# all 7 drugs analysed
DOSE=DOSEall[!is.na(DOSEall$No_randomised),]

tableDRUGsStudy=with(DOSE,table(Study_No,Drug))
idtokeep1=unique(DOSE$Study_No)[apply(tableDRUGsStudy,1,max)>=2]#keep studies with at least 2 doses of the same drug
idtokeep2=unique(DOSE$Study_No)[tableDRUGsStudy[,colnames(tableDRUGsStudy)=="placebo"]==1]#keep placebo-controlled
idtokeep=unique(c(idtokeep1,idtokeep2))
DOSE$sameDrug=with(DOSE,!is.na(match(Study_No, idtokeep)))
DOSEsameDrug=DOSE[DOSE$sameDrug,]#create a database that only has studies with the same drug


####    sort report
cat(paste("We have",length(unique(DOSEsameDrug$Study_No)),"studies comparing different doses of the same drug"))
cat("\n Nr of studies with multiple doses of the same drug:")
print(apply(table(DOSEsameDrug$Drug,DOSEsameDrug$Study_No)>0,1,sum))

dis=names(apply(table(DOSEsameDrug$Drug,DOSEsameDrug$Study_No)>0,1,sum))#names of drugs with multiple doses per arm
dis=dis[dis!="placebo"]

##################################################################################
###       Meta-analysis of each drug                                             ##
##################################################################################
##############
## Response
##############

sink("Per drug dose response.txt")
pdf("Per drug dose response.pdf")
for(i in 1:length(dis)) 
{#iterate in drugs
  cat("***************************","\n")
  cat(paste("Studies in",dis[i],"\n"))
  cat("***************************","\n")
  studis=unique(DOSEsameDrug$Study_No[DOSEsameDrug$Drug==dis[i]])
  mymoredata=DOSEsameDrug[!is.na(match(DOSEsameDrug$Study_No,studis)),]
  mymoredata=mymoredata[!is.na(mymoredata$logRR),]
  mymoredata=exludesinglearmsdata.fun(mymoredata,studyid = Study_No)
  cat(paste("There are", length(unique(mymoredata$Study_No)), "studies", "\n"))
  
  if(max(mymoredata$logRR,na.rm = T)==0 | length(unique(mymoredata$Study_No))<2){cat(paste("not enough efficacy data"),"\n")}
  else{
    maxdose=max(mymoredata$Dose_delivered_mean)
    mindose=min(mymoredata$Dose_delivered_mean)

        if(dis[i]=="citalopram")knots=c(10,20,50)/1
                if(dis[i]=="escitalopram")knots=c(10,20,50)/2.22
                  if(dis[i]=="fluoxetine")knots=c(10,20,50)/1
                          if(dis[i]=="mirtazapine")knots=c(10,20,50)/0.79
                              if(dis[i]=="paroxetine")knots=c(10,20,50)/1.18
                                 if(dis[i]=="sertraline") knots=c(10,20,50)/0.41
                                      if(dis[i]=="venlafaxine")knots=c(10,20,50)/0.27

#scale the max axis in each graph

    if(dis[i]=="citalopram")xmax=80/1
    if(dis[i]=="escitalopram")xmax=80/2.22
    if(dis[i]=="fluoxetine")xmax=80/1
    if(dis[i]=="mirtazapine")xmax=80/0.79
    if(dis[i]=="paroxetine")xmax=80/1.18
    if(dis[i]=="sertraline") xmax=80/0.41
    if(dis[i]=="venlafaxine")xmax=400

    
    cat(paste("The knots for i=", i, "are:",round(knots), "\n"))
    text=paste(length(studis),"studies with",unique(mymoredata$Drug)[1],"vs",unique(mymoredata$Drug)[-1])

       # if(dis[i]=="venlafaxine"){
       #   mymoredata$Study_No==128
       # mymoredata$logRR[1]<-0
      #  mymoredata$logRR[2]<-mymoredata$logRR[2]-mymoredata$logRR[1]
       # mymoredata$selogRR[1]<-NA
       # }
  
    
    #cubic splines
    tryCatch({#start tryCatch to avoid stopping with stupid errors
      doseresRR=dosresmeta(formula=logRR~rcs(Dose_delivered_mean,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
      summary(doseresRR)
      newdata=data.frame(Dose_delivered_mean=seq(0,maxdose,1))
      xref=min(mymoredata$Dose_delivered_mean)
      with(predict(doseresRR, newdata,xref, exp = TRUE), {
        plot(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),pred, log = "y", type = "l",
             xlim = c(0, xmax), ylim = c(.5, 5),xlab="Actual mean dose",ylab="RR",main=c("Splines",text))
        matlines(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
      with(mymoredata,rug(Dose_delivered_mean, quiet = TRUE))},error=function(e){cat("ERROR in Spline",":",conditionMessage(e), "\n")})
    
    #printing predictions for venlafaxine
    if(dis[i]=="venlafaxine"){
      cat("\n******Predicted RR and 95% CI with spline model for Venlafaxine response****** \n")
      predictions=predict(doseresRR, data.frame(Dose_delivered_mean=c(0,37.5,75,150,225,300,375)),xref, exp = TRUE)[,c(1,3,4,5)]
      names(predictions)<-c("dose","RR","lowCI", "highCI")
      print(predictions)}
    #printing predictions for venlafaxine
    if(dis[i]=="mirtazapine"){
      cat("\n******Predicted RR and 95% CI with spline model for Mirtazapine response****** \n")
      predictions=predict(doseresRR, data.frame(Dose_delivered_mean=c(0,7.5,15,30,45,60)),xref, exp = TRUE)[,c(1,3,4,5)]
      names(predictions)<-c("dose","RR","lowCI", "highCI")
      print(predictions)}
    
    
    
  }#end else
}
dev.off()
sink()

##############
## Dropout
##############

sink("Per drug dose dropout.txt")
pdf("Per drug dose dropout.pdf")
for(i in 1:length(dis))
{#iterate in drugs
  cat("***************************","\n")
  cat(paste("Studies in",dis[i],"\n"))
  cat("***************************","\n")

  studis=unique(DOSEsameDrug$Study_No[DOSEsameDrug$Drug==dis[i]])
  mymoredata=DOSEsameDrug[!is.na(match(DOSEsameDrug$Study_No,studis)),]
  mymoredata=mymoredata[!is.na(mymoredata$logRRdrop),]
  mymoredata=exludesinglearmsdata.fun(mymoredata,studyid = Study_No)
  
  
  cat(paste("There are", length(unique(mymoredata$Study_No)), "studies", "\n"))
  
  if(max(mymoredata$logRRdrop,na.rm = T)==0 | length(unique(mymoredata$Study_No))<2){cat(paste("not enough dropout data"),"\n")}
  else{
    
    maxdose=max(mymoredata$Dose_delivered_mean)
    mindose=min(mymoredata$Dose_delivered_mean)
    
    if(dis[i]=="citalopram")knots=c(10,20,50)/1
    if(dis[i]=="escitalopram")knots=c(10,20,50)/2.22
    if(dis[i]=="fluoxetine")knots=c(10,20,50)/1
    if(dis[i]=="mirtazapine")knots=c(10,20,50)/0.79
    if(dis[i]=="paroxetine")knots=c(10,20,50)/1.18
    if(dis[i]=="sertraline") knots=c(10,20,50)/0.41
    if(dis[i]=="venlafaxine")knots=c(10,20,50)/0.27
    
    #scale the max axis in each graph
    
    if(dis[i]=="citalopram")xmax=80/1
    if(dis[i]=="escitalopram")xmax=80/2.22
    if(dis[i]=="fluoxetine")xmax=80/1
    if(dis[i]=="mirtazapine")xmax=80/0.79
    if(dis[i]=="paroxetine")xmax=80/1.18
    if(dis[i]=="sertraline") xmax=80/0.41
    if(dis[i]=="venlafaxine")xmax=400
    
    cat(paste("The knots for i=", i, "are:",round(knots), "\n"))
    text=paste(length(studis),"studies with",unique(mymoredata$Drug)[1],"vs",unique(mymoredata$Drug)[-1])
    
    
    #cubic splines
    tryCatch({#start tryCatch to avoid stopping with stupid errors
      doseresRR=dosresmeta(formula=logRRdrop~rcs(Dose_delivered_mean,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata[,])
      summary(doseresRR)
      newdata=data.frame(Dose_delivered_mean=seq(0,maxdose,1))
      xref=min(mymoredata$Dose_delivered_mean)
      with(predict(doseresRR, newdata,xref, exp = TRUE), {
        plot(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),pred, log = "y", type = "l",
             xlim = c(0, xmax), ylim = c(.5, 5),xlab="Actual mean dose",ylab="RR dropout",main=c("Splines",text))
        matlines(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
      with(mymoredata,rug(Dose_delivered_mean, quiet = TRUE))},error=function(e){cat("ERROR in Spline",":",conditionMessage(e), "\n")})
    
    #printing predictions for venlafaxine
    if(dis[i]=="venlafaxine"){
      cat("\n******Predicted RR and 95% CI with spline model for Venlafaxine dropout****** \n")
      predictions=predict(doseresRR, data.frame(Dose_delivered_mean=c(0,37.5,75,150,225,300,375)),xref, exp = TRUE)[,c(1,3,4,5)]
      names(predictions)<-c("dose","RR","lowCI", "highCI")
      print(predictions)}
    #printing predictions for venlafaxine
    if(dis[i]=="mirtazapine"){
      cat("\n******Predicted RR and 95% CI with spline model for Mirtazapine dropout****** \n")
      predictions=predict(doseresRR, data.frame(Dose_delivered_mean=c(0,7.5,15,30,45,60)),xref, exp = TRUE)[,c(1,3,4,5)]
      names(predictions)<-c("dose","RR","lowCI", "highCI")
      print(predictions)}
    
    
  }#end else
}

dev.off() 
sink()
############################
## dropout due to AE
############################

sink("Per drug dose dropout AE.txt")
pdf("Per drug dose dropout AE.pdf")
for(i in 1:length(dis)) 
{#iterate in drugs
  cat("***************************","\n")
  cat(paste("Studies in",dis[i],"\n"))
  cat("***************************","\n")
  
  studis=unique(DOSEsameDrug$Study_No[DOSEsameDrug$Drug==dis[i]])
  mymoredata=DOSEsameDrug[!is.na(match(DOSEsameDrug$Study_No,studis)),]
  mymoredata=mymoredata[!is.na(mymoredata$logRRdropAE),]
  mymoredata=exludesinglearmsdata.fun(mymoredata,studyid = Study_No)
  
  cat(paste("There are", length(unique(mymoredata$Study_No)), "studies", "\n"))
  
  if(max(mymoredata$logRRdropAE,na.rm = T)==0 | length(unique(mymoredata$Study_No))<2){cat(paste("not enough dropout data"),"\n")}
  else{
    maxdose=max(mymoredata$Dose_delivered_mean)
    mindose=min(mymoredata$Dose_delivered_mean)
    
    if(dis[i]=="citalopram")knots=c(10,20,50)/1
    if(dis[i]=="escitalopram")knots=c(10,20,50)/2.22
    if(dis[i]=="fluoxetine")knots=c(10,20,50)/1
    if(dis[i]=="mirtazapine")knots=c(10,20,50)/0.79
    if(dis[i]=="paroxetine")knots=c(10,20,50)/1.18
    if(dis[i]=="sertraline") knots=c(10,20,50)/0.41
    if(dis[i]=="venlafaxine")knots=c(10,20,50)/0.27
    
    #scale the max axis in each graph
    
    if(dis[i]=="citalopram")xmax=80/1
    if(dis[i]=="escitalopram")xmax=80/2.22
    if(dis[i]=="fluoxetine")xmax=80/1
    if(dis[i]=="mirtazapine")xmax=80/0.79
    if(dis[i]=="paroxetine")xmax=80/1.18
    if(dis[i]=="sertraline") xmax=80/0.41
    if(dis[i]=="venlafaxine")xmax=400
    
    
    cat(paste("The knots for i=", i, "are:",round(knots), "\n"))
    text=paste(length(studis),"studies with",unique(mymoredata$Drug)[1],"vs",unique(mymoredata$Drug)[-1])
    
    
    #cubic splines
    mymoredata$Dropouts_sideeffects=replace(mymoredata$Dropouts_sideeffects, mymoredata$Dropouts_sideeffects==0,0.05)#correct for zero events in some arms
    
    tryCatch({#start tryCatch to avoid stopping with stupid errors
      doseresRR=dosresmeta(formula=logRRdropAE~rcs(Dose_delivered_mean,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
      summary(doseresRR)
      newdata=data.frame(Dose_delivered_mean=seq(0,maxdose,1))
      xref=min(mymoredata$Dose_delivered_mean)
      with(predict(doseresRR, newdata,xref, exp = TRUE), {
        plot(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),pred, log = "y", type = "l",
             xlim = c(0, xmax), ylim = c(.5, 5),xlab="Actual mean dose",ylab="RR dropout AE",main=c("Splines",text))
        matlines(get("rcs(Dose_delivered_mean, knots)Dose_delivered_mean"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
      with(mymoredata,rug(Dose_delivered_mean, quiet = TRUE))},error=function(e){cat("ERROR in Spline",":",conditionMessage(e), "\n")})
      
    #printing predictions for venlafaxine
      if(dis[i]=="venlafaxine"){
    cat("\n******Predicted RR and 95% CI with spline model for Venlafaxine dropoutAE****** \n")
    predictions=predict(doseresRR, data.frame(Dose_delivered_mean=c(0,37.5,75,150,225,300,375)),xref, exp = TRUE)[,c(1,3,4,5)]
    names(predictions)<-c("dose","RR","lowCI", "highCI")
    print(predictions)}
    #printing predictions for venlafaxine
    if(dis[i]=="mirtazapine"){
      cat("\n******Predicted RR and 95% CI with spline model for Mirtazapine dropoutAE****** \n")
      predictions=predict(doseresRR, data.frame(Dose_delivered_mean=c(0,7.5,15,30,45,60)),xref, exp = TRUE)[,c(1,3,4,5)]
names(predictions)<-c("dose","RR","lowCI", "highCI")
print(predictions)}
    

  }#end else
}

dev.off() 
sink()
