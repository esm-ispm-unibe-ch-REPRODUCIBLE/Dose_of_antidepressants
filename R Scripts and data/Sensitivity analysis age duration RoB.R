

####################################################################################################################
#     Sensitivity analysis excluding age>=65 and follow-up less than 6 weeks and high/moderate RoB studies
###################################################################################################################


sink("Sensitivity analysis in studies with participants less than 65 years old follow-up 6 weeks or more and low RoB.txt")
par(mfrow=c(1,3))

########################################################
#####FOR AGE ##########################################
########################################################

oxo<-DOSESSRIs$age>=65|is.na(DOSESSRIs$age)
DOSE=DOSESSRIs[!oxo,]
DOSE=exludesinglearmsdata.fun(DOSE,Study_No)

##REPORTING
cat("\n \n \n ****************************************************************** \n")
cat("\n  DOSE RESPONSE ANALYSIS OF SSRIs in patients less than 65 years old  \n")
cat("\n ****************************************************************** \n \n \n")
cat("\n", paste("There are", length(unique(DOSE$Study_No)), "studies comparing all doses in patients less than 65 years old.", "\n"))

cat("\nThe knots I used in the splines are at doses 10,20,50 mg")
knots=c(10,20,50) 

################
#1. response
###############

cat("\n-----------------------------------------------\n")
cat("\n-------- RESPONSE -----------------------------\n")

mymoredata=DOSE[DOSE$exc==F,]

cat("\n-------- Splines response -----------------------------\n")
#cubic splines

cat("\n******For the spline model we have in total",length(unique(mymoredata$Study_No)),"studies")

  doseresRR=dosresmeta(formula=logRR~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0,80,1))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, type = "l",
         xlim = c(0, 80), ylim = c(.5, 4),xlab="Dose",ylab="RR",main=c("Response"),col="deepskyblue", lwd=2)
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col="deepskyblue",lty="dashed",lwd=0.5)})
  #with(mymoredata,points(hayasaka_ddd[logRR!=0],exp(logRR[logRR!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
# create the RRs at specific doses
  cat("\n******Predicted RR and 95% CI with spline model for response****** \n")
  predictions=predict(doseresRR, data.frame(hayasaka_ddd=c(0,10,20,30,40,60,80)),xref, exp = TRUE)[,c(1,3,4,5)]
  names(predictions)<-c("dose hayddd","RR","lowCI", "highCI")
  print(predictions)


################################
#3. dropout due to AE
###############################
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT DUE TO AE --------------------\n")
mymoredata=DOSE[DOSE$excdropAE==F,] 
mymoredata$Dropouts_sideeffects=replace(mymoredata$Dropouts_sideeffects, mymoredata$Dropouts_sideeffects==0,0.05)#correct for zero events in some arms

cat("\n-------- Splines dropout AE -----------------------------\n")
#cubic splines

cat("\n******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies\n")

  doseresRR=dosresmeta(formula=logRRdropAE~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0,80,1))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, type = "l",
         xlim = c(0, 80), ylim =  c(.5, 4),xlab="Dose",ylab="RR",main=c("Dropout AE"),col="deeppink",lwd=2)
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col="deeppink",lty="dashed",lwd=0.5)})
  #with(mymoredata,points(hayasaka_ddd[logRRdropAE!=0],exp(logRRdropAE[logRRdropAE!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
  # create the RRs at specific doses
  cat("\n******Predicted RR and 95% CI with spline model for dropout AE****** \n")
  predictions=predict(doseresRR, data.frame(hayasaka_ddd=c(0,10,20,30,40,60,80)),xref, exp = TRUE)[,c(1,3,4,5)]
  names(predictions)<-c("dose hayddd","RR","lowCI", "highCI")
  print(predictions)

  
  ################
  #2. dropout
  ###############
  cat("\n-----------------------------------------------\n")
  cat("\n-------- DROPOUT  -----------------------------\n")
  
  mymoredata=DOSE[DOSE$excdrop==F,] 
  
  
  
  cat("\n-------- Splines dropout -----------------------------\n")
  
  cat("******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies")
  
  doseresRR=dosresmeta(formula=logRRdrop~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0,80,1))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, type = "l",
         xlim = c(0, 80), ylim = c(.5, 4),xlab="Dose",ylab="RR",main=c("Dropout"),col="purple",lwd=2)
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col="purple",lty="dashed",lwd=0.5)})
  #with(mymoredata,points(hayasaka_ddd[logRRdrop!=0],exp(logRRdrop[logRRdrop!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
  
  # create the RRs at specific doses
  cat("\n******Predicted RR and 95% CI with spline model for dropout****** \n")
  predictions=predict(doseresRR, data.frame(hayasaka_ddd=c(0,10,20,30,40,60,80)),xref, exp = TRUE)[,c(1,3,4,5)]
  names(predictions)<-c("dose hayddd","RR","lowCI", "highCI")
  print(predictions)
  
  
  ########################################################
  #####FOR WEEKS of follow-up ############################
  ########################################################
  
  DOSESSRIs$weeks<-as.numeric(DOSESSRIs$weeks)
  oxo<-DOSESSRIs$weeks<6|is.na(DOSESSRIs$weeks)
  DOSE=DOSESSRIs[!oxo,]
  DOSE=exludesinglearmsdata.fun(DOSE,Study_No)
  
  ##REPORTING
  cat("\n \n \n ****************************************************************** \n")
  cat("\n  DOSE RESPONSE ANALYSIS OF SSRIs in studies with 6 or more weeks of follow-up  \n")
  cat("\n ****************************************************************** \n \n \n")
  
  cat("\n", paste("There are", length(unique(DOSE$Study_No)), "studies with 6 or more weeks of follow-up", "\n"))
  
  cat("\nThe knots I used in the splines are at doses 10,20,50 mg")
  knots=c(10,20,50) 
  
  ################
  #1. response
  ###############
  
  cat("\n-----------------------------------------------\n")
  cat("\n-------- RESPONSE -----------------------------\n")
  
  mymoredata=DOSE[DOSE$exc==F,]
  
  cat("\n-------- Splines response -----------------------------\n")
  #cubic splines
  
  cat("\n******For the spline model we have in total",length(unique(mymoredata$Study_No)),"studies")
  
  doseresRR=dosresmeta(formula=logRR~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0,80,1))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, type = "l",
         xlim = c(0, 80), ylim = c(.5, 4),xlab="Dose",ylab="RR",main=c("Response"),col="deepskyblue", lwd=2)
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col="deepskyblue",lty="dashed",lwd=0.5)})
  #with(mymoredata,points(hayasaka_ddd[logRR!=0],exp(logRR[logRR!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
  # create the RRs at specific doses
  cat("\n******Predicted RR and 95% CI with spline model for response****** \n")
  predictions=predict(doseresRR, data.frame(hayasaka_ddd=c(0,10,20,30,40,60,80)),xref, exp = TRUE)[,c(1,3,4,5)]
  names(predictions)<-c("dose hayddd","RR","lowCI", "highCI")
  print(predictions)
  
  
  ################################
  #3. dropout due to AE
  ###############################
  cat("\n-----------------------------------------------\n")
  cat("\n-------- DROPOUT DUE TO AE --------------------\n")
  mymoredata=DOSE[DOSE$excdropAE==F,] 
  mymoredata$Dropouts_sideeffects=replace(mymoredata$Dropouts_sideeffects, mymoredata$Dropouts_sideeffects==0,0.05)#correct for zero events in some arms
  
  cat("\n-------- Splines dropout AE -----------------------------\n")
  #cubic splines
  
  cat("\n******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies\n")
  
  doseresRR=dosresmeta(formula=logRRdropAE~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0,80,1))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, type = "l",
         xlim = c(0, 80), ylim =  c(.5, 4),xlab="Dose",ylab="RR",main=c("Dropout AE"),col="deeppink",lwd=2)
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col="deeppink",lty="dashed",lwd=0.5)})
  #with(mymoredata,points(hayasaka_ddd[logRRdropAE!=0],exp(logRRdropAE[logRRdropAE!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
  # create the RRs at specific doses
  cat("\n******Predicted RR and 95% CI with spline model for dropout AE****** \n")
  predictions=predict(doseresRR, data.frame(hayasaka_ddd=c(0,10,20,30,40,60,80)),xref, exp = TRUE)[,c(1,3,4,5)]
  names(predictions)<-c("dose hayddd","RR","lowCI", "highCI")
  print(predictions)
  
  
  ################
  #2. dropout
  ###############
  cat("\n-----------------------------------------------\n")
  cat("\n-------- DROPOUT  -----------------------------\n")
  
  mymoredata=DOSE[DOSE$excdrop==F,] 
  
  
  
  cat("\n-------- Splines dropout -----------------------------\n")
  
  cat("******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies")
  
  doseresRR=dosresmeta(formula=logRRdrop~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(hayasaka_ddd=seq(0,80,1))
  xref=min(mymoredata$hayasaka_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, type = "l",
         xlim = c(0, 80), ylim = c(.5, 4),xlab="Dose",ylab="RR",main=c("Dropout"),col="purple",lwd=2)
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col="purple",lty="dashed",lwd=0.5)})
  #with(mymoredata,points(hayasaka_ddd[logRRdrop!=0],exp(logRRdrop[logRRdrop!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
  
  # create the RRs at specific doses
  cat("\n******Predicted RR and 95% CI with spline model for dropout****** \n")
  predictions=predict(doseresRR, data.frame(hayasaka_ddd=c(0,10,20,30,40,60,80)),xref, exp = TRUE)[,c(1,3,4,5)]
  names(predictions)<-c("dose hayddd","RR","lowCI", "highCI")
  print(predictions)
  




########################################################
#####FOR RoB ###########################################
########################################################

##REPORTING

  cat("\n \n \n ****************************************************************** \n")
  cat("\n  DOSE RESPONSE ANALYSIS OF studies with low RoB  \n")
  cat("\n ****************************************************************** \n \n \n")
  
cat("\n \n  Frequencies of the RoB (in arms) \n \n")
print(table(DOSESSRIs$Overall_study_RoB))

DOSE=DOSESSRIs[DOSESSRIs$Overall_study_RoB=="Low risk",]
DOSE=exludesinglearmsdata.fun(DOSE,Study_No)

cat("\n", paste("There are", length(unique(DOSE$Study_No)), "studies with low RoB", "\n"))


knots=c(10,20,50) 

################
#1. response
###############

cat("\n-----------------------------------------------\n")
cat("\n-------- RESPONSE -----------------------------\n")

mymoredata=DOSE[DOSE$exc==F,]

cat("\n-------- Splines response -----------------------------\n")
#cubic splines

cat("\n******For the spline model we have in total",length(unique(mymoredata$Study_No)),"studies")

doseresRR=dosresmeta(formula=logRR~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
print(summary(doseresRR))
newdata=data.frame(hayasaka_ddd=seq(0,80,1))
xref=min(mymoredata$hayasaka_ddd)
with(predict(doseresRR, newdata,xref, exp = TRUE), {
  plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, type = "l",
       xlim = c(0, 80), ylim = c(.5, 4),xlab="Dose",ylab="RR",main=c("Response"),col="deepskyblue", lwd=2)
  matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col="deepskyblue",lty="dashed",lwd=0.5)})
#with(mymoredata,points(hayasaka_ddd[logRR!=0],exp(logRR[logRR!=0])))
with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
# create the RRs at specific doses
cat("\n******Predicted RR and 95% CI with spline model for response****** \n")
predictions=predict(doseresRR, data.frame(hayasaka_ddd=c(0,10,20,30,40,60,80)),xref, exp = TRUE)[,c(1,3,4,5)]
names(predictions)<-c("dose hayddd","RR","lowCI", "highCI")
print(predictions)


################################
#3. dropout due to AE
###############################
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT DUE TO AE --------------------\n")
mymoredata=DOSE[DOSE$excdropAE==F,] 
mymoredata$Dropouts_sideeffects=replace(mymoredata$Dropouts_sideeffects, mymoredata$Dropouts_sideeffects==0,0.05)#correct for zero events in some arms

cat("\n-------- Splines dropout AE -----------------------------\n")
#cubic splines

cat("\n******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies\n")

doseresRR=dosresmeta(formula=logRRdropAE~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
print(summary(doseresRR))
newdata=data.frame(hayasaka_ddd=seq(0,80,1))
xref=min(mymoredata$hayasaka_ddd)
with(predict(doseresRR, newdata,xref, exp = TRUE), {
  plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, type = "l",
       xlim = c(0, 80), ylim =  c(.5, 4),xlab="Dose",ylab="RR",main=c("Dropout AE"),col="deeppink",lwd=2)
  matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col="deeppink",lty="dashed",lwd=0.5)})
#with(mymoredata,points(hayasaka_ddd[logRRdropAE!=0],exp(logRRdropAE[logRRdropAE!=0])))
with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
# create the RRs at specific doses
cat("\n******Predicted RR and 95% CI with spline model for dropout AE****** \n")
predictions=predict(doseresRR, data.frame(hayasaka_ddd=c(0,10,20,30,40,60,80)),xref, exp = TRUE)[,c(1,3,4,5)]
names(predictions)<-c("dose hayddd","RR","lowCI", "highCI")
print(predictions)


################
#2. dropout
###############
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT  -----------------------------\n")

mymoredata=DOSE[DOSE$excdrop==F,] 



cat("\n-------- Splines dropout -----------------------------\n")

cat("******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies")

doseresRR=dosresmeta(formula=logRRdrop~rcs(hayasaka_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata)
print(summary(doseresRR))
newdata=data.frame(hayasaka_ddd=seq(0,80,1))
xref=min(mymoredata$hayasaka_ddd)
with(predict(doseresRR, newdata,xref, exp = TRUE), {
  plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, type = "l",
       xlim = c(0, 80), ylim = c(.5, 4),xlab="Dose",ylab="RR",main=c("Dropout"),col="purple",lwd=2)
  matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col="purple",lty="dashed",lwd=0.5)})
#with(mymoredata,points(hayasaka_ddd[logRRdrop!=0],exp(logRRdrop[logRRdrop!=0])))
with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))

# create the RRs at specific doses
cat("\n******Predicted RR and 95% CI with spline model for dropout****** \n")
predictions=predict(doseresRR, data.frame(hayasaka_ddd=c(0,10,20,30,40,60,80)),xref, exp = TRUE)[,c(1,3,4,5)]
names(predictions)<-c("dose hayddd","RR","lowCI", "highCI")
print(predictions)



#dev.off()
sink()
