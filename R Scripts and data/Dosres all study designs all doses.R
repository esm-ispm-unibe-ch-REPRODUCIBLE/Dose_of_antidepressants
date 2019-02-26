

##################################################################
#     ANALYSIS INCLUDING all dose and all drugs
#################################################################

pdf("Meta-analytic dose plots for all drugs and doses.pdf")
sink("Meta-analytic dose-response analysis for all drugs and doses.txt")


#####CLEAN THE DATA####

#Create 3 index variables to exlcude studies per outcome (response, dropout, dropoutAE) that do not contribute to the dose-response (e.g. 0 events, different doses etc)
DOSESSRIs=cleandosresdata1.fun(DOSESSRIs,Study_No,logRR,Responders,No_randomised,hayasaka_ddd,"exc")
DOSESSRIs=cleandosresdata1.fun(DOSESSRIs,Study_No,logRRdrop,Dropouts_total,No_randomised,hayasaka_ddd,"excdrop")
DOSESSRIs=cleandosresdata1.fun(DOSESSRIs,Study_No,logRRdropAE,Dropouts_sideeffects,No_randomised,hayasaka_ddd,"excdropAE")

DOSE=DOSESSRIs

cat("\n \n \n DOSE RESPONSE ANALYSIS OF ALL ACTIVE DRUGS GIVEN AT ANY DOSE AND PLACEBO \n \n \n")
cat("From all analyses I excluded studies with less than 2 different doses evaluated.\n 
    Active drugs as well as Placebo are included.\n")

##REPORTING
write.csv(DOSE, "DOSEmainanalysis.csv")
cat("\n", paste("There are", length(unique(DOSE$Study_No)), "studies comparing all doses .", "\n"))
cat("which include the drugs:", unique(DOSE$Drug), "\n")
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
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Response"))
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(hayasaka_ddd[logRR!=0],exp(logRR[logRR!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
# create the RRs at specific doses
  cat("\n******Predicted RR and 95% CI with spline model for response****** \n")
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
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Dropout"))
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(hayasaka_ddd[logRRdrop!=0],exp(logRRdrop[logRRdrop!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
  
  # create the RRs at specific doses
  cat("\n******Predicted RR and 95% CI with spline model for dropout****** \n")
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
    plot(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim =  c(.5, 5),xlab="Dose",ylab="RR",main=c("Dropout AE"))
    matlines(get("rcs(hayasaka_ddd, knots)hayasaka_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(hayasaka_ddd[logRRdropAE!=0],exp(logRRdropAE[logRRdropAE!=0])))
  with(mymoredata,rug(hayasaka_ddd, quiet = TRUE))
  # create the RRs at specific doses
  cat("\n******Predicted RR and 95% CI with spline model for dropout AE****** \n")
  predictions=predict(doseresRR, data.frame(hayasaka_ddd=c(0,10,20,30,40,60,80)),xref, exp = TRUE)[,c(1,3,4,5)]
  names(predictions)<-c("dose hayddd","RR","lowCI", "highCI")
  print(predictions)
  
dev.off()
sink()
