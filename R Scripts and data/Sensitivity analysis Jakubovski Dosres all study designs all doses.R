

##################################################################
#     SENSITIVITY ANALYSIS using Jakubovski_ddd
#     ANALYSIS INCLUDING all dose and all drugs
#################################################################

pdf("Sensitivity analysis Jakobovski Meta-analytic dose plots for all SSRIs and doses.pdf")
sink("Sensitivity analysis Jakobovski Meta-analytic dose-response analysis for all drugs and doses.txt")

cat("\n \n \n DOSE RESPONSE ANALYSIS OF ALL ACTIVE DRUGS GIVEN AT ANY DOSE using Jakubovski AND PLACEBO \n \n \n")
cat("From all analyses I excluded studies with zero events and studies with less than 2 different doses evaluated.\n 
    Active drugs as well as Placebo are included.\n")



#delete single arm studies

#Create 3 index variables to exlcude studies per outcome (response, dropout, dropoutAE) that do not contribute to the dose-response (e.g. 0 events, different doses etc)
DOSEj1=cleandosresdata.fun(DOSEj,Study_No,logRR,Responders,No_randomised,jakubovski_ddd,"exc")
DOSEj1=cleandosresdata.fun(DOSEj1,Study_No,logRRdrop,Dropouts_total,No_randomised,jakubovski_ddd,"excdrop")
DOSEj1=cleandosresdata.fun(DOSEj1,Study_No,logRRdropAE,Dropouts_sideeffects,No_randomised,jakubovski_ddd,"excdropAE")

##REPORTING
cat("\n", paste("There are", length(unique(DOSEj1$Study_No)), "studies comparing all doses .", "\n"))
cat("which include the drugs:", unique(DOSEj1$Drug), "\n")
cat("\nThe knots I used in the splines are at doses 10,20,30 mg")
knots=c(10,20,50)
################
#1. response
###############

cat("\n-----------------------------------------------\n")
cat("\n-------- RESPONSE -----------------------------\n")

mymoredata=DOSEj1[DOSEj1$exc==F,]


text=paste(length(unique(mymoredata$Study_No)),"studies comparing all drugs and doses for response")


cat("\n-------- Splines response -----------------------------\n")
#cubic splines

cat("\n******For the spline model we have in total",length(unique(mymoredata$Study_No)),"studies")

  doseresRR=dosresmeta(formula=logRR~rcs(jakubovski_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Responders,n=No_randomised,se=selogRR,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(jakubovski_ddd=seq(0,80,1))
  xref=min(mymoredata$jakubovski_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(jakubovski_ddd[logRR!=0],exp(logRR[logRR!=0])))
  with(mymoredata,rug(jakubovski_ddd, quiet = TRUE))

################
#2. dropout
###############
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT  -----------------------------\n")

mymoredata=DOSEj1[DOSEj1$excdrop==F,] 



cat(paste("\n******For the linear model there are", length(unique(mymoredata$Study_No)), "studies", "\n"))


  
cat("\n-------- Splines dropout -----------------------------\n")

cat("******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies")

  doseresRR=dosresmeta(formula=logRRdrop~rcs(jakubovski_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_total,n=No_randomised,se=selogRRdrop,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(jakubovski_ddd=seq(0,80,1))
  xref=min(mymoredata$jakubovski_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(jakubovski_ddd[logRRdrop!=0],exp(logRRdrop[logRRdrop!=0])))
  with(mymoredata,rug(jakubovski_ddd, quiet = TRUE))


################################
#3. dropout due to AE
###############################
cat("\n-----------------------------------------------\n")
cat("\n-------- DROPOUT DUE TO AE --------------------\n")
mymoredata=DOSEj1[DOSEj1$excdropAE==F,] 


text=paste(length(unique(mymoredata$Study_No)),"studies comparing all drugs and doses for dropout due to AE")


cat("\n-------- Splines dropout AE -----------------------------\n")
#cubic splines

cat("\n******For the splines model we have in total",length(unique(mymoredata$Study_No)),"studies\n")

  doseresRR=dosresmeta(formula=logRRdropAE~rcs(jakubovski_ddd,knots), proc="1stage",id=Study_No, type=type,cases=Dropouts_sideeffects,n=No_randomised,se=selogRRdropAE,data=mymoredata)
  print(summary(doseresRR))
  newdata=data.frame(jakubovski_ddd=seq(0,80,1))
  xref=min(mymoredata$jakubovski_ddd)
  with(predict(doseresRR, newdata,xref, exp = TRUE), {
    plot(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),pred, log = "y", type = "l",
         xlim = c(0, 80), ylim = c(.5, 5),xlab="Dose",ylab="RR",main=c("Splines",text))
    matlines(get("rcs(jakubovski_ddd, knots)jakubovski_ddd"),cbind(ci.ub,ci.lb),col=1,lty="dashed")})
  #with(mymoredata,points(jakubovski_ddd[logRRdropAE!=0],exp(logRRdropAE[logRRdropAE!=0])))
  with(mymoredata,rug(jakubovski_ddd, quiet = TRUE))

dev.off()
sink()
