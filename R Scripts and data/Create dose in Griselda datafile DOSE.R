#####################  

#create various dosages
myreplace=function(targetvector, replacementvector,selectionvector){
  #targetvector of lenght l has values some of which we want to replace
  #replacementvector of lenght l has the values that you want to copy in a new vector
  #selectionvector of lenght l has T or F indicating which values should be kept
  out=targetvector
  out[selectionvector]=replacementvector[selectionvector]
  out
}


DOSE$bollini_ddd=DOSE$Dose_delivered_mean
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.666,DOSE$Drug=="citalopram")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*1.332,DOSE$Drug=="escitalopram")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="fluoxetine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.67,DOSE$Drug=="mirtazapine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="paroxetine")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.24,DOSE$Drug=="sertraline")
DOSE$bollini_ddd=myreplace(DOSE$bollini_ddd,DOSE$Dose_delivered_mean*0.2,DOSE$Drug=="venlafaxine")

DOSE$hayasaka_ddd=DOSE$Dose_delivered_mean
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="citalopram")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*2.22,DOSE$Drug=="escitalopram")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="fluoxetine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.79,DOSE$Drug=="mirtazapine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*1.18,DOSE$Drug=="paroxetine")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.41,DOSE$Drug=="sertraline")
DOSE$hayasaka_ddd=myreplace(DOSE$hayasaka_ddd,DOSE$Dose_delivered_mean*0.27,DOSE$Drug=="venlafaxine")

DOSE$ddd=DOSE$Dose_delivered_mean
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="citalopram")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*2,DOSE$Drug=="escitalopram")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="fluoxetine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.67,DOSE$Drug=="mirtazapine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="paroxetine")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.40,DOSE$Drug=="sertraline")
DOSE$ddd=myreplace(DOSE$ddd,DOSE$Dose_delivered_mean*0.20,DOSE$Drug=="venlafaxine")

DOSE$jakubovski_ddd=DOSE$Dose_delivered_mean
DOSE$jakubovski_ddd=myreplace(DOSE$jakubovski_ddd,DOSE$Dose_delivered_mean*0.60,DOSE$Drug=="citalopram")
DOSE$jakubovski_ddd=myreplace(DOSE$jakubovski_ddd,DOSE$Dose_delivered_mean*1.2,DOSE$Drug=="escitalopram")
DOSE$jakubovski_ddd=myreplace(DOSE$jakubovski_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="fluoxetine")
DOSE$jakubovski_ddd=myreplace(DOSE$jakubovski_ddd,DOSE$Dose_delivered_mean*0.67,DOSE$Drug=="mirtazapine")
DOSE$jakubovski_ddd=myreplace(DOSE$jakubovski_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="paroxetine")
DOSE$jakubovski_ddd=myreplace(DOSE$jakubovski_ddd,DOSE$Dose_delivered_mean*0.17,DOSE$Drug=="sertraline")
DOSE$jakubovski_ddd=myreplace(DOSE$jakubovski_ddd,DOSE$Dose_delivered_mean*0.20,DOSE$Drug=="venlafaxine")

DOSE$meps_ddd=DOSE$Dose_delivered_mean
DOSE$meps_ddd=myreplace(DOSE$meps_ddd,DOSE$Dose_delivered_mean*1.23,DOSE$Drug=="citalopram")
DOSE$meps_ddd=myreplace(DOSE$meps_ddd,DOSE$Dose_delivered_mean*2.52,DOSE$Drug=="escitalopram")
DOSE$meps_ddd=myreplace(DOSE$meps_ddd,DOSE$Dose_delivered_mean*1,DOSE$Drug=="fluoxetine")
DOSE$meps_ddd=myreplace(DOSE$meps_ddd,DOSE$Dose_delivered_mean*1.62,DOSE$Drug=="mirtazapine")
DOSE$meps_ddd=myreplace(DOSE$meps_ddd,DOSE$Dose_delivered_mean*1.02,DOSE$Drug=="paroxetine")
DOSE$meps_ddd=myreplace(DOSE$meps_ddd,DOSE$Dose_delivered_mean*0.43,DOSE$Drug=="sertraline")
DOSE$meps_ddd=myreplace(DOSE$meps_ddd,DOSE$Dose_delivered_mean*0.23,DOSE$Drug=="venlafaxine")

                           

### KEEP ONLY USEFUL VARIABLES and DRUGS WE WANT

DOSE=DOSE[,c("Study_No","Overall_study_RoB","Study_year","Drug","Dose_range","No_randomised","Responders","Dropouts_total","Dropouts_sideeffects","remitters","age","weeks","hayasaka_ddd","ddd", "jakubovski_ddd","meps_ddd","Dose_delivered_mean")]


#DOSE=DOSE[,c("Study_No","No of arms","Study_year","Drug","Dose_range","No_randomised","Responders","Dropouts_total","Dropouts_sideeffects","N compimputed","Mean","SD","hayasaka_ddd","ddd", "jakubovski_ddd","meps_ddd","Dose_delivered_mean")]


##ORDER THE DATABASE SO THAT WE HAVE WITHIN EACH STUDY PLACEBO OR LEAST DOSE FIRST
DOSE=DOSE[with(DOSE,order(Study_No,hayasaka_ddd)),]

#exclude single-arm studies
DOSE=exludesinglearmsdata.fun(DOSE,Study_No)

#CREATE STUDY TYPE
DOSE$type="cc"

## Create two other datasets according to SSRIs and others and then DOSEall
DOSEall=DOSE
DOSESSRIs=DOSE[!is.na(match(DOSE$Drug,SSRIs)),]
DOSESSRIs=exludesinglearmsdata.fun(DOSESSRIs,Study_No)
DOSEOTHERs=DOSE[!is.na(match(DOSE$Drug,OTHERs)),]
DOSEOTHERs=exludesinglearmsdata.fun(DOSEOTHERs,Study_No)





################################################################################################
#  Produce per arm LOG RR for RESPONSE, DROPOUT AND DROPOUT DUE TO AE
###############################################################################################

DOSESSRIs=createdatasetdoseresponse.fun(DOSESSRIs,Responders,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRR",nameofselogRR="selogRR")
DOSESSRIs=createdatasetdoseresponse.fun(DOSESSRIs,Dropouts_total,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRdrop",nameofselogRR="selogRRdrop")
DOSESSRIs=createdatasetdoseresponse.fun(DOSESSRIs,Dropouts_sideeffects,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRdropAE",nameofselogRR="selogRRdropAE")
DOSESSRIs=createdatasetdoseresponse.fun(DOSESSRIs,remitters,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRrem",nameofselogRR="selogRRrem")


#COPY THE DATABASE DOSESSRIs to DOSE TO THE DOSEj to be used for the jakubovski_ddd analysis
DOSEj=DOSESSRIs

DOSEall=createdatasetdoseresponse.fun(DOSEall,Responders,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRR",nameofselogRR="selogRR")
DOSEall=createdatasetdoseresponse.fun(DOSEall,Dropouts_total,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRdrop",nameofselogRR="selogRRdrop")
DOSEall=createdatasetdoseresponse.fun(DOSEall,Dropouts_sideeffects,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRdropAE",nameofselogRR="selogRRdropAE")
DOSEall=createdatasetdoseresponse.fun(DOSEall,remitters,No_randomised,Study_No,hayasaka_ddd,nameoflogRR="logRRrem",nameofselogRR="selogRRrem")


#Create a database with the SSRIs in placebo-controlled trials only
PC=unique(DOSESSRIs$Study_No)[(tapply(DOSESSRIs$Drug=="placebo",DOSESSRIs$Study_No,sum)==1)]
DOSESSRIsPC=DOSESSRIs[DOSESSRIs$Study_No%in%PC,]


