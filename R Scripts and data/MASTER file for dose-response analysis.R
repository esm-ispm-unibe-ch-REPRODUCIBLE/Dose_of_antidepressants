            ############################################################
            #         Master analysis for Dose-response in Griselda
            ############################################################
            
#First you need to install the following packages:   
install.packages("netmeta")
install.packages("meta")
install.packages("metafor")
install.packages("readxl") 
install.packages("dosresmeta")
install.packages("rms") 

#load libraries
library(netmeta)
library(meta)
library(metafor)
library(readxl)
library(dosresmeta)
library(rms)

            
#Load the functions needed
source("Functions needed in the dose-response analysis.R")
            
            #########################
            #load data and prepare
            GRISELDAdose <- read_excel("GRISELDAdose.xlsx",  na = "NA")
            GRISELDAdose<-as.data.frame(GRISELDAdose)
            #delete studies that have missing dose 
            GRISELDAdose=GRISELDAdose[!is.na(GRISELDAdose$Dose_delivered_mean),]
            #Exclude flexible doses studies
            DOSE=GRISELDAdose[GRISELDAdose$Dosing_schedule=="Fixed",]
            SSRIs=c("citalopram", "escitalopram", "fluoxetine", "paroxetine", "sertraline","placebo")
            OTHERs=c("venlafaxine", "mirtazapine", "placebo")
            DOSE=DOSE[!is.na(match(DOSE$Drug,c(SSRIs,OTHERs))),]
            DOSE=exludesinglearmsdata.fun(DOSE,Study_No)
            #Include only trials with two or more arms of the same drug (and placebo)
            source("Select only single-drug studies.R")#this creates a new dataset, newDOSE
            
            #delete single arm studies
            DOSE=exludesinglearmsdata.fun(newDOSE,Study_No)
            
            
            #The following script creates the dose (fluoxetine equivalent) in the datafile DOSE
            #It also creates a database for SSRIs, for OTHERs (DOSESSRIs, DOSEOTHERs, DOSEall), a database for SSRIs in Placebo-controlled trials only (DOSESSRIsPC)
            # and a database DOSEj to be used for the jakubovski_ddd analysis
            source("Create dose in Griselda datafile DOSE.R")
           
            write.csv(DOSE,"Data to be used only for study reporting.csv")
            
            ################################################################################################
            #  Analyses
            ###############################################################################################
            
          
            # to produce graphs for all SSRIs drugs and doses together including placebo
            source("Dosres all study designs all doses.R")
            
            # to produce graphs for each drug separately run
            source("Dose-response analysis for each drug separately.R")
            
            ####SENSITIVITY ANALYSES
            
            # to produce graphs for all SSRIs drugs and doses together including placebo
            source("Sensitivity analysis only Placebo-controlled  all doses.R")
            
            #Sensitivity in number and position of knots 
            source("Sensitivity analysis to knots Dosres all study designs all doses.R")
            
            #Sensitivity in dose transformation using Jakubovski_ddd
            source("Sensitivity analysis Jakubovski Dosres all study designs all doses.R")
            
            #Sensitivity in dose transformation using meps_ddd
            source("Sensitivity analysis meps Dosres all study designs all doses.R")
          
            
            rm(list=ls())
            
            