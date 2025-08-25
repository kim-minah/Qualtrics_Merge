#---------------------------------------------------------------
# Program: Assign New Column Names for Qualtric Outputs v2
# Author: Minah Kim
# Date: 6/3/21

# Updates from v1: 
# -> Pilot data included

# Notes:
#   This script was created by comparing the Qualtrics output
#   for 4/5 yr olds, 6 yr olds, 7 yr olds & 8-12 yr olds:
#   -> 4/5 yr olds uses CBCL-preschool, the rest uses CBCL
#   -> not all questionnaires were given to all ages and not in the same order 
#     (see 'Merge Colnames' section)
#---------------------------------------------------------------

#---------------#
# Load packages #
#---------------#
library(psych)
library(stringi)

#-----------------#
# Set working dir #
#-----------------#
# Minah's local machine (comment if running from someplace else)
setwd("~/GitHub/Qualtrics_Merge/input/")

#------------------------#
# Read in reference data #
#------------------------#
dat<-read.csv('Qualtrics_Questions.csv',check.names=FALSE,
              header=FALSE)


#---------------------------#
# Find Out Assessment Names #
#---------------------------#
assessment_names <- dat[1,stri_detect(dat[1,],regex="\\w")]
unlist(assessment_names)

# [Main Study]
# DEMOS: Demographics
# CSUS: Children's Social Understanding Scale
# SSIS: Social Skills Improvement System
# SDQ: Strengths and Difficulties Questionnaire
# CBCL Child Behavior Checklist
# SRS: Social Responsiveness Scale
# ERC: Emotion Regulation Checklist
# Life Events: Significant Child Life Events Questionnaire
# PSI: Parenting Stress Index
# CNE: Coping with Children's Negative Emotions Scale 
# CPRS: Child-Parent Relationship Scale
# [Pilot]
# GEM: Griffith Empathy Measure
# ICUP: Inventory of Callous-Unemotional Traits - Parent Version

#--------------------#
# Colnames for DEMOS #
#--------------------#
demos <- unlist(dat[2,5:45])
names(demos) <- NULL
demos_num <- c(1:6,"6_TEXT",7:9,"10_1","10_2","10_3","11_1","11_2","11_3",
               "12_1","12_2","12_3","13_1","13_2","13_3","14_1","14_2","14_3",
               15,"15_TEXT",16:17,"17_TEXT",18,"18_TEXT",19:20,"20_TEXT",21,
               "21_TEXT",22:23,"23_TEXT","24")
demos <- paste0("DEMOS_Q",demos_num)

#-------------------#
# Colnames for CSUS #
#-------------------#
csus <- unlist(dat[2,46:63])
names(csus) <- NULL
csus_num <- c("1_1","1_2","1_3","2_1","2_2","2_3","3_1","3_2","3_3","4_1","4_2",
              "4_3","5_1","5_2","5_3","6_1","6_2","6_3")
csus <- paste0("CSUS_Q",csus_num)

#-------------------#
# Colnames for SSIS #
#-------------------#
ssis <- unlist(dat[2,64:188])
names(ssis) <- NULL

# Create a function so that the first 44 questions are listed by groups of 4 
# (which is how it was structured in Qualtrics) and by the two diff question 
# types ('how often' & 'how important')
ssis_num <- vector(length=88)
t <- 1
for (j in seq(1,88,by=8)){
  for (i in 0:3){
    ssis_num[j] <- paste0(i+t,"_1")
    j <- j+1}
  for (s in 0:3){
    ssis_num[j] <- paste0(s+t,"_2")
    j <- j+1
  }
  t <- t+4
}
ssis[1:88] <- ssis_num
ssis_num_therest <- c("45_1","46_1","45_2","46_2",paste0(47:79,"_1"))
ssis[89:125] <- ssis_num_therest
ssis <- paste0("SSIS_Q",ssis)
#------------------#
# Colnames for SDQ #
#------------------#
sdq <- unlist(dat[2,189:213])
names(sdq) <- NULL
sdq <- paste0("SDQ_Q",1:25)

#-----------------------------#
# Colnames for CBCL Preschool #
#-----------------------------#
cbcl_preschool <- unlist(dat[2,214:317])
names(cbcl_preschool) <- NULL
cbcl_preschool <- c(paste0("CBCL_Preschool_Q",1:101),"CBCL_Preschool_Q101_TEXT",
                    "CBCL_Preschool_Q102","CBCL_Preschool_Q103")

#-------------------#
# Colnames for CBCL #
#-------------------#
cbcl <- unlist(dat[14,214:396])
cbcl_questions <- unlist(dat[15,214:396]) # see ChildPsych_Qualtrics_Merge script notes for why this is needed
names(cbcl) <- NULL
cbcl_num <- paste0(c(rep("Q1_",12),rep("Q2_",12),rep("Q3_",6)),unlist(stri_extract_all_regex(cbcl[1:12],"[12].+")))                    
cbcl_num <- c(cbcl_num,paste0(rep("Q4_",6),unlist(stri_extract_all_regex(cbcl[1:6],"[12].+"))))                    
cbcl_num <- c(cbcl_num,paste0("Q",5:10),paste0("Q11_",c("1_1","1_2","1_3",
                                                        "1_4","1_4_TEXT",
                                                        "1_5","1_5_TEXT",
                                                        "1_6","1_6_TEXT")),
              "Q12","Q12_TEXT","Q13","Q13_TEXT","Q14","Q14_TEXT","Q15",
              "Q16","Q16_TEXT","Q17","Q17_TEXT","Q18","Q19")
temp <- unlist(stri_extract_all_regex(cbcl[65:179],"_.+"))
length(temp)
temp2 <- paste0(rep("Q",115),rep(20:42,each=5))
temp3 <- paste0(temp2,temp)
cbcl_num <- c(cbcl_num,temp3,"Q44_1","Q44_2","Q44_3","Q45")
cbcl <- paste0("CBCL_",cbcl_num)

#------------------#
# Colnames for SRS #
#------------------#
srs <- unlist(dat[2,318:382])
names(srs) <- NULL
srs <- paste0("SRS_Q",1:65)

#------------------#
# Colnames for ERC #
#------------------#
erc <- unlist(dat[2,383:406])
names(erc) <- NULL
erc <- paste0("ERC_Q",1:24)

#--------------------------#
# Colnames for LIFE EVENTS #
#--------------------------#
life_events <- unlist(dat[2,407:454])
names(life_events) <- NULL
life_events_num <- vector(length=48)
t <- 1
for (i in seq(1,48,by=2)){
  life_events_num[i] <- paste0(t,"_1")
  life_events_num[i+1] <- paste0(t,"_2")
  t <- t+1
  }
life_events <- paste0("LIFE_EVENTS_Q",life_events_num) 

#------------------#
# Colnames for PSI #
#------------------#
psi <- unlist(dat[2,455:490])
names(psi) <- NULL
psi <- paste0("PSI_Q",1:36)

#------------------#
# Colnames for CNE #
#------------------#
cne <- unlist(dat[2,491:562])
names(cne) <- NULL
cne_num <- paste0(rep(1:12,each=6),"_",1:6)
cne <- paste0("CNE_Q",cne_num)

#-------------------#
# Colnames for CPRS #
#-------------------#
cprs <- unlist(dat[2,563:592])
names(cprs) <- NULL
cprs <- paste0("CPRS_Q",1:30)

#----------------#
# Merge Colnames #
#----------------#
starting_colnames <- c("StartDate", "EndDate", "Duration (in seconds)", "RecordedDate", "ExternalReference")
col_four_five <- c(starting_colnames,demos,csus,ssis,sdq,cbcl_preschool,srs,erc,life_events,psi,cne,cprs)
col_six <- c(starting_colnames,demos,csus,sdq,ssis,cbcl,srs,erc,life_events,psi,cne,cprs)
col_seven <- c(starting_colnames,demos,csus,sdq,ssis,cbcl,srs,erc,life_events,
               psi,cprs)
col_eight_twelve <- c(starting_colnames,demos,sdq,ssis,cbcl,srs,erc,life_events,psi,cprs)
# colnames_all <- list(col_four_five,col_six, col_seven, col_eight_twelve)

#---------------------------------------------------------------
#---------------------#
# Read in Pilot Data #
#---------------------#

pilotfiles = sort(Sys.glob('Emotional+Epigenetic+Development*.csv'))
pilot_1 <- read.csv(pilotfiles[1],
                    check.names=FALSE,
                    header=FALSE)
pilot_2 <- read.csv(pilotfiles[2], check.names=FALSE, header=FALSE)

#--------------------#
# Colnames for DEMOS #
#--------------------#
# Pilot_1 has the same Demographics questions except for DEMOS_Q3: Which is your child's dominant hand?
demos_num_pilot <- c(1:2,4:6,"6_TEXT",7:9,"10_1","10_2","10_3","11_1","11_2","11_3",
               "12_1","12_2","12_3","13_1","13_2","13_3","14_1","14_2","14_3",
               15,"15_TEXT",16:17,"17_TEXT",18,"18_TEXT",19:20,"20_TEXT",21,
               "21_TEXT",22:23,"23_TEXT","24")
demos_pilot <- paste0("DEMOS_Q",demos_num_pilot)


#-------------------#
# Colnames for CSUS #
#-------------------#
# Same as Main Study, can use 'csus'

#-------------------#
# Colnames for SSIS #
#-------------------#
# Same as Main Study, can use 'ssis'

#------------------#
# Colnames for SDQ #
#------------------#
# Same as Main Study, can use 'sdq'

#------------------#
# Colnames for GEM #
#------------------#
gem <- paste0("GEM_Q",c("Example",1:23))
gem_questions <- unlist(pilot_1[2,226:249])

#-------------------#
# Colnames for ICUP #
#-------------------#
# Question 7 was accidentally left out in Qualtrics for the 6+ Pilot data
icup <- paste0("ICUP_Q",c(1:6,8:24))
icup_five <- paste0("ICUP_Q",c(1:24))
icup_questions <- unlist(pilot_2[2,250:273])

#-------------------#
# Colnames for CBCL #
#-------------------#
# Same as Main Study, can use 'cbcl'

#-----------------------------#
# Colnames for CBCL Preschool #
#-----------------------------#
# For pilot - 5 yrs

# Two questions are not in the same order as it is in Main Study (M)
# P24 = M25
# P25 = M24

#  Q24 and Q26 are the same ("Doesn't get along with other children")
#   Only one participant and survey answers show that Q24 and Q26 have same answers
#   so will just get rid of the Q26 column (done in ChildPsych_Qualtrics_Merge_v3.R)

# Q101 is broken into two questions in Main Study:
#   Does the child have any illness or disability (either physical or mental)? If yes, please   
#   describe. -> Does the child have any illnesses or disability (either physical or mental) - 
#   Selected Choice & Does the child have any illnesses or disability (either physical or mental) 
#   - Yes - please describe - Text
# To get around this, will not include a CBCL_Preschool_Q101, only CBCL_Preschool_Q101_TEXT

# In Main Study CBCL Preschool ends at CBCL_Preschool_Q103, in Pilot there are 25 more
cbcl_preschool_pilot_num <- c(1:23,25,24,26:100,"101_TEXT",102:128)
cbcl_preschool_pilot <- paste0("CBCL_Preschool_Q",cbcl_preschool_pilot_num)
cbcl_preschool_pilot_questions <- unlist(pilot_2[2,378:402])
#--------------------------#
# Colnames for LIFE EVENTS #
#--------------------------#
# Same as Main Study, can use 'life_events'

#------------------#
# Colnames for SRS #
#------------------#
# Same as Main Study, can use 'srs'

#------------------#
# Colnames for ERC #
#------------------#
# Same as Main Study, can use 'erc'

#----------------#
# Merge Colnames #
#----------------#
col_pilot <- c(starting_colnames, demos_pilot, csus, ssis, sdq, gem, icup, cbcl,
               life_events,srs, erc)
col_pilot_five <- c(starting_colnames, demos_pilot, csus,ssis,sdq,gem,icup_five,cbcl_preschool_pilot,life_events)

colnames_all <- list(col_four_five,col_six, col_seven, col_eight_twelve, col_pilot,col_pilot_five)
