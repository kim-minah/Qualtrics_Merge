library(psych)
library(car)
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(lattice)
library(pander)
library(stringr)
library(ggpubr)
install.packages("jtools")
library(jtools)
install.packages("janitor")
library(janitor)


setwd("~/Desktop/Projects/ChildPsych/VCT2/Qualtrics")
#T2_Qualtrics_all <- read.csv("T2_Qualtrics_121619.csv", na.strings = c("", "NA"), header=TRUE)
T2_Qualtrics_all <- read.csv("T2_Qualtrics_111320.csv", na.strings = c("", "NA"), header=TRUE)

### Cleaning up the Demographic Information ###

#Pull only the demographic columns
T2_Demos <- subset(T2_Qualtrics_all, select=Q216:Q182_1)


#Rename colnames to something sensible
names(T2_Demos) <- c("Child_ID", "DOB", "Handedness", "Sex", "Intersex", "Gender", "Gender_Desc", "Grade", "SchoolType", "NumSibs",
                     "Sib1_Age", "Sib1_Sex", "Sib1_Gender", "Sib2_Age", "Sib2_Sex", "Sib2_Gender", "Sib3_Age", "Sib3_Sex",
                     "Sib3_Gender", "Sib4_Age", "Sib4_Sex", "Sib4_Gender", "Sib5_Age", "Sib5_Sex", "Sib5_Gender", "Caregiver1",
                     "Caregiver1_Desc", "Caregiver1_Job", "Caregiver1_Ed", "Caregiver1_Ed_Desc", "Caregiver2", "Caregiver2_Desc", 
                     "Caregiver2_Job", "Caregiver2_Ed", "Caregiver2_Ed_Desc", "Residence", "Residence_Desc", "BirthWeeks", 
                     "NICU", "NICU_Desc", "NemoExp" )
#Drop the actual question text row
T2_Demos <- T2_Demos[-1, ]
#Remove any columns that weren't used
T2_Demos <- remove_empty(T2_Demos, which="cols")
#Remove columns we don't really care about for now
T2_Demos <- within(T2_Demos, rm(DOB, Sex, Intersex, Gender_Desc, Sib1_Age, Sib1_Sex, Sib2_Age, Sib2_Sex,
                                Sib3_Age, Sib3_Sex, Sib4_Age, Sib4_Sex, Sib1_Gender, Sib2_Gender, Sib3_Gender, Sib4_Gender, Sib5_Gender))

#Change NA to 0 for number of siblings
T2_Demos$NumSibs <- as.numeric(T2_Demos$NumSibs)
T2_Demos$NumSibs[is.na(T2_Demos$NumSibs)] <- 0
str(T2_Demos$NumSibs)


#Recode caregiver educaiton
T2_Demos$Caregiver1_Ed <- recode_factor(T2_Demos$Caregiver1_Ed, 'High School' = "1", 'Vocational' = "2", 'Associates' = "2", 'Bachelors' = "3",
                                 'Masters' = "4", "PhD or other terminal degree (please specify)" = "5")
T2_Demos$Caregiver2_Ed <- recode_factor(T2_Demos$Caregiver2_Ed, 'High School' = "1", 'Vocational' = "2", 'Associates' = "2", 'Bachelors' = "3",
                                        'Masters' = "4", "PhD or other terminal degree (please specify)" = "5")

#Create an education score
T2_Demos$CaregiverReside <- NA
T2_Demos$CaregiverReside <- ifelse(grepl("Caregiver 1,Caregiver 2", T2_Demos$Residence), "2", "1")

T2_Demos$ChildSESEd <- as.numeric(T2_Demos$Caregiver1_Ed) + as.numeric(T2_Demos$Caregiver2_Ed) 

#If "Other" was selected for certain columns, replace with the supplied text


ggplot(T2_Demos, aes(x=as.factor(ChildSESEd))) + geom_bar()
ggplot(T2_Demos, aes(x=SchoolType)) + geom_bar()
ggplot(T2_Demos, aes(x=NumSibs)) + geom_bar()
ggplot(T2_Demos, aes(x=NemoExp)) + geom_bar()
ggplot(T2_Demos, aes(x=NICU)) + geom_bar()

write.csv(T2_Demos, "T2_Demos.csv")

### Scoring CSUS ###
#need to recode variables, if scored with likert change to numbers
#need to reverse score Q4 and Q10
CSUS <- dplyr::select(T2_Qualtrics_all, Q216, Q24_1:Q46_1)
names(CSUS) <- c("Child_ID", paste0("CSUS_", 1:18))
CSUS <- CSUS[-1, ]

CSUS$CSUS_4 <- dplyr::recode(CSUS$CSUS_4, "1"="4", "2"="3", "3"="2", "4"="1")
CSUS$CSUS_10 <- dplyr::recode(CSUS$CSUS_10, "1"="4", "2"="3", "3"="2", "4"="1")

CSUS[,c(2:19)] = apply(CSUS[,c(2:19)], 2, function(x) as.numeric(as.character(x)))

CSUS <- transform(CSUS, T2_CSUS_Total = rowMeans(CSUS[,-1], na.rm = TRUE))
write.csv(CSUS, "T2_CSUS.csv")


### Scoring SDQ ###
SDQ <- dplyr::select(T2_Qualtrics_all, Q216, Q81_1_1:Q86_1_5)
names(SDQ) <- c("Child_ID", paste0("SDQ_", 1:25))
SDQ <- SDQ[-1, ]

SDQ$SDQ_7 <- dplyr::recode(SDQ$SDQ_7, "0"="2", "1"="1", "2"="0")
SDQ$SDQ_11 <- dplyr::recode(SDQ$SDQ_11, "0"="2", "1"="1", "2"="0")
SDQ$SDQ_14 <- dplyr::recode(SDQ$SDQ_14, "0"="2", "1"="1", "2"="0")
SDQ$SDQ_21 <- dplyr::recode(SDQ$SDQ_21, "0"="2", "1"="1", "2"="0")
SDQ$SDQ_25 <- dplyr::recode(SDQ$SDQ_25, "0"="2", "1"="1", "2"="0")

SDQ[,c(2:26)] = apply(SDQ[,c(2:26)], 2, function(x) as.numeric(as.character(x)))

SDQ$T2_SDQ_EmotionalProb <- SDQ$SDQ_3 + SDQ$SDQ_8 + SDQ$SDQ_13 +
                            SDQ$SDQ_16 + SDQ$SDQ_24
SDQ$T2_SDQ_ConductProb <- as.numeric(SDQ$SDQ_5) + as.numeric(SDQ$SDQ_7) + as.numeric(SDQ$SDQ_12) +
                          as.numeric(SDQ$SDQ_18) + as.numeric(SDQ$SDQ_22)
SDQ$T2_SDQ_Hyperactivity <- as.numeric(SDQ$SDQ_2) + as.numeric(SDQ$SDQ_10) + as.numeric(SDQ$SDQ_15) +
                            as.numeric(SDQ$SDQ_21) + as.numeric(SDQ$SDQ_25)
SDQ$T2_SDQ_PeerProb <- as.numeric(SDQ$SDQ_6) + as.numeric(SDQ$SDQ_11) + as.numeric(SDQ$SDQ_14) +
                      as.numeric(SDQ$SDQ_19) + as.numeric(SDQ$SDQ_23)
SDQ$T2_SDQ_Prosocial <- as.numeric(SDQ$SDQ_1) + as.numeric(SDQ$SDQ_4) + as.numeric(SDQ$SDQ_9) +
                        as.numeric(SDQ$SDQ_17) + as.numeric(SDQ$SDQ_20)
write.csv(SDQ, "T2_SDQ.csv")

SDQ_Social <- SDQ[,c(1,4,9,14,17,25,7,12,15,20,24,2,5,10,18,21)]
SDQ_Emotional <- SDQ[,c(1,4,9,14,17,25)]
SDQ_Problem <- SDQ[,c(1,6,8,13,19,23,3,11,16,22,26)]
#### SSIS
SSIS <- dplyr::select(T2_Qualtrics_all, Q216, Q58_1_1:Q77_1_5)
SSISimp <- SSIS[, c(1,6:9,14:17,22:25,30:33,38:41,46:49,54:57,62:65,70:73,78:81,86:89,92,93)]
names(SSISimp) <- c("Child_ID", paste0("SSISimp_", 1:46))
SSISimp <- SSISimp[-1, ]
SSISimp[, c(2:47)] <- sapply(SSISimp[, c(2:47)], FUN = function(x){recode(x, "c" ="2", "i" ="1", "n" ="0")})
SSISimp[,c(2:47)] = apply(SSISimp[,c(2:47)], 2, function(x) as.numeric(as.character(x)))
str(SSISimp)
SSISimp <- transform(SSISimp, T2_SSIS_ParentImport = rowMeans(SSISimp[,-1], na.rm = TRUE))

SSISimp$T2_SSISimp_Communication <- rowMeans(SSISimp[,c(5,11,15,21,25,31,41)], na.rm = TRUE)
SSISimp$T2_SSISimp_Cooperation <- rowMeans(SSISimp[,c(3,8,13,18,28,38)], na.rm = TRUE)
SSISimp$T2_SSISimp_Assertion <- rowMeans(SSISimp[,c(2,6,12,16,26,36,46)], na.rm = TRUE)
SSISimp$T2_SSISimp_Responsibility <- rowMeans(SSISimp[,c(7,17,23,27,33,43)], na.rm = TRUE)
SSISimp$T2_SSISimp_Empathy <- rowMeans(SSISimp[,c(4,9,14,19,29,29)], na.rm = TRUE)
SSISimp$T2_SSISimp_Engagement <- rowMeans(SSISimp[,c(10,20,24,30,34,40,44)], na.rm = TRUE)
SSISimp$T2_SSISimp_SelfControl <- rowMeans(SSISimp[,c(22,32,35,37,42,45,47)], na.rm = TRUE)

hist(SSISimp$T2_SSISimp_Cooperation)
ParentImportant <- dplyr::select(SSISimp, Child_ID, T2_SSIS_ParentImport, T2_SSISimp_Communication, T2_SSISimp_Cooperation, T2_SSISimp_Assertion, T2_SSISimp_Responsibility,
                                 T2_SSISimp_Empathy, T2_SSISimp_Engagement, T2_SSISimp_SelfControl)
#drop columns about how important to parent
SSIS <- SSIS[, -c(6:9,14:17,22:25,30:33,38:41,46:49,54:57,62:65,70:73,78:81,86:89,92,93)]
names(SSIS) <- c("Child_ID", paste0("SSIS_", 1:79))
SSIS <- SSIS[-1, ]



SSIS[, c(2:80)] <- sapply(SSIS[, c(2:80)], FUN = function(x){dplyr::recode(x, "A" ="3", "O" ="2", "S" ="1", "N" ="0")})
SSIS[,c(2:80)] = apply(SSIS[,c(2:80)], 2, function(x) as.numeric(as.character(x)))
str(SSIS)

#should try and add feature that if there are >4 NAs in questions 1-46 can fill with O=2
#if there are >3 NAs in questions 47-79 can fill with S=1

#Social Skills Subscales
SSIS$T2_SSIS_Communication <- rowSums(SSIS[,c(5,11,15,21,25,31,41)])
SSIS$T2_SSIS_Cooperation <- rowSums(SSIS[,c(3,8,13,18,28,38)])
SSIS$T2_SSIS_Assertion <- rowSums(SSIS[,c(2,6,12,16,26,36,46)])
SSIS$T2_SSIS_Responsibility <- rowSums(SSIS[,c(7,17,23,27,33,43)])
SSIS$T2_SSIS_Empathy <- rowSums(SSIS[,c(4,9,14,19,29,29)])
SSIS$T2_SSIS_Engagement <- rowSums(SSIS[,c(10,20,24,30,34,40,44)])
SSIS$T2_SSIS_SelfControl <- rowSums(SSIS[,c(22,32,35,37,42,45,47)])
SSIS$T2_SSIS_SocialSkillsSum <- rowSums(SSIS[,c(2:47)])

SSIS_Social <- SSIS[,c(1,2:47)]
#Problem Behavior Subscales
SSIS$T2_SSIS_Externalizing <- rowSums(SSIS[,c(48,50,52,55,57,59,64,66,71,73,77,79)])
SSIS$T2_SSIS_Bullying <- rowSums(SSIS[,c(50,53,57,60,64)])
SSIS$T2_SSIS_Hyperactivity <- rowSums(SSIS[,c(48,52,54,55,59,61,68)])
SSIS$T2_SSIS_Internalizing <- rowSums(SSIS[,c(58,62,65,67,69,72,74,75,78,80)])
SSIS$T2_SSIS_ProblemBehavSum <- rowSums(SSIS[,c(48:80)])

SSIS_Problem <- SSIS[,c(1,48:80)]

#Autism Spectrum Subscale
SSIS$T2_SSIS_SSASD <- rowSums(SSIS[,c(11,20,21,30,31,39,40,41)])
SSIS$T2_SSIS_rSSASD <- 24-SSIS$T2_SSIS_SSASD
SSIS$T2_SSIS_PBASD <- rowSums(SSIS[,c(49,51,56,58,63,70,76)])
SSIS$T2_SSIS_ASD <- SSIS$T2_SSIS_rSSASD + SSIS$T2_SSIS_PBASD

write.csv(SSIS, "T2_SSIS.csv")


#### CBCL ####
CBCL_syn <- dplyr::select(T2_Qualtrics_all, Q216, Q218_1:Q241_3)
names(CBCL_syn) <- c("Child_ID", paste0("CBCL_", 1:118))
CBCL_syn <- CBCL_syn[-1, ]
CBCL_syn[,c(2:119)] <- apply(CBCL_syn[,c(2:119)], 2, function(x) as.numeric(as.character(x)))
str(CBCL_syn)

#calculate syndrome scales
CBCL_syn$T2_CBCL_AnxDep <- rowSums(CBCL_syn[,c(15,30:34,36,46,51,53,78,98,119)])
CBCL_syn$T2_CBCL_WithDep <- rowSums(CBCL_syn[,c(6,43,72,76,82,109,110,118)])
CBCL_syn$T2_CBCL_Somatic <- rowSums(CBCL_syn[,c(48,50,52,55,57:63)])
CBCL_syn$T2_CBCL_SocialProb <- rowSums(CBCL_syn[,c(12,13,26,28,35,37,39,49,69,71,86)])
CBCL_syn$T2_CBCL_ThoughtProb <- rowSums(CBCL_syn[,c(10,19,41,47,65,66,67,73,77,83,90,91,92,99,107)])
CBCL_syn$T2_CBCL_AttentionProb <- rowSums(CBCL_syn[,c(2,5,9,11,14,18,42,68,85,87)])
CBCL_syn$T2_CBCL_RuleBreak <- rowSums(CBCL_syn[,c(3,27,29,40,44,70,74,79,80,88,89,97,103,106,108,112,113)])
CBCL_syn$T2_CBCL_Aggressive <- rowSums(CBCL_syn[,c(4,17,20:24,38,64,75,93:96,101,102,104,111)])
CBCL_syn$T2_CBCL_OtherProb <- rowSums(CBCL_syn[,c(7,8,16,25,45,54,56,81,84,100,105,114:117)])

write.csv(CBCL_syn, "T2_CBCL_Syndrome.csv")

CBCL_Social <- CBCL_syn[,c(1,12,13,26,28,35,37,39,49,69,71,86)]
CBCL_Problem <- CBCL_syn[,c(1,15,30:34,36,46,51,53,78,98,119,6,43,72,76,82,109,110,118,48,50,52,55,57:63,10,19,41,47,
                            65,66,67,73,77,83,90,91,92,99,107,2,5,9,11,14,18,42,68,85,87,3,27,29,40,44,70,74,79,80,
                            88,89,97,103,106,108,112,113,4,17,20:24,38,64,75,93:96,101,102,104,111,
                            7,8,16,25,45,54,56,81,84,100,105,114:117)]
CBCL_Attention <- CBCL_syn[,c(2,5,9,11,14,18,42,68,85,87)]

#### Emotion Regulation Checklist ###
ERC <- dplyr::select(T2_Qualtrics_all, Q216, Q440_1:Q447_3)
names(ERC) <- c("Child_ID", paste0("ERC_", 1:24))
ERC <- ERC[-1, ]

ERC$ERC_9 <- dplyr::recode(ERC$ERC_9, "1"="4", "2"="3", "3"="2", "4"="1")
ERC$ERC_11 <- dplyr::recode(ERC$ERC_11, "1"="4", "2"="3", "3"="2", "4"="1")
ERC$ERC_16 <- dplyr::recode(ERC$ERC_16, "1"="4", "2"="3", "3"="2", "4"="1")
ERC$ERC_18 <- dplyr::recode(ERC$ERC_18, "1"="4", "2"="3", "3"="2", "4"="1")
ERC$ERC_19 <- dplyr::recode(ERC$ERC_19, "1"="4", "2"="3", "3"="2", "4"="1")

ERC[,c(2:25)] <- apply(ERC[,c(2:25)], 2, function(x) as.numeric(as.character(x)))

ERC$T2_ERC_LN <- rowSums(ERC[,c(3,7,9:15,18,21,23,25)])
ERC$T2_ERC_ER <- rowSums(ERC[,c(2,4:6,8,16,17,19,20,21)])
ERC$T2_ERC_Total <- ERC$T2_ERC_ER - ERC$T2_ERC_LN

write.csv(ERC, "T2_ERC.csv")

### SRS Questionnaire ###
## AWR subscale: 2,7,25,32,45,52,54,56
## Cog subscale: 5,10,15,17,30,40,42,44,48,58,59,62
## Com subscale: 12,13,16,18,19,21,22,26,33,35,36,37,38,41,46,47,51,53,55,57,60,61
## Mot subscale: 1,3,6,9,11,14,20,24,28,29,31,39,49,50,63

SRS <- dplyr::select(T2_Qualtrics_all, Q216, Q424_1:Q439_5)
names(SRS) <- c("Child_ID", paste0("SRS_", 1:65))
SRS <- SRS[-1, ]

SRS[,c(2:66)] <- apply(SRS[,c(2:66)], 2, function(x) as.numeric(as.character(x)))

SRS$T2_SRS_Awr_Raw <- rowSums(SRS[,c(3,8,26,33,46,53,55,57)])
SRS$T2_SRS_Cog_Raw <- rowSums(SRS[,c(6,11,16,18,31,41,43,45,49,59,60,63)])
SRS$T2_SRS_Com_Raw <- rowSums(SRS[,c(13,14,17,19,20,22,23,27,34,36,37,38,39,42,47,48,52,54,56,58,61,62)])
SRS$T2_SRS_Mot_Raw <- rowSums(SRS[,c(2,4,7,10,12,15,21,25,29,30,32,40,50,51,64)])
SRS$T2_SRS_Total_Raw <- rowSums(SRS[,c(67,68,69,70)])

write.csv(SRS, "T2_SRS.csv")



all_items_raw <- merge(SDQ, CBCL_syn, by="Child_ID", all=FALSE)
all_items_raw <- merge(all_items_raw, SSIS, by="Child_ID", all=FALSE)
#all_items_raw <- merge(all_items_raw, SRS, by="Child_ID", all=TRUE)
#all_items_raw <- merge(all_items_raw, ERC, by="Child_ID", all=TRUE)

all_social <- merge(SSIS_Social, SDQ_Social, by="Child_ID")
all_social <- merge(all_social, CBCL_Social, by="Child_ID")

all_prob <- merge(SSIS_Problem, SDQ_Problem, by="Child_ID")
all_prob <- merge(all_prob, CBCL_Problem, by="Child_ID")
