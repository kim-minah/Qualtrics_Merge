################################################################################
#-------------------------------------------------------------------------------
# Program: ChidPsych_Qualtrics_Merge_v3.R 
# Author: Minah Kim
# Date: Spring 2021
# Purpose: Merge Emotional Development Questionnaire data from Child Psych study

# Updates from v2: 
# -> Questionnaire names included in colnames via ChildPsych_QualtricsColNames_v2.R 
# -> Pilot data included

# Input: 
# -> Exported Qualtrics outputs pasted into 'input' directory
#   (two files that will always be there: pilot data, pilot data - 5 yrs)
#   (four files that need to be updated: main study 4-5 yrs, 6 yrs, 7 yrs & 8-12 yrs)

# Output: 
# -> Childpsych_Merged_Qualtrics_M_DD_YYYY.csv
# -> Qualtrics outputs get stored in archived_qualtrics_exports directory
# -> If there were previously merged spreadsheets they become stored in    
#    archived_merged_qualtrics directory
# -----------------------------------------------------------------------------
################################################################################

#---------------#
# Load packages #
#---------------#
library(psych)
library(stringi)
library(filesstrings) # to use file.move()

#-----------------#
# Set working dir #
#-----------------#
# Minah's local machine (comment if running from someplace else)
setwd("~/GitHub/Qualtrics_Merge/input/")


#--------------#
# Read in data #
#--------------#
# Read in each csv and make them elements of a list
qualtrics <- sort(Sys.glob('Emotional*.csv'), decreasing = FALSE)
myDatasets <- lapply(qualtrics, function(i){read.csv(i,
                                                     check.names=FALSE,
                                                     as.is=TRUE)})
# lapply(myDatasets,describe)
# lapply(myDatasets,str)


# Optional (if you want to name the list elements)
names(myDatasets) <- paste0("EmDev_",c("4-5","6","7","8-12","pilot","pilot_5"))
#----------------------------#
# Col names for each dataset #
#----------------------------#
source("ChildPsych_QualtricsColNames_v2.R")
# Outputs a list titled colnames_all

#-----------------------#
# Delete Rows & Columns #
#-----------------------#
# Delete unnecessary row: 
# -> row 2 has imported IDs
# Delete unnecessary columns: 
# -> Status, IPAddress, Progress, Finished, ResponseId, RecipientLastName,   
#   RecipientFirstName, RecipientEmail, LocationLatitude, LocationLongitude, 
#   DistributionChannel, UserLanguage 
for (i in 1:length(myDatasets)){
  myDatasets[[i]] <- myDatasets[[i]][-2,]
  myDatasets[[i]] <- myDatasets[[i]][,-c(3:5,7,9:12,14:17)]
}
# Remove first row for 2nd ~ last element in list
# -> Row 1 has the actual questionnaire questions. Will only retain this row for the first element (4-5 yr old data) for the merged dataset. However the 4-5 yr old data has CBCL_Preschool instead of CBCL and so CBCL questions were extracted in ChildPsych_QualtricsColNames_v2.R (see vector 'cbcl_questions') and added back in later. Same with GEM & ICUP since they were only offered during pilot and CBCL Preschool during pilot contained more questions so those questions were also extracted.
for (i in 2:length(myDatasets)){
  myDatasets[[i]] <- myDatasets[[i]][-1,]
}

# First five columns for all datasets: StartDate, EndDate, Duration (in seconds),
# RecordedDate, ExternalReference
for (i in 1:length(myDatasets)){
  print(colnames(myDatasets[[i]])[1:5])
  }

# Compare colname list and dataset dimensions
str(colnames_all)
# 4&5 ys: 593; 6 yrs: 672; 7 yrs: 600; 8-12 yrs: 582; pilot: 580; pilot_5: 437
lapply(myDatasets,dim) 
# 4&5 ys: 593; 6 yrs: 673; 7 yrs: 600; 8-12 yrs: 582; pilot: 583; pilot_5: 438
# Basically it is off by one for the Qualtrics output for 6 yr olds because
# of a variable at the very end called "Q93#2_1_TEXT - Topics" which doesn't exist in any of the other Qualtrics outputs and has blank values for all participants.
# I think it was at one point tied to CBCL's Q93#2_1_TEXT variable but now that 
# that option doesn't exist in Qualtrics anymore it's being appended to the end
# as like a random stray. Will remove.
# Similarly the last three columns need to be removed from the pilot data (Score,	Child ID,	Q204_2_TEXT - Topics)
# For the Pilot_5yr data, column 287 in CBCL Preschool has to be removed cause it's a duplicate question 
myDatasets[[2]][,673] <- NULL
myDatasets[[5]][,c(581:583)] <- NULL
myDatasets[[6]][,287] <- NULL
lapply(myDatasets,dim) 
# 4&5 ys: 593; 6 yrs: 672; 7 yrs: 600; 8-12 yrs: 582; pilot: 580; pilot_5: 437

#-----------------#
# Assign Colnames #
#-----------------#
for (i in 1:length(myDatasets)){
  colnames(myDatasets[[i]]) <- colnames_all[[i]]
}

# Double check 
# lapply(myDatasets,colnames)
# View(myDatasets[[2]][1:3,1:10])

#------------------------#
# Create Merged Colnames #
#------------------------#
# Create merged column names with union function
colmerged1 <- union(colnames(myDatasets[[1]]),colnames(myDatasets[[2]]))
colmerged2 <- union(colmerged1,colnames(myDatasets[[3]]))
colmerged3 <- union(colmerged2,colnames(myDatasets[[4]]))
colmerged3 <- union(colmerged3,colnames(myDatasets[[5]]))
colmerged_final <- union(colmerged3, colnames(myDatasets[[6]]))

# Reorder so that CBCL_Preschool from Main Study is combined with CBCL_Preschool
# from pilot
CBCL_PRESCHOOL <- colmerged_final[215:318]
colmerged_final <- colmerged_final[-c(215:318)]
colmerged_final <- c(colmerged_final[1:720], CBCL_PRESCHOOL,colmerged_final[721:745])
# Reorder so that CBCL is next to CBCL Preschool
CBCL <- colmerged_final[490:672]
colmerged_final <- colmerged_final[-c(490:672)]
colmerged_final <- c(colmerged_final[1:537], CBCL, colmerged_final[538:666])
# Change order of ICUP_Q7
ICUP_Q7 <- colmerged_final[537]
colmerged_final <- colmerged_final[-537]
colmerged_final <- c(colmerged_final[1:519],ICUP_Q7,colmerged_final[520:848])

#---------------------------------------#
# Add Merged Colnames to List Elements  #
#---------------------------------------#
# Add colmerged_final to each list element 
# setdiff function output -> values that are in the first vector but not second vector 
# 'dat$newvar <- x' is the same as 'dat[,newvar] <- x' or 'dat[newvar] <- x'
for (i in 1:length(myDatasets)){
  myDatasets[[i]][,setdiff(colmerged_final,colnames(myDatasets[[i]]))] <- ""
}
lapply(myDatasets,colnames)
# Double-check
# View(myDatasets[[1]][1:3,593:length(myDatasets[[1]])])

# Double check that all the col names are there
# Compare the colnames of each list element with colmerged_final. If there's a mismatch there will be a FALSE output. Sum them to see how mismatches there are. 
lapply(myDatasets,function(x){sum(sort(colnames(x))!=sort(colmerged_final))})
# zero mismatches

#------------------#
# Merge List Items #
#------------------#
# Make sure all the columns are in the same order
for (i in 1:length(myDatasets)){
  myDatasets[[i]] <- myDatasets[[i]][,colmerged_final] # re-orders the columns for each list element so that it is in the same order as colmerged_final vector
}

# Append timepoint info into the pilot data. All pilot participants are timepoint 2
myDatasets[[5]]$ExternalReference <- stri_replace_all_regex(myDatasets[[5]]$ExternalReference,"(\\w{7})","$1_2")
myDatasets[[6]]$ExternalReference <- stri_replace_all_regex(myDatasets[[6]]$ExternalReference,"(\\w{7})","$1_2")
myDatasets[[5]]$DEMOS_Q1 <- stri_replace_all_regex(myDatasets[[5]]$DEMOS_Q1,"(\\w{7})","$1_2")
myDatasets[[6]]$DEMOS_Q1 <- stri_replace_all_regex(myDatasets[[6]]$DEMOS_Q1,"(\\w{7})","$1_2")

# Delete test data
myDatasets[[2]] <- myDatasets[[2]][-7,] # Ali's test
myDatasets[[5]] <- myDatasets[[5]][-c(1,12,13,14),] # Jess's tests

# Merge list items together
qualtrics_merged <- do.call(rbind,myDatasets)
# Note: all the columns became type chr, but when you read in the data again, R should be able to recognize non string as non string (e.g. external reference ID read in as integer)

# Add in CBCL questions in row 1 since it currently has blank values)
qualtrics_merged[1,538:720] <- cbcl_questions

# Similarly, add in GEM & ICUP questions in row 1 
qualtrics_merged[1,490:513] <- gem_questions
qualtrics_merged[1,514:537] <- icup_questions

# Add in the additional CBCL Preschool questions from pilot
qualtrics_merged[1,825:849] <- cbcl_preschool_pilot_questions

#-------------------------------------#
# Write Outputs & Archive Old Exports #
#-------------------------------------#
# Re-set working dir
setwd("~/GitHub/Qualtrics_Merge")

# Make directories if they don't exist
if (!dir.exists("archived_merged_qualtrics")){
  dir.create("archived_merged_qualtrics",showWarnings = FALSE)
}
if (!dir.exists("archived_qualtrics_exports")){
  dir.create("archived_qualtrics_exports",showWarnings = FALSE)
}

# Move previously merged qualtrics output into archive folder (if exists)
previousExists <- function(x){
  tryCatch(
    if (file.exists(temp)){
      file.move(paste0(getwd(),"/",temp),"archived_merged_qualtrics",overwrite = TRUE)},
    warning=function(w){
          },
    error=function(e){  
      cat("Previously merged dataset does not exist in folder so nothing to archive!")
    }  
  )
}
temp <- list.files(pattern="Childpsych_Merged_Qualtrics*")
previousExists(temp)


# Move qualtrics exports into archive folder
lapply(qualtrics[1:4], function(i){file.move(paste0(getwd(),"/input/",i),"archived_qualtrics_exports",overwrite = TRUE)})


# Get Qualtrics download date from Qualtrics output file name
temp2 <- qualtrics[1]
temp2 <- stri_extract_all_regex(temp2,"([A-Z]\\w+)\\+(\\d+),\\+(\\d+)")
temp2 <- stri_replace_all_regex(temp2,"([A-Z]\\w+)\\+(\\d+),\\+(\\d+)","$1_$2_$3")

# Write csv
write.csv(qualtrics_merged, paste0('Childpsych_Merged_Qualtrics_',temp2,".csv"), row.names=F)
#namedate = paste0("archive/childpsych_qualtrics_merged_", format(Sys.Date(), "%Y%m%d"), ".csv")
#write.csv(qualtrics_merged, namedate, row.names=F)

#-----------------#
# Quality Control #
#-----------------#
# Compare External Reference column with DEMOS_Q1 column
QC <- qualtrics_merged[2:dim(qualtrics_merged)[1],c("ExternalReference","DEMOS_Q1")]
rownames(QC) <- NULL
QC$ER_ID <- unlist(stri_extract_all_regex(QC$ExternalReference,"\\w{7}"))
QC$D_ID <- unlist(stri_extract_all_regex(QC$DEMOS_Q1,"\\w{7}"))
ID_check <- ifelse(QC$ExternalReference!=""|QC$DEMOS_Q1!="",QC$ER_ID==QC$D_ID,"blank")
which(ID_check=="FALSE") # 42  98 114 138 149 182 238

### Changes made in Qualtrics on 6/18/21 ###
# DEMOS_Q1 C1766902_2 -> C176902_2
# External Reference C170362 ->  C170361_3 (had brother's ID entered as external ref)
# DEMOS_Q1 C173970_3 -> C172970_3 & ExternalReference C172970_T3 -> C172970_3
# External Reference C170342_4 -> C170362_4 
# DEMOS_Q1 C161697_3 -> C171697_3
# DEMOS_Q1 C17250_4 -> C172590_4
# DEMOS_Q1 c106915 -> C106915

which(ID_check=="blank")
