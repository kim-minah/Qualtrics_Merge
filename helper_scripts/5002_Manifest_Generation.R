#   ********************************************************           
#   Title: 5002_Manifest_Generation.R 
#   Author: Erin Barney
#   Date: 2017-11-16
#   Purpose: Script to generate manifest file data for uploading
#   to RexDB. Also appropriately renames trial/timepoint data and 
#   saves it into a single location.
#   Input: 
#      * 5001abcct/5001char_edf.csv
#      * 5001abcct/5001char_notes.csv
#      * 5001abcct/ndar.csv
#      * 5002abcct/ET ABCCT QC - Main Study.csv
#      * 5002abcct/ndar.csv
#      * 5002abcct*/summary_*_trial_rexdb.csv
#      * 5002abcct*/summary_*_timepoint_rexdb.csv
#   Output: 
#      * 5002abcct/data_for_rexdb/et-*-main-visit-manifest.*.csv
#      * 5002abcct/data_for_rexdb/et-*-main-trial-data-.*.csv
#      * 5002abcct/data_for_rexdb/et-*-main-visit-data-.*.csv
#      * 5001abcct/data_for_rexdb/et-*-feasibility-manifest.*.csv
#      * 5001abcct/data_for_rexdb/et-*-feasibility-trial-data-.*.csv
#      * 5001abcct/data_for_rexdb/et-*-feasibility-visit-data-.*.csv
#   ********************************************************

# derived_results_version = "M1.4" # comment this line out when not debugging, will get this variable from 5002_ALL_500Hz.R
UAT = 0
INTERIM = 0
derived_results_version = "M1.4"
# Generate manifest file based on rex data
# Generate trial and visit data files based on rex data
current_folder_main = getwd()

# read in characterization files
char = read.csv('5002abcct/ET ABCCT QC - Main Study.csv')
ndar = read.csv('5002abcct/ndar.csv')
char = subset(char, char$edf!=0 & char$edf!='')

ndar = subset(ndar, select=c(full_path,hash))
ndar$full_path = substr(ndar$full_path, 5, 12)
names(ndar) = c('edf','ndar_output_file_name')
ndar$ndar_output_file_name = paste0(ndar$ndar_output_file_name, '.txt')

char = subset(char, select=c(id,timepoint,day_within_timepoint,edf,etrunlog_et_date,coder_session_completion_notes))
names(char)[names(char) == 'etrunlog_et_date'] <- 'et_doe'

edfsdates = merge(char,ndar, by='edf', all.x=T, all.y=T)
edfsdates = reshape(edfsdates, timevar='day_within_timepoint', idvar=c('id','timepoint'),direction='wide')

# fix timepoint to have t in front
edfsdates$timepoint = paste0('t',edfsdates$timepoint)

#### create table of correct scripts to run ####
Rcodes <- matrix(c(
  # '/5001abcct_am_dynamic', '../5002abcct_am_dynamic/5002am_dynamic_merge.R', 'summary_amdynamic','fam',
  # '/5001abcct_bm','../5002abcct_bm/5002bm_merge.R', 'summary_bm','fbm',
  # '/5001abcct_si','../5002abcct_si/5002si_merge.R', 'summary_si','fsi',
  # '/5001abcct_ss','../5002abcct_ss/5002ss_merge.R', 'summary_ss','fss',
  # '/5001abcct_vs','../5002abcct_vs/5002vs_merge.R', 'summary_vs','fvs',
  # '/5001abcct_plr','../5002abcct_plr/5002plr_merge_with_regpipe.R', 'summary_plr','fplr',
  # '/5001abcct_ds','5001ds_merge.R', 'summary_ds','fds',
  # '/5001abcct_go','5001go_merge.R', 'summary_go','fgo',
  # '/5001abcct_sso','5001sso_merge.R', 'summary_sso','fsso',
  # 
  
  '/5002abcct_am_dynamic', '5002am_dynamic_merge.R', 'summary_amdynamic','mam',
  '/5002abcct_bm','5002bm_merge.R', 'summary_bm','mbm',
  '/5002abcct_si','5002si_merge.R', 'summary_si','msi',
  '/5002abcct_ss','5002ss_merge.R', 'summary_ss','mss',
  '/5002abcct_vs', '5002vs_merge.R', 'summary_vs','mvs',
  '/5002abcct_plr','5002plr_merge_with_regpipe.R', 'summary_plr','mplr'),
  
  ncol=4,byrow=TRUE)
colnames( Rcodes ) <- c("foldername","codename","outputname","task")




#### read in summary files for each task individually #####
# reset working directory to be the current folder
setwd( current_folder_main )
date = as.character(as.Date(Sys.Date()))
date = gsub('-','-',date)

# read in summary files (by participant and by edf)
for(i in 1:nrow(Rcodes) ){
  assign(paste0(Rcodes[i,"task"]),read.csv(paste0(current_folder_main,Rcodes[i,"foldername"],"/",Rcodes[i,"outputname"],"_timepoint_rexdb.csv")))
  # assign(paste0(Rcodes[i,"task"],"_edf"),read.csv(paste0(current_folder_main,Rcodes[i,"foldername"],"/",Rcodes[i,"outputname"],"_edf.csv")))
  assign(paste0(Rcodes[i,"task"],"_trial"),read.csv(paste0(current_folder_main,Rcodes[i,"foldername"],"/",Rcodes[i,"outputname"],"_trial_rexdb.csv")))
}

#### create heads composite variable ####
composite = merge(subset(mam, select=c(subject,visit,etam_heads_perct)), 
                  subset(msi, select=c(subject,visit,etsi_heads_perct)), 
                  by=c('subject','visit'), all.x=T, all.y=T)
composite = merge(composite,
                  subset(mss, select=c(subject,visit,etss_face_perct)),
                  by=c('subject','visit'), all.x=T, all.y=T)
# Fred 1/26/18: When any of the 3 inputs to the composite are NA, the whole thing should be NA. We could revisit this later as it's 
# possible that we can impute on the basis of data we have in those cases but for now it's safer to just discard.
composite$et_heads_amsiss = ((composite$etam_heads_perct + composite$etsi_heads_perct + composite$etss_face_perct) / 3)
composite = subset(composite, !is.na(composite$et_heads_amsiss))
# alternative method for leaving out the NAs and calculating a composite value regardless
# composite$et_heads_amsiss = rowMeans(cbind(composite$etam_heads_perct, composite$etsi_heads_perct, composite$etss_face_perct), na.rm=T)

# write.csv(composite, 'composite.csv', row.names=F)
nrow(mam)
mam = merge(mam, 
            subset(composite, select=c(subject, visit, et_heads_amsiss)),
            by=c('subject','visit'), all.x=T, all.y=T)
nrow(mam)

#### function ####
generate_manifest = function(visitdata, trialdata, version, exp, exp_short) {
  
  # rename and save data files for trial and visit
  rversion_timepoint = visitdata[ , grepl( "rversion" , names( visitdata ), ignore.case=T ) ][1]
  rversion_trial = trialdata[ , grepl( "rversion" , names( trialdata ), ignore.case=T ) ][1]
  exp_short = tolower(exp_short)
  if (UAT==1) {
    if (exp_short=='am') {
      trial_version = 5
      timepoint_version = 3
    } else if (exp_short=='bm') {
      trial_version = 5
      timepoint_version = 3
    } else if (exp_short=='plr') {
      trial_version = 4
      timepoint_version = 2
    } else if (exp_short=='si') {
      trial_version = 6
      timepoint_version = 4
    } else if (exp_short=='ss') {
      trial_version = 4
      timepoint_version = 2
    } else if (exp_short=='vs') {
      trial_version = 4
      timepoint_version = 2
    } else {
      trial_version = 0
      timepoint_version = 0
    }
  } else {
    if (exp_short=='am') {
      trial_version = 4
      timepoint_version = 3
    } else if (exp_short=='bm') {
      trial_version = 3
      timepoint_version = 2
    } else if (exp_short=='plr') {
      trial_version = 3
      timepoint_version = 2
    } else if (exp_short=='si') {
      trial_version = 5
      timepoint_version = 4
    } else if (exp_short=='ss') {
      trial_version = 3
      timepoint_version = 2
    } else if (exp_short=='vs') {
      trial_version = 3
      timepoint_version = 2
    } else {
      trial_version = 0
      timepoint_version = 0
    }
  }
  
  fname_timepoint = paste0('5002abcct/data_for_rexdb/et-',exp_short,'-main-visit-data.', timepoint_version, '.csv')
  fname_trial = paste0('5002abcct/data_for_rexdb/et-',exp_short,'-main-trial-data.', trial_version, '.csv')

  # Trial/Visit Data: only save interim participants in output if interim flag is on
  # 2018-02-06: only include T1 and T2 if interim flag is on
  if (INTERIM==1) {
    dcclist = read.csv('5002abcct/interim/input/InterimSubjectsWide_DCC.csv')
    
    # set up DCC list of interim analysis subjects
    dcclist <- reshape(dcclist,  # reshape the DCC's list to be long format
                       varying = c("T1", "T2","T3"), 
                       v.names = "Include",
                       timevar = "Timepoint", idvar=c('Code'),
                       direction = "long")
    dcclist$id_time = paste0(dcclist$Code,'_t',dcclist$Timepoint)
    dcclist$Include = toupper(dcclist$Include)
    dcclist = subset(dcclist, dcclist$Include=='Y')
    dcclist <- dcclist[order(dcclist$id_time),]
    
    dcclist$subject = dcclist$Code
    dcclist$visit = paste0('t',dcclist$Timepoint)
    dcclist=subset(dcclist, select=c(subject, visit))
    visitdata = merge(visitdata, dcclist, by=c('subject','visit'))
    trialdata = merge(trialdata, dcclist, by=c('subject','visit'))
    
    visitdata = subset(visitdata, visitdata$visit!='t3')
    visitdata$assessment_id = seq_along(visitdata$subject) # redo assessment_id
    trialdata = subset(trialdata, trialdata$visit!='t3')
    trialdata$assessment_id = seq_along(trialdata$subject) # redo assessment_id
  }
  
  write.csv(visitdata, fname_timepoint, row.names=F)
  write.csv(trialdata, fname_trial, row.names=F)
  
  
  
  # generate manifest files
  manifest = subset(visitdata, select=c(subject, visit))
  manifest = merge(edfsdates, manifest, by.y=c('subject','visit'), by.x=c('id','timepoint'), all.y=TRUE)
  # manifest = edfsdates
  
  # sort by id
  manifest <- manifest[order(manifest$id),] 
  
  # convert to character
  manifest$edf.1 = as.character(manifest$edf.1)
  manifest$edf.2 = as.character(manifest$edf.2)
  manifest$coder_session_completion_notes.1 = as.character(manifest$coder_session_completion_notes.1)
  manifest$coder_session_completion_notes.2 = as.character(manifest$coder_session_completion_notes.2)
  
  # missing data
  manifest$coder_session_completion_notes.1 = ifelse(is.na(manifest$coder_session_completion_notes.1), 'Missing data', manifest$coder_session_completion_notes.1)
  manifest$coder_session_completion_notes.2 = ifelse(is.na(manifest$coder_session_completion_notes.2), 'Missing data', manifest$coder_session_completion_notes.2)
  
  # add columns
  manifest$study = 'abcct-main'
  manifest$derived_results_version = derived_results_version
  manifest$assessment_id = seq_along(manifest$id)
  manifest$method = 'ET'
  manifest$experiment = exp
  manifest$pipeline_version = rversion_timepoint
  manifest$trial_pipeline_version = rversion_trial
  manifest$data_file_name = substring(fname_timepoint,26,1000)
  manifest$trial_data_file_name = substring(fname_trial,26,1000)
  manifest$analyst_initials = 'EB'
  manifest$date_dv_generated = date
  manifest$date = date
  manifest$raw_input_file_name_day1 = ifelse(is.na(manifest$edf.1),NA,paste0(manifest$edf.1,'.edf'))
  manifest$raw_input_file_name_day2 = ifelse(is.na(manifest$edf.2),NA,paste0(manifest$edf.2,'.edf'))
  manifest$other_output_file_name_day1 = ifelse(is.na(manifest$edf.1),NA,paste0(manifest$edf.1,'.mat'))
  manifest$other_output_file_name_day2 = ifelse(is.na(manifest$edf.2),NA,paste0(manifest$edf.2,'.mat'))

  manifest$notes = paste0('Day 1: ', manifest$coder_session_completion_notes.1, '; Day 2: ', manifest$coder_session_completion_notes.2)
  
  # rename columns
  names(manifest)[names(manifest) == 'ndar_output_file_name.1'] <- 'ndar_output_file_name_day1'
  names(manifest)[names(manifest) == 'ndar_output_file_name.2'] <- 'ndar_output_file_name_day2'
  names(manifest)[names(manifest) == 'id'] <- 'subject'
  names(manifest)[names(manifest) == 'timepoint'] <- 'visit'
  
  # set some columns to NA
  manifest$ndar_output_file_name_other = 'NA'
  manifest$raw_input_file_name_other = 'NA'
  manifest$other_output_file_name_other = 'NA'
  
  
  manifest = subset(manifest, select=c(subject,
                                       date,
                                       assessment_id,
                                       study,
                                       visit,
                                       derived_results_version,
                                       method,
                                       experiment,
                                       raw_input_file_name_day1,
                                       raw_input_file_name_day2,
                                       raw_input_file_name_other,
                                       ndar_output_file_name_day1,
                                       ndar_output_file_name_day2,
                                       ndar_output_file_name_other,
                                       other_output_file_name_day1,
                                       other_output_file_name_day2,
                                       other_output_file_name_other,
                                       pipeline_version,
                                       trial_pipeline_version,
                                       data_file_name,
                                       trial_data_file_name,
                                       analyst_initials,
                                       date_dv_generated,
                                       notes))
  manifest_fname = paste0('5002abcct/data_for_rexdb/et-',exp_short,'-main-visit-manifest.1.csv')
  
  # Manifests: only save interim participants in output if interim flag is on
  # 2018-02-06: only include T1 and T2 if interim flag is on
  if (INTERIM==1) {
    dcclist = read.csv('5002abcct/interim/input/InterimSubjectsWide_DCC.csv')
    
    # set up DCC list of interim analysis subjects
    dcclist <- reshape(dcclist,  # reshape the DCC's list to be long format
                       varying = c("T1", "T2","T3"), 
                       v.names = "Include",
                       timevar = "Timepoint", idvar=c('Code'),
                       direction = "long")
    dcclist$id_time = paste0(dcclist$Code,'_t',dcclist$Timepoint)
    dcclist$Include = toupper(dcclist$Include)
    dcclist = subset(dcclist, dcclist$Include=='Y')
    dcclist <- dcclist[order(dcclist$id_time),]
    
    dcclist$subject = dcclist$Code
    dcclist$visit = paste0('t',dcclist$Timepoint)
    dcclist=subset(dcclist, select=c(subject, visit))
    manifest = merge(manifest, dcclist, by=c('subject','visit'))
    
    manifest = subset(manifest, manifest$visit!='t3')
    manifest$assessment_id = seq_along(manifest$subject) # redo assessment_id
  }
  
  write.csv(manifest, manifest_fname, row.names=F)
  
}


#### run function ####
generate_manifest(mam, mam_trial, 5002, 'Activity Monitoring', 'am')
generate_manifest(mbm, mbm_trial, 5002, 'Biological Motion', 'bm')
generate_manifest(mplr, mplr_trial, 5002, 'Pupillary Light Reflex', 'plr')
generate_manifest(msi, msi_trial, 5002, 'Social Interactive', 'si')
generate_manifest(mss, mss_trial, 5002, 'Static Scenes', 'ss')
generate_manifest(mvs, mvs_trial, 5002, 'Visual Search', 'vs')













#### fill folder of scout data ####
# read in summary files (by participant and by edf)
for(i in 1:nrow(Rcodes) ){
  assign(paste0(Rcodes[i,"task"]),read.csv(paste0(current_folder_main,Rcodes[i,"foldername"],"/",Rcodes[i,"outputname"],"_timepoint.csv")))
  assign(paste0(Rcodes[i,"task"],"_edf"),read.csv(paste0(current_folder_main,Rcodes[i,"foldername"],"/",Rcodes[i,"outputname"],"_edf.csv")))
  assign(paste0(Rcodes[i,"task"],"_trial"),read.csv(paste0(current_folder_main,Rcodes[i,"foldername"],"/",Rcodes[i,"outputname"],"_trial.csv")))
}


move_scout_data = function(visitdata, edfdata, trialdata, version, exp, exp_short) {
  if (version==5001) {
    exp_short = tolower(exp_short)
    
    fname_timepoint = paste0('5001abcct/data_for_scouts/summary_',exp_short,'_timepoint.csv')
    fname_edf = paste0('5001abcct/data_for_scouts/summary_',exp_short,'_edf.csv')
    fname_trial = paste0('5001abcct/data_for_scouts/summary_',exp_short,'_trial.csv')
    
    write.csv(visitdata, fname_timepoint, row.names=F)
    write.csv(edfdata, fname_edf, row.names=F)
    write.csv(trialdata, fname_trial, row.names=F)
    
  } else if (version==5002) {
    exp_short = tolower(exp_short)
    
    fname_timepoint = paste0('5002abcct/data_for_scouts/summary_',exp_short,'_timepoint.csv')
    fname_edf = paste0('5002abcct/data_for_scouts/summary_',exp_short,'_edf.csv')
    fname_trial = paste0('5002abcct/data_for_scouts/summary_',exp_short,'_trial.csv')
    
    write.csv(visitdata, fname_timepoint, row.names=F)
    write.csv(edfdata, fname_edf, row.names=F)
    write.csv(trialdata, fname_trial, row.names=F)
  }
  
}


# run function
move_scout_data(mam, mam_edf, mam_trial, 5002, 'Activity Monitoring', 'amdynamic')
move_scout_data(mbm, mbm_edf, mbm_trial, 5002, 'Biological Motion', 'bm')
move_scout_data(mplr, mplr_edf, mplr_trial, 5002, 'Pupillary Light Reflex', 'plr')
move_scout_data(msi, msi_edf, msi_trial, 5002, 'Social Interactive', 'si')
move_scout_data(mss, mss_edf, mss_trial, 5002, 'Static Scenes', 'ss')
move_scout_data(mvs, mvs_edf, mvs_trial, 5002, 'Visual Search', 'vs')

