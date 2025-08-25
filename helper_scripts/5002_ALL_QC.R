#   ********************************************************           
#   Title: 5002_ALL_QC.R 
#   Author: Erin Barney
#   Date: 2017-08-01
#   Purpose: Script to run all 5002 merging scripts at once, then pulls together
#   the results into aggregated files both by EDF and by participant (gives 
#   information about DVs, valid trials/paradigm, and looking percentages).
#   Just to be used for updating ET QC spreadsheet regularly.
#   Also runs the following scripts:
#     * Valid_DVs.R
#     * Visit_Window_Tracking.R
#
#   Update: 2017-08-01
#   Update Purpose: Only use for 500Hz files (does the full dynamic analysis instead of just static)
#
#   Input: 
#      * 5002abcct/ET ABCCT QC - Main Study.csv
#      * 5002abcct/5002_invalid_paradigms.csv (from ET QC spreadsheet)
#      * aois_analyze_all_out files for each 5002 analysis
#   Output: 
#      * 5002abcct/erin_qc_dv_summaries/summary_ET_QC.csv (this file gets used in ABCCT ET QC)
#      * 5002abcct*/summary_*_trial.csv
#      * 5002abcct*/summary_*_timepoint.csv
#      * 5002abcct*/summary_*_edf.csv
#      * 5002abcct*/summary_*_trial_rexdb.csv
#      * 5002abcct*/summary_*_timepoint_rexdb.csv
#   ********************************************************

derived_results_version = "M1.4_60hz"
current_folder_analyses = getwd()


#### run PLR_Merge_Recent.R script ####
if (grepl('Erin', current_folder_analyses)) {
  print('Running PLR_Merge_Recent.R on server to merge together PLR files before doing other pipeline R scripts.')
  setwd( paste0('C:/Users/Erin.TIL-BUTTON/Documents/Projects - Erin/U19 - Erin/abcctetserver copy/MainStudy/Pipeline/manual_plr_results'))
  suppressWarnings(source( 'PLR_Merge_Recent.R' ))
  print('Finished runnning PLR_Merge_Recent.R, will return to previous directory now.')
  setwd( paste0(current_folder_analyses) )
  
}else { 
  print('Not in Erins working directory. You will need to run PLR_Merge_Recent.R manually (ABC-CT ET server/MainStudy/Pipeline/manual_plr_results/).')
}


#### create 5002 characterization file ####
qc_table=read.csv('5002abcct/ET ABCCT QC - Main Study.csv', as.is=TRUE)
qc_table = subset(qc_table, qc_table$edf!=0 & qc_table$edf!='')
char_5002_datesedfs = subset(qc_table, select=c(id,timepoint,day_within_timepoint,edf,etrunlog_et_date))
names(char_5002_datesedfs)[names(char_5002_datesedfs) == 'etrunlog_et_date'] <- 'et_doe'

# this is just for getting the correct EDF/date by day lists for the timepoint summary
char_5002_datesedfs = reshape(char_5002_datesedfs, timevar='day_within_timepoint', idvar=c('id','timepoint'),direction='wide')
write.csv(char_5002_datesedfs,'5002abcct/char_5002_timepoint_short.csv',row.names=FALSE)
####

#### create table of correct scripts to run ####
Rcodes <- matrix(c('/5002abcct', '5002_merge.R', 'abcct_merge(5002)', 'summary_abcct','abcct',
                   '/5002abcct_am_static', '5002am_static_merge.R','abcct_am_static_merge(5002)',  'summary_amstatic','am',
                   # '/5002abcct_am_dynamic', '5002am_dynamic_merge.R','abcct_amdynamic_merge(5002)',  'summary_amdynamic','am',
                   '/5002abcct_bm','5002bm_merge.R','abcct_bm_merge(5002)',  'summary_bm','bm',
                   '/5002abcct_si','5002si_merge.R','abcct_si_merge(5002)',  'summary_si','si',
                   '/5002abcct_ss','5002ss_merge.R','abcct_ss_merge(5002)',  'summary_ss','ss',
                   '/5002abcct_vs', '5002vs_merge.R','abcct_vs_merge(5002)',  'summary_vs','vs',
                   '/5002abcct_plr','5002plr_merge_with_regpipe.R','abcct_plr_merge(5002)',  'summary_plr','plr'
),
                 ncol=5,byrow=TRUE)
colnames( Rcodes ) <- c("foldername","codename","function","outputname","task")

# minimum trials (if below this, not valid)
am_min=4
bm_min=10
plr_min=5
si_min=6
ss_min=3
vs_min=3








##### run merge scripts for each task individually ######
for(i in 1:nrow(Rcodes) ){
  setwd( paste0(current_folder_analyses,Rcodes[i,"foldername"]) )
  source( Rcodes[i, "codename"] )
  output = NULL
  rst = NULL
  p = NULL
  pval = NULL
}






#### read in summary files for each task individually #####
# reset working directory to be the current folder
setwd( current_folder_analyses )

# read in summary files (by timepoint, edf, and trial)
for(i in 1:nrow(Rcodes) ){
  assign(paste0(Rcodes[i,"task"]),read.csv(paste0(current_folder_analyses,Rcodes[i,"foldername"],"/",Rcodes[i,"outputname"],"_timepoint.csv")))
  assign(paste0(Rcodes[i,"task"],"_edf"),read.csv(paste0(current_folder_analyses,Rcodes[i,"foldername"],"/",Rcodes[i,"outputname"],"_edf.csv")))
  assign(paste0(Rcodes[i,"task"],"_trial"),read.csv(paste0(current_folder_analyses,Rcodes[i,"foldername"],"/",Rcodes[i,"outputname"],"_trial.csv")))
}

#### valid_trials_by_edf table #####
# make table of valid trials for all tasks by EDF
valid_trials_by_edf = am_edf[ , c('edf1','etam_trials_with_valid_overall')]
valid_trials_by_edf = merge(x = valid_trials_by_edf, y = bm_edf[ , c('edf1', 'etbm_trials_with_valid_overall')], by = 'edf1', all.x=T, all.y=T)
valid_trials_by_edf = merge(x = valid_trials_by_edf, y = plr_edf[ , c('edf1', 'etplr_trials_with_valid_overall')], by = 'edf1', all.x=T, all.y=T)
valid_trials_by_edf = merge(x = valid_trials_by_edf, y = si_edf[ , c('edf1', 'etsi_trials_with_valid_overall')], by = 'edf1', all.x=T, all.y=T)
valid_trials_by_edf = merge(x = valid_trials_by_edf, y = ss_edf[ , c('edf1', 'etss_trials_with_valid_overall')], by = 'edf1', all.x=T, all.y=T)
valid_trials_by_edf = merge(x = valid_trials_by_edf, y = vs_edf[ , c('edf1', 'etvs_trials_with_valid_overall')], by = 'edf1', all.x=T, all.y=T)
names(valid_trials_by_edf) <- c('edf', 
                                'am_valid_trials', 
                                'bm_valid_trials', 
                                'plr_valid_trials',
                                'si_valid_trials', 
                                'ss_valid_trials', 
                                'vs_valid_trials')

# if valid trials are NA, change them to 0
valid_trials_by_edf$am_valid_trials = ifelse(is.na(valid_trials_by_edf$am_valid_trials),0,valid_trials_by_edf$am_valid_trials)
valid_trials_by_edf$bm_valid_trials = ifelse(is.na(valid_trials_by_edf$bm_valid_trials),0,valid_trials_by_edf$bm_valid_trials)
# valid_trials_by_edf$plr_valid_trials = ifelse(is.na(valid_trials_by_edf$plr_valid_trials),'TBD',valid_trials_by_edf$plr_valid_trials)
# valid_trials_by_edf$plr_valid_trials = ifelse(is.na(valid_trials_by_edf$plr_valid_trials),0,valid_trials_by_edf$plr_valid_trials)
valid_trials_by_edf$plr_valid_trials = valid_trials_by_edf$plr_valid_trials
valid_trials_by_edf$si_valid_trials = ifelse(is.na(valid_trials_by_edf$si_valid_trials),0,valid_trials_by_edf$si_valid_trials)
valid_trials_by_edf$ss_valid_trials = ifelse(is.na(valid_trials_by_edf$ss_valid_trials),0,valid_trials_by_edf$ss_valid_trials)
valid_trials_by_edf$vs_valid_trials = ifelse(is.na(valid_trials_by_edf$vs_valid_trials),0,valid_trials_by_edf$vs_valid_trials)
valid_trials_by_edf_org <- valid_trials_by_edf
valid_trials_by_edf = merge(abcct_edf,valid_trials_by_edf,by.x='edf1',by.y='edf')
# write.csv(valid_trials_by_edf, paste0(current_folder_analyses, '/5002abcct/summary_valid_trials_by_edf.csv'), quote = FALSE, row.names = FALSE)

#### valid_trials_by_participant table #####
# make table of valid trials for all tasks by participant/timepoint
valid_trials_by_participant = am[ , c('subject','visit','etam_trials_with_valid_overall')]
valid_trials_by_participant = merge(x = valid_trials_by_participant, y = bm[ , c('subject', 'visit', 'etbm_trials_with_valid_overall')], by = c('subject', 'visit'),all.x=T, all.y=T)
valid_trials_by_participant = merge(x = valid_trials_by_participant, y = plr[ , c('subject', 'visit', 'etplr_trials_with_valid_overall')], by = c('subject', 'visit'),all.x=T, all.y=T)
valid_trials_by_participant = merge(x = valid_trials_by_participant, y = si[ , c('subject', 'visit', 'etsi_trials_with_valid_overall')], by = c('subject', 'visit'),all.x=T, all.y=T)
valid_trials_by_participant = suppressWarnings(merge(x = valid_trials_by_participant, y = ss[ , c('subject', 'visit', 'etss_trials_with_valid_overall')], by = c('subject', 'visit'),all.x=T, all.y=T))
valid_trials_by_participant = suppressWarnings(merge(x = valid_trials_by_participant, y = vs[ , c('subject', 'visit', 'etvs_trials_with_valid_overall')], by = c('subject', 'visit'),all.x=T, all.y=T))
names(valid_trials_by_participant) <- c('subject', 
                                        'visit',
                                        'am_valid_trials', 
                                        'bm_valid_trials', 
                                        'plr_valid_trials',
                                        'si_valid_trials', 
                                        'ss_valid_trials', 
                                        'vs_valid_trials')

# calculate if a participant is valid based on number of trials
ids = unique(valid_trials_by_participant$subject)
valid_timepoint = 0
output = c()
for(name in ids){ 
  tps = unique( valid_trials_by_participant[valid_trials_by_participant$subject == name, 'visit'])
  for( tp in tps ){
    p = valid_trials_by_participant[valid_trials_by_participant$subject == name & valid_trials_by_participant$visit == tp, ]

    am_valid=ifelse(p$am_valid_trials<am_min,0,1)
    bm_valid=ifelse(p$bm_valid_trials<bm_min,0,1)
    plr_valid=ifelse((p$plr_valid_trials<plr_min | is.na(p$plr_valid_trials)),0,1)
    plr_valid=ifelse(is.na(p$plr_valid),0,plr_valid)
    si_valid=ifelse(p$si_valid_trials<si_min,0,1)
    ss_valid=ifelse(p$ss_valid_trials<ss_min,0,1)
    vs_valid=ifelse(p$vs_valid_trials<vs_min,0,1)
    valid_timepoint=ifelse((sum(am_valid,bm_valid,plr_valid,si_valid,ss_valid,vs_valid, na.rm=TRUE) < 1),0,1)
    valid_timepoint_paradigms=sum(am_valid,bm_valid,plr_valid,si_valid,ss_valid,vs_valid, na.rm=TRUE)
    rst = c( as.character(p$subject[1]), 
             as.character(p$visit[1]),
             as.character(p$am_valid_trials[1]),
             as.character(p$bm_valid_trials[1]),
             as.character(p$plr_valid_trials[1]),
             as.character(p$si_valid_trials[1]),
             as.character(p$ss_valid_trials[1]),
             as.character(p$vs_valid_trials[1]),
             valid_timepoint_paradigms,
             valid_timepoint
    )
    output = rbind( output, rst )
  }
}
colnames(output) = c('subject',
                     'visit',
                     'am_valid_trials', 
                     'bm_valid_trials', 
                     'plr_valid_trials',
                     'si_valid_trials', 
                     'ss_valid_trials', 
                     'vs_valid_trials',
                     'valid_timepoint_paradigms',
                     'valid_timepoint')
valid_trials_by_participant=output
# write.csv(valid_trials_by_participant, paste0(current_folder_analyses,'/5002abcct/summary_valid_trials_by_participant.csv'), quote = FALSE, row.names = FALSE)


#### summary_ET_QC table #####
# make table of valid onscreen looking percent for all tasks by EDF
summary_ET_QC <- merge(x = valid_trials_by_edf_org,
                       y = am_edf[ , c('edf1','etam_looking_perct_alltrials','etam_looking_perct_mean_ifallbutlookingvalid')],
                       by.x = 'edf', by.y = 'edf1',all.x=T, all.y=T)
summary_ET_QC = merge(x = summary_ET_QC,
                      y = bm_edf[ , c('edf1','etbm_looking_perct_alltrials','etbm_looking_perct_mean_ifallbutlookingvalid')], 
                      by.x = 'edf', by.y = 'edf1',all.x=T, all.y=T)
summary_ET_QC = merge(x = summary_ET_QC, 
                      y = plr_edf[ , c('edf1','etplr_looking_perct_alltrials','etplr_looking_perct_mean_ifallbutlookingvalid')], 
                      by.x = 'edf', by.y = 'edf1',all.x=T, all.y=T)
summary_ET_QC = suppressWarnings(merge(x = summary_ET_QC, 
                                       y = si_edf[ , c('edf1','etsi_looking_perct_alltrials','etsi_looking_perct_mean_ifallbutlookingvalid')], 
                                       by.x = 'edf', by.y = 'edf1',all.x=T, all.y=T))
summary_ET_QC = suppressWarnings(merge(x = summary_ET_QC, 
                                       y = ss_edf[ , c('edf1','etss_looking_perct_alltrials','etss_looking_perct_mean_ifallbutlookingvalid')], 
                                       by.x = 'edf', by.y = 'edf1',all.x=T, all.y=T))
summary_ET_QC = suppressWarnings(merge(x = summary_ET_QC, 
                                       y = vs_edf[ , c('edf1','etvs_looking_perct_alltrials','etvs_looking_perct_mean_ifallbutlookingvalid')], 
                                       by.x = 'edf', by.y = 'edf1',all.x=T, all.y=T))
names(summary_ET_QC) <- c('edf', 
                          'am_valid_trials', 
                          'bm_valid_trials', 
                          'plr_valid_trials',
                          'si_valid_trials', 
                          'ss_valid_trials', 
                          'vs_valid_trials',
                          'am_valid_looking_perct_alltrials', 
                          'am_valid_looking_perct_mean_ifallbutlookingvalid', 
                          'bm_valid_looking_perct_alltrials', 
                          'bm_valid_looking_perct_mean_ifallbutlookingvalid', 
                          'plr_valid_looking_perct_alltrials',
                          'plr_valid_looking_perct_mean_ifallbutlookingvalid',
                          'si_valid_looking_perct_alltrials', 
                          'si_valid_looking_perct_mean_ifallbutlookingvalid',
                          'ss_valid_looking_perct_alltrials', 
                          'ss_valid_looking_perct_mean_ifallbutlookingvalid', 
                          'vs_valid_looking_perct_alltrials',
                          'vs_valid_looking_perct_mean_ifallbutlookingvalid')

# reorder columns
summary_ET_QC = summary_ET_QC[,c('edf',
                                 'am_valid_trials',
                                 'am_valid_looking_perct_alltrials',
                                 'am_valid_looking_perct_mean_ifallbutlookingvalid',
                                 'bm_valid_trials',
                                 'bm_valid_looking_perct_alltrials',
                                 'bm_valid_looking_perct_mean_ifallbutlookingvalid',
                                 'plr_valid_trials',
                                 'plr_valid_looking_perct_alltrials',
                                 'plr_valid_looking_perct_mean_ifallbutlookingvalid',
                                 'si_valid_trials',
                                 'si_valid_looking_perct_alltrials',
                                 'si_valid_looking_perct_mean_ifallbutlookingvalid',
                                 'ss_valid_trials',
                                 'ss_valid_looking_perct_alltrials',
                                 'ss_valid_looking_perct_mean_ifallbutlookingvalid',
                                 'vs_valid_trials',
                                 'vs_valid_looking_perct_alltrials',
                                 'vs_valid_looking_perct_mean_ifallbutlookingvalid')]

# if plr_valid_trials is NA, change all PLR values to "TBD" = to be determined
summary_ET_QC$plr_valid_looking_perct_mean_ifallbutlookingvalid=ifelse(is.na(summary_ET_QC$plr_valid_trials),
                                                           'TBD',summary_ET_QC$plr_valid_looking_perct_mean_ifallbutlookingvalid)
summary_ET_QC$plr_valid_looking_perct_alltrials=ifelse(is.na(summary_ET_QC$plr_valid_trials),
                                                         'TBD',summary_ET_QC$plr_valid_looking_perct_alltrials)
summary_ET_QC$plr_valid_trials=ifelse(is.na(summary_ET_QC$plr_valid_trials),
                                      'TBD',summary_ET_QC$plr_valid_trials)

# merge abcct_edf with summary_ET_QC
summary_ET_QC = merge(abcct_edf,summary_ET_QC,by.x='edf1',by.y='edf', all.x=T, all.y=T)

# rename some columns
names(summary_ET_QC)[names(summary_ET_QC)=='edf1'] <- 'edf'
names(summary_ET_QC)[names(summary_ET_QC)=='interview_date_1'] <- 'interview_date'

# delete some unnecessary columns
summary_ET_QC$interview_date_2 = summary_ET_QC$edf2 = NULL

# merge summary_ET_QC with valid timepoint info
valid_timepoint_table=valid_trials_by_participant[ , c('subject','visit','valid_timepoint_paradigms','valid_timepoint')]
valid_timepoint_table=suppressWarnings(data.frame(valid_timepoint_table))
summary_ET_QC = merge(summary_ET_QC,valid_timepoint_table,by=c('subject','visit'))

# reorder columns in summary_ET_QC
summary_ET_QC = subset(summary_ET_QC, select=c('edf', 'subject', 'visit', 'site', 'interview_date', 'dx', 'overall_static_nn_mean_alltrials', 'overall_static_raw_mean_alltrials', 
                                               'overall_nn_raw_mean_alltrials', 'overall_nn_lin_mean_alltrials', 'overall_min_cal_error_mean_alltrials', 
                                               'overall_looking_perct_alltrials', 'overall_static_nn_mean_validtrials', 'overall_static_raw_mean_validtrials', 
                                               'overall_nn_raw_mean_validtrials', 'overall_nn_lin_mean_validtrials', 'overall_min_cal_error_mean_validtrials', 
                                               'overall_looking_perct_mean_ifallbutlookingvalid', 
                                               'overall_rversion', 'overall_order', 'session', 'am_valid_trials', 'am_valid_looking_perct_alltrials', 
                                               'am_valid_looking_perct_mean_ifallbutlookingvalid', 'bm_valid_trials', 'bm_valid_looking_perct_alltrials', 
                                               'bm_valid_looking_perct_mean_ifallbutlookingvalid', 'plr_valid_trials', 'plr_valid_looking_perct_alltrials', 
                                               'plr_valid_looking_perct_mean_ifallbutlookingvalid', 'si_valid_trials', 'si_valid_looking_perct_alltrials', 
                                               'si_valid_looking_perct_mean_ifallbutlookingvalid', 'ss_valid_trials', 'ss_valid_looking_perct_alltrials', 
                                               'ss_valid_looking_perct_mean_ifallbutlookingvalid', 'vs_valid_trials', 'vs_valid_looking_perct_alltrials', 
                                               'vs_valid_looking_perct_mean_ifallbutlookingvalid', 'valid_timepoint_paradigms', 'valid_timepoint'))

# add sample rate info
sampletime = aggregate(cbind(sampletime_median,sampletime_expected,sampletime_assumed)~edf, data=abcct_trial, mean, na.rm=TRUE)
summary_ET_QC = merge(summary_ET_QC, subset(sampletime, select=c(edf,sampletime_median)), by=c('edf'))

# sort summary_ET_QC by date
summary_ET_QC = summary_ET_QC[ order(summary_ET_QC$interview_date), ]


write.csv(summary_ET_QC, paste0(current_folder_analyses, '/5002abcct/erin_qc_dv_summaries/summary_ET_QC.csv'), quote = FALSE, row.names = FALSE)







#### run Valid_DVs.R script ####
setwd( paste0(current_folder_analyses,'/5002abcct') )
suppressWarnings(source( 'Valid_DVs_60Hz_QC.R' ))
setwd( paste0(current_folder_analyses) )



#### copy summary_ET_QC.csv to QC_prep folder ####
# if the directories are the same as Erin's, copy the necessary files into the MainSTudy/QC/QC_Prep folder and run ETQC_Prep.R
if (current_folder_analyses == 'C:/Users/Erin.TIL-BUTTON/Desktop/Pipeline/analyses') {
  print('Copying summary_ET_QC.csv to the ET Server/MainStudy/QC/QC_Prep.')
  filestocopy = c('5002abcct/erin_qc_dv_summaries/summary_ET_QC.csv')
  targetdir = c('C:/Users/Erin.TIL-BUTTON/Documents/Projects - Erin/U19 - Erin/abcctetserver copy/MainStudy/QC/QC_Prep')
  file.copy(from=filestocopy, to=targetdir, 
            overwrite = TRUE, copy.mode = TRUE)
  
  print('Running ETQC_Prep.R.')
  setwd( paste0('C:/Users/Erin.TIL-BUTTON/Documents/Projects - Erin/U19 - Erin/abcctetserver copy/MainStudy/QC'))
  suppressWarnings(source( 'ETQC_Prep.R' ))
  print('Finished runnning ETQC_Prep.R, will return to previous directory now.')
  setwd( paste0(current_folder_analyses) )
  
}else { 
  print('Not in Erins working directory, will not copy files and run script automatically.')
  print('You will need to manually copy summary_ET_QC.csv to the ET Server/MainStudy/QC/QC_Prep folder.')
  print('You will need to manually run ETQC_Prep.R on the ET Server/MainStudy/QC.')
}
