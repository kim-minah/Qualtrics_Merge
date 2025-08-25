#### characterization merge ####
# # read in characterization data
# char_seattle_asd = read.csv('Characterization/input/CharDatabase_v01asd.csv', as.is=T)
# char_seattle_dd = read.csv('Characterization/input/CharDatabase_v01dd.csv', as.is=T)
# char_seattle_td = read.csv('Characterization/input/CharDatabase_v01td.csv', as.is=T)
# char_seattle_asdsib = read.csv('Characterization/input/CharDatabase_v01asdsib.csv', as.is=T)
# 
# # lowercase column names
# names(char_seattle_asd) = tolower(names(char_seattle_asd))
# names(char_seattle_td) = tolower(names(char_seattle_td))
# names(char_seattle_dd) = tolower(names(char_seattle_dd))
# names(char_seattle_asdsib) = tolower(names(char_seattle_asdsib))
# names(char_bernier) = tolower(names(char_bernier))
# names(char_yale) = tolower(names(char_yale))
# 
# # bind char_seattle files
# char_seattle_asd = subset(char_seattle_asd, select=-c(etnirs_1_series,x))
# char_seattle_td = subset(char_seattle_td, select=-c(etnirs.1.series,x))
# char_seattle_asdsib = subset(char_seattle_asdsib, select=-c(etnirs_1_series))
# char_seattle_dd = subset(char_seattle_dd, select=-c(etnirs_1_series))
# char_seattle = rbind(char_seattle_asd, char_seattle_td, char_seattle_asdsib, char_seattle_dd)
# write.csv(char_seattle, 'Characterization/input/char_seattle.csv', row.names=F)

# note: merged (yale, seattle, bernier) outside of R in Excel by hand

# read in final char file
char = read.csv('Characterization/simons_all_char_final.csv', as.is=T)

# calculate age for seattle
char$char_date_1 = as.Date(char$char_date_1, format='%m/%d/%Y')
char$dob = as.Date(char$dob, format='%m/%d/%Y')
age_months_char2 = round( ( (char$char_date_1 - char$dob) / 30.4167 ), 0 )
char$age_months_char = ifelse(is.na(char$age_months_char), age_months_char2, char$age_months_char)

# create dx cols
# unique(tolower(char$group))
recode_group = function(a){
  a = as.character(a)
  a = tolower(a)
  if ( is.na(a) ) {
    return( 'Unknown' )
  } else if ( a == 'tbd' | a == 'tbd (asd?)' ) { 
    return('Unknown')
  } else if ( a == 'asd' | a == 'asd-t' | a == 'prt' | a == 'wlc') {
    return('ASD')
  } else if (a == 'no clinical dx' | a == 'td' | a == 'typ'){
    return( 'TD' )
  } else if (a == 'dd' | a == 'dyslexia' ) {
    return( 'DD' )
  } else if (a == 'td - see notes' | a == 'typ-p' ) {
    return( 'Non-ASD' )
  } 
  # else {
  #   return ('Unknown')
  # }
}

recode_group2 = function(a){
  a = as.character(a)
  a = tolower(a)
  if ( is.na(a) ) {
    return( 'Unknown' )
  } else if ( a == 'tbd' | a == 'tbd (asd?)' ) { 
    return('Unknown')
  } else if ( a == 'asd' | a == 'asd-t' | a == 'prt' | a == 'wlc') {
    return('ASD')
  } else if (a == 'no clinical dx' | a == 'td' | a == 'typ' |
             a == 'dd' | a == 'dyslexia' | a == 'td - see notes' | a == 'typ-p' ){
    return( 'Non-ASD' )
  } 
  # else {
  #   return ('Unknown')
  # }
}
char$dx_final = mapply( recode_group, char[ , "group"] )
# unique(char$dx_final)
char$dx_asd_non = mapply( recode_group2, char[ , "group"] )
# unique(char$dx_asd_non)

# create IQ col
char$bestiq_fsiq = as.numeric(char$bestiq_fsiq)
char$das_gca_comp = as.numeric(char$das_gca_comp)
char$sb_standard_score = as.numeric(char$sb_standard_score)
char$iq_merged = ifelse(!is.na(char$bestiq_fsiq),  char$bestiq_fsiq, 
                        ifelse(!is.na(char$das_gca_comp), char$das_gca_comp, 
                               ifelse(!is.na(char$sb_standard_score), char$sb_standard_score, NA)))

# remove PHI from char
char = subset(char, select=-c(dob, char_date_1))

#### timepoint ET and characterization merge ####
# read in timepoint trial datasets
amy = read.csv('Yale_Seattle/summary_5500am_dynamic_timepoint.csv', as.is=T)
ams = read.csv('Seattle/summary_5500am_dynamic_timepoint.csv', as.is=T)
amb = read.csv('Bernier/summary_5800am_dynamic_timepoint.csv', as.is=T)
dbys = read.csv('Yale_Seattle/summary_5500db_timepoint.csv', as.is=T)
dbb = read.csv('Bernier/summary_5800db_timepoint.csv', as.is=T)
srys = read.csv('Yale_Seattle/summary_5500sr_timepoint.csv', as.is=T)
srb = read.csv('Bernier/summary_5800sr_timepoint.csv', as.is=T)
dsys = read.csv('Yale_Seattle/summary_5500ds_dynamic_timepoint.csv', as.is=T)
dsb = read.csv('Bernier/summary_5800ds_dynamic_timepoint.csv', as.is=T)
bm = read.csv('Yale_Seattle/summary_5500bm_timepoint.csv', as.is=T)
tom = read.csv('Yale_Seattle/summary_5500tom_timepoint.csv', as.is=T)

# remove seattle data from yale-seattle AM dataset
amy1 = amy[grep("CE[1-9]", amy$id), ]
amy2 = amy[grep("CC[1-9]", amy$id), ]
amy = rbind(amy1, amy2)
amys = rbind(amy, ams)

# rbind yale-seattle and bernier ET data
am = rbind(amys, amb)
db = rbind(dbys, dbb)
sr = rbind(srys, srb)
ds = rbind(dsys, dsb)

# update col names
timepoint = subset(sr, select=c(id, timepoint, edf1, edf2, edf3, edf4,
                                et_doe1, et_doe2, et_doe3, rversion))
fixtimepointdata = function(x, z) {
  x = subset(x, select=-c(group, timepoint, edf1, edf2, edf3, edf4,
                      et_doe1, et_doe2, et_doe3, rversion))
  names(x)[2:length(names(x))] = paste0(z, '_', names(x)[2:length(names(x))])
  return(x)
}
am = fixtimepointdata(am, 'am')
bm = fixtimepointdata(bm, 'bm')
db = fixtimepointdata(db, 'db')
ds = fixtimepointdata(ds, 'ds')
sr = fixtimepointdata(sr, 'sr')
tom = fixtimepointdata(tom, 'tom')

# merge all timepoint files together
et_timepoint = Reduce(function(x, y) merge(x, y, all.x=T, all.y=T, by=c('id')), list(am, bm, db, ds, sr, tom))

# merge with char
out_timepoint = merge(timepoint, et_timepoint, all.x=T, all.y=T, by='id')
out_timepoint = merge(out_timepoint, char, all.x=T, all.y=F, by='id')
write.csv(out_timepoint, 'Output/summary_simons_timepoint_char.csv', row.names=F)



#### trial ET and characterization merge ####
# read in timepoint trial datasets
amy = read.csv('Yale_Seattle/summary_5500am_dynamic_trial.csv', as.is=T)
ams = read.csv('Seattle/summary_5500am_dynamic_trial.csv', as.is=T)
amb = read.csv('Bernier/summary_5800am_dynamic_trial.csv', as.is=T)
dbys = read.csv('Yale_Seattle/summary_5500db_trial.csv', as.is=T)
dbb = read.csv('Bernier/summary_5800db_trial.csv', as.is=T)
srys = read.csv('Yale_Seattle/summary_5500sr_trial.csv', as.is=T)
srb = read.csv('Bernier/summary_5800sr_trial.csv', as.is=T)
dsys = read.csv('Yale_Seattle/summary_5500ds_dynamic_trial.csv', as.is=T)
dsb = read.csv('Bernier/summary_5800ds_dynamic_trial.csv', as.is=T)
bm = read.csv('Yale_Seattle/summary_5500bm_trial.csv', as.is=T)
tom = read.csv('Yale_Seattle/summary_5500tom_trial.csv', as.is=T)

# remove seattle data from yale-seattle AM dataset
amy1 = amy[grep("CE[1-9]", amy$id), ]
amy2 = amy[grep("CC[1-9]", amy$id), ]
amy = rbind(amy1, amy2)
amys = rbind(amy, ams)

# fix cols for rbind
fixcols_ys = function(x) {
  x = subset(x, select=-c(timepoint, session, experiment, session_notes, session_completion))
  x$location = ifelse(x$location=='SCAC', 'SCITL', 'Yale')
  return(x)
}
fixcols_b = function(y) {
  y$location = 'Bernier'
  return(y)
}
amys = fixcols_ys(amys); amb = fixcols_b(amb)
dbys = fixcols_ys(dbys); dbb = fixcols_b(dbb)
dsys = fixcols_ys(dsys); dsb = fixcols_b(dsb)
srys = fixcols_ys(srys); srb = fixcols_b(srb)
bm = fixcols_ys(bm)
tom = fixcols_ys(tom)

# rbind yale-seattle and bernier ET data
am = rbind(amys, amb)
db = rbind(dbys, dbb)
sr = rbind(srys, srb)
ds = rbind(dsys, dsb)



#### frequency tables ####
trialfreq = function(x, z) {
  # merge trial data with char
  x = subset(x, select=-c(group, dx))
  x = merge(x, char, all.x=T, all.y=F, by='id')
  
  # find trial frequencies
  x$sourcefile_short = substr(x$sourcefile, 16, 150)
  y = data.frame(table(x$sourcefile_short, x$location))
  names(y) = c('sourcefile_short', 'location', 'freq')
  y = reshape(y, timevar='location', idvar='sourcefile_short', direction='wide')
  if ( sum(grepl('Bernier', names(y))) ) {
    y$freq = rowSums(subset(y, select=c(freq.Bernier, freq.SCITL, freq.Yale)))
  } else {
    y$freq = rowSums(subset(y, select=c(freq.SCITL, freq.Yale)))
  }
  
  max_subjects = max(y$freq)
  y$freq_perct = y$freq / max_subjects
  
  if (z=='db') {
    max_subjects = as.integer(max_subjects / 5)
    y$freq = y$freq / 5
  } else if (z=='sr') {
    max_subjects = as.integer(max_subjects / 3)
    y$freq = y$freq / 3
  }

  
  # write output of trial frequencies
  fname = paste0('Output/trial_freq/Trial_Frequencies_',z,'.csv')
  write.csv(y, fname, row.names = F)
  print(sprintf('%s Trials with > 0.50 viewers (%i total): %i / %i', toupper(z), max_subjects, sum(y$freq_perct > .5), nrow(y)))
  
  # merge trial frequencies with trial data
  x = merge(x, y, all.x=T, all.y=F, by='sourcefile_short')
  
  # write output of final trial data
  fname=paste0('Output/summary_simons_',z,'_trial_char.csv')
  write.csv(x, fname, row.names=F)
  return(x)
}

am = trialfreq(am, 'am')
db = trialfreq(db, 'db')
ds = trialfreq(ds, 'ds')
sr = trialfreq(sr, 'sr')
tom = trialfreq(tom, 'tom')
bm = trialfreq(bm, 'bm')


#### output char merged with location ####
location = unique(subset(sr, select=c(id, location)))
char_out = merge(char, location, all.x=F, all.y=T, by='id')
write.csv(char_out, 'Output/simons_all_char_final_location.csv', row.names=F)