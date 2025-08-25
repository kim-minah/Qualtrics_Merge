# script to merge Simons Seattle data with characterization data
INITIAL_CHAR_MERGE = 0

#### merge with characterization data #####
et_agg <- data.frame(output)
et_trial <- x

if (INITIAL_CHAR_MERGE==1) {
  if ( length(grep('5500simonsseattle',getwd())) > 0 ) {
    char_asd = read.csv('../4002fnirs_bm/4002_char_asd.csv', as.is=T)
    char_asdsib = read.csv('../4002fnirs_bm/4002_char_asdsib.csv', as.is=T)
    char_dd = read.csv('../4002fnirs_bm/4002_char_dd.csv', as.is=T)
    char_td = read.csv('../4002fnirs_bm/4002_char_td.csv', as.is=T)
  } else if ( length(grep('4002',getwd())) > 0 ) {
    char_asd = read.csv('4002_char_asd.csv', as.is=T)
    char_asdsib = read.csv('4002_char_asdsib.csv', as.is=T)
    char_dd = read.csv('4002_char_dd.csv', as.is=T)
    char_td = read.csv('4002_char_td.csv', as.is=T)
  }

  # add dx
  char_asd$dx = 'ASD'
  char_asdsib$dx = 'ASD-sib'
  char_dd$dx = 'DD'
  char_td$dx = 'TD'

  # lowercase names
  names(char_asd) = tolower(names(char_asd))
  names(char_td) = tolower(names(char_td))
  names(char_asdsib) = tolower(names(char_asdsib))
  names(char_dd) = tolower(names(char_dd))

  # setdiff(names(char_td),names(char_asd))
  # setdiff(names(char_asd),names(char_td))
  # setdiff(names(char_asd),names(char_asdsib))
  # setdiff(names(char_asd),names(char_dd))
  # setdiff(names(char_asdsib),names(char_asd))
  # setdiff(names(char_dd),names(char_asd))

  # bind char files
  char = rbind(char_asd, char_td, char_dd, char_asdsib)
  write.csv(char, '4002_char_all.csv', row.names=F)
}


# read in long char file
# NOTE: char file must have date columns in Excel's short date format!
if ( length(grep('5500simonsseattle',getwd())) > 0 ) {
  char = read.csv('../4002fnirs_bm/4002_char_all.csv', as.is=T)
} else if ( length(grep('4002',getwd())) > 0 ) {
  char = read.csv('4002_char_all.csv', as.is=T)
}

# remove columns from ET data so they don't duplicate when merging
if  ( length(grep('4002',getwd())) > 0 ) {
  # print('Removing dx column')
  # print(grep('dx', names(et_trial)))
  # print(names(et_trial))
  et_trial = subset(et_trial, select=-c(dx))
  et_agg = subset(et_agg, select=-c(dx))
  # print(grep('dx', names(et_trial)))
  # print(names(et_trial))
}

# subset asd char file only and merge with et_agg
char = subset(char, select=c(id, dx, dob, char.date.1, char.date.2, 
                             racial.categories, ethnic.categories, sex, ados.module, ados_sa_severity, ados_rrb_severity,
                             mullen_vr_tscore, mullen_fm_tscore, mullen_rl_tscore, mullen_el_tscore, mullen_elc_tscore,
                             mullen_vr_ageequiv_mos, mullen_fm_ageequiv_mos, mullen_rl_ageequiv_mos, mullen_el_ageequiv_mos,
                             sb_nonverbal_scaledscore, sb_verbal_scaledscore, sb_standard_score,
                             das_verbal_ss, das_nonverbal_ss, das_spatial_ss, das_gca_comp,
                             nepsy_ar_scaled,
                             abas_gac, abas_conc, abas_soc, abas_prac,
                             brief_isci_tscore, brief_fi_tscore, brief_emi_tscore, brief_gec_tscore))

char$racial.categories = tolower(char$racial.categories)
char$ethnic.categories = tolower(char$ethnic.categories)
char$sex = toupper(char$sex)
colstoconvert <- c('mullen_vr_ageequiv_mos', 'mullen_fm_ageequiv_mos', 'mullen_rl_ageequiv_mos', 'mullen_el_ageequiv_mos',
                   'das_gca_comp','sb_standard_score')
char[colstoconvert] <- sapply(char[colstoconvert],as.numeric)
char$dob = as.Date(as.character(char$dob),  format='%m/%d/%Y')
char$char.date.1 = as.Date(as.character(char$char.date.1),  format='%m/%d/%Y')
char$char.date.2 = as.Date(as.character(char$char.date.2),  format='%m/%d/%Y')

char$age_mullen = (char$char.date.1 - char$dob) / (365.2425/12)
char$age_mullen = as.numeric(char$age_mullen)

# add DQ and FSIQ merged columns
char$mullen_nvdq = round( ((char$mullen_fm_ageequiv_mos + char$mullen_vr_ageequiv_mos) / 2) / char$age_mullen * 100 )
char$mullen_vdq = round( ((char$mullen_el_ageequiv_mos + char$mullen_rl_ageequiv_mos) / 2) / char$age_mullen * 100 )
char$mullen_fsdq = round( ((char$mullen_el_ageequiv_mos + char$mullen_rl_ageequiv_mos + 
                       char$mullen_fm_ageequiv_mos + char$mullen_vr_ageequiv_mos) / 4) / char$age_mullen * 100 )
char$fsiq_merged_test = ifelse(!is.na(char$sb_standard_score), 'SB', ifelse(
  !is.na(char$das_gca_comp), 'DAS', ifelse(
    !is.na(char$mullen_fsdq), 'Mullen', NA) ) )
char$fsiq_merged = ifelse(!is.na(char$sb_standard_score), char$sb_standard_score, ifelse(
  !is.na(char$das_gca_comp), char$das_gca_comp, ifelse(
    !is.na(char$mullen_fsdq), char$mullen_fsdq, NA) ) )

# merge with ET data
et_trial_char_merge = merge(et_trial, char, by=c('id'), all.x=T)
et_agg_char_merge = merge(et_agg, char, by=c('id'), all.x=T)

# ET age
et_trial_char_merge$et_doe = as.Date(et_trial_char_merge$et_doe, format='%m/%d/%Y')
et_agg_char_merge$et_doe1 = as.Date(et_agg_char_merge$et_doe1, format='%m/%d/%Y')

et_trial_char_merge$age_et_days = et_trial_char_merge$et_doe - et_trial_char_merge$dob
et_agg_char_merge$age_et_days = et_agg_char_merge$et_doe1 - et_agg_char_merge$dob
et_trial_char_merge$age_et_months = (et_trial_char_merge$age_et_days) / (365.2425/12)
et_agg_char_merge$age_et_months = (et_agg_char_merge$age_et_days) / (365.2425/12)
et_trial_char_merge$age_et_months_round = round(et_trial_char_merge$age_et_months)
et_agg_char_merge$age_et_months_round = round(et_agg_char_merge$age_et_months)