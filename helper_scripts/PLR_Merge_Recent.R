#   ********************************************************           
#   Title: PLR_Merge_Recent.R 
#   Author: Erin Barney
#   Date: 2017-11-06
#   Purpose: Script to merge old PLR results with more recent ones completed for QC
#   Input: 
#   * all files with pattern "manual_plr*.csv" (This should be run from the 
#   ABCCT ET server/MainStudy/Pipeline/manual_plr_result/ folder. There should
#   should be one file in there called "manual_plr_result.csv" with the majority
#   of PLR results, and then a few "manual_plr_result_XXXXXXXX.csv" files with
#   individual PLR results that we want to merge into the main file.
#   Output: 
#   * manual_plr_result.csv (You'll need to move this file to the Pipeline/plr/ folder.)
#   * archive/manual_plr_result_YYYYMMDD.csv
#   ********************************************************

print(sort( Sys.glob('manual_plr*.csv' ), decreasing = TRUE))

# read in each table individually, for bug checking if column names aren't the same
# plr_csvs = sort( Sys.glob('manual_plr*.csv' ), decreasing = TRUE)
# for (f in plr_csvs) {
#   assign(substring(f,19,26),read.csv(paste0(f), as.is=TRUE))
# 
# }

load_data <- function() { 
  files = sort( Sys.glob('manual_plr*.csv' ), decreasing = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

plr_all <- load_data()

# create identifier so you can find duplicates
plr_all$identifier = paste0(plr_all$plrTag, '_', plr_all$pupil)

# take the last (aka most recent) as the non-duplicated version
plr_dup = plr_all[duplicated(plr_all$identifier, fromLast = TRUE),]
plr_nodup = plr_all[!duplicated(plr_all$identifier, fromLast = TRUE),]
plr_nodup$identifier = NULL

# write output
write.csv(plr_nodup, 'manual_plr_result.csv', row.names=F)
namedate = paste0("archive/manual_plr_result_", format(Sys.Date(), "%Y%m%d"), ".csv")
write.csv(plr_nodup, namedate, row.names=F)