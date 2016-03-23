## code runs hspf to extract precip and potential ET time-series from the
## orginal and updated meterologic WDM files for Big Elk Creek modeling, then
## the time-series from the two WDM are compared and summerized

## working directory 
chr.sum.wdm.periods.dir <- "M:/Presentations/2016-02-09 Bacteria TWG 17/tables-charts-figures/summarize-wdm-periods"

## readin uci file
chr.uci <- scan(
  file = paste0(chr.sum.wdm.periods.dir, "/model/met-wdm-out.uci"), 
  what = "character", sep = "\n")
## find line with wdm file name
gsub(" [aA-zZ0-9]{1,}\\.wdm"," this.wdm", chr.uci[min(grep("WDM1", chr.uci))])


## rund hspf
shell(cmd=paste0(chr.sum.wdm.periods.dir,"/model/winhspf.bat"))

list.files(path = chr.sum.wdm.periods.dir)
