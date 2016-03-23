## code runs hspf to extract precip and potential ET time-series from the
## orginal and updated meterologic WDM files for Big Elk Creek modeling, then
## the time-series from the two WDM are compared and summerized

## working directory 
chr.sum.wdm.periods.dir <- "M:/Presentations/2016-02-09 Bacteria TWG 17/tables-charts-figures/summarize-wdm-periods"

## met wdm file names
chr.cur.wdm <- "bigelk_in.wdm"
chr.upd.wdm <- "bigelkwqupdt.wdm"

## read in uci file
chr.uci <- scan(
  file = paste0(chr.sum.wdm.periods.dir, "/model/met-wdm-out.uci"), 
  what = "character", sep = "\n")

## run and read for original met-wdm
##
## find line with wdm file name 
chr.uci.org <- chr.uci
chr.uci.org[min(grep("WDM1", chr.uci.org))] <- 
  gsub(" [aA-zZ0-9]{1,}\\.wdm",paste0(" ", chr.cur.wdm), 
       chr.uci[min(grep("WDM1", chr.uci.org))])
cat(chr.uci.org, 
    file = paste0(chr.sum.wdm.periods.dir, "/model/met-wdm-out.uci"), 
    sep = "\n" )
## run hspf
shell(cmd=paste0(chr.sum.wdm.periods.dir,"/model/winhspf.bat"))
