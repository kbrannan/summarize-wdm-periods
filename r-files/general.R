## code runs hspf to extract precip and potential ET time-series from the
## orginal and updated meterologic WDM files for Big Elk Creek modeling, then
## the time-series from the two WDM are compared and summerized

## working directory 
chr.sum.wdm.periods.dir <- "M:/Presentations/2016-02-09 Bacteria TWG 17/tables-charts-figures/summarize-wdm-periods"

## load functions
source(file = paste0(chr.sum.wdm.periods.dir, "/r-files/functions.R"))

## set working directory
setwd(chr.sum.wdm.periods.dir)

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
## change end date of simulation to end of wdm file
chr.uci.org[grep("START.*END", chr.uci.org)] <- 
  "  START       1995/10/01 00:00  END    2010/12/31 24:00"
cat(chr.uci.org, 
    file = paste0(chr.sum.wdm.periods.dir, "/model/met-wdm-out.uci"), 
    sep = "\n" )
## run hspf
shell(cmd=paste0("cd ", chr.sum.wdm.periods.dir,"/model & winhspf.bat"))
## copy output for current run
shell(cmd = paste0("cd ", chr.sum.wdm.periods.dir, 
       "/model & copy met-wdm.out ", gsub("\\.","_",chr.cur.wdm), ".out"))
## read plotgen output
df.org.met.wdm <- rpltgen(chr.dir = paste0(chr.sum.wdm.periods.dir, "/model"),
                          chr.file = paste0(gsub("\\.","_",chr.cur.wdm), ".out"))
## run and read for updated met-wdm
##
## find line with wdm file name 
chr.uci.upd <- chr.uci
chr.uci.upd[min(grep("WDM1", chr.uci.upd))] <- 
  gsub(" [aA-zZ0-9]{1,}\\.wdm",paste0(" ", chr.upd.wdm), 
       chr.uci[min(grep("WDM1", chr.uci.upd))])
## change end date of simulation to end of wdm file
chr.uci.upd[grep("START.*END", chr.uci.upd)] <- 
  "  START       1995/10/01 00:00  END    2014/05/30 24:00"
cat(chr.uci.upd, 
    file = paste0(chr.sum.wdm.periods.dir, "/model/met-wdm-out.uci"), 
    sep = "\n" )
## run hspf
shell(cmd=paste0("cd ", chr.sum.wdm.periods.dir,"/model & winhspf.bat"))
## copy output for current run
shell(cmd = paste0("cd ", chr.sum.wdm.periods.dir, 
                   "/model & copy met-wdm.out ", gsub("\\.","_",chr.upd.wdm), ".out"))
## read plotgen output
df.upd.met.wdm <- rpltgen(chr.dir = paste0(chr.sum.wdm.periods.dir, "/model"),
                          chr.file = paste0(gsub("\\.","_",chr.upd.wdm), ".out"))
## clean up
rm(list=ls(pattern="chr\\.uci.*"))

## create facors for months and years
format(head(df.org.met.wdm$tmp.date), "%Y")
format(head(df.org.met.wdm$tmp.date), "%b")
