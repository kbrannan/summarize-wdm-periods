## code runs hspf to extract precip and potential ET time-series from the
## orginal and updated meterologic WDM files for Big Elk Creek modeling, then
## the time-series from the two WDM are compared and summerized

## working directory 
chr.sum.wdm.periods.dir <- "M:/Presentations/2016-02-09 Bacteria TWG 17/tables-charts-figures/summarize-wdm-periods"

## rund hspf
shell(cmd=paste0(chr.sum.wdm.periods.dir,"/model/winhspf.bat"))

list.files(path = chr.sum.wdm.periods.dir)
