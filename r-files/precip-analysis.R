## load packages
library(doBy, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr)

## create single long format data frmae of met data
names(df.org.met.wdm)



junk <- gather(df.org.met.wdm, key = src, val=value, OR350145.PREC, OR358182.PREC, OR350145.PEVT, OR358182.PEVT)
junk <- data.frame(junk,
                       station=gsub("\\.[aA-zZ]{1,}","", junk$src),
                       par=gsub("^.*\\.","", junk$src))
junk <- junk[, -4]

junk2 <- gather(junk, key = id, value = obs, value)
junk2 <- junk2[,-2]

lil.junk <- head(junk,25)

gsub("^.*\\.","", lil.junk$src)

lil.junk <- data.frame(lil.junk,
                       station=gsub("\\.[aA-zZ]{1,}","", lil.junk$src),
                       par=gsub("^.*\\.","", lil.junk$src))

# average annual
df.ann.means.org <- summaryBy(OR350145.PREC + OR358182.PREC ~ year, 
          data = df.org.met.wdm, FUN = c(sum, length))
df.ann.means.upd <- summaryBy(OR350145.PREC + OR358182.PREC ~ year, 
          data = df.upd.met.wdm, FUN = c(sum, length))
df.ann.means <- data.frame(
  org = sum((df.ann.means.org[ ,4] / 
               sum(df.ann.means.org[ ,4])) * df.ann.means.org[ ,2]),
  upd = sum((df.ann.means.upd[ ,4] / 
               sum(df.ann.means.upd[ ,4])) * df.ann.means.upd[ ,2]))
rm(df.ann.means.org, df.ann.means.upd)

## annual sums
df.ann.sums.org <- summaryBy(OR350145.PREC + OR358182.PREC ~ year, 
                             data = df.org.met.wdm, FUN = c(sum))
df.ann.sums.upd <- summaryBy(OR350145.PREC + OR358182.PREC ~ year, 
                             data = df.upd.met.wdm, FUN = c(sum))

# average monthly
df.monthly.means.years.org <- summaryBy(OR350145.PREC + OR358182.PREC ~ month + year, 
                              data = df.org.met.wdm, FUN = sum)
df.monthly.means.org <- summaryBy(OR350145.PREC.sum + OR358182.PREC.sum ~ month,
                                  data = df.monthly.means.years.org, FUN = mean)

df.monthly.means.years.upd <- summaryBy(OR350145.PREC + OR358182.PREC ~ month + year, 
                                        data = df.upd.met.wdm, FUN = sum)
df.monthly.means.upd <- summaryBy(OR350145.PREC.sum + OR358182.PREC.sum ~ month,
                                  data = df.monthly.means.years.upd, FUN = mean)
df.monthly.means <- 
  data.frame(
    month = df.monthly.means.org$month,
    OR350145.PREC.org = df.monthly.means.org$OR350145.PREC.sum.mean,
    OR358182.PREC.org = df.monthly.means.org$OR358182.PREC.sum.mean,
    OR350145.PREC.upd = df.monthly.means.upd$OR350145.PREC.sum.mean,
    OR358182.PREC.upd = df.monthly.means.upd$OR358182.PREC.sum.mean)
rm(df.monthly.means.years.org, df.monthly.means.org, 
   df.monthly.means.years.upd, df.monthly.means.upd)

## put all data into single data.frame with form
## var        date     month  year    src    val
## ave_ann    NA       NA     NA      factor numerical
## sum_ann    NA       NA     factor  factor numerical
## ave_mon    NA       factor NA      factor numerical
## sum_mon    NA       factor factor  factor numerical
## sum_daily  POSIXct  factor facor   factor numerical
##
## month as abrev Jan through Dec
## year as 4-digits
## src either org or upd
## ave annual
df.prec <- data.frame(var = "ave_ann",
                      date = NA,
                      month = NA,
                      year = NA,
                      src = c("org","upd"),
                      val = df.ann.means[1, ])
## sum annual
df.prec <- data.frame(df.prec,
                      var = "sum_ann",
                      date = NA,
                      month = NA,
                      year = 
                      src = c("org","upd"),
                      val = df.ann.means[1, ])

## plots 
##
## daily values
p.daily.prec <- ggplot(data = df.upd.met.wdm)
p.daily.prec <- p.daily.prec + geom_segment(aes(x = tmp.date, xend = tmp.date,
                                     y = 0, yend = OR350145.PREC))
p.daily.prec <- p.daily.prec + 
  geom_segment(data = df.org.met.wdm,
               aes(x = tmp.date, xend = tmp.date,
                   y = 0, yend = OR350145.PREC), 
               color = "red")
plot(p.daily.prec)

## average annual
p.ave.ann.prec <- ggplot(data = df.ann.means)
p.ave.ann.prec <- p.ave.ann.prec + geom_bar()
plot(p.ave.ann.prec)

