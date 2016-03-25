## load packages
library(doBy, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library('dplyr-master')

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

