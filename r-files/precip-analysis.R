## load packages
library(doBy, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr)

## create single long format data frmae of met data
## gather data for orginial period
df.tmp <- gather(df.org.met.wdm, key = src, val=value, OR350145.PREC, OR358182.PREC, OR350145.PEVT, OR358182.PEVT)
df.tmp <- data.frame(df.tmp,
                       station=gsub("\\.[aA-zZ]{1,}","", df.tmp$src),
                       par=gsub("^.*\\.","", df.tmp$src), 
                     period = "org")
df.tmp <- df.tmp[, -4]
df.tmp <- gather(df.tmp, key = id, value = obs, value)
df.tmp <- df.tmp[,-7]
df.data <- df.tmp
rm(df.tmp)
## gather data for updated period
df.tmp <- gather(df.upd.met.wdm, key = src, val=value, OR350145.PREC, OR358182.PREC, OR350145.PEVT, OR358182.PEVT)
df.tmp <- data.frame(df.tmp,
                     station=gsub("\\.[aA-zZ]{1,}","", df.tmp$src),
                     par=gsub("^.*\\.","", df.tmp$src), 
                     period = "upd")
df.tmp <- df.tmp[, -4]
df.tmp <- gather(df.tmp, key = id, value = obs, value)
df.tmp <- df.tmp[,-7]
## put org and upd periods together
df.data <- rbind(df.data,df.tmp)
rm(df.tmp)

## annual sums
df.ann.sums <- summaryBy(obs ~ year + station + par + period,
                         df.data, FUN = sum)
## monthly sums
df.mon.sums <- summaryBy(obs ~ month + year + station + par + period,
                         df.data, FUN = sum)

# average annual (weight by number of obs per year)
df.tmp <- summaryBy(obs ~ year + station + par + period,
                         df.data, FUN = c(sum,length))
df.tmp.prod <- cbind(df.tmp, prod = df.tmp$obs.sum * df.tmp$obs.length)
df.tmp.length <- summaryBy(obs.length ~ period + station + period + par, df.tmp, FUN = sum)
df.tmp.un <- merge(df.tmp.prod, df.tmp.length)
df.tmp.div <- cbind(df.tmp.un, frac= df.tmp.un$prod/df.tmp.un$obs.length.sum)
df.ann.aves <- summaryBy(frac ~ period + station + par, df.tmp.div, FUN = sum)
rm(list=ls(pattern="df.tmp*"))

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

