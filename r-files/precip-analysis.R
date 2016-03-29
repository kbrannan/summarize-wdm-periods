## load packages
library(doBy, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)



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
names(df.ann.sums) <- c(names(df.ann.sums)[-5],"val")
## monthly sums
df.mon.sums <- summaryBy(obs ~ month + year + station + par + period,
                         df.data, FUN = sum)
names(df.mon.sums) <- c(names(df.mon.sums)[-6], "val")
# monthly averages
df.mon.ave <- summaryBy(val ~ month + station + par + period,
                        df.mon.sums, FUN = mean)
names(df.mon.ave) <- c(names(df.mon.ave)[-5], "val")
# average annual (weight by number of obs per year)
df.tmp <- summaryBy(obs ~ year + station + par + period,
                         df.data, FUN = c(sum,length))
df.tmp.prod <- cbind(df.tmp, prod = df.tmp$obs.sum * df.tmp$obs.length)
df.tmp.length <- summaryBy(obs.length ~ period + station + period + par, df.tmp, FUN = sum)
df.tmp.un <- merge(df.tmp.prod, df.tmp.length)
df.tmp.div <- cbind(df.tmp.un, frac= df.tmp.un$prod/df.tmp.un$obs.length.sum)
df.ann.aves <- summaryBy(frac ~ station + par + period, df.tmp.div, FUN = sum)
names(df.ann.aves) <- c(names(df.ann.aves)[-4], "val")
rm(list=ls(pattern="df.tmp*"))

## put all data into single data.frame with form
## var      date     month  year    station par    period val
## ann_ave  NA       NA     NA      factor  factor factor numerical
## ann_sum  NA       NA     factor  factor  factor factor numerical
## mon_ave  NA       factor NA      factor  factor factor numerical
## mon_sum  NA       factor factor  factor  factor factor numerical
## day_sum  POSIXct  factor facor   factor  factor factor numerical
##
## month as abrev Jan through Dec
## year as 4-digits
## station is OR350145 or OR358182
## par is PREC or PEVT
## period is org or upd
## quick fix of names for df.data
names(df.data) <- c("date",names(df.data)[c(-1, -7)], "val")
df.prec <- rbind(cbind(var = "ann_ave", date = NA, month = NA, year = NA,
                       df.ann.aves),
                 cbind(var = "ann_sum", date = NA, month = NA,
                       df.ann.sums),
                 cbind(var = "mon_ave", date = NA, month = df.mon.ave$month,
                       year = NA,
                       df.mon.ave[ , c("station", "par", "period", "val")]),
                 cbind(var = "mon_sum", date = NA, 
                       df.mon.sums),
                 cbind(var = "day_sum", df.data)
)


## need to change date back to POSIXct
df.prec$date <- as.POSIXct(df.prec$date, origin = "1970-01-01 00:00.00")

## changing order of period factor levels so upd will be behind org
df.prec$period <- factor(as.character(df.prec$period),
                         levels = c("upd", "org"))
## changing order of station factor levels so higher elev first
df.prec$station <- factor(as.character(df.prec$station),
                         levels = c("OR350145", "OR358182"))
## changing order of month factor levels
df.prec$month <- factor(as.character(df.prec$month),
                          levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                     "Jun", "Jul", "Aug", "Sep", "Oct",
                                     "Nov", "Dec"))


## clean up
rm(list=ls(pattern = "^df\\.")[-grep("df.prec",ls(pattern = "^df\\."))])

## plots 
##
## daily sum
p.prec.daily <- ggplot(data = df.prec[df.prec$var == "day_sum" &
                                        df.prec$par == "PREC", ])

p.prec.daily <- p.prec.daily + geom_segment(aes(x = date, xend = date, 
                                 y = 0, yend = val, color = period)) +
  facet_wrap(~station, ncol = 1, nrow = 2) + ylab("Precip Depth (inches)") +
  xlab("") + guides(color = FALSE)
plot(p.prec.daily)

## annual ave
p.ann.ave.prec <- ggplot(data = df.prec[df.prec$var == "ann_ave" &
                                          df.prec$par == "PREC", ])
p.ann.ave.prec <- p.ann.ave.prec + 
  geom_bar(aes(x = period, y = val, fill = period), 
           stat = "identity") + ylab("Precip Depth (inches)") +
  facet_wrap(~station, ncol = 1, nrow = 2) + guides(fill = FALSE) +
  scale_x_discrete(breaks = c("org", "upd"), 
                   labels = c("Orgnial", "Updated")) +
  geom_text(aes(x = period, y = 0.5 * val, label = sprintf(fmt = "%.1f",
                                                           round(val,1))))
plot(p.ann.ave.prec)

## annual sum
p.ann.sum.prec <- ggplot(data = df.prec[df.prec$var == "ann_sum" &
                                          df.prec$par == "PREC", ])
p.ann.sum.prec <- p.ann.sum.prec + 
  geom_bar(aes(x = year, y = val, fill = period), 
           stat = "identity", position = "dodge") + ylab("Precip Depth (inches)") +
  facet_wrap(~station, ncol = 1, nrow = 2) + guides(fill = FALSE)
plot(p.ann.sum.prec)

## monthly sum
df.tmp <- df.prec[df.prec$var == "mon_sum" &
                    df.prec$par == "PREC", ]
df.tmp$date <- as.POSIXct(strptime(paste0(df.tmp$year, "-", df.tmp$month, "-01"), 
                format = "%Y-%b-%d"))
p.mon.sum.prec <- 
  ggplot(data = df.tmp)
p.mon.sum.prec <- p.mon.sum.prec + 
  geom_bar(aes(x = date, y = val, fill = period), 
           stat = "identity", position = "dodge") + ylab("Precip Depth (inches)") +
  facet_wrap(~station, ncol = 1, nrow = 2) + guides(fill = FALSE)
plot(p.mon.sum.prec)
rm(df.tmp)

## monthly ave
p.mon.ave.prec <- ggplot(data = df.prec[df.prec$var == "mon_ave" &
                                          df.prec$par == "PREC", ])
p.mon.ave.prec <- p.mon.ave.prec + 
  geom_bar(aes(x = month, y = val, fill = period), 
           stat = "identity", position = "dodge") + ylab("Precip Depth (inches)") +
  facet_wrap(~station, ncol = 1, nrow = 2) + guides(fill = FALSE)
plot(p.mon.ave.prec)

## cdf of daily sum (hyetograph)


df.tmp <- df.prec[df.prec$par == "PREC" & df.prec$var == "day_sum", ]
df.edf.org.OR350145 <- ecdf(df.tmp[df.tmp$period == "org" &
                                     df.tmp$station == "OR350145", "val"])
df.edf.org.OR358182 <- ecdf(df.tmp[df.tmp$period == "org" &
                                     df.tmp$station == "OR358182", "val"])
df.edf.upd.OR350145 <- ecdf(df.tmp[df.tmp$period == "upd" &
                                     df.tmp$station == "OR350145", "val"])
df.edf.upd.OR358182 <- ecdf(df.tmp[df.tmp$period == "upd" &
                                     df.tmp$station == "OR358182", "val"])

