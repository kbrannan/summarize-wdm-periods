

## load packages
library(doBy, quietly = TRUE)
library(ggplot2, quietly = TRUE)

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
