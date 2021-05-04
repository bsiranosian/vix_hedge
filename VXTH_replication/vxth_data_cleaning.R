# backtesting a portfolio of options to replicate the VXTH index
# and then extending it to UPRO/TMF
# first step is to read and clean the index and option data
options(stringsAsFactors = F)
library(ggplot2)
library(xts)
library(dplyr)

vxth.df <- read.table("~/option_data/VXTH.tsv", sep='\t', header=T)
vxth.price <- vxth.df[,c('date', 'close')]
colnames(vxth.price)[2] <- 'VXTH'
vxth.price$date <- as.Date(vxth.price$date)
rownames(vxth.price) <- vxth.price$date
ggplot(vxth.price) +
    geom_line(aes(x=date, y=VXTH)) +
    scale_x_date()

# the allocation data from CBOE website
vxth.allocation <- read.table("~/option_data/VIXTH_daily_allocations.tsv", sep='\t', quote='', header=T)
colnames(vxth.allocation)[1] <- 'date'
vxth.allocation$date <- as.Date(vxth.allocation$date, format='%m/%d/%Y')
vxth.allocation$Expiration <- as.Date(vxth.allocation$Expiration, format='%m/%d/%Y')
# VXTH prices are identical to the dataframe above



# spx and vix and tmf prices - get them all together
spx.df <- read.table('~/option_data/spx_vix_close.tsv', sep='\t', quote='', header=T)
spx.df$date <- as.Date(as.character(spx.df$date), format='%Y%m%d')
rownames(spx.df) <- spx.df$date
upro.df <- read.table('~/option_data/sim_data/uprosim_extended.csv', sep=',', quote='', header=T)
upro.df$date <- as.Date(upro.df$date)
tmf.df <- read.table('~/option_data/sim_data/tmfsim_extended.csv', sep=',', quote='', header=T)
tmf.df$date <- as.Date(tmf.df$date)
# merge it all together - subset to this set of dates we're interested in
# 2006 - 2020 (when the VIX options got started)
price.df <- left_join(left_join(left_join(spx.df, vxth.price, by='date'), tmf.df, by='date'), upro.df, by='date')
# subset to 2006- 2019 for now
min.date <- as.Date('2006-03-22')
max.date <- as.Date('2020-12-31')
price.df <- price.df[price.df$date >= min.date, ]
price.df <- price.df[price.df$date <= max.date, ]
rownames(price.df) <- price.df$date
write.table(format(price.df, digits=2, nsmall=2), '~/option_data/vxth_project/all_prices.tsv', sep='\t', quote=F, row.names = F, col.names = T)
price.xts <- xts(price.df[,2:ncol(price.df)],order.by = price.df$date)

# add VIX and SPX prices into the allocation dataframe
va.df <- left_join(vxth.allocation, price.df, by='date')
plot(density(va.df$SPX.x - va.df$SPX.y))
# most values are almost identical. There's some accounting for dividends that isnt done here I believe....
table(va.df$SPX.x - va.df$SPX.y)
table(va.df$VXTH.x - va.df$VXTH.y)
# remove extra rows : going to just use the Y examples becuse those are my authoritative data
va.df <- va.df[, !colnames(va.df) %in% c('VXTH.x', 'SPX.x')]
colnames(va.df)[colnames(va.df)=='SPX.y'] <- 'SPX'
colnames(va.df)[colnames(va.df)=='VXTH.y'] <- 'VXTH'
# when do we transition between hedges?
# first non-duplicated exp date
transition.dates <- !duplicated(va.df$Expiration)
va.df$trade.today <- transition.dates
# which(transition.dates)
# what percentage of the portfolio is dedicated to the hedge on each day
va.df$allocation.pct <- va.df$Number_Of_Calls * 100 * va.df$Call.Mid / va.df$VXTH
barplot(va.df$allocation.pct)
barplot(va.df$allocation.pct[va.df$trade.today])

va.df.trade <- va.df[va.df$trade.today, ]
ggplot(va.df.trade) + geom_point(aes(x=VIX, y=Number_Of_Calls))

# RULES for the hedge
# ONE MONTH FORWARD VALUE OF VIX (??)
# X <= 15      | 0.0%
# 15 > X <= 30 | 1.0%
# 30 > X <= 50 | 0.5%
# X > 50       | 0.0%

# VIX option data
vixopt <- read.table('~/option_data/VIX_option_prices_2006-02-24_2019-12-31.csv', sep=',', header=T)
# keep just these columns
# [1] "date"         "exdate"       "cp_flag"      "strike_price" "best_bid"     "best_offer"
# [7] "delta"        "midpoint"     "spread"       "dte"
# transformations
vixopt$date <- as.Date(as.character(vixopt$date), format='%Y%m%d')
vixopt$exdate <- as.Date(as.character(vixopt$exdate), format='%Y%m%d')
# strike price is multipled by 1000
vixopt$strike_price <- vixopt$strike_price / 1000
# Some other price calcs midpoint and spread
vixopt$midpoint <- (vixopt$best_bid + vixopt$best_offer) /2
vixopt$spread <- abs(vixopt$best_bid - vixopt$best_offer)
# calculate dte
date.int <- as.integer(vixopt$date)
exdate.int <- as.integer(vixopt$exdate)
vixopt$dte <- exdate.int - date.int
# filter to reasonable dte
max_dte <- 120
vixopt <- vixopt[vixopt$dte <=max_dte, ]
keep.cols <- c("date","exdate","cp_flag","strike_price","best_bid","best_offer","delta","midpoint","spread","dte")
vixopt <- vixopt[, keep.cols]
min.date <- as.Date('2006-03-22')
vixopt <-vixopt[vixopt$date >= min.date,]

# get the new purchased data
new.2019 <- read.table('~/option_data/purchased_data/VIX_2019.csv', sep=',', header=T)
new.2020 <- read.table('~/option_data/purchased_data/VIX_2020.csv', sep=',', header=T)

# transform to these columns
# [1] "date"         "exdate"       "cp_flag"      "strike_price" "best_bid"     "best_offer"
# [7] "delta"        "midpoint"     "spread"       "dte"
n1 <- new.2019[, c("quotedate", "expiration", "type", "strike", "bid", "ask", "delta")]
n2 <- new.2020[, c("quotedate", "expiration", "type", "strike", "bid", "ask", "delta")]
colnames(n2) <- colnames(vixopt)[1:ncol(n2)]
colnames(n1) <- colnames(vixopt)[1:ncol(n1)]
#
n2$date <- as.character(as.Date(n2$date, format='%m/%d/%Y'))
n2$exdate <- as.character(as.Date(n2$exdate, format='%m/%d/%Y'))
n2$cp_flag[n2$cp_flag=='call'] <- 'C'
n2$cp_flag[n2$cp_flag=='put'] <- 'P'
n2$midpoint <- (n2$best_bid + n2$best_offer) /2
n2$spread <- abs(n2$best_bid - n2$best_offer)
#
n1$date <- as.character(as.Date(n1$date, format='%m/%d/%Y'))
n1$exdate <- as.character(as.Date(n1$exdate, format='%m/%d/%Y'))
n1$cp_flag[n1$cp_flag=='call'] <- 'C'
n1$cp_flag[n1$cp_flag=='put'] <- 'P'
n1$midpoint <- (n1$best_bid + n1$best_offer) /2
n1$spread <- abs(n1$best_bid - n1$best_offer)
#
uexp2 <- unique(n2$exdate)
uexp1 <- unique(n1$exdate)

# calculate dte
date.int <- as.integer(as.Date(n2$date))
exdate.int <- as.integer(as.Date(n2$exdate))
n2$dte <- exdate.int - date.int

date.int <- as.integer(as.Date(n1$date))
exdate.int <- as.integer(as.Date(n1$exdate))
n1$dte <- exdate.int - date.int

# filter to reasonable dte
max_dte <- 120
n2 <- n2[n2$dte <=max_dte, ]

rm(new.2020)
rm(new.2019)
rm(n1)

# # again prices match up well
# n1.compare <- n1
# vixopt.compare <- vixopt[as.character(vixopt$date) %in% unique(n1$date),]
# n1.compare$uuid <- paste0(n1.compare$date, n1.compare$exdate, n1.compare$cp_flag, n1.compare$strike_price)
# vixopt.compare$uuid <- paste0(vixopt.compare$date, vixopt.compare$exdate, vixopt.compare$cp_flag, vixopt.compare$strike_price)
# vixopt.compare <- vixopt.compare[!duplicated(vixopt.compare$uuid),]
# rownames(n1.compare) <- n1.compare$uuid
# rownames(vixopt.compare) <- vixopt.compare$uuid
# common.uuids <- intersect(n1.compare$uuid, vixopt.compare$uuid)
# length(common.uuids)
# n1.common <- n1.compare[common.uuids, ]
# vixopt.common <- vixopt.compare[common.uuids, ]
#
# plot.df <- data.frame(uuid=common.uuids, n1.mid=n1.common$midpoint, n1.spread=n1.common$spread, vixopt.mid=vixopt.common$midpoint, vixopt.spread=vixopt.common$spread)
# # midpoint is almost perfectly correlated, spread has more variance
# # but if the mid is good, we're good to go
# ggplot(plot.df) + geom_point(aes(x=vixopt.mid, y=n1.mid), alpha=0.1)
# ggplot(plot.df) + geom_point(aes(x=vixopt.spread, y=n1.spread), alpha=0.1)
# cor(plot.df$n1.mid, plot.df$vixopt.mid)
# cor(plot.df$n1.mid, plot.df$vixopt.mid, method='spearman')
# cor(plot.df$n1.spread, plot.df$vixopt.spread)
# cor(plot.df$n1.spread, plot.df$vixopt.spread, method='spearman')
# summary(lm(n1.mid~vixopt.mid, data=plot.df))
# summary(lm(n1.spread~vixopt.spread, data=plot.df))

# add to vixopt
vixopt <- rbind(vixopt, n2)
rm(n2)

# check the expiration dates
exps <- sort(unique(vixopt$exdate))
table(weekdays(exps))
# all Tuesday and Wednesady so thats fine
# check against trade dates we have from both datasets
trade.dates.1 <- unique(vixopt$date)
trade.dates.2 <- unique(price.df$date)
bad.dates.1 <- exps[!(exps %in% trade.dates.1)]
bad.dates.2 <- exps[!(exps %in% trade.dates.2)]
bad.dates <- sort(unique(c(bad.dates.1, bad.dates.2)))
bad.dates
# some holidays and "days of mourning in here"
# so need to correct for those dates
correct.date.map <- c('2018-12-05'='2018-12-04')
# others are in 2020 so thats fine for now because I dont have that data yet
vixopt$exdate[vixopt$exdate == as.Date('2018-12-05')] <- as.Date('2018-12-04')

# transform to list of matrices
trade.dates <- price.df$date
vix.opt.dates <- unique(vixopt$date)
trade.dates[!(trade.dates %in% vix.opt.dates)]
# somehow  "2020-09-29" is missing
# add it back with the same data from the day before?
add.back <- vixopt[vixopt$date== "2020-09-28", ]
add.back$date <- as.Date("2020-09-29")
add.back$dte <- add.back$dte-1
vixopt <- rbind(vixopt, add.back)
vixopt <- vixopt[order(vixopt$date),]
uniq.dates <- unique(trade.dates)
price.list <-lapply(split.data.frame(vixopt, vixopt$date), function(x) {
    acast(x, cp_flag~exdate~strike_price, value.var = 'best_offer', fun.aggregate = mean)
})
names(price.list) <- uniq.dates
saveRDS(price.list, '~/option_data/vxth_project/price_ASK_list_2021-01-11.rds')

delta.list <-lapply(split.data.frame(vixopt, vixopt$date), function(x) {
    acast(x, cp_flag~exdate~strike_price, value.var = 'delta', fun.aggregate = mean)
})
names(delta.list) <- uniq.dates
saveRDS(delta.list, '~/option_data/vxth_project/delta_list_2021-01-11.rds')

rm(vixopt)
# goddamn memory
gc()


## FUTURES PRICES
# all files concatenated into one
fut.df <- read.table('~/option_data/vix_futures/VIX_fut_all.csv', sep = ',', quote='', header=T)
colnames(fut.df)[1] <- 'date'
fut.df$date <- as.Date(fut.df$date)
# convert exp date to something useful
exdate.key <- read.table('~/option_data/vix_futures/expdate_key.tsv', sep='\t', header=T)
exdate.map <- as.Date(exdate.key$exdate)
names(exdate.map) <- exdate.key$shortcode
fut.df$exdate <- exdate.map[fut.df$Futures]
# sort by exdate then trade date
fut.df <- fut.df[order(fut.df$exdate, fut.df$date),]
fut.df$VIX <- price.df[as.character(fut.df$date), "VIX"]

# quandl vix futures
q1.df <- read.table('~/option_data/vix_futures/quandl_vix_futures_vx1.csv', sep=',', quote='', header=T)[,c(1,5)]
q2.df <- read.table('~/option_data/vix_futures/quandl_vix_futures_vx2.csv', sep=',', quote='', header=T)[,c(1,5)]
colnames(q1.df)[1] <- 'date'
colnames(q2.df)[1] <- 'date'
q.df <- left_join(q1.df, q2.df, by='date')
colnames(q.df) <- c('date', 'VX1', 'VX2')
q.df$date <- as.Date(q.df$date)
rownames(q.df) <- q.df$date
q.df$VIX.spot <- spx.df[as.character(q.df$date), "VIX"]
# rescaling of futures price by 10 occurred on 2007-03-26
# last date in this dataset would be 2007-03-23
last.date.rescale <- as.Date('2007-03-23')
rescale.rows <- q.df$date <= last.date.rescale
q.df[rescale.rows, c('VX1','VX2')] <- q.df[rescale.rows, c('VX1','VX2')] / 10
# theres a little bit of bad data - dates where the value is 0 or NA
# where VX2 is NA, can just use VX1
q.df[is.na(q.df$VX2), 'VX2'] <- q.df[is.na(q.df$VX2), 'VX1']
# where zero
zs <- which(q.df$VX1==0)
zs2 <- which(q.df$VX2==0)
q.df$prem1 <- q.df$VX1 - q.df$VIX.spot
q.df$prem2 <- q.df$VX2 - q.df$VIX.spot
# for each zero, take the average of the premium direcly before and after and get the value form spot VIX
for (i in 1:length(zs)){
    q.df[zs[i], 'VX1'] <-  q.df[zs[i], "VIX.spot"] + mean(q.df[zs[i]-1, 'prem1'], q.df[zs[i]+1, 'prem1'])
}
# VX2 is slightly more difficult
q.df[zs2[1:4], 'VX2'] <-  q.df[zs2[1:4], "VIX.spot"] + mean(q.df[zs2[1]-1, 'prem2'], q.df[zs2[4]+1, 'prem2'])
for (i in 5:length(zs2)){
    q.df[zs2[i], 'VX2'] <-  q.df[zs2[i], "VIX.spot"] + mean(q.df[zs2[i]-1, 'prem2'], q.df[zs2[i]+1, 'prem2'])
}
# fixed!
q.df[q.df$VX1==0,]
q.df[q.df$VX2==0,]

# remove premium cols
# and jan 2021 data
q.df <- q.df[5:nrow(q.df), 1:4]
# reverse dates
q.df <- q.df[order(q.df$date),]
# save a clean version
write.table(q.df, '~/option_data/vix_futures/quandl_vix_futures_cleaned.tsv', sep='\t', quote = F, row.names = F, col.names = T)

qm <- melt(q.df, id.vars = 'date')
ggplot(qm) +
    geom_line(aes(x=date, y=value, color=variable)) +
    scale_x_date() +
    theme_bw() +
    scale_color_brewer(palette='Set1')
