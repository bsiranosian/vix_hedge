# replicating the VIX index with SPX options
# These results, and combined with the results of Table 2, suggest the best way to form a portfolio that replicates VIX
# and has lower cost, will buy monthly an ITM‐OTM put spread and sell an ATM‐OTM call spread. This will allow the
# portfolio to benefit from volatility shocks by having a put spread while also having relatively low cost from selling a call
# spread. This call spread not only helps to fund the volatility and price protection, but when combined with the net long‐
# equity position will have a payoff structure similar to a covered call. However, this means that the portfolio is
# Buy ITM-OTM put spread (5% on each)
# Sell ATM-OTM call spread (5%)

# need to load and process the option data
# take a subset of the original datasets which are >2.5G total
# keep columns
library(dplyr)
library(reshape2)
library(gplots)
library(ggplot2)
library(viridis)
library(PerformanceAnalytics)
# old code to read an mesh data

if(F){
    # SPX VIX data
    dat <- read.table('~/option_data/spx_vix_close.tsv',sep='\t', quote='', header=T)
    dat$date    <- as.Date.character(dat$date, format="%Y%m%d" )
    rownames(dat) <- dat$date
    # add SPY to this as the actual asset to hold
    spy.data <- stockDataDownload('SPY', from = '1996-01-01', to='2021-01-01')
    upro.data <- stockDataDownload('UPRO', from = '1996-01-01', to='2021-01-01')
    tmf.data <- stockDataDownload('TMF', from = '1996-01-01', to='2021-01-01')

    dat$SPY <- spy.data$close$SPY
    upro.data.df <- as.data.frame(upro.data$close)
    upro.data.xts <- xts(upro.data.df, order.by=as.Date(rownames(upro.data.df)))

    # load the UPROSIM and TMFSIM data for funsies
    uprosim.return <- read.table('~/option_data/sim_data/UPROSIM.csv', sep=',', header = T)[,1:2]
    uprosim.return$Date <- as.Date(uprosim.return$Date, format='%m/%d/%y')
    uprosim.return$UPRO <- as.numeric(gsub('%', '', uprosim.return$UPRO)) /100
    uprosim.price <- data.frame(date=uprosim.return$Date, UPRO=cumprod(uprosim.return$UPRO +1))
    rownames(uprosim.price) <- uprosim.price$date
    common.dates <- intersect(as.character(uprosim.price$date), as.character(index(upro.data$close)))
    uprosim.compare <- data.frame(date=common.dates, UPRO=upro.data.df[common.dates, "UPRO"], UPROSIM=uprosim.price[common.dates, "UPRO"])
    uprosim.compare$UPRO <- uprosim.compare$UPRO / uprosim.compare$UPRO[1]
    uprosim.compare$UPROSIM <- uprosim.compare$UPROSIM / uprosim.compare$UPROSIM[1]
    ucm <- melt(uprosim.compare, id.vars = 'date')

    # compare the two on a plot
    ggplot(ucm) + geom_line(aes(x=as.Date(date), y=value, col=variable)) +
        scale_x_date()

    # extend the UPRO data to the common start date
    start.date <- as.Date('1996-01-01')
    end.date <- as.Date('2009-06-25')
    uprosim.pricex <- xts(uprosim.price$UPRO, order.by = uprosim.price$date)
    upro.extend <- uprosim.pricex['1996-01-01/2009-06-25']
    # this needs to be scaled so the value on the last date matches up
    match.val <- upro.data.df[1,"UPRO"]
    last.val <- as.numeric(upro.extend["2009-06-25"])
    upro.extend <- upro.extend * (match.val/last.val)
    upro.extend <- rbind(upro.extend['1996-01-01/2009-06-24'], upro.data.xts)
    upro.extend.df <- data.frame(date=index(upro.extend), UPRO=coredata(upro.extend))
    ggplot(upro.extend.df) + geom_line(aes(x=date, y=UPRO))
    # add it into dat
    dat$UPRO <- upro.extend.df$UPRO
    # write this out
    write.table(dat, '~/option_data/using_stock_prices.tsv', sep='\t', quote=F, col.names=T, row.names = T)
}

# loading and processing option data into matrix
if (F) {
# load option data
# filtered down to 1.2G by changing columns
# 2,4,5,6,7,8,9,10,12,16,17
f1 <- '~/option_data/SPX_filtered/SPX_option_prices_01Jan1996-31Dec2019.csv'
d1 <- read.table(f1, header = T, sep=',')
# drop cols we don't need
remove.cols <- c('volume', 'open_interest', 'optionid', 'forward_price')
d1 <- d1[, !(colnames(d1) %in% remove.cols)]
dim(d1)
head(d1)

d1$date <- as.character(d1$date)
d1$exdate <- as.character(d1$exdate)
# strike price is multipled by 1000
d1$strike_price <- d1$strike_price / 1000
# Some other price calcs midpoint and spread
d1$midpoint <- (d1$best_bid + d1$best_offer) /2
d1$spread <- abs(d1$best_bid - d1$best_offer)

# calculate dte
date.int <- as.integer(as.Date(d1$date, format='%Y%m%d'))
exdate.int <- as.integer(as.Date(d1$exdate, format='%Y%m%d'))
d1$dte <- exdate.int - date.int
# filter to reasonable dte
max_dte <- 120
d1 <- d1[d1$dte <=max_dte, ]

# add in the new 2019-2020 data
# and verify the Dec 2019 data matches up with prev dataset
# new.2019 <- read.table('~/option_data/purchased_data/SPX_2019.csv', sep=',', header=T)
new.2020 <- read.table('~/option_data/purchased_data/SPX_2020.csv', sep=',', header=T)
new.2020w <- read.table('~/option_data/purchased_data/SPXW_2020.csv', sep=',', header=T)

# transform to these columns
# [1] "date"         "exdate"       "cp_flag"      "strike_price" "best_bid"     "best_offer"
# [7] "delta"        "midpoint"     "spread"       "dte"
n2 <- new.2020[, c("quotedate", "expiration", "type", "strike", "bid", "ask", "delta")]
n2w <- new.2020w[, c("quotedate", "expiration", "type", "strike", "bid", "ask", "delta")]
colnames(n2) <- colnames(d1)[1:ncol(n2)]
colnames(n2w) <- colnames(d1)[1:ncol(n2w)]
#
n2$date <- gsub('-','',as.character(as.Date(n2$date, format='%m/%d/%Y')))
n2$exdate <- gsub('-', '', as.character(as.Date(n2$exdate, format='%m/%d/%Y')))
n2$cp_flag[n2$cp_flag=='call'] <- 'C'
n2$cp_flag[n2$cp_flag=='put'] <- 'P'
n2$midpoint <- (n2$best_bid + n2$best_offer) /2
n2$spread <- abs(n2$best_bid - n2$best_offer)
#
n2w$date <- gsub('-','',as.character(as.Date(n2w$date, format='%m/%d/%Y')))
n2w$exdate <- gsub('-', '', as.character(as.Date(n2w$exdate, format='%m/%d/%Y')))
n2w$cp_flag[n2w$cp_flag=='call'] <- 'C'
n2w$cp_flag[n2w$cp_flag=='put'] <- 'P'
n2w$midpoint <- (n2w$best_bid + n2w$best_offer) /2
n2w$spread <- abs(n2w$best_bid - n2w$best_offer)
#
uexp1 <- unique(n2$exdate)
uexpw <- unique(n2w$exdate)

# calculate dte
date.int <- as.integer(as.Date(n2$date, format='%Y%m%d'))
exdate.int <- as.integer(as.Date(n2$exdate, format='%Y%m%d'))
n2$dte <- exdate.int - date.int

date.int <- as.integer(as.Date(n2w$date, format='%Y%m%d'))
exdate.int <- as.integer(as.Date(n2w$exdate, format='%Y%m%d'))
n2w$dte <- exdate.int - date.int
# need to merge
n2$uuid <- paste0(n2$date, n2$exdate, n2$cp_flag, n2$strike_price)
n2w$uuid <- paste0(n2w$date, n2w$exdate, n2w$cp_flag, n2w$strike_price)
length(intersect(n2$uuid, n2w$uuid))
dups <- n2w$uuid[n2w$uuid %in% n2$uuid]
n2[n2$uuid==dups[1], ]
n2w[n2w$uuid==dups[1], ]
# there are some duplicates, with different midpoints (but still close)
# I can just take the mean of these in the acast step I guess
n2 <- rbind(n2, n2w)
n2 <- n2[, colnames(n2) != "uuid"]
# filter to reasonable dte
max_dte <- 120
n2 <- n2[n2$dte <=max_dte, ]

rm(new.2020)
rm(new.2020w)
rm(n2w)

# done with the 2019 data
# n2.compare <- n2
# d1.compare <- d1[d1$date %in% unique(n2$date),]
# n2.compare$uuid <- paste0(n2.compare$date, n2.compare$exdate, n2.compare$cp_flag, n2.compare$strike_price)
# d1.compare$uuid <- paste0(d1.compare$date, d1.compare$exdate, d1.compare$cp_flag, d1.compare$strike_price)
# d1.compare <- d1.compare[!duplicated(d1.compare$uuid),]
# rownames(n2.compare) <- n2.compare$uuid
# rownames(d1.compare) <- d1.compare$uuid
# common.uuids <- intersect(n2.compare$uuid, d1.compare$uuid)
# length(common.uuids)
# n2.common <- n2.compare[common.uuids, ]
# d1.common <- d1.compare[common.uuids, ]
#
# plot.df <- data.frame(uuid=common.uuids, n2.mid=n2.common$midpoint, n2.spread=n2.common$spread, d1.mid=d1.common$midpoint, d1.spread=d1.common$spread)
# # midpoint is almost perfectly correlated, spread has more variance
# # but if the mid is good, we're good to go
# ggplot(plot.df) + geom_point(aes(x=d1.mid, y=n2.mid), alpha=0.1)
# ggplot(plot.df) + geom_point(aes(x=d1.spread, y=n2.spread), alpha=0.1)
# cor(plot.df$n2.mid, plot.df$d1.mid)
# cor(plot.df$n2.mid, plot.df$d1.mid, method='spearman')
# cor(plot.df$n2.spread, plot.df$d1.spread)
# cor(plot.df$n2.spread, plot.df$d1.spread, method='spearman')
# summary(lm(n2.mid~d1.mid, data=plot.df))
# summary(lm(n2.spread~d1.spread, data=plot.df))

# add to d1
d1 <- rbind(d1, n2)
rm(n2)
# should convert everything to the same date format...
d1$date <- as.Date(d1$date, format='%Y%m%d')
d1$exdate <- as.Date(d1$exdate, format='%Y%m%d')

trade.dates.d1 <- unique(d1$date)
trade.dates.dat <- unique(dat$date)
setdiff(trade.dates.d1, trade.dates.dat)
uniq.dates <- trade.dates.d1

# some expdates are on Saturdays - this shouldnt happen2
exps <- unique(d1$exdate)
bad.exps <- exps[!(exps %in% uniq.dates)]
# some of the dates are 2021, thats fine
bad.exps <- as.Date(grep("2021", bad.exps, value=T, invert = T))
length(exps)
length(bad.exps)
bad.exps.minus1 <- bad.exps -1
bad.exps.minus1.bad <- bad.exps.minus1[!(bad.exps.minus1 %in% uniq.dates)]
bad.exps.minus2 <- bad.exps.minus1.bad -1
all(bad.exps.minus2 %in% uniq.dates)
# so need to decrease by 1 or two days
decrease.by.1 <- bad.exps[(bad.exps.minus1 %in% uniq.dates)]
decrease.by.2 <- bad.exps[!(bad.exps.minus1 %in% uniq.dates)]

# Not even that painful to do
dc1.index <- d1$exdate %in% decrease.by.1
d1$exdate[dc1.index] <- d1$exdate[dc1.index] -1
dc2.index <- d1$exdate %in% decrease.by.2
d1$exdate[dc2.index] <- d1$exdate[dc2.index] -2
# FIXED!
unique(d1$exdate[!(d1$exdate %in% uniq.dates)])
# but keep them as character?
d1$date <- as.character(d1$date)
d1$exdate <- as.character(d1$exdate)
# save some memory
d1 <- d1[, c("date", "exdate", "cp_flag", "strike_price", "midpoint")]

# what's the best way to access this - list of arrays for each date
arr.list <-lapply(split.data.frame(d1, d1$date), function(x) {
    acast(x, cp_flag~exdate~strike_price, value.var = 'midpoint', fun.aggregate = mean)
})
names(arr.list) <- uniq.dates
saveRDS(arr.list, '~/option_data/arr_list_2021-01-03.rds')
rm(d1)
# goddamn memory
gc()
}

# loading the processed date
arr.list <- readRDS('~/option_data/arr_list_2021-01-03.rds')
# data on stock prices, has SPX, VIX, SPY, UPRO
dat <- read.table('~/option_data/using_stock_prices.tsv', sep='\t', quote='', header=T)
uniq.dates <- sort(unique(names(arr.list)))
# so now we have an easy way to lookup mid prices for each option
# date, P/C, expdate, strike
heatmap.2(log10(arr.list[[5000]]["P",,]), Rowv = NA, Colv=NA, trace='none', col=viridis(128), margins=c(3,6))
this.date <- '2020-12-15'
this.spx <- dat[this.date, "SPX"]
heatmap.2(log10(arr.list[["2020-12-15"]]["P",,]), Rowv = NA, Colv=NA, trace='none', col=viridis(128), main=paste('Puts', this.date, this.spx), margins=c(3,6))

# function to pick the best date from a list of expiratons
# MUST BE AT LEAST DTE AWAY
get_best_tenor <- function(trade.date, tenors, dte){
    diffs <- as.integer(as.Date(tenors)) - as.integer(as.Date(trade.date))
    # ensure theres something thats greater than dte
    if(any(diffs>dte)){
        return(tenors[diffs>dte][1])
    } else {
        return(tenors[length(tenors)])
    }
    # old version just picked min diff
    # return(tenors[which.min(abs(diffs - dte))])
}

# get closest strike with a non-na midpoint price for this day
# pct is percentage of current SPX close to select the strike
get_best_strike <- function(trade.date, strikes, pct){
    spx.close <- dat[trade.date, "SPX"]
    diffs <- strikes - (spx.close * pct)
    return(strikes[which.min(abs(diffs))])
}


# function to return everything we need when trade is made
get_trade_params <- function(trade.date, dte, pct.mod){
    # get available expdates
    tenors <- dimnames(arr.list[[trade.date]])[[2]]
    strikes <- as.numeric(dimnames(arr.list[[trade.date]])[[3]])
    # test for the best one
    bt <- get_best_tenor(trade.date, tenors, dte)
    # some option expirations are listed on weekends.
    # if for some reason this date is not a valid trade date,
    # print(paste('BT: ', bt))
    # strikes must be non-na for puts and calls for thsi tenor
    strikes.puts <- strikes[!is.nan(arr.list[[trade.date]]['P', bt, ])]
    strikes.calls <- strikes[!is.nan(arr.list[[trade.date]]['C', bt, ])]
    strikes.final <- intersect(strikes.puts, strikes.calls)

    # when we make a trade, select one tenor
    # and 4 strikes
    pct.put.long <- 1 + pct.mod
    pct.put.short <- 1 - pct.mod
    pct.call.long <- 1 + pct.mod
    pct.call.short <- 1

    strike.put.long <- get_best_strike(trade.date, strikes.final, pct.put.long)
    strike.put.short <- get_best_strike(trade.date, strikes.final, pct.put.short)
    strike.call.long <- get_best_strike(trade.date, strikes.final, pct.call.long)
    strike.call.short <- get_best_strike(trade.date, strikes.final, pct.call.short)

    # check to make sure the spread strikes are not the same - if so, space them out
    if (strike.put.long==strike.put.short){
        strike.put.long <- strikes.final[which(strikes.final == strike.put.long) + 1]
    }
    if (strike.call.long==strike.call.short){
        strike.call.long <- strikes.final[which(strikes.final == strike.call.long) + 1]
    }

    # also calculate max loss and max gain, to help with the allocations
    # max loss if all puts exprire wothless and the calls are ITM
    # max gain if all calls OTM and puts all ITM
    max.loss <- strike.call.long - strike.call.short
    max.gain <- strike.put.long - strike.put.short
    return(list(tenor=bt, strike.put.long=strike.put.long,
             strike.put.short=strike.put.short,
             strike.call.long=strike.call.long,
             strike.call.short=strike.call.short,
             max.loss=max.loss, max.gain=max.gain))
}

# quickly get spread value on a given day, with the params
get_spread_value <- function(this.date, params, put.multiple=1, call.multiple=1){
    # ensure this strike and exp is present in the date, theres some msissing in early date
    tryCatch({
        mid.put.long <- arr.list[[this.date]]['P', params[['tenor']], as.character(params[['strike.put.long']])]
        mid.put.short <- arr.list[[this.date]]['P', params[['tenor']], as.character(params[['strike.put.short']])]
        mid.call.long <- arr.list[[this.date]]['C', params[['tenor']], as.character(params[['strike.call.long']])]
        mid.call.short <- arr.list[[this.date]]['C', params[['tenor']], as.character(params[['strike.call.short']])]
        current.price <- (mid.put.long * put.multiple) -
        (mid.put.short * put.multiple) +
        (mid.call.long * call.multiple) -
        (mid.call.short * call.multiple)
        # short.values <- c(mid.put.short, mid.call.short)
        # long.values <- c(mid.put.long, mid.call.long)
        # current.price <- sum(long.values) - sum(short.values)
        return(current.price)
    }, error=function(e) {
        return(NaN)
    })
}

#function to get the value of the spread at expiration, based on the strikes
get_value_at_expiration <- function(this.date, params, put.multiple=1, call.multiple=1) {
    spx.close <- dat[this.date, 'SPX']
    # value for each option, 0 if otm
    put.long.value <- max(0, as.numeric(params[['strike.put.long']]) - spx.close) * put.multiple
    put.short.value <- max(0, as.numeric(params[['strike.put.short']]) - spx.close) * put.multiple
    call.long.value <- max(0, spx.close - as.numeric(params[['strike.call.long']])) * call.multiple
    call.short.value <- max(0, spx.close - as.numeric(params[['strike.call.short']])) * call.multiple
    exp.value <- put.long.value - put.short.value + call.long.value - call.short.value
    return(exp.value)
}

get_portfolios <- function(vix.fixed.allocation){
    # going to track these portfolios
    # 1) SPX alone
    # 2) SPX + VIX
    # 3) SPX + VIX (sd+)
    # 4) SPX + opt
    # 5) SPX + opt (sd+)
    # 6) UPRO
    # 7) UPRO + VIX (sd+)
    # 8) UPRO + opt (sd+)
    # allocation percentages in normal times
    # rows are spx, upro, vix, opt
    portfolio.allocations.vixlow <- matrix(c(1,0,0,0,
                                             1-vix.fixed.allocation,0,vix.fixed.allocation,0,
                                             1-vix.fixed.allocation,0,vix.fixed.allocation,0,
                                             1-vix.fixed.allocation,0,0,vix.fixed.allocation,
                                             1-vix.fixed.allocation,0,0,vix.fixed.allocation,
                                             0,1,0,0,
                                             0,1-vix.fixed.allocation,vix.fixed.allocation,0,
                                             0,1-vix.fixed.allocation,0,vix.fixed.allocation),
                                           nrow = 4, dimnames = list(c('SPX','UPRO', 'VIX', 'OPT'), c('SPX', 'VIX', 'VIXsd', 'OPT', 'OPTsd', 'UPRO', 'UPROvixsd', 'UPROoptsd')))
    portfolio.allocations.vixhigh <- matrix(c(1,0,0,0,
                                              1-vix.fixed.allocation,0,vix.fixed.allocation,0,
                                              1,0,0,0,
                                              1-vix.fixed.allocation,0,0,vix.fixed.allocation,
                                              1,0,0,0,
                                              0,1,0,0,
                                              0,1,0,0,
                                              0,1,0,0),
                                            nrow = 4, dimnames = list(c('SPX','UPRO', 'VIX', 'OPT'), c('SPX', 'VIX', 'VIXsd', 'OPT', 'OPTsd', 'UPRO', 'UPROvixsd', 'UPROoptsd')))
    return(list(portfolio.allocations.vixhigh=portfolio.allocations.vixhigh,
                portfolio.allocations.vixlow=portfolio.allocations.vixlow))
}

# this.date <- '2020-10-01'
# this.date <- '2020-11-05'
######################################
# Fixed params #######################
######################################
vix.mean <- mean(dat[, "VIX"])
vix.sd <- sd(dat[, "VIX"])
vix.threshold <- vix.mean + vix.sd
starting.balance <- 100000
current.balance <- starting.balance
# how many put spreads vs call spreads do we do.
put.multiple <- 1
call.multiple <- 1


######################################
# Variable params ####################
######################################
starting.i <- 1
this.date <- uniq.dates[starting.i]
dte.open <- 30
dte.close <- 0
params <- NULL
vix.fixed.allocation <- 0.1
# how far ITM/OTM are the options?
pct.mod <- 0.1
# get portfolios
portfolios <- get_portfolios(vix.fixed.allocation)
portfolio.allocations.vixhigh <- portfolios[['portfolio.allocations.vixhigh']]
portfolio.allocations.vixlow <- portfolios[['portfolio.allocations.vixlow']]
# which portfolios change between vix high and low?
n.portfolio <- ncol(portfolio.allocations.vixlow)
n.allocations <- nrow(portfolio.allocations.vixlow)
change.portfolios <- !sapply(1:n.portfolio, function(x) identical(portfolio.allocations.vixhigh[,x], portfolio.allocations.vixlow[,x]))
use.dates <- uniq.dates[starting.i:length(uniq.dates)]
portfolio.values <- matrix(0, nrow=length(use.dates), ncol=n.portfolio, dimnames=list(as.character(uniq.dates), colnames(portfolio.allocations.vixlow)))
portfolio.values[this.date, ] <- rep(starting.balance, times=ncol(portfolio.values))

if (is.null(params)){
    spx.close <- dat[this.date, "SPX"]
    vix.close <- dat[this.date, "VIX"]
    upro.close <- dat[this.date, "UPRO"]

    # get the option trade stats
    params <- get_trade_params(this.date, dte.open, pct.mod)
    # print(as.character(params))
    current.value <- get_spread_value(this.date, params, put.multiple, call.multiple)
    # get max loss and gain after the purchase price
    max.loss <- params[['max.loss']] + current.value
    max.gain <- params[['max.gain']] - current.value
    last.value <- current.value
    trade.today = TRUE
    allocate.vec <- c(spx.close, upro.close, vix.close, max.loss)
    price.vec <- c(spx.close, upro.close, vix.close, current.value)
    # allocate the initial portfolios
    if (vix.close > vix.threshold){
        use.portfolio.allocation <- portfolio.allocations.vixhigh
        in.vix.high <- T
    } else {
        use.portfolio.allocation <- portfolio.allocations.vixlow
        in.vix.high <- F
    }
    portfolio.shares <-  use.portfolio.allocation *
        matrix(portfolio.values[this.date, ], ncol=n.portfolio, nrow=n.allocations, byrow = T) /
        matrix(price.vec, ncol = n.portfolio, nrow=n.allocations)
    portfolio.value.today <- colSums(portfolio.shares * price.vec)
    portfolio.values[this.date, ] <- portfolio.value.today
}

for (this.date in uniq.dates[(starting.i + 1):length(uniq.dates)]){
# for (this.date in uniq.dates[(starting.i + 1):length(uniq.dates)][1:120]){
    spx.close <- dat[this.date, "SPX"]
    vix.close <- dat[this.date, "VIX"]
    upro.close <- dat[this.date, "UPRO"]

    current.dte <- as.integer(as.Date(params[['tenor']])) - as.integer(as.Date(this.date))
    # custom get value at expiration
    if(current.dte <= 0){
        current.value <- get_value_at_expiration(this.date, params, put.multiple, call.multiple)
    } else {
        current.value <- get_spread_value(this.date, params, put.multiple, call.multiple)
        # check for nan
        if (is.nan(current.value)){ current.value <- last.value }
    }
    # accounting value today
    price.vec <- c(spx.close, upro.close, vix.close, current.value)
    portfolio.value.today <- colSums(portfolio.shares * price.vec)
    portfolio.values[this.date, ] <- portfolio.value.today
    save.last.value <- last.value

    # do we exit the hedge AS SOON AS vix gets above threshold?
    # I think so, rather than waiting for the options to expire
    # so put a vix check higher
    # if in vix high, check if below because we can then open a new hedge
    if (in.vix.high & (vix.close<vix.threshold)){
        in.vix.high <- F
        # open a new hedged trade
        trade.today <- TRUE
        use.portfolio.allocation <- portfolio.allocations.vixlow
        print(paste(this.date, 'TRADE - entering VIX low from HIGH'))
        params <- get_trade_params(this.date, dte.open, pct.mod)
        # print(as.character(params))
        current.value <- get_spread_value(this.date, params, put.multiple, call.multiple)
        # print(current.value)
        max.loss <- params[['max.loss']] + current.value
        max.gain <- params[['max.gain']] - current.value
        last.value <- current.value
        allocate.vec <- c(spx.close, upro.close, vix.close, max.loss)
        price.vec <- c(spx.close, upro.close, vix.close, current.value)
        portfolio.shares <-  use.portfolio.allocation *
            matrix(portfolio.values[this.date, ], ncol=n.portfolio, nrow=n.allocations, byrow = T) /
            matrix(price.vec, ncol = n.portfolio, nrow=n.allocations)
        print(portfolio.shares)

    } else if (in.vix.high & vix.close>vix.threshold){
        # continue unhedged
        trade.today = FALSE
    } else if (!in.vix.high & (vix.close<vix.threshold)){
        # continue hedged trade, open a new one if necessary
        if(current.dte <= dte.close){
            trade.today <- TRUE
            print(paste(this.date, 'TRADE - VIX low environment'))
            use.portfolio.allocation <- portfolio.allocations.vixlow
            params <- get_trade_params(this.date, dte.open, pct.mod)
            current.value <- get_spread_value(this.date, params, put.multiple, call.multiple)
            last.value <- current.value
            max.loss <- params[['max.loss']] + current.value
            max.gain <- params[['max.gain']] - current.value
            allocate.vec <- c(spx.close, upro.close, vix.close, max.loss)
            price.vec <- c(spx.close, upro.close, vix.close, current.value)
            print(paste('CV:', current.value, "ML:", max.loss))
            portfolio.shares <-  use.portfolio.allocation *
                matrix(portfolio.values[this.date, ], ncol=n.portfolio, nrow=n.allocations, byrow = T) /
                matrix(price.vec, ncol = n.portfolio, nrow=n.allocations)
            print(portfolio.shares)

        } else {
            trade.today = FALSE
        }

    } else if (!in.vix.high & (vix.close>vix.threshold)){
        # Transition to vix high, new unhedged trade
        in.vix.high <- T
        trade.today <- TRUE
        print(paste(this.date, 'TRADE - entering VIX HIGH from LOW'))
        use.portfolio.allocation <- portfolio.allocations.vixhigh
        price.vec <- c(spx.close, upro.close, vix.close, current.value)
        allocate.vec <- c(spx.close, upro.close, vix.close, max.loss)

        # we need to keep the option portfolio the same wihout modifying or rebalancing it
        # so only change the portfolios that are different between vixhigh and vixlow?
        portfolio.shares.new <-  use.portfolio.allocation *
            matrix(portfolio.values[this.date, ], ncol=n.portfolio, nrow=n.allocations, byrow = T) /
            matrix(price.vec, ncol = n.portfolio, nrow=n.allocations)
        portfolio.shares[, change.portfolios] <- portfolio.shares.new[,change.portfolios]
        # only need new options if at expiration
        if(current.dte <= dte.close){
            params <- get_trade_params(this.date, dte.open, pct.mod)
            current.value <- get_spread_value(this.date, params, put.multiple, call.multiple)
            max.loss <- params[['max.loss']] + current.value
            max.gain <- params[['max.gain']] - current.value
            allocate.vec <- c(spx.close, upro.close, vix.close, max.loss)
            portfolio.shares <-  use.portfolio.allocation *
                matrix(portfolio.values[this.date, ], ncol=n.portfolio, nrow=n.allocations, byrow = T) /
                matrix(allocate.vec, ncol = n.portfolio, nrow=n.allocations)
        }
        print(portfolio.shares)
    }
    if(any(portfolio.shares <0 | is.na(portfolio.shares))){
        print(params)
        print(current.value)
        print(price.vec)
        stop('NEGATIVE SHARES?')}
    last.value <- current.value
}


portfolio.values.df <- as.data.frame(portfolio.values)
portfolio.values.df$date <- as.Date(rownames(portfolio.values.df))
rownames(portfolio.values.df) <- portfolio.values.df$date
pvm <- melt(portfolio.values.df, id.vars = "date")

ggplot(pvm) +
    geom_line(aes(x=date, y=value/100000, col=variable)) +
    scale_x_date() + scale_y_continuous() + theme_bw() + scale_y_log10()
ggplot(pvm) +
    geom_line(aes(x=date, y=value/100000, col=variable)) +
    scale_x_date() + scale_y_continuous() + theme_bw()

# pvm.05 <- pvm
# pvm.01 <- pvm

# plot just the SPX stuff
plot.portfolios <- c('SPX', 'OPT', 'OPTsd')
ggplot(pvm[pvm$variable%in%plot.portfolios,]) +
    geom_line(aes(x=date, y=value, col=variable), size=1) +
    scale_x_date() + scale_y_continuous() + theme_bw() +
    ggtitle('Hedged SPX portfolios') + xlab('Date') + ylab('Value') +
    scale_y_log10()

plot.portfolios <- c('SPX', 'UPRO', 'UPROvixsd', 'UPROoptsd')
ggplot(pvm[pvm$variable%in%plot.portfolios,]) +
    geom_line(aes(x=date, y=value, col=variable), size=1) +
    scale_x_date() + scale_y_continuous() + theme_bw()
ggplot(pvm[pvm$variable%in%plot.portfolios,]) +
    geom_line(aes(x=date, y=value, col=variable), size=1) +
    scale_x_date() + scale_y_log10() + theme_bw() +
    ggtitle('Hedged UPRO portfolios') + xlab('Date') + ylab('Value') +
    scale_y_log10()


pvx <- xts(portfolio.values.df[,1:n.portfolio], order.by = portfolio.values.df$date)
pvxr <- Return.calculate(pvx)
pvxm <- to.period(pvx, period = "months", OHLC = F)
pvxmr <- (Return.calculate(to.period(pvx, period='months', OHLC=F)))
SharpeRatio(pvxmr,annualize = T)
CalmarRatio(pvxmr)
SterlingRatio(pvxmr)
maxDrawdown(pvxmr)
chart.Drawdown(pvxmr,legend.loc = "bottomright")
StdDev.annualized(pvxmr)
# calculate CAGR
total.days <- as.integer(as.Date(uniq.dates[length(uniq.dates)]) - as.Date(uniq.dates[1]))
CAGR <- (portfolio.values.df[nrow(portfolio.values.df),1:n.portfolio] / portfolio.values.df[1,1:n.portfolio]) ^ (365/total.days) -1
print(CAGR*100)


# write out a version of this to use with portfoliovisualizer
write.table(cbind(portfolio.values.df$date, as.data.frame(pvxr[, "OPT"]))[2:nrow(pvxr),], '~/option_data/portfolio_exports/OPT.csv', col.names = F, row.names = F, quote = F, sep=',')
write.table(portfolio.values.df[, c("date", "OPT")], '~/option_data/portfolio_exports/OPT_values.csv', col.names = F, row.names = F, quote = F, sep=',')
# write.table(cbind(portfolio.values.df$date, as.data.frame(pvxr[, "OPT"]))[2:nrow(pvxr),], '~/option_data/portfolio_exports/OPT.csv', col.names = F, row.names = F, quote = F, sep=',')
write.table(portfolio.values.df[, c("date", "OPTsd")], '~/option_data/portfolio_exports/OPTsd_values.csv', col.names = F, row.names = F, quote = F, sep=',')
write.table(portfolio.values.df[, c("date", "SPX")], '~/option_data/portfolio_exports/SPX_values.csv', col.names = F, row.names = F, quote = F, sep=',')
write.table(portfolio.values.df[, c("date", "UPROoptsd")], '~/option_data/portfolio_exports/UPROoptstd_values.csv', col.names = F, row.names = F, quote = F, sep=',')

# metrics calculated from these portfolios
# sharpe of just spx in this time period
datx <- xts(dat[,2:3], order.by = dat$date)
datxm <- to.period(datx, period='months', OHLC=F)
datxmr <- Return.calculate(datxm)
SharpeRatio(datxmr)

colnames(tracking.df) <- c('date',
                           'tenor', 'strike.put.long', 'strike.put.short',
                           'strike.call.long', 'strike.call.short', 'trade.today',
                           'last.value', 'current.value', 'day.pl', 'current.balance')
head(tracking.df)
tracking.df$current.value <- as.numeric(tracking.df$current.value)
tracking.df$last.value <- as.numeric(tracking.df$last.value)
tracking.df$current.balance <- as.numeric(tracking.df$current.balance)
tracking.df$day.pl <- as.numeric(tracking.df$day.pl)
tracking.df$date2 <- as.Date(tracking.df$date, format="%Y%m%d")
tracking.df$SPX <- dat[tracking.df$date, 'SPX']
tracking.df$VIX <- dat[tracking.df$date, 'VIX']
# percent returns
tracking.df$day.pl.pct <- (tracking.df$current.value - tracking.df$last.value) / abs(tracking.df$last.value)

# write.table(tracking.df, '~/option_data/spx_vix_portfolio_stats.tsv', sep='\t', quote=F, row.names=F, col.names=T)

ggplot(tracking.df) +
    geom_line(aes(x=date2, y=as.numeric(current.balance))) +
    scale_x_date() + scale_y_continuous() + theme_bw()

ggplot(tracking.df) +
    geom_line(aes(x=date2, y=as.numeric(current.value) / as.numeric(tracking.df[1,'current.value'])), col='black') +
    geom_line(aes(x=date2, y=SPX/ tracking.df[1,'SPX']), col='firebrick') +
    geom_line(aes(x=date2, y=VIX/ tracking.df[1,'VIX']), col='steelblue') +
    scale_x_date() + scale_y_continuous() + theme_bw()



























# function to track spread value over time
# two legs only for now, first is long second is short
# returns dataframe with price details for each date of this spread
track_spread_price <- function(uuid_long, uuid_short){
    long.df <- d1s2[d1s2$uuid==uuid_long, ]
    short.df <- d1s2[d1s2$uuid==uuid_short, ]
    long.df <- long.df[order(long.df$date), ]
    short.df <- short.df[order(short.df$date), ]
    if (all(long.df$date %in% short.df$date) & all(short.df$date %in% long.df$date)){
        res.df <- data.frame(date=long.df$date, long.mid = long.df$midpoint, short.mid= -short.df$midpoint,
                             price=long.df$midpoint - short.df$midpoint, dte=long.df$dte)
        return(res.df)
    } else {
        common.dates <- intersect(long.df$date, short.df$date)
        stop("TODO")
    }
}

# get the price of the spread on a single day
get_spread_price <- function(uuid_long, uuid_short, date){
    return(d1s2[d1s2$uuid==uuid_long & d1s2$date==date, "midpoint"] -
               d1s2[d1s2$uuid==uuid_short & d1s2$date==date, "midpoint"])
}


# ITM-OTM spread, 5% on each leg
# we buy the ITM put (first leg),
# sell the otm put (second leg)
trade_date <- as.Date("2013-12-13")
dte <- 30
ps1 <- select_put_spread(trade_date, dte, -0.05, 0.05 )
put.uuid.long <- ps1[1, 'uuid']
put.uuid.short <- ps1[2, 'uuid']
ps1.price <- track_spread_price(put.uuid.long, put.uuid.short)
ps1.price$SPX <- datx[ps1.price$date, 'SPX']
# for calls, we're short the ATM and long the OTM
cs1 <- select_call_spread(trade_date, dte, 0.05, 0)
call.uuid.long <- cs1[1, 'uuid']
call.uuid.short <- cs1[2, 'uuid']
cs1.price <- track_spread_price(call.uuid.long, call.uuid.short)

ggplot(ps1.price) + geom_point(aes(x=-dte, y=price)) + theme_bw() + ylim(c(0, max(ps1.price$price)))
ggplot(cs1.price) + geom_point(aes(x=-dte, y=-price)) + theme_bw() + ylim(c(0, max(-cs1.price$price)))
ps1.price$combined.price <- ps1.price$price + cs1.price$price
ggplot(ps1.price) + geom_point(aes(x=-dte, y=combined.price)) + theme_bw() + geom_line(aes(x=-dte, y=SPX-1800))

# P/L for the whole spread going to exp
pl <- ps1.price$combined.price[nrow(ps1.price)] - ps1.price$combined.price[1]

# now we need the main portfolio construction
# for each trade date, decide if we need to open a trade
# close a trade, or do nothing.
# record the price of the spreads and the action taken on each
# as well as the 4 options held at each point in time.
all.trade.dates <- unique(d1s2$date)
start_i <- 1
dte.target <- 30
put.otm.targets <- c(-0.05, 0.05)
call.otm.targets <- c(0.05, 0)
current.date <- all.trade.dates[start_i]
current.put.spread <- NULL
current.call.spread <- NULL
initial.value <- 1000
current.value <- initial.value
tracking.df <- data.frame()
track.value <- rep(0,length(all.trade.dates))

# base case - get the first spread
if (is.null(current.call.spread)|is.null(current.put.spread)){
    print('INITIAL')
    trade.today = T
    current.put.spread <- select_put_spread(current.date, dte.target, put.otm.targets[1], put.otm.targets[2])
    current.call.spread <- select_call_spread(current.date, dte.target, call.otm.targets[1], call.otm.targets[2])
    initial.put.spread.price <- get_spread_price(current.put.spread["long", "uuid"], current.put.spread["short", "uuid"], current.date)
    initial.call.spread.price <- get_spread_price(current.call.spread["long", "uuid"], current.call.spread["short", "uuid"], current.date)
    current.put.spread.price <- initial.put.spread.price
    current.call.spread.price <- initial.call.spread.price

    initial.price <- initial.put.spread.price + initial.call.spread.price
    last.price <- initial.price
    current.price <- initial.price
}

for (i in start_i:length(all.trade.dates)){
# for (i in start_i:(start_i + 75)){
    last.price <- current.price
    current.date <- all.trade.dates[i]
    print(paste(i, current.date))
    # print(paste0('DTE: ',current.put.spread[1, "exdate"]))
    # main loop
    # do we need to trade today?
    # if any of the dte=0
    # do accounting first, then do trade
    trade.today <- F
    current.put.spread.price <- get_spread_price(current.put.spread["long", "uuid"], current.put.spread["short", "uuid"], current.date)
    current.call.spread.price <- get_spread_price(current.call.spread["long", "uuid"], current.call.spread["short", "uuid"], current.date)
    current.price <- current.put.spread.price + current.call.spread.price
    # error correction - if cant calculate current spread price (because of missing data, use the last spread price)
    if (length(current.price)==0){
        current.price <- last.price
    }
    pl.initial <- current.price - initial.price
    pl.day <- current.price  - last.price
    spread.expiration <- current.put.spread[1, "exdate"]
    if ((spread.expiration - current.date) <= 2) {
        # initiate a trade
        # print('TRADE')
        trade.today = T
        last.put.spread <- current.put.spread
        last.call.spread <- current.call.spread
        # get new spreads
        current.put.spread <- select_put_spread(current.date, dte.target, put.otm.targets[1], put.otm.targets[2])
        current.call.spread <- select_call_spread(current.date, dte.target, call.otm.targets[1],call.otm.targets[2])

        initial.put.spread.price <- get_spread_price(current.put.spread["long", "uuid"], current.put.spread["short", "uuid"], current.date)
        initial.call.spread.price <- get_spread_price(current.call.spread["long", "uuid"], current.call.spread["short", "uuid"], current.date)
        current.put.spread.price <- initial.put.spread.price
        current.call.spread.price <- initial.call.spread.price

        initial.price <- initial.put.spread.price  + initial.call.spread.price
        current.price <- initial.price
    }
    # iterate
    current.value <- current.value + pl.day
    tracking.df <- rbind(tracking.df,
        c(as.character(current.date), as.character(spread.expiration), trade.today,
        current.price, current.put.spread.price, current.call.spread.price,
        current.put.spread["long", "uuid"], current.put.spread["short", "uuid"],
        current.call.spread["long", "uuid"], current.call.spread["short", "uuid"],
        pl.day, current.value
        ))
    # track.value[i] <- current.value

}

colnames(tracking.df) <- c('date', 'spread.expiration', 'trade.today',
                           'combined.spread.price', 'put.spread.price', 'call.spread.price',
                           'long.put.uuid', 'short.put.uuid', 'long.call.uuid', 'short.call.uuid',
                           'pl.day', 'current.value')
write.table(tracking.df, '~/option_data/spx_vix_portfolio_stats.tsv', sep='\t', quote=F, row.names=F, col.names=T)

# dataframe that tracks what happens each day
    # long put uuid, short put uuid, short call uuid, long call uuid
    # trade initated today
    # price of each spread

# if we treat this like a portfolio to be optimized, just like the SPX or VIX data


plot(1:nrow(tracking.df), tracking.df$current.value, type='l')
ggplot(tracking.df) +
    geom_line(aes(x=as.Date(date), y=as.numeric(current.value))) +
    scale_x_date() + scale_y_continuous()


# there's stil some bugs resutling in NA trades
# definitely underperforms, even with adaptive allocation
# do the model better, something with a set of matrices or something
