# Vix thresholding, and transition states
# how often do we go from low to high and vice versa?
# how helpful are these thresholds for determining when vix will spike?
# any other signals that can be used to determine when it's a good time to hedge?
library(ggplot2)
library(xts)
library(reshape2)
library(PerformanceAnalytics)

# price data for all instuments
price.df <- read.table('~/option_data/vxth_project/all_prices.tsv', sep='\t', quote='', header=T,
                       colClasses = c('Date', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
rownames(price.df) <- as.character(price.df$date)

price.xts <- xts(price.df[,2:ncol(price.df)],order.by = price.df$date)

# VIX over the history
ggplot(price.df, aes(x=date, y=VIX)) +
    geom_line() +
    theme_bw() +
    labs(title='VIX prices') +
    geom_hline(yintercept = 15, col='blue') +
    geom_hline(yintercept = 30, col='purple') +
    geom_hline(yintercept = 50, col='firebrick')

# calculate which regime the time series is in
vix.thresh <- c(15,30,50)
price.df$VIX.regime <- sapply(price.df$VIX, function(x) sum(x > vix.thresh))

# keep only times that it transitions
mydf <-data.frame(x = price.df$VIX.regime[1:(length(price.df$VIX.regime)-1)],
                  y = price.df$VIX.regime[2:length(price.df$VIX.regime)])
table(mydf)


# calculate some metrics of returns of options of various delta and tenor
# so is this just picking an option and looking at the returns over time?
#   how often does an option hit a monetization threshold?
# start with 30 day, 30 delta
mid.list <- readRDS('~/option_data/vxth_project/price_list_2021-01-11.rds')
delta.list <- readRDS('~/option_data/vxth_project/delta_list_2021-01-11.rds')
source('~/projects/vix_hedge/VXTH_replication/vixth_functions.R')

dte <- 120
delta <- 0.1
trade.dates <- as.character(price.df$date[1:(nrow(price.df)-dte+1)])

# get best option for each trade date
best.options <- lapply(trade.dates, function(x){
    unlist(get_trade_params(as.character(x), dte, delta))
})
best.options.df <- data.frame(do.call(rbind, best.options))
best.options.df$date <- trade.dates
best.options.df$optid <- paste(best.options.df$tenor, best.options.df$strike, "C", sep='_')
# Vix today
best.options.df$VIX <- price.df[best.options.df$date, "VIX"]

# unqiue version
bo.uniq <- best.options.df[!duplicated(best.options.df$optid),]

# track a specific option though its lifetime
# returns a vector of midprices, with names
# corresponding to each date
track_option_price <- function(start_date, exp, strike, pc){
    start_date <- as.character(start_date)
    strike <- as.character(strike)
    exp <- as.character(exp)
    test.dates <- as.character(seq.Date(from=as.Date(start_date), to=as.Date(exp), by=1))
    dates <- test.dates[test.dates %in% names(mid.list)]
    # don't look on last day, can calculate by vix
    dates <- dates[1:(length(dates)-1)]
    prices <- sapply(dates, function(d) {
        tryCatch(mid.list[[as.character(d)]][pc, as.character(exp), as.character(strike)],
                 error=function(e) NA)
    })
    # calculate expiration
    prices <-c(prices, max(0, price.df[exp, "VIX"] - as.numeric(strike)))
    names(prices)[length(prices)] <- exp
    return(prices)
}

# get prices for all of these
price.list <- sapply(1:nrow(bo.uniq),function(i){
    start.date <- bo.uniq[i,"date"]
    exp <- bo.uniq[i,"tenor"]
    strike <- bo.uniq[i, 'strike']
    track_option_price(start.date, exp, strike, "C")
    # track_option_price(x['date'], x['tenor'], x['strike'], "C")
})
names(price.list) <- bo.uniq$date

# max returns
max_returns <- as.data.frame(sapply(price.list, function(x) max(x, na.rm = T) /x[1]))
max_returns$date <- rownames(max_returns)
colnames(max_returns) <- c('return', 'date')

head(max_returns[order(max_returns$return, decreasing = T), ])

ggplot(max_returns, aes(x=return)) +
    geom_histogram() +
    theme_bw() +
    labs(title=paste0('Returns of ', dte, ' dte, ', delta, ' delta VIX calls'))

# how many exceed fixed multiple?
w.na <- which(sapply(price.list, function(x) is.na(x[length(x)])))
n.contract <- nrow(max_returns) - length(w.na)


# how many expire worthless?
n.worthless <- sum(sapply(price.list, function(x) x[length(x)]==0), na.rm = T)
n.contract
n.worthless
n.worthless / n.contract

multiples <- c(2,5,10,20,50)
exceed <- sapply(multiples, function(x) sum(max_returns$return >=x, na.rm = T))
exceed
round(exceed / n.contract *100, 2)
