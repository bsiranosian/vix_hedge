# VIXTH backtest
library(ggplot2)
library(xts)
library(reshape2)
library(PerformanceAnalytics)


# price data for all instuments
price.df <- read.table('~/option_data/vxth_project/all_prices.tsv', sep='\t', quote='', header=T,
                       colClasses = c('Date', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
price.xts <- xts(price.df[,2:ncol(price.df)],order.by = price.df$date)
# option price array list
# this is midpoint
mid.list <- readRDS('~/option_data/vxth_project/price_list_2021-01-11.rds')
# bids
bid.list <- readRDS('~/option_data/vxth_project/price_BID_list_2021-01-11.rds')
# asks
ask.list <- readRDS('~/option_data/vxth_project/price_ASK_list_2021-01-11.rds')
delta.list <- readRDS('~/option_data/vxth_project/delta_list_2021-01-11.rds')
# vix futures
fut.df <- read.table('~/option_data/vix_futures/quandl_vix_futures_cleaned.tsv', sep='\t', quote='', header = T,
                    colClasses = c('Date', 'numeric', 'numeric', 'numeric'))
fut.xts <- xts(fut.df[,2:ncol(fut.df)],order.by = fut.df$date)
# So we have data from 2006-03-22 onward (until Dec 2019 for now)

trade.dates <- names(mid.list)


# Steps for this backtest
# portfolios: SPX, UPRO, UPRO/TMF, SPX hedge, UPRO hedge, UPRO/TMF jhedge

# first go: hedge is only adjusted once per month. This will include rebalancing of other portfolios
# second go: hedge adjusted on a daily basis, including rebalancing and deciding if VIX is in the right regime

# We can use a few assets for determining the allocation to the hedge,
# Spot VIX, VX futures (front month).

# parameters to tune:
  # when to put on the hedge (always or only in periods of vol > 15)
  # when to scale hedge down?
  # when to remove hedge completely?
  # Hedge allocation amount (at each level)
  # daily rebalancing etc (which will incur transaction costs)
# RULES for the hedge



# function to pick the best date from a list of expiratons
# MUST BE AT LEAST DTE AWAY
get_best_tenor <- function(trade.date, tenors, dte){
  diffs <- as.integer(as.Date(tenors)) - as.integer(as.Date(trade.date))
  # Picks min days away
  return(tenors[which.min(abs(diffs - dte))])
  # OTHER VERSION
  # ensure theres something thats greater than dte
  # if(any(diffs>dte)){
  #   return(tenors[diffs>dte][1])
  # } else {
  #   return(tenors[length(tenors)])
  # }
}

# get closest strike with a non-na midpoint price for this day
# pct is percentage of current SPX close to select the strike
get_best_strike <- function(trade.date, strikes, pct){
  spx.close <- dat[trade.date, "SPX"]
  diffs <- strikes - (spx.close * pct)
  return(strikes[which.min(abs(diffs))])
}


# function to return everything we need when trade is made
get_trade_params <- function(trade.date, dte, option.delta){
  # get available expdates and delta values
  tenors <- dimnames(mid.list[[trade.date]])[[2]]
  strikes <- as.numeric(dimnames(mid.list[[trade.date]])[[3]])
  # test for the best one
  bt <- get_best_tenor(trade.date, tenors, dte)
  # get available deltas for this tenor
  deltas <- delta.list[[trade.date]]["C", bt, ]
  # sometimes they're all NA! great!
  # reset by moving one exp forward
  if(all(is.na(deltas))){
    bt <- tenors[which(tenors==bt) + 1]
    deltas <- delta.list[[trade.date]]["C", bt, ]
  }
  # get the one closest to option delta
  best.strike <- names(deltas)[which.min(abs(option.delta - deltas))]

  return(list(tenor=bt, strike=best.strike))
}

# ONE MONTH FORWARD VALUE OF VIX - VX futures front month
# X <= 15      | 0.0%
# 15 > X <= 30 | 1.0%
# 30 > X <= 50 | 0.5%
# X > 50       | 0.0%

# get which regime (1-4) vx is in based on the futures value
get_vix_regime <- function(vx, vix.thresholds){
  return(as.integer(which(rowSums(vx > vix.thresholds) ==1)))
}

get_portfolios <- function(vix.allocations){
  # porfolios to test
  # SPX
  # UPRO
  # UPRO/TMF
  # SPX + VIX
  # UPRO + VIX
  # UPRO/TMF + VIX

  vix.allocations[1]
  vix.allocations[2]
  vix.allocations[3]
  vix.allocations[4]
  portfolio.allocations <- lapply(1:4, function(x) {
    matrix(c(1,0,0,0,
             0,1,0,0,
             0,0.55,.45,0,
             1-vix.allocations[x],0,0,vix.allocations[x],
             0,1-vix.allocations[x],0,vix.allocations[x],
             0,0.55-(vix.allocations[x]/2),0.45-(vix.allocations[x]/2),vix.allocations[x]),
           nrow = 4, dimnames = list(c('SPX','UPRO', 'TMF', 'VIXOPT'), c('SPX', 'UPRO', 'UPRO/TMF', 'SPX/VIXOPT', 'UPRO/VIXOPT', 'UPRO/TMF/VIXOPT')))

  })
  return(portfolio.allocations)
}


######################################
# Fixed params #######################
######################################
starting.balance <- 100000
current.balance <- starting.balance
vix.thresholds <- matrix(c(0, 15,
                           15, 30,
                           30, 50,
                           50, 100),
                         byrow = T, nrow=4, dimnames=list(1:4, c('low', 'high')))


######################################
# Variable params ####################
######################################
starting.i <- 1
this.date <- trade.dates[starting.i]
dte.open <- 35
dte.close <- 0
params <- NULL
# delta of the vix calls to buy
option.delta <- 0.3
# get portfolios
# a matrix for portfolios under the four vix levels
vix.allocations <- c(0.00, 0.01, 0.01, 0)
portfolio.allocations <- get_portfolios(vix.allocations)
daily.rebalance <- F

# which portfolios change between vix high and low?
n.portfolio <- ncol(portfolio.allocations[[1]])
n.allocations <- nrow(portfolio.allocations[[1]])

# change.portfolios <- !sapply(1:n.portfolio, function(x) identical(portfolio.allocations.vixhigh[,x], portfolio.allocations.vixlow[,x]))
use.dates <- trade.dates[starting.i:length(trade.dates)]
portfolio.values <- matrix(0, nrow=length(trade.dates), ncol=n.portfolio, dimnames=list(as.character(trade.dates), colnames(portfolio.allocations[[1]])))
portfolio.values[this.date, ] <- rep(starting.balance, times=ncol(portfolio.values))

# set up the initial params
if (is.null(params)){
  spx.close <- as.numeric(price.xts[this.date, "SPX"])
  vix.close <- as.numeric(price.xts[this.date, "VIX"])
  upro.close <- as.numeric(price.xts[this.date, "UPRO"])
  tmf.close <- as.numeric(price.xts[this.date, "TMF"])
  vx.close <- as.numeric(fut.xts[this.date, "VX1"])

  # which vix regime are we in
  vix.regime <- get_vix_regime(vx.close, vix.thresholds)
  last.vix.regime <- vix.regime
  portfolio.allocations.vix <- portfolio.allocations[[vix.regime]]

  # get the option trade stats
  params <- get_trade_params(this.date, dte.open, option.delta)
  # print(as.character(params))
  current.value <- mid.list[[this.date]]['C', params$tenor, params$strike]
  last.value <- current.value
  trade.today = TRUE

  price.vec <- c(spx.close, upro.close, tmf.close, current.value)
  # allocate the initial portfolios
  portfolio.shares <-  portfolio.allocations.vix *
    matrix(portfolio.values[this.date, ], ncol=n.portfolio, nrow=n.allocations, byrow = T) /
    matrix(price.vec, ncol = n.portfolio, nrow=n.allocations)
  portfolio.value.today <- colSums(portfolio.shares * price.vec)
  portfolio.values[this.date, ] <- portfolio.value.today
}


for (this.date in trade.dates[(starting.i + 1):length(trade.dates)]){
  # reset params
  last.vix.regime <- vix.regime
  last.value <- current.value

  # values today
  spx.close <- as.numeric(price.xts[this.date, "SPX"])
  vix.close <- as.numeric(price.xts[this.date, "VIX"])
  upro.close <- as.numeric(price.xts[this.date, "UPRO"])
  tmf.close <- as.numeric(price.xts[this.date, "TMF"])
  vx.close <- as.numeric(fut.xts[this.date, "VX1"])
  # some missing data...
  if (length(vx.close)==0){
    vix.regime <- last.vix.regime
  } else {
    vix.regime <- get_vix_regime(vx.close, vix.thresholds)
  }
  portfolio.allocations.vix <- portfolio.allocations[[vix.regime]]


  current.dte <- as.integer(as.Date(params[['tenor']])) - as.integer(as.Date(this.date))
  # custom get value at expiration
  if(current.dte <= 0){
    current.value <- max(0, vix.close - as.numeric(params$strike))
  } else {
    tryCatch(current.value <- bid[[this.date]]['C', params$tenor, params$strike],error=function(e) return(last.value))
    # check for nan
    if (is.nan(current.value)){ current.value <- last.value }
  }

  # accounting value today
  price.vec <- c(spx.close, upro.close, tmf.close, current.value)
  portfolio.value.today <- colSums(portfolio.shares * price.vec)
  portfolio.values[this.date, ] <- portfolio.value.today
  save.last.value <- last.value

  # if we want to rebalance daily and not just at expiration
  if (daily.rebalance){
    portfolio.shares <-  portfolio.allocations.vix *
      matrix(portfolio.values[this.date, ], ncol=n.portfolio, nrow=n.allocations, byrow = T) /
      matrix(price.vec, ncol = n.portfolio, nrow=n.allocations)
  }
  # only need new options if at expiration
  # for right now - only rebalancing here
  if(current.dte <= dte.close){
    params <- get_trade_params(this.date, dte.open, option.delta)
    print(paste(this.date, 'VIX:', vix.close, 'VX1', vx.close, "REGIME:", vix.regime))
    print(as.character(params))
    current.value <- ask.list[[this.date]]['C', params$tenor, params$strike]
    price.vec <- c(spx.close, upro.close, tmf.close, current.value)
    # allocate the initial portfolios
    portfolio.shares <-  portfolio.allocations.vix *
      matrix(portfolio.values[this.date, ], ncol=n.portfolio, nrow=n.allocations, byrow = T) /
      matrix(price.vec, ncol = n.portfolio, nrow=n.allocations)
    print(portfolio.shares)

  }

  if(any(portfolio.shares <0 | is.na(portfolio.shares))){
    print(params)
    print(current.value)
    print(price.vec)
    stop('NEGATIVE SHARES?')}
}


portfolio.values.df <- as.data.frame(portfolio.values)
portfolio.values.df$date <- as.Date(rownames(portfolio.values.df))
rownames(portfolio.values.df) <- portfolio.values.df$date
portfolio.values.df$VXTH <- price.df$VXTH * (starting.balance / price.df$VXTH[1])

pvd2 <- portfolio.values.df[,c(1,3,4,5,6,8,7)]
# add VXTH
# pvd2 <- portfolio.values.df[,c(1,4,7)]
pvm <- melt(portfolio.values.df, id.vars = "date")

ggplot(pvm) +
  geom_line(aes(x=date, y=value/100000, col=variable)) +
  scale_x_date() + scale_y_continuous() + theme_bw() + scale_y_log10()
ggplot(pvm) +
  geom_line(aes(x=date, y=value/100000, col=variable)) +
  scale_x_date() + scale_y_continuous() + theme_bw()


# STATSTICS
pvx <- xts(portfolio.values.df[, colnames(portfolio.values.df)!='date'], order.by = portfolio.values.df$date)
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
total.days <- as.integer(as.Date(trade.dates[length(trade.dates)]) - as.Date(trade.dates[1]))
CAGR <- (portfolio.values.df[nrow(portfolio.values.df),colnames(portfolio.values.df)!='date'] / portfolio.values.df[1,colnames(portfolio.values.df)!='date']) ^ (365/total.days) -1
print(CAGR*100)

# show some figures for the blog post
# first - replication of the index
keep.ports <- c('SPX', 'SPX/VIXOPT', 'VXTH')
pvm.keep <- pvm[pvm$variable %in% keep.ports,]
pvm.keep$variable <- as.character(pvm.keep$variable)
pvm.keep[pvm.keep$variable =='SPX', 'variable'] <- 'SPX BENCHMARK'
pvm.keep[pvm.keep$variable =='VXTH', 'variable'] <- 'VXTH BENCHMARK'
pvm.keep[pvm.keep$variable =='SPX/VIXOPT','variable'] <- 'VXTH REPLICATION'
p <- ggplot(pvm.keep) +
  geom_line(aes(x=date, y=value, col=variable), size=1) +
  scale_color_manual(values=c('SPX BENCHMARK' = 'grey70',
                              'VXTH BENCHMARK' = 'grey50',
                              'VXTH REPLICATION' = 'firebrick')) +
  scale_x_date() + scale_y_continuous() + theme_bw() +
  ggtitle('Hedged SPX portfolios') + xlab('Date') + ylab('Value (log)') +
  scale_y_log10() + labs(col = "Portfolio")

ggsave('~/projects/vix_hedge/VXTH_replication/SPX_equity_curves.png', p, width = 6, height=4)

# show some figures for the blog post
# first - replication of the index
keep.ports <- c('SPX', 'UPRO/TMF', 'UPRO/TMF/VIXOPT')
pvm.keep2 <- pvm[pvm$variable %in% keep.ports,]
pvm.keep2$variable <- as.character(pvm.keep2$variable)
pvm.keep2[pvm.keep2$variable =='SPX', 'variable'] <- 'SPX BENCHMARK'
pvm.keep2[pvm.keep2$variable =='UPRO/TMF', 'variable'] <- 'UPRO/TMF BENCHMARK'
pvm.keep2[pvm.keep2$variable =='UPRO/TMF/VIXOPT','variable'] <- 'UPRO/TMF VXTH'
p <- ggplot(pvm.keep2) +
  geom_line(aes(x=date, y=value, col=variable), size=1) +
  scale_color_manual(values=c('SPX BENCHMARK' = 'grey70',
                              'UPRO/TMF BENCHMARK' = 'grey50',
                              'UPRO/TMF VXTH' = 'orange')) +
  scale_x_date() + scale_y_continuous() + theme_bw() +
  ggtitle('Hedged UPRO/TMF portfolios') + xlab('Date') + ylab('Value') +
  # scale_y_log10() +
  labs(col = "Portfolio")
p

ggsave('~/projects/vix_hedge/VXTH_replication/UPRO_equity_curves.png', p, width = 6, height=4)
