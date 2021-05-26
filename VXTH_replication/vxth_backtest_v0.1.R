# VIXTH backtest
library(ggplot2)
library(xts)
library(reshape2)
library(PerformanceAnalytics)
# functions for backtest are stored here:
source('~/projects/vix_hedge/VXTH_replication/vixth_functions.R')

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
# Data from 2006-03-22 onward (until Dec 2019 for now)
trade.dates <- names(mid.list)
# where to save results for each run of the backtest
out.basedir <- '/home/bsiranos/pcloud_sync/stanford/class/mse_448/backtest_results'


# Steps for this backtest
# portfolios: SPX, UPRO, UPRO/TMF, SPX hedge, UPRO hedge, UPRO/TMF hedge
# first go: hedge is only adjusted once per month. This will include rebalancing of other portfolios
# second go: hedge adjusted on a daily basis, including rebalancing and deciding if VIX is in the right regime
# parameters to tune:
  # when to put on the hedge (always or only in periods of vol > 15)
  # when to scale hedge down?
  # when to remove hedge completely?
  # Hedge allocation amount (at each level)
  # daily rebalancing etc (which will incur transaction costs)
# RULES for the hedge

# 2021-05-12
# todo
  # logging of trades on a specific day
  # Calculation of stats



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
vix.allocations <- c(0.00, 0.01, 0.05, 0)
portfolio.allocations <- get_portfolios(vix.allocations)
daily.rebalance <- F

# save to a folder all of this info from the run
# tt <- gsub(':', '-', gsub(' ', '_', as.character(Sys.time())))
# outdir <- file.path(out.basedir, tt)
# dir.create(outdir)

# which portfolios change between vix high and low?
# change.portfolios <- !sapply(1:n.portfolio, function(x) identical(portfolio.allocations.vixhigh[,x], portfolio.allocations.vixlow[,x]))
n.portfolio <- ncol(portfolio.allocations[[1]])
n.allocations <- nrow(portfolio.allocations[[1]])
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
  current.dte <- as.integer(as.Date(params[['tenor']])) - as.integer(as.Date(this.date))
  current.value <- mid.list[[this.date]]['C', params$tenor, params$strike]
  # set up trade logging df
  trade.log <- c(this.date, vix.close, vx.close, vix.regime, unlist(params), current.dte, current.value, 0)
  trade.log.df <- data.frame()
  trade.log.df <- rbind(trade.log.df, c(trade.log))
  colnames(trade.log.df) <- c('date', 'vix.close', 'vx.close', 'vix.regime', 'tenor', 'strike', 'purchase.dte', 'purchase.value', 'expiration.value')
  # print(as.character(params))
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
    tryCatch(current.value <- mid.list[[this.date]]['C', params$tenor, params$strike],error=function(e) return(last.value))
    # check for nan
    if (is.nan(current.value)){ current.value <- last.value }
  }
  today.option.value <- current.value

  # accounting value today
  price.vec <- c(spx.close, upro.close, tmf.close, current.value)
  portfolio.value.today <- colSums(portfolio.shares * price.vec)
  portfolio.values[this.date, ] <- portfolio.value.today

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
    new.dte <- as.integer(as.Date(params[['tenor']])) - as.integer(as.Date(this.date))
    print(paste(this.date, 'VIX:', vix.close, 'VX1', vx.close, "REGIME:", vix.regime))
    print(as.character(params))
    current.value <- mid.list[[this.date]]['C', params$tenor, params$strike]
    price.vec <- c(spx.close, upro.close, tmf.close, current.value)
    # allocate the initial portfolios
    portfolio.shares <-  portfolio.allocations.vix *
      matrix(portfolio.values[this.date, ], ncol=n.portfolio, nrow=n.allocations, byrow = T) /
      matrix(price.vec, ncol = n.portfolio, nrow=n.allocations)
    print(portfolio.shares)
    # save this info for each trade date
    trade.log.df[nrow(trade.log.df), "expiration.value"] <- today.option.value
    trade.log <- c(this.date, vix.close, vx.close, vix.regime, unlist(params), new.dte, current.value, 0)
    trade.log.df <- rbind(trade.log.df, trade.log)

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
# add VXTH
portfolio.values.df$VXTH <- price.df$VXTH * (starting.balance / price.df$VXTH[1])
# date first
portfolio.values.df <- portfolio.values.df[, c("date", colnames(portfolio.values.df)[colnames(portfolio.values.df) != 'date'])]
# STATSTICS
pvx <- xts(portfolio.values.df[, colnames(portfolio.values.df)!='date'], order.by = portfolio.values.df$date)
pvxr <- Return.calculate(pvx)
pvxm <- to.period(pvx, period = "months", OHLC = F)
pvxmr <- (Return.calculate(to.period(pvx, period='months', OHLC=F)))
# calculate CAGR
total.days <- as.integer(as.Date(trade.dates[length(trade.dates)]) - as.Date(trade.dates[1]))
CAGR <- (portfolio.values.df[nrow(portfolio.values.df),colnames(portfolio.values.df)!='date'] / portfolio.values.df[1,colnames(portfolio.values.df)!='date']) ^ (365/total.days) -1
CAGR <- CAGR *100
rownames(CAGR) <- 'CAGR'
# other stats
pvx.stats <- data.frame(CAGR = t(CAGR),
                        sharpe = SharpeRatio(pvxmr,annualize = T)[1,],
                        calmar = CalmarRatio(pvxmr)[1,],
                        sterling = SterlingRatio(pvxmr)[1,],
                        maxDrawdown = maxDrawdown(pvxmr)[1,],
                        stddev.annualized = StdDev.annualized(pvxmr)[1,])
# save stats
outf.stats <- file.path(outdir, 'return_statistics.tsv')
write.table(round(pvx.stats,3), outf.stats, sep='\t', quote=F, row.names = T, col.names = T)
outf.values <- file.path(outdir, 'portfolio_values.tsv')
write.table(format(portfolio.values.df, digits=5), outf.values, sep='\t', quote=F, row.names = F, col.names = T)
outf.trade.log <- file.path(outdir, 'trade_log.tsv')
trade.log.df$purchase.value <- as.numeric(trade.log.df$purchase.value)
trade.log.df$expiration.value <- as.numeric(trade.log.df$expiration.value)
trade.log.df$return <- trade.log.df$expiration.value / trade.log.df$purchase.value
write.table(trade.log.df, outf.trade.log, sep='\t', quote=F, row.names = F, col.names = T)

# melt for plotting
pvm <- melt(portfolio.values.df, id.vars = "date")
# ggplots, log and not
p1 <- ggplot(pvm) +
  geom_line(aes(x=date, y=value/100000, col=variable)) +
  theme_bw() +
  scale_y_log10() +
  scale_x_date() +
  labs('Backtest equity curves', y='log Value / 100000', x='Date')

p2 <- ggplot(pvm) +
  geom_line(aes(x=date, y=value/100000, col=variable)) +
  theme_bw() +
  scale_x_date() +
  scale_y_continuous() +
  labs('Backtest equity curves', y='Value / 100000', x='Date')

p3 <- chart.Drawdown(pvxmr,legend.loc = "bottomright")

# Save figures
pdf(file.path(outdir, 'curves.pdf'), height=8, width=10)
print(p1)
print(p2)
print(p3)
dev.off()

# a text file with the trade parameters
outf.log <- file.path(outdir, 'params.log')
write.table(c('# starting balance', starting.balance), outf.log, sep='\t', quote=F, row.names = F, col.names = F)
write.table(c('# VIX thresholds'), outf.log, sep='\t', quote=F, row.names = F, col.names = F, append = T)
write.table(vix.thresholds, outf.log, sep='\t', quote=F, row.names = F, col.names = F, append=T)
write.table(c('# dte.open'), outf.log, sep='\t', quote=F, row.names = F, col.names = F, append = T)
write.table(dte.open, outf.log, sep='\t', quote=F, row.names = F, col.names = F, append=T)
write.table(c('# dte.close'), outf.log, sep='\t', quote=F, row.names = F, col.names = F, append = T)
write.table(dte.close, outf.log, sep='\t', quote=F, row.names = F, col.names = F, append=T)
write.table(c('# option.delta'), outf.log, sep='\t', quote=F, row.names = F, col.names = F, append = T)
write.table(option.delta, outf.log, sep='\t', quote=F, row.names = F, col.names = F, append=T)
write.table(c('# vix.allocations'), outf.log, sep='\t', quote=F, row.names = F, col.names = F, append = T)
write.table(vix.allocations, outf.log, sep='\t', quote=F, row.names = F, col.names = F, append=T)
write.table(c('# daily.rebalance'), outf.log, sep='\t', quote=F, row.names = F, col.names = F, append = T)
write.table(daily.rebalance, outf.log, sep='\t', quote=F, row.names = F, col.names = F, append=T)

# show some figures for the blog post
# first - replication of the index
if(F){
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
}
