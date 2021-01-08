# backtest with the added option portfolio
# subset dat to the days we have in the options portfolio
library(xts)
library(portfolioBacktest)
dat.backtest <- dat[as.character(dat$date) %in% as.character(tracking.df$date2),]
dat.backtest <- cbind(dat.backtest, tracking.df$current.balance)
colnames(dat.backtest) <- c('date', 'SPX', 'VIX', 'opt')
dat.backtest$opt <- as.numeric(dat.backtest$opt)
dat.backtest <- dat.backtest[!(is.na(dat.backtest$opt)),]
datx2 <- xts(x=dat.backtest[,c("SPX", "VIX", 'opt')], order.by = dat.backtest$date)
datx2p <- list(adjusted=datx2)
vix_fixed_allocation <- 0.25
vix_mean <- mean(dat[, "VIX"])
vix_sd <- sd(dat[, "VIX"])

ggplot(dat.backtest) +
    geom_line(aes(x=as.Date(date), y=opt)) +
    geom_line(aes(x=as.Date(date), y=VIX*25), col='firebrick') +
    geom_line(aes(x=as.Date(date), y=SPX), col='steelblue') +
    scale_x_date() + scale_y_continuous() +
    theme_bw()


vix_sdp_fun <- function(dataset){
    prices <- dataset$adjusted
    if(prices[nrow(prices), 'VIX'] > (vix_mean + vix_sd)){
        return(c(1,0,0))
    } else{
        return(c(1- vix_fixed_allocation, vix_fixed_allocation, 0))
    }
}

opt_sdp_fun <- function(dataset){
    prices <- dataset$adjusted
    if(prices[nrow(prices), 'VIX'] > (vix_mean + vix_sd)){
        return(c(1,0,0))
    } else{
        return(c(1- vix_fixed_allocation, 0, vix_fixed_allocation))
    }
}


# 85/15 shown to be optimal from the 1996-2020 period in optimiztion
# these portfolios always hold the hedge
vix_portfolio_fun <- function(dataset) {
    return(c(1- vix_fixed_allocation, vix_fixed_allocation, 0))
}
# always holding the hedge
opt_portfolio_fun <- function(dataset) {
    return(c(1- vix_fixed_allocation, 0, vix_fixed_allocation))
}
# only holding SPX
spx_portfolio_fun <- function(dataset) {
    return(c(1, 0, 0))
}


portfolios <- list("SPX" = spx_portfolio_fun, "vix always" = vix_portfolio_fun, "opt always"=opt_portfolio_fun,
                    "vix sd+" = vix_sdp_fun, "opt sd+" = opt_sdp_fun)
bt.month <- portfolioBacktest(portfolios, list(datx2p), rebalance_every = 25, optimize_every = 25, paral_portfolios=6)
backtestSummary(bt.month)$performance
backtestChartCumReturns(bt.month) + scale_color_brewer(palette = "Set1") + theme_bw() + scale_y_log10() + ggtitle(paste('VIX allocation:', vix_fixed_allocation))


bt.day <- portfolioBacktest(portfolios, list(datx2p))
backtestSummary(bt.day)$performance
backtestChartCumReturns(bt.day)


backtestBoxPlot(bt, measure = "Sharpe ratio")
backtestBoxPlot(bt.month, measure = "Sharpe ratio")

head(bt$fun1$data1$wealth)
tail(bt$fun1$data1$return)
tail(bt$fun1$data1$w_designed)
tail(bt$fun1$data1$w_bop)
tail(bt$fun1$data1$performance)
