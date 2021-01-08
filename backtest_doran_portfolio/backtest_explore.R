library(ggplot2)
library(rafalib)
library(portfolioBacktest)
library(xts)
data("dataset10")

# equal weight portfolio
# returns equal weights for each asset (Column) in the dataset
eq_portfolio_fun <- function(dataset) {
    prices <- dataset$adjusted
    # print(head(dataset$adjusted))
    # print(tail(dataset$adjusted))
    # print(nrow(dataset$adjusted))
    N <- ncol(prices)
    return(rep(1/N, N))
}

# define quintile portfolio
quintile_portfolio_fun <- function(dataset) {
    X <- diff(log(dataset$adjusted))[-1]  # compute log returns
    N <- ncol(X)
    # design quintile portfolio
    ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
    w <- rep(0, N)
    w[ranking[1:round(N/5)]] <- 1/round(N/5)
    return(w)
}

# SPX/VIX portfolio weights from portfoliovisualizer
# 85/15 shown to be optimal from the 1996-2020 period in optimiztion
vix_portfolio_fun <- function(dataset) {
    return(c(0.85, 0.15))
}
# only holding SPX
spx_portfolio_fun <- function(dataset) {
    return(c(1, 0))
}

dat <- read.table('~/option_data/spx_vix_close.tsv',sep='\t', quote='', header=T)
dat$date    <- as.Date.character(dat$date, format="%Y%m%d" )
# plot of SPX and VIX from 1996 - 2020
ggplot(dat) +
    geom_line(aes(x=date,y=SPX)) +
    geom_line(aes(x=date, y=VIX), color='firebrick') +
    scale_x_date() +
    scale_y_log10(
        name = "SPX",
        sec.axis = sec_axis(~., name="VIX")
    ) +
    theme_bw()

# transform to xts objects
# extensible time series objects
datx <- xts(x=dat[,c("SPX", "VIX")], order.by = dat$date)
datx1 <- xts(x=dat[,c("SPX")], order.by = dat$date)
datxp <- list(adjusted=datx)
datx1p <- list(adjusted=datx1)

portfolios <- list(eq_portfolio_fun, quintile_portfolio_fun)
portfolios2 <- list("50/50" = eq_portfolio_fun, "85/15" = vix_portfolio_fun, "100/0" = spx_portfolio_fun)
bt <- portfolioBacktest(portfolios, list(datxp), rebalance_every = 1, optimize_every = 1)
bt <- portfolioBacktest(portfolios2, list(datxp))
bt <- portfolioBacktest(eq_portfolio_fun, list(datxp), rebalance_every = 30, optimize_every = 30, T_rolling_window = 10000)
bt.month <- portfolioBacktest(portfolios2, list(datxp), rebalance_every = 30, optimize_every = 30)

backtestSummary(bt)$performance
backtestSummary(bt.month)$performance

backtestChartCumReturns(bt)
backtestChartCumReturns(bt.month)

backtestBoxPlot(bt, measure = "Sharpe ratio")
backtestBoxPlot(bt.month, measure = "Sharpe ratio")

head(bt$fun1$data1$wealth)
tail(bt$fun1$data1$return)
tail(bt$fun1$data1$w_designed)
tail(bt$fun1$data1$w_bop)
tail(bt$fun1$data1$performance)

# wealth has returns
# bt$fun1$data1$w_designed has intended weights at each period
# bt$fun1$data1$w_bop has actual weights on each day
# bt$fun1$data1$return has return values for each day


# VIX ALLOCATION STRATEGY
# The weight in VIX is determined in month t using Equation (1) that
# uses only the data from months t − 60 to t − 1. The VIX weight is calculated at the end of month t based on the portfolio
# allocation to VIX from the predicted value from Equation (1) for each possible VIX level and S&P return, weighted by
# the likelihood of each scenario. For example, using the coefficients from the second regression in Table 2, the weight
# in VIX if the S&P 500 was down 3% and VIX was 20 would be 18%. If this occurred once in the sample, the weight
# would be one divided by 60, and this weight would be multiplied by 18%. This is done for every outcome and then all
# outcomes are added together. In the case of positive S&P 500 outcomes, the optimal allocation to the hedge would be
# 0%. This leads to an average allocation to VIX of 5.05%.

# basically, remove VIX hedge when VIX above a certain level
# long run average: VIX(mean)
# mean + 1 SD: VIX(1+SD)
# mean - 1 SD: VIX(1-SD)
# VIX(1 + SD) is the winner and the one to replicate

# equation 1 from the paper
# r VIX ,t = α + βr S&P500,t + γr S 2 &P500,t + δVIXt−1 + ε t ,
# r VIX, t  = return of fix index on month t,
# VIX t-1 = VIX index in % on month t-1
# rs&p500 - return of s&p on month t,
# alpha = constant (intercept)
# epsilon(t)  = residual (error)

# # so coefficients are ( from table 2, using full sample )
# alpha: 0.188
# beta: -2.98
# gammma: 18.712
# delta: -0.952
# epsilon

# estimate r_vix, t
# returns for vix in the given month
# given spr (returns of s&p in month t)
# and vixt1 (vix in month t-1)
eq_1 <- function(spr, vixt1){
    coeffs_all <- list(
        alpha = 0.188,
        beta = -2.98,
        g = 18.712,
        delta = -0.952
        )

    # months returns < 0
    coeffs_neg <- list(
        alpha = 0.307,
        beta = -5.621,
        g = -1.489,
        delta = -1.69
    )
    # months returns > 0
    coeffs_pos <- list(
        alpha = 0.188,
        beta = -2.98,
        g = 18.712,
        delta = -0.952
    )

    # decide what coefficients to use
    if (spr >= 0 ){
        use_coeffs <- coeffs_pos
    } else {
        use_coeffs <- coeffs_neg
    }

    eq <- use_coeffs$alpha +
        (use_coeffs$beta * spr) +
        (use_coeffs$g * (spr^2)) +
        (use_coeffs$delta * vixt1)
    return(eq)
}



# calculate monthly returns in the dataset?
# so just end of month - start of month
datx_month <- split(datx, f='months')
month_return <- do.call(rbind, lapply(datx_month, function(x) {
    (as.numeric(x[nrow(x), ]) - as.numeric(x[1, ]))
}))
month_return_pct <- do.call(rbind, lapply(datx_month, function(x) {
    (as.numeric(x[nrow(x), ]) - as.numeric(x[1, ])) / as.numeric(x[1, ])
}))

# month end values
month_end_values <- do.call(rbind, lapply(datx_month, function(x) tail(x, n=1)))

colnames(month_return) <- c('SPX', 'VIX')
colnames(month_return_pct) <- c('SPX', 'VIX')
mypar(1,2)
hist(month_return_pct[,1] * 100, breaks=100, main='SPX monthly returns')
abline(v=0, col='firebrick')
hist(month_return_pct[,2]* 100, breaks=100, main='VIX monthly returns')
abline(v=0, col='firebrick')


# test this equation for the example in the paper
# S&P down 3%, vix=20
eq_1(-0.03, 0.20)
# should be 18 from the paper(?) but we get 13.6...

# this is the expected return for this month. How do we get weights?


# using data for t-60 to t-1
# The VIX weight is calculated at the end of month t based on the portfolio
# allocation to VIX from the predicted value from Equation (1) for each possible
# VIX level and S&P return, weighted by the likelihood of each scenario
use_month_end <- cbind(month_return_pct[,"SPX"], month_end_values[,"VIX"])
colnames(use_month_end) <- c('SPX_return', 'VIX')

# so I need to calculate this big table?
t <- 291
t_max <- nrow(use_month_end)
min_t <- max(1, t-60)
max_t <- t-1

use_month_end_t <- as.data.frame(use_month_end[min_t:max_t, ])
# for each possible SPX return and VIX level, calculate eq1
use_month_end_t$eq1_t <- as.numeric(apply(use_month_end_t, 1, function(x) eq_1(x[1], x[2]/100)))
# cant short vix so make this min 0
use_month_end_t[use_month_end_t$eq1_t < 0, "eq1_t"] <- 0

ggplot(use_month_end_t) +
    geom_point(aes(x=SPX_return*100, y=VIX, color=eq1_t), size=2) +
    scale_color_viridis_c() + theme_bw()

# so the desired allocation is basically the average of all of these values?
mean(use_month_end_t$eq1_t)

# DOES ANY OF THIS EVEN MATTER?
# what if we just use fixed weights and do the adaptive allocation based on mean, sd
# using fixed allocation of 5%
# and just define these mean and SD
# based on current values? # or some fixed lookback period?
# start with fixed
# mean of daily vix close values
vix_mean <- mean(dat[, "VIX"])
vix_sd <- sd(dat[, "VIX"])
vix_fixed_allocation <- 0.15

vix_mean_fun <- function(dataset){
    prices <- dataset$adjusted
    # print(tail(prices))
    if(prices[nrow(prices), 'VIX'] > vix_mean){
        # print('gREATER THAN MEAN')
        return(c(1,0))
    } else{
        return(c(1- vix_fixed_allocation, vix_fixed_allocation))
    }
}

vix_sdp_fun <- function(dataset){
    prices <- dataset$adjusted
    if(prices[nrow(prices), 'VIX'] > (vix_mean + vix_sd)){
        return(c(1,0))
    } else{
        return(c(1- vix_fixed_allocation, vix_fixed_allocation))
    }
}

vix_sdp0.75_fun <- function(dataset, x) {
    prices <- dataset$adjusted
    if(prices[nrow(prices), 'VIX'] > (vix_mean + (0.75*vix_sd))){
        return(c(1,0))
    } else{
        return(c(1- vix_fixed_allocation, vix_fixed_allocation))
    }
}
vix_sdp0.5_fun <- function(dataset, x) {
    prices <- dataset$adjusted
    if(prices[nrow(prices), 'VIX'] > (vix_mean + (0.5*vix_sd))){
        return(c(1,0))
    } else{
        return(c(1- vix_fixed_allocation, vix_fixed_allocation))
    }
}

vix_sdn_fun <- function(dataset){
    prices <- dataset$adjusted
    if(prices[nrow(prices), 'VIX'] > (vix_mean - vix_sd)){
        return(c(1,0))
    } else{
        return(c(1- vix_fixed_allocation, vix_fixed_allocation))
    }
}


portfolios2 <- list("85/15" = vix_portfolio_fun, "100/0" = spx_portfolio_fun,
                    "mean" = vix_mean_fun, "sd+" = vix_sdp_fun, "sd+0.5" = vix_sdp0.5_fun,
                    "sd+0.75" = vix_sdp0.75_fun, "sd-" = vix_sdn_fun)
bt <- portfolioBacktest(portfolios2, list(datxp), show_progress_bar = T)
bt.month <- portfolioBacktest(portfolios2, list(datxp), rebalance_every = 23, optimize_every = 23, show_progress_bar = T)

backtestSummary(bt)$performance
backtestSummary(bt.month)$performance

p1 <- backtestChartCumReturns(bt)
p1 + theme_bw() + scale_y_log10() +scale_color_brewer(palette = "Set1")

p2 <- backtestChartCumReturns(bt.month)
p2 + theme_bw() + scale_color_brewer(palette = "Set1")

backtestBoxPlot(bt, measure = "Sharpe ratio")
backtestBoxPlot(bt.month, measure = "Sharpe ratio")

# if using upro / VIX, optial allocation from PV is 70/30 (to balance out Vol more I guess)

# replicating the VIX index with SPX options
# These results, and combined with the results of Table 2, suggest the best way to form a portfolio that replicates VIX
# and has lower cost, will buy monthly an ITM‐OTM put spread and sell an ATM‐OTM call spread. This will allow the
# portfolio to benefit from volatility shocks by having a put spread while also having relatively low cost from selling a call
# spread. This call spread not only helps to fund the volatility and price protection, but when combined with the net long‐
# equity position will have a payoff structure similar to a covered call. However, this means that the portfolio is
# Buy ITM-OTM put spread (5% on each)
# Sell ATM-OTM call spread (5%)
