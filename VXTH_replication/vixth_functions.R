
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
