# scale the upro and tmf data
# and extend the sim dataseries to be useful in portfoliovisualizer

upro.data <- stockDataDownload('UPRO', from = '1996-01-01', to='2021-01-01')
tmf.data <- stockDataDownload('TMF', from = '1996-01-01', to='2021-01-01')
upro.data.df <- as.data.frame(upro.data$close)
upro.data.xts <- xts(upro.data.df, order.by=as.Date(rownames(upro.data.df)))
tmf.data.df <- as.data.frame(tmf.data$close)
tmf.data.xts <- xts(tmf.data.df, order.by=as.Date(rownames(tmf.data.df)))


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
uprosim.pricex <- xts(uprosim.price$UPRO, order.by = uprosim.price$date)
upro.extend <- uprosim.pricex['1986-05-20/2009-06-25']
# this needs to be scaled so the value on the last date matches up
match.val <- upro.data.df[1,"UPRO"]
last.val <- as.numeric(upro.extend["2009-06-25"])
upro.extend <- upro.extend * (match.val/last.val)
upro.extend <- rbind(upro.extend['1986-05-20/2009-06-24'], upro.data.xts)
upro.extend.df <- data.frame(date=index(upro.extend), UPRO=coredata(upro.extend))
ggplot(upro.extend.df) + geom_line(aes(x=date, y=UPRO))
# write this out as values
write.table(upro.extend.df, '~/option_data/sim_data/uprosim_extended.csv', sep=',', row.names = F, col.names = T, quote=F)

###################################################################################
# do the same thing for TMF
tmfsim.return <- read.table('~/option_data/sim_data/TMFSIM.csv', sep=',', header = T)[,1:2]
tmfsim.return$Date <- as.Date(tmfsim.return$Date, format='%m/%d/%y')
tmfsim.return$TMF <- as.numeric(gsub('%', '', tmfsim.return$TMF)) /100
tmfsim.price <- data.frame(date=tmfsim.return$Date, TMF=cumprod(tmfsim.return$TMF +1))
rownames(tmfsim.price) <- tmfsim.price$date

common.dates <- intersect(as.character(tmfsim.price$date), as.character(index(tmf.data$close)))
tmfsim.compare <- data.frame(date=common.dates, TMF=tmf.data.df[common.dates, "TMF"], tmfsim=tmfsim.price[common.dates, "TMF"])
# equalize further out to get rid of variance around first point
tmfsim.compare$TMF <- tmfsim.compare$TMF / tmfsim.compare$TMF[1000]
tmfsim.compare$tmfsim <- tmfsim.compare$tmfsim / tmfsim.compare$tmfsim[1000]
tcm <- melt(tmfsim.compare, id.vars = 'date')

# compare the two on a plot
ggplot(tcm) + geom_line(aes(x=as.Date(date), y=value, col=variable)) +
    scale_x_date()

# extend the TMF data to the common start date
tmfsim.pricex <- xts(tmfsim.price$TMF, order.by = tmfsim.price$date)
tmf.extend <- tmfsim.pricex['1986-05-20/2009-04-17']
# this needs to be scaled so the value on the last date matches up
match.val <- tmf.data.df[1,"TMF"]
last.val <- as.numeric(tmf.extend["2009-04-17"])
tmf.extend <- tmf.extend * (match.val/last.val)
tmf.extend <- rbind(tmf.extend['1986-05-20/2009-04-15'], tmf.data.xts)
tmf.extend.df <- data.frame(date=index(tmf.extend), TMF=coredata(tmf.extend))
ggplot(tmf.extend.df) + geom_line(aes(x=date, y=TMF))
# write this out as values
write.table(tmf.extend.df, '~/option_data/sim_data/tmfsim_extended.csv', sep=',', row.names = F, col.names = T, quote=F)
