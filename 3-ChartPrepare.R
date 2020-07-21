# 3-ChartPrepare.R
# Prepare charts for report

# load required data
load("tokenList.rdata")
load("data_r.rdata")
load("data_p.rdata")
load("coinSummary.rdata")
load("sp500.rdata")
load("data_dcc.rdata")

# format the data
date_r<-as.Date(data_r[,1], format="%m/%d/%Y")
date_p<-as.Date(data_p[,1], format="%m/%d/%Y")
date_vec<-date_r
data_r[,-1] <- lapply(data_r[,-1], function(x) as.numeric(x))
data_p[,-1] <- lapply(data_p[,-1], function(x) as.numeric(x))
data_dcc[,-1]<- lapply(data_dcc[,-1],function(x) as.numeric(x))
coinSummary_2 <- gsub('_USDT','',coinSummary$coin)
coinSummary_2 <- gsub('_USD','',coinSummary_2)
# =====================================
# # Chart1 - plot dcc with price index
# create xaxis label
# time_label_m<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b")
date_vec<-date_p
time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"), format="%Y")
time_label<-character()
for (i in (1:length(time_label_y))){
  if (i %% 4 == 1){
    time_label[i]<-time_label_y[i]
  } else if (i %% 4 == 2){
    time_label[i]<-"Q2"
  } else if (i %% 4 == 3) {
    time_label[i]<-"Q3"
  } else {
    time_label[i]<-"Q4"
  }
}
timeFreq<-"quarter"

# yaxis label
y_label<-seq(-1, 1, by=0.2)

# plot DCC data series-daily
i<-1; j<-9

for (i in (1:length(coinSummary$coin))){
  for (j in (1:length(coinSummary$coin))){
    data1_p<-eval(parse(text=paste0("data_p$",coinSummary[i,1],"_p")))
    data2_p<-eval(parse(text=paste0("data_p$",coinSummary[j,1],"_p")))
    data1_p<-data1_p*(1/data1_p[max(which(data1_p>0)[1],424)]) # use 3/1/2017 as baseline if it is higher
    data2_p<-data2_p*(1/data2_p[max(which(data2_p>0)[1],424)])
    
    vec_length <-length(data1_p)
    # create xts object for plot
    if (i==j){
      dcc_vec <- rep(1,vec_length)
    } else if(j>i) {
      dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[i+1],"_", colnames(data_r)[j+1])))
    } else {
      dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[j+1],"_", colnames(data_r)[i+1])))
    }
    
    #
    postscript(file.path(folderDccPic,paste0(tokenList[i,2],"usd",tokenList[j,2],"usdDCC.eps")), fonts="serif",
               width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")
    
    # plot fucntion
    plotDccdata(date_vec, time_label, y_label, timeFreq, dcc_vec, data1_p, data2_p, coinSummary_2, i, j)
    
    # add sub plot of volume
    dev.off()
    message(i," , ", j)
  }
}

# ===================================
# Chart1.5 - DCC since March 1 2018
date_vec<-date_p[startIndex2:endIndex]
time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b")
time_label<-time_label_y
timeFreq<-"month"
# # create xaxis label
# # time_label_m<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b")
# time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"), format="%Y")
# time_label<-character()
# for (i in (1:length(time_label_y))){
#   if (i %% 4 == 1){
#     time_label[i]<-time_label_y[i]
#   } else if (i %% 4 == 2){
#     time_label[i]<-"Q2"
#   } else if (i %% 4 == 3) {
#     time_label[i]<-"Q3"
#   } else {
#     time_label[i]<-"Q4"
#   }
# }

# yaxis label
y_label<-seq(-1, 1, by=0.2)

# plot DCC data series-daily
i<-1; j<-9

for (i in (1:length(coinSummary$coin))){
  for (j in (1:length(coinSummary$coin))){
    data1_p<-eval(parse(text=paste0("data_p$",coinSummary[i,1],"_p")))
    data2_p<-eval(parse(text=paste0("data_p$",coinSummary[j,1],"_p")))
    data1_p<-data1_p*(1/data1_p[max(which(data1_p>0)[1],424)]) # use 3/1/2017 as baseline if it is higher
    data2_p<-data2_p*(1/data2_p[max(which(data2_p>0)[1],424)])
    data1_p<-data1_p[startIndex2:endIndex]
    data2_p<-data2_p[startIndex2:endIndex]
    
    vec_length <-length(data1_p)
    # create xts object for plot
    if (i==j){
      dcc_vec <- rep(1,vec_length)
    } else if(j>i) {
      dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[i+1],"_", colnames(data_r)[j+1])))[startIndex2:endIndex]
    } else {
      dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[j+1],"_", colnames(data_r)[i+1])))[startIndex2:endIndex]
    }
    #
    postscript(file.path(folderDccPic,paste0(tokenList[i,2],"usd",tokenList[j,2],"usdDCCMar2018.eps")), fonts="serif",
               width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")
    
    # plot fucntion
    plotDccdata(date_vec, time_label, y_label, timeFreq, dcc_vec, data1_p, data2_p, coinSummary_2, i, j)
    
    # add sub plot of volume
    dev.off()
    message(i," , ", j)
  }
}

message("DCC since March 2018 is done~!")

# ===================================
# Chart2 - 30 DCC in one Chart
postscript(file.path(folderPic,paste0("30DCCbtcLegend.eps")), fonts="serif",
           width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")

plot30Dcc(date_vec, time_label, y_label, data_dcc, data_p, tokenList)

# add sub plot of volume
dev.off()
message("DCC Chart is done~!")

# ===================================
# Chart3 - 30 Price in one chart since Jan 2018

date_vec<-date_p[startIndex1:endIndex]
time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b")
time_label<-time_label_y

postscript(file.path(folderPic,paste0("30PriceBaseline300Jan18Legend.eps")), fonts="serif",
           width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")

plot30Price(date_vec, time_label, data_p, tokenList, startIndex1, endIndex)

# add sub plot of volume
dev.off()
message("Price Chart is done~!")


# ===================================
# Chart4 - 30 Price in one chart since Mar 2018

date_vec<-date_p[startIndex2:endIndex]
time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b")
time_label<-time_label_y

postscript(file.path(folderPic,paste0("30PriceBaseline300March18Legend.eps")), fonts="serif",
           width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")

plot30Price(date_vec, time_label, data_p, tokenList, startIndex2, endIndex)

# add sub plot of volume
dev.off()
message("Price Chart is done~!")

# ===================================
# Chart5 - Exchange Daily Rev Rank.eps
fileName<-paste0("ex_daily_rev.csv")
datatemp<-read.table(file.path(folderInput, fileName), header=TRUE, sep=",", stringsAsFactors = FALSE, comment.char = "")
datatemp [datatemp==0]<-NA
assign("exDailyRev",datatemp)

postscript(file.path(folderPic,paste0("ExchangeDailyRevRank", ".eps")), fonts="serif",
           width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")

plotExRev(exDailyRev)

dev.off()

# ===================================
# Chart6 - BNB price and volume chart
# read data of BNB
i<-30
fileName<-paste0("binance-coin_usd_daily_all.csv")
datatemp<-loadDailyCryptoData(folderCmc, fileName, tokenList, i)
assign(paste0(tokenList$fsym[i], "_", tokenList$tsym[i]),datatemp)

# create xaxis label
date_vec<-as.Date(BNB_USD$date, format="%m/%d/%Y")

time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"), format="%Y")
time_label<-character()
for (i in (1:length(time_label_y))){
  if ((i+2) %% 4 == 1){
    time_label[i]<-time_label_y[i]
  } else if ((i+2) %% 4 == 2){
    time_label[i]<-"Q2"
  } else if ((i+2) %% 4 == 3) {
    time_label[i]<-"Q3"
  } else {
    time_label[i]<-"Q4"
  }
}

postscript(file.path(folderPic,paste0("BNBusdPriceVolume2017-07-25", ".eps")), fonts="serif",
           width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")
# plot BNB price with volume
plotBnbPriceVolume(date_vec, time_label, BNB_USD)

dev.off()
message("BNB Price Volume chart is done!")

# ===================================
# Chart7 - 13 crypto correlation matrix chart

data_r1<-data_r[startIndex2:endIndex,c(1:11, 31, 22, 25)]
data_r1$date <- as.Date(data_r1$date, format="%m/%d/%Y") 
data_r1[, -1]<-as.numeric(unlist(data_r1[,-1]))
token_var <-tokenList$fsym[c(1:10, 30, 21, 24)]
colnames(data_r1)<-c("date", token_var)

# cross correation matrix for *_btc
cryptoData<-data_r1[,-1]
cormat<-round(cor(cryptoData),2)
postscript(file.path(folderPic,paste0("13CryptoCorrMatrixMar2018.eps")), fonts="serif",
           width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")
plotCorrMatrix(cormat)
dev.off()

# ===================================

