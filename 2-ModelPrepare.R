# 2-ModelPrepare.R
# Estimate DCC Matrix


# load required data
load("tokenList.rdata")
load("data_r.rdata")
load("data_p.rdata")
load("coinSummary.rdata")
load("sp500.rdata")

# calculate DCC matrix for the coins
data_dcc <- as.data.frame(matrix(ncol=1 , nrow=length(data_r$BTC_USD_r)))
colnames(data_dcc)<-"date"
data_dcc$date <- as.Date(data_r$date, format="%m/%d/%Y") 

date_r<-data_r[,1]
data_r[, -1]<-as.numeric(unlist(data_r[,-1]))
x_r<-xts(data_r[,-1], order.by=as.Date(date_r, format="%m/%d/%Y"))
coinSummary<-as.data.frame(coinSummary)
coinSummary[,4:8]<-as.numeric(unlist(coinSummary[,4:8]))

i=29; j=30
for (i in (1:length(coinSummary$coin))){
  for (j in ((i+1):length(coinSummary$coin))){
    tst<-createDccData(x_r, coinSummary, i, j) # create data series used for dcc estimation
    dataTemp<-estimateDCCMatrix(tst, coinSummary, i, j) # dcc estimation
    data_dcc<- sqldf(paste0("select a.*, b.dataTemp as ", 
                             paste0(colnames(tst)[1],"_", colnames(tst)[2]), " from data_dcc a left join dataTemp", 
                             " b on a.date=b.datetemp")) # create dcc matrix
    message(i," , ", j)
  }  
}

save(data_dcc, file="data_dcc.rdata")





