# 1-ReportUpdateMaster.R
# Prepare Crypto Data for Analysis

# =========================================
# Data Preparation
# Read token list
tokenList<- read.table(file.path(folderInput, tokenListFile),
                        header=TRUE, sep=",", stringsAsFactors = FALSE, comment.char = "")
save(tokenList, file="tokenList.rdata")
# =========================================
# Import data from token list
i<-1
for (i in (1:length(tokenList$Index))){
  fileName<-paste0(tokenList$coinName[i], "_usd_daily_all.csv")
  datatemp<-loadDailyCryptoData(folderCmc, fileName, tokenList, i) # loadDailyCryptoData function
  assign(paste0(tokenList$fsym[i], "_", tokenList$tsym[i]),datatemp)
  progress <- i/length(tokenList$fsym)
  message("Reading data of ",tokenList$fsym[i], "_", tokenList$tsym[i], " | Progress ", paste(round(100*progress, 2), "%", sep="") )
}

# =========================================
# Create data_p matrix
i<-1
data_p <- as.data.frame(BTC_USD$date[-1]) # to keep the data length the same as return
colnames(data_p)[1]<-"date"
for (i in (1:length(tokenList$fsym))){
  dfname<-paste0(tokenList$fsym[i], "_", tokenList$tsym[i])
  datatemp<-createPriceMatrix(dfname) # createPriceMatrix function
  
  # join the data based on date
  data_p <- sqldf(paste0("select a.*, b.datatemp as ", 
                         paste0(dfname, "_p"), " from data_p a left join datatemp", 
                         " b on a.date=b.datetemp"))
  progress <- i/length(tokenList$fsym)
  message("Calculate price of ",tokenList$fsym[i], "_", tokenList$tsym[i], " | Progress ", paste(round(100*progress, 2), "%", sep=""))
}
save(data_p, file="data_p.rdata")
# =========================================
# Create data_r matrix
i<-1
data_r <- as.data.frame(BTC_USD$date[-1]) 
colnames(data_r)[1]<-"date"
for (i in (1:length(tokenList$fsym))){

  dfname<-paste0(tokenList$fsym[i], "_", tokenList$tsym[i])
  datatemp<-createReturnMatrix(dfname) # create return matrix function
  
  # join the data based on date
  data_r <- sqldf(paste0("select a.*, b.datatemp as ", 
                         paste0(dfname, "_r"), " from data_r a left join datatemp", 
                         " b on a.date=b.datetemp"))    
  progress <- i/length(tokenList$fsym)
  message("Calculate return of ",tokenList$fsym[i], "_", tokenList$tsym[i], " | Progress ", paste(round(100*progress, 2), "%", sep="") )
}
save(data_r, file="data_r.rdata")
# =========================================
# create coin summary table
date_r<-data_r[,1]
data_r[, -1]<-as.numeric(unlist(data_r[,-1]))
x_r<-xts(data_r[,-1], order.by=as.Date(date_r, format = "%m/%d/%Y"))

coinSummary <- as.data.frame(matrix(ncol=8 , nrow=length(tokenList$Index)))
colnames(coinSummary)=c("coin","start","end","obs","ar", "i", "ma", "twindow")
i<-1
for (i in (1:length(tokenList$Index))){

  zz<-eval(parse(text=paste0("x_r$",colnames(x_r[,i]))))
  zz<-zz[paste(startdate,enddate, sep="/"), colnames(zz)]
  zz<-na.approx(na.trim(zz, side="both"))
  
  coinSummary[i,]<-createCoinSummary(zz, tokenList, twindow) # create coin summary function
  
  progress <- i/length(tokenList$Index)
  message("Progress ", paste(round(100*progress, 2), "%", sep="") )
}
save(coinSummary, file="coinSummary.rdata")
# =========================================
# create sp500 data
datatemp<-loadSp500Data(folderSp, sp500DataFile)
assign("sp500",datatemp)
save(sp500, file="sp500.rdata")



