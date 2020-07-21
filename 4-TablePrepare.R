# 4-TablePrepare.R
# Prepare tables for report


# load required data
load("tokenList.rdata")
load("data_r.rdata")
load("data_p.rdata")
load("coinSummary.rdata")
load("sp500.rdata")
load("data_dcc.rdata")



# variable to plug in LaTex file
YTDVar<-"December 31, 2018"
save(YTDVar, file="ytdvar.rdata")

date_p<-data_p[,1]
data_p[, -1]<-as.numeric(unlist(data_p[,-1]))
table_r <- as.data.frame(matrix(ncol=4 , nrow=length(tokenList$fsym)))
colnames(table_r)<-c("token", "name", "YTD", "Mar" )
table_r$token<-tokenList$fsym
table_r$name<-tokenList$token_name

i<-1
for (i in (1:30)){
  p0<-data_p[startIndex1,i+1]
  p1<-data_p[endIndex,i+1]
  table_r$YTD[i]<-(p1/p0-1)
}

for (i in (1:30)){
  p0<-data_p[startIndex2,i+1]
  p1<-data_p[endIndex,i+1]
  table_r$Mar[i]<-(p1/p0-1)
}

save(table_r, file="table_r.rdata")



