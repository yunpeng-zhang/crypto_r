# 0-ReportFunctionLib.R 
#  Report Function Library includes:
# 


# Data Preparation Functions
# =================================================
loadDailyCryptoData <- function(folderCmc, fileName, tokenList, i) {
  datatemp<-read.table(file.path(folderCmc, fileName), header=TRUE, sep=",", stringsAsFactors = FALSE, comment.char = "")
  
  datatemp$fsym<-tokenList[i,2]
  datatemp$tsym<-tokenList[i,3]
  datatemp<-datatemp[,c(1,8:9,2:6)]
  
  colnames(datatemp)<-c("date","fsym","tsym","open","high","low","close","vol.f")
  datatemp [datatemp==0]<-NA

  return(datatemp)
}
# =================================================
createPriceMatrix<-function(dfname){
  datetemp<- eval(parse(text = paste0(dfname, "$date")))
  datetemp<-datetemp
  datatemp <- eval(parse(text = paste0(dfname, "$close")))
  datatemp[which(is.infinite(datatemp))] <- NA
  datatemp<-cbind(datetemp, datatemp)
  datatemp<-as.data.frame(datatemp)
  return(datatemp)
}
# =================================================
createReturnMatrix<-function(dfname){
  datetemp<- eval(parse(text = paste0(dfname, "$date")))[-1]
  datatemp <- diff(log(eval(parse(text = paste0(dfname, "$close")))), lag=1)
  datatemp[which(is.infinite(datatemp))] <- NA
  
  # combine datatemp
  datatemp<-cbind(datetemp, datatemp)
  datatemp<-as.data.frame(datatemp)
  return(datatemp)
}
# =================================================
createCoinSummary <- function(zz, tokenList, twindow) {
  coinSummary[1]<-paste0(tokenList$fsym[i],"_",tokenList$tsym[i])
  # https://stackoverflow.com/questions/6465222/access-zoo-or-xts-index
  coinSummary[2]<-as.character(as.Date(index(zz)[1]))
  coinSummary[3]<-as.character(as.Date(index(zz)[length(zz)]))
  coinSummary[4]<-length(zz)
  if(coinSummary[4]>(twindow+100)) {
    zzz<-auto.arima(zz[(1+twindow):length(zz)])
  } else {
    zzz<-auto.arima(zz[1:length(zz)])
  }
  coinSummary[5:7]<-rbind(arimaorder(zzz))
  coinSummary[8]<-if(coinSummary[i,4]>(twindow+100)) twindow else 0
  return(coinSummary)
}
# =================================================
loadSp500Data<-function(folderSp, sp500DataFile){
  datatemp<-read.table(file.path(folderSp,sp500DataFile), header=TRUE, sep=",", stringsAsFactors = FALSE, comment.char = "")
  datatemp$fsym<-"SP500"
  datatemp$tsym<-"Index"
  datatemp<-datatemp[,c(1,8:9, 3:5,2,6:7)]
  colnames(datatemp)<-c("date","fsym","tsym","open","high","low","close","vol.f", "sp500_r")
  return(datatemp)
}
# =================================================
# Model Estimation Functions
createDccData<-function(x_r, coinSummary, i, j){
# generate the data for dcc analysis
data1<-eval(parse(text=paste0("x_r$",coinSummary[i,1])))
data1<-data1[paste(startdate,enddate, sep="/"), colnames(data1)]
twindow<-coinSummary[i,8]
if(coinSummary[i,4]>(twindow+100)) {
  data1<-(data1[(1+twindow):length(data1)])
}
data2<-eval(parse(text=paste0("x_r$",coinSummary[j,1])))
data2<-data2[paste(startdate,enddate, sep="/"), colnames(data2)]
if(coinSummary[j,4]>(twindow+100)) {
  data2<-(data2[(1+twindow):length(data2)])
}
x12 <- merge(data1, data2)
tst <- na.approx(na.trim(x12, side="both"))
return(tst)
}
# =================================================
estimateDCCMatrix<-function(tst, coinSummary, i, j){
  garch_1_d.spec = ugarchspec(mean.model = list(armaOrder = c(min(1,max(coinSummary[i,5],0)),coinSummary[i,7])),
                              variance.model = list(garchOrder = c(1,1),
                                                    model = "sGARCH"), distribution.model = "norm")
  garch_2_d.spec = ugarchspec(mean.model = list(armaOrder = c(min(1,max(coinSummary[j,5],0)),coinSummary[j,7])),
                              variance.model = list(garchOrder = c(1,1),
                                                    model = "sGARCH"), distribution.model = "norm")
  # dcc specification - GARCH(1,1) for conditional correlations
  dcc.spec <-multispec( list(garch_1_d.spec, garch_2_d.spec) )
  dcc.garch12_d.spec = dccspec(uspec = dcc.spec, dccOrder = c(1,1), distribution = "mvnorm")
  
  # run dcc-garch model
  dcc.fit <- dccfit(dcc.garch12_d.spec, data = tst, fit.control=list(scale=TRUE))
  
  # Obtain conditional Correlation..
  r1=rcor(dcc.fit, type="R")
  r1.z=zoo(r1[1,2,], order.by=time(tst))
  datetemp<-index(r1.z)
  datatemp<-as.data.frame(r1.z)
  colnames(datatemp)<-"datatemp"
  datatemp<-cbind(datetemp, datatemp)
  return(datatemp)
}
# =================================================
# Charting Functions
numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.1f", val)) }

per_format <- function(m){sprintf("%1.0f%%", 100*m)}

plotDccdata<-function(date_vec, time_label, y_label, timeFreq, dcc_vec, data1_p, data2_p, coinSummary_2, i, j){
  par(
    lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
    lwd = 1,				    # function "lines" takes a vector of values for more than one line, and will cycle through that vector
    pch = ".",				  # point types
    bty = "n",					# suppresses box around the plot, keep part by letters like "c" 
    cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
    xaxt ="s", 
    yaxt ="s",  	 		  # s=standard, n=suppress axis
    
    cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 # axis label size, color and font
    cex.main =  1.2, col.main= grey(.4),  font.main = 8, # main label, color and font
    cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6, # sub label, color and font
    
    #tick marks
    lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
    tck = -.01  			,         # negative is external ticks, 1 is gridlines
    lend = "square"			,       # tick mark end "round" default, "butt" butt lines, "square" square line caps

    fg = grey(.2),	 # foreground color
    bg = grey(.95),
    family= "serif",
    mar=c(6,6,2,4)   # https://nicercode.github.io/guides/plotting/
  )
  
  plot(as.Date(date_vec), dcc_vec, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(-1,1),
       col.ticks="white", col="red", lwd=1.5)

  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = timeFreq),
            labels= time_label,col="white",tck=1)   

  axis(2,col="white",tck=1,col.ticks="white", at=seq(-1, 1, by =0.2), labels = numformat(y_label))
  title(ylab="DCC Estimate", line=3, cex.lab=1.5)

  par(new=T)
  max_v<-max(max(data1_p, na.rm=TRUE),max(data2_p, na.rm=TRUE))
  plot(as.Date(date_vec), data1_p, ylim=c(0,max_v), type="l",col="gray50",xaxt="n",yaxt="n",xlab="",ylab="")
  # https://stackoverflow.com/questions/11775692/how-to-specify-the-actual-x-axis-values-to-plot-as-x-axis-ticks-in-r
  axis(4, col=grey(.95), at = seq(0, max_v, by = round(max_v/6)))
  par(new=T)
  plot(as.Date(date_vec), data2_p,  ylim=c(0,max_v),type="l",col="lightblue3",xaxt="n",yaxt="n",xlab="",ylab="")
  axis(4, col=grey(.95), at = seq(0, max_v, by = round(max_v/6)))

  par(new=TRUE) 
  plot(as.Date(date_vec), dcc_vec, type="l",
       xaxt="n",
       ylim=c(-1, 1),
       col="red", lwd=1.5, axes = FALSE, xlab = "", ylab = "")

  par(xpd=TRUE)
  coord <- par("usr")
  
  legend(coord[1]+(coord[2]-coord[1])/3, coord[3]-coord[4]/4, col=c("red","gray50", "lightblue3"),lty=1,
         legend=c("DCC",paste(coinSummary_2[i]), paste(coinSummary_2[j])),  
         text.col="gray50", cex=1.5, pt.cex = 1, box.col = "gray95", box.lwd = 0,
         ncol =8, inset = c(0, 0), bty = "n")
  
  # legend(as.Date(date_vec)[1],-0.58,col=c("red","gray50", "lightblue3"),lty=1,
  #        legend=c("DCC",paste(coinSummary[i]), paste(coinSummary[j])), 
  #        text.col="gray50", cex=1.5, pt.cex = 1, box.col = "gray95", box.lwd = 0)
}

# =================================================
# plot 30 dcc in one chart
plot30Dcc<-function(date_vec, time_label, y_label, data_dcc, data_p, tokenList){
  par(
    lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
    lwd = 1,				  # function "lines" takes a vector of values for more than one line, and will cycle through that vector
    pch = ".",				  # point types
    bty = "n",								#suppresses box around the plot, keep part by letters like "c" 
    cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
    xaxt ="s", 
    yaxt ="s",	 		  # s=standard, n=suppress axis
    
    cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 #axis label size, color and font
    cex.main =  1.2, col.main= grey(.4),  font.main = 8,		#main label, color and font
    cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6,		#sub label, color and font
    
    #tick marks
    lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
    tck = -.01  			, # negative is external ticks, 1 is gridlines
    lend = "square"			, # tick mark end "round" default, "butt" butt lines, "square" square line caps

    fg = grey(.2),	      	  # foreground color
    bg = grey(.95),
    family= "serif",
    # https://nicercode.github.io/guides/plotting/
    mar=c(10,6,2,4)
  )
  
  color_vec=character()
  plot(as.Date(date_vec), data_dcc[,2], type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(-1,1),
       col.ticks="white", col="gray50", lwd=0.25)
  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"),
            labels= time_label,col="white",tck=1)   
  axis(2,col="white",tck=1,col.ticks="white", at=seq(-1, 1, by =0.2), labels = numformat(y_label))
  title(ylab="DCC Estimate", line=3, cex.lab=1.5)
  
  color_vec[1]<-"gray50"
  i=1
  j=1
  
  vec_length<-length(data_dcc[,1])

    for (i in (1:1)){
    for (j in (2:30)){
      # create xts object for plot
      if (i==j){
        dcc_vec <- rep(1,vec_length)
      } else if(j>i) {
        dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[i+1],"_", colnames(data_r)[j+1])))
      } else {
        dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[j+1],"_", colnames(data_r)[i+1])))
      }

      par(new=T)
      plot(as.Date(date_vec), dcc_vec, ylim=c(-1, 1), type="l",col=colors()[j+10],xaxt="n",yaxt="n",xlab="",ylab="", lwd=0.25, ann=FALSE)
      # https://stackoverflow.com/questions/11775692/how-to-specify-the-actual-x-axis-values-to-plot-as-x-axis-ticks-in-r
      color_vec[j]<-colors()[j+10]
    }
  }    
  
  data1_p<-eval(parse(text=paste0("data_p$BTC_USD_p")))
  data1_p<-data1_p*(1/data1_p[max(which(data1_p>0)[1],424)]) # use 3/1/2017 as baseline if it is higher
  
  par(new=T)
  max_v<-max(max(data1_p, na.rm=TRUE),max(data1_p, na.rm=TRUE))
  plot(as.Date(date_vec), data1_p,  ylim=c(0,max_v),type="l",col="gray30",xaxt="n",yaxt="n",xlab="",ylab="", lwd=1)
  axis(4, col=grey(.95), at = seq(0, max_v, by = 5*ceiling(max_v/70)))

  par(xpd=TRUE)
  coord <- par("usr")
  
  # add legend to the chart
  legend(coord[1]+20, coord[3]-2, col=color_vec,lty=1,
         legend=tokenList$fsym[1:30],  
         text.col="gray50", cex=1.2, pt.cex = 1, box.col = "gray95", box.lwd = 0,
         ncol =8, inset = c(0, 0), bty = "n")
}

# =================================================
# plot 30 price in one chart
plot30Price<-function(date_vec, time_label, data_p, tokenList, startIndex, endIndex){
  # par options for plot function
  par(
    lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
    lwd = 1,				  # function "lines" takes a vector of values for more than one line, and will cycle through that vector
    pch = ".",				  # point types
    bty = "n",								#suppresses box around the plot, keep part by letters like "c" 
    cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
    xaxt ="s", 
    yaxt ="s",	 		  # s=standard, n=suppress axis
    
    cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 #axis label size, color and font
    cex.main =  1.2, col.main= grey(.4),  font.main = 8,		#main label, color and font
    cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6,		#sub label, color and font
    
    #tick marks
    lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
    tck = -.01  			, # negative is external ticks, 1 is gridlines
    lend = "square"			, # tick mark end "round" default, "butt" butt lines, "square" square line caps
    fg = grey(.2),	      	  # foreground color
    bg = grey(.95),
    family= "serif",
    # https://nicercode.github.io/guides/plotting/
    # bottom, left, top, right
    mar=c(10,6,2,4)
  )
  
  i=2
  max_vec=rep(0,30)
  for (i in 1:30){
    data1_p<-data_p[startIndex:endIndex,i+1]
    price_vec<-data1_p*(1/data1_p[max(which(data1_p>0)[1],1)])
    max_vec[i]<-max(price_vec, na.rm = TRUE)
  }
  
  max_max_vec <- max(max_vec)
  color_vec=character()
  
  data1_p<-data_p[startIndex:endIndex,2]
  price_vec<-data1_p*(1/data1_p[max(which(data1_p>0)[1],1)])
  
  plot(as.Date(date_vec), price_vec, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0, 3),
       col.ticks="white", col="gray50", lwd=0.25)
  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = "month"),
            labels= time_label,col="white",tck=1)   
  # strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b-%y")
  axis(2,col="white",tck=1,col.ticks="white", at = seq(0, 3, by = 0.5*ceiling(3/600)))
  if (startIndex<790){
    title(ylab="Jan 1, 2018 = 1", line=3, cex.lab=1.5)
  } else {
    title(ylab="Mar 1, 2018 = 1", line=3, cex.lab=1.5)
  }
  
  color_vec[1]<-"gray50"
  
  for (i in (1:1)){
    for (j in (2:30)){
      data1_p<-data_p[startIndex:endIndex,j+1]
      price_vec<-data1_p*(1/data1_p[max(which(data1_p>0)[1],1)])
      
      par(new=T)
      plot(as.Date(date_vec), price_vec, ylim=c(0, 3), type="l",col=colors()[j+10],
           xaxt="n",yaxt="n",xlab="",ylab="", lwd=0.25, ann=FALSE)
      # https://stackoverflow.com/questions/11775692/how-to-specify-the-actual-x-axis-values-to-plot-as-x-axis-ticks-in-r
      # axis(4, col=grey(.95), at = seq(0, max_v, by = 10*ceiling(max_v/70)))
      color_vec[j]<-colors()[j+10]
    }
  }    
  par(xpd=TRUE)
  coord <- par("usr")
  
  # add legend to the chart
  legend(coord[1]+20, coord[3]-0.3, col=color_vec,lty=1,
         legend=tokenList$fsym[1:30],  
         text.col="gray50", cex=1.2, pt.cex = 1, box.col = "gray95", box.lwd = 0,
         ncol =8, inset = c(0, 0), bty = "n")
}
# =================================================
# plot BNB Price and Volume
plotBnbPriceVolume<-function(date_vec, time_label, BNB_USD){
  par(
    lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
    lwd = 1,				  # function "lines" takes a vector of values for more than one line, and will cycle through that vector
    pch = ".",				  # point types
    bty = "n",								#suppresses box around the plot, keep part by letters like "c" 
    cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
    xaxt ="s", 
    yaxt ="s",	 		  # s=standard, n=suppress axis
    
    cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 #axis label size, color and font
    cex.main =  1.2, col.main= grey(.4),  font.main = 8,		#main label, color and font
    cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6,		#sub label, color and font

    #tick marks
    lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
    tck = -.01  			, # negative is external ticks, 1 is gridlines
    lend = "square"			, # tick mark end "round" default, "butt" butt lines, "square" square line caps
    fg = grey(.2),	      	  # foreground color
    bg = grey(.95),
    family= "serif",
    # https://nicercode.github.io/guides/plotting/
    mar=c(4,6,4,4)
  )
  
  # plot the chart
  max_v=max(BNB_USD$close)
  y_label<- seq(0, max_v, by = 5*ceiling(max_v/60))
  
  plot(as.Date(date_vec), BNB_USD$close, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0, max_v),
       col.ticks="white", col="red", lwd=1.5)
  # http://r.789695.n4.nabble.com/plot-dates-in-x-axis-td875997.html
  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"),
            labels= time_label,col="white",tck=1)   
  axis(2,col="white",tck=1,col.ticks="white", at=seq(0, max_v, by = 5*ceiling(max_v/60)), labels = numformat(y_label))
  title(ylab="BNB Price", line=3, cex.lab=1.5)

  par(new=T)
  barplot(BNB_USD$vol.f, beside=TRUE, yaxt="n", ylim = c(0,10^9))
  max_vol<-max(BNB_USD$vol.f)
}
# =================================================
# plot Exchange Revenue
plotExRev<-function(exDailyRev){
  par(
    lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
    lwd = 1,				  # function "lines" takes a vector of values for more than one line, and will cycle through that vector
    pch = ".",				  # point types
    bty = "n",								#suppresses box around the plot, keep part by letters like "c" 
    cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
    xaxt ="s", 
    yaxt ="s",	 		  # s=standard, n=suppress axis
    
    cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 #axis label size, color and font
    cex.main =  1.2, col.main= grey(.4),  font.main = 8,		#main label, color and font
    cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6,		#sub label, color and font

    #tick marks
    lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
    tck = -.01  			, # negative is external ticks, 1 is gridlines
    lend = "square"			, # tick mark end "round" default, "butt" butt lines, "square" square line caps
    fg = grey(.2),	      	  # foreground color
    bg = grey(.95),
    family= "serif",
    # https://nicercode.github.io/guides/plotting/
    mar=c(10,7,2,4)
  )
  
  # plot the chart
  max_v=ceiling(max(exDailyRev$daily_rev)/10^6)*10^6
  y_label<- seq(0, max_v, by = max_v/4)
  
  barplot(exDailyRev$daily_rev, width = 5, space = NULL, xlab="", ylab="",
          names.arg = exDailyRev$exchange, las=2, border=0, yaxt="n",
          horiz= FALSE, beside=FALSE,  ylim = c(0,4*10^6))
  axis(2,col="white",tck=1,col.ticks="white", at=y_label, labels = numformat(y_label/10^6), las=2)
  title(ylab="Daily Revenue", line=5, cex.lab=1.5) 
}

# =================================================
# plot Token Correlation Matrix
plotCorrMatrix<-function(cormat){
  par(
    lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
    lwd = 1,				  # function "lines" takes a vector of values for more than one line, and will cycle through that vector
    pch = ".",				  # point types
    bty = "n",								#suppresses box around the plot, keep part by letters like "c" 
    cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
    xaxt ="s", 
    yaxt ="s",	 		  # s=standard, n=suppress axis
    
    cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 #axis label size, color and font
    cex.main =  1.2, col.main= grey(.4),  font.main = 8,		#main label, color and font
    cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6,		#sub label, color and font
    
    #tick marks
    lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
    tck = -.01  			, # negative is external ticks, 1 is gridlines
    lend = "square"			, # tick mark end "round" default, "butt" butt lines, "square" square line caps
    fg = grey(.2),	      	  # foreground color
    bg = grey(.95),
    family= "serif",
    # https://nicercode.github.io/guides/plotting/
    mar=c(4,6,4,4)
  )
  
  melted_cormat <- melt(cormat)
  ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value, label=value)) + 
    geom_tile() +
    geom_text(size = 4, color="white") +
    xlab("") +
    ylab("") +
    scale_fill_gradient2(low = "gray100", high = "gray40", mid = "gray70", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation")
}