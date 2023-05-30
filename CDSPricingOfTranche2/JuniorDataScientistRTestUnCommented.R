

#
gc();
rm(list=ls());
gc();

#
require(quantmod)
require(ggplot2)
require(xts)

#
options(scipen=999);


#
Times <-  NULL
Prices <- NULL



while(1) {
  #
  tryCatch({
    Year <- 2018
    currentYear <- as.numeric(format(Sys.time(),'%Y'))
    while (Year != currentYear) { 
      currentQuote <- getQuote('SPY')
      Year <- as.numeric(format(currentQuote['Trade Time'],'%Y'))
    }
    
    
    #
    if (is.null(Times)) {
      Times <- Sys.time()-15*60 
      Prices <- currentQuote['Last']
    } else {
      Times <- c(Times,Sys.time())
      Prices <- rbind(Prices,currentQuote['Last'])
    } 
    
    
    #
    Data <- xts(Prices,order.by=Times)
    Data <- na.omit(to.minutes(Data,indexAt='endof'))
    
    
    #
    if (nrow(Data)>5) { 
      chartSeries(Data,theme='white',TA='addRSI(n=5);addBBands(n=5)')
    }
    
    
    #
    Sys.sleep(1)
    
    
   # 
  },error=function(e) {print(e);Sys.sleep(10)}) 
}

