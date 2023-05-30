


#Here's a little function I wrote to gather and chart "pseudo-real time" data from yahoo:

gc();
rm(list=ls());
gc();

require(quantmod)
require(ggplot2)
require(xts)


options(scipen=999);


Times <-  NULL
Prices <- NULL

while(1) {
  
  tryCatch({
    #Load current quote
    Year <- 2018
    currentYear <- as.numeric(format(Sys.time(),'%Y'))
    while (Year != currentYear) { #Sometimes yahoo returns bad quotes
      currentQuote <- getQuote('SPY')
      Year <- as.numeric(format(currentQuote['Trade Time'],'%Y'))
    }
    
    #Add current quote to the dataset
    if (is.null(Times)) {
      Times <- Sys.time()-15*60 #Quotes are delayed 15 minutes
      Prices <- currentQuote['Last']
    } else {
      Times <- c(Times,Sys.time())
      Prices <- rbind(Prices,currentQuote['Last'])
    } 
    
    #Convert to 1-minute bars
    Data <- xts(Prices,order.by=Times)
    Data <- na.omit(to.minutes(Data,indexAt='endof'))
    
    #Plot the data when we have enough
    if (nrow(Data)>5) { 
      chartSeries(Data,theme='white',TA='addRSI(n=5);addBBands(n=5)')
    }
    
    #Wait 1 second to avoid overwhelming the server
    Sys.sleep(1)
    
    #On errors, sleep 10 seconds and hope it goes away
  },error=function(e) {print(e);Sys.sleep(10)}) 
}

