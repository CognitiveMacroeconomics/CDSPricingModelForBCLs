##clear system memory and workspace
gc();
rm(list=ls());
gc();

##import require libraries 
require(plyr);# contains method and function calls to be used to filter and select subsets of the data
require(lubridate);# handles that date elements of the data
require(dplyr);# contains method and function calls to be used to filter and select subsets of the data
require(data.table);#used to store and manipiulate data in the form of a data table similar to a mySQL database table
require(xts);#used to manipulate date-time data into quarters
require(ggplot2);#used to plot data
require(magrittr);#contains method/function calls to use for chaining e.g. %>%
require(scales);#component of ggplot2 that requires to be manually loaded to handle the scaling of various axies
require(foreach);#handles computations for each element of a vector/data frame/array/etc required the use of %do% or %dopar% operators in base R
require(iterators);#handles iteration through for each element of a vector/data frame/array/etc
require(stringr);#handles strings. Will be used to remove all quotation marks from the data
require(foreign);#to export data into formats readable by STATA and other statistical packages
require(xlsx);#3-party package to export data into excel xlsx format
require(timeDate);
require(plotly);
require(quantmod);
require(gdata);
require(xtable);
require(smooth);
require(Mcomp);
require(stringr);
require(rlist);

##Supress sciientific notation 5e+10 etc
options(scipen=999);


##<<<<<<<<<<<<<<<<<<<<<<<<<DECLARE GLOBAL VARIABLES>>>>>>>>>>>>>>>>>>>>>>>>
##Declare all global variables to be used in this class
##Data extraction configiration information
##<<<<<<<<<<<<<<<<<<<<<<<<<DECLARE GLOBAL VARIABLES>>>>>>>>>>>>>>>>>>>>>>>>########
##Declare all global variables to be used in this class
##Data extraction configiration information
##Declare all global variables to be used in this class
startYrFullData <- 2000;
endYrFullData <- 2016;
startYr <- 2005;
endYr <- 2016;
systemWidePercentage <- 0.3;
liquidityEffciencyRatio <- 15;
globalDefaultProbability <- c("0.0005", "0.0008", "0.0012", "0.0016", "0.0028", "0.0064");
rateDeltaBPS <- 1;
bondFaaceValue <- 100;
rateDelta <- rateDeltaBPS*(0.01/100);
maturityPeriod <- 1;


mergedLVTSTXNsNameString <- NULL;

dataFrequency <- "dateTime"; #the options are monthly, quarterly, annual, daily, dateTime 
##(does not make sense since it does not distingush the year)
dateTimeNettingFrequency <- "1 min"; #This is used only if the dataFrequency is dateTime.
#Available options are "x min", "x hour", "x day" etc where x is the integer value of the interval

ratesBase <- "http://research.stlouisfed.org/fred2/series/"; ##FRED CAD interbank rates base source

interbankRatesSeriesID <- "IRSTCI01CAM156N";
liquidityPremiumSeriesID <- "IBLSD678FRBCLE";
zeroCouponBondIndexID <- "XBB.TO"; ##yahoo finance ticker for iShares DEX Universe Bond Index Fund
zeroCouponBondIndex2ID <- "XSB.TO"; ##yahoo finance ticker for ##iShares DEX Short Term Bond Index Fund

##interbank rates download URL
interbankRateURL <- paste0(ratesBase, interbankRatesSeriesID, '/downloaddata/', interbankRatesSeriesID, '.xls');
liquidityPremiumURL <- paste0(ratesBase, liquidityPremiumSeriesID, '/downloaddata/', liquidityPremiumSeriesID, '.xls');

###############<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<OUTPUT DATA SAVE PATHS>>>>>>>>>>>>>>>>>>>>>>>>>>###################
tranche2BilateralTXNsFlowRecyclingDataSavePath <- 
  c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/tranche2BilateralTXNsFlowRecyclingDataSeries",startYr,"-",endYr,".Rdata",
          sep = "", collapse = NULL));


tranche2BilateralTXNsFlowRecyclingSummaryDataSavePath <- 
  c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/tranche2BilateralTXNsFlowRecyclingSummaryDataSeries",startYr,"-",endYr,".Rdata",
          sep = "", collapse = NULL));

protectionSellerPrmiumAndDefaultLegDataSavePath <- 
  c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/protectionSellerPrmiumAndDefaultLegDataSeries",startYr,"-",endYr,".Rdata",
          sep = "", collapse = NULL));


protectionSellerPrmiumAndDefaultLegDataSeriesSavePath <- 
  c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/protectionSellerPrmiumAndDefaultLegData",startYr,"-",endYr,".Rdata",
          sep = "", collapse = NULL));

tranche2BilateralTXNsFlowRecyclingCSVDataSavePath <- 
  c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/tranche2BilateralTXNsFlowRecyclingDataSeries",startYr,"-",endYr,".csv",
          sep = "", collapse = NULL));

lvtsT2NetDebitCapDataPath <- c(paste("C:/Projects/FundingDataTables/Raw Transactions Data/YrsSpecificLVTST2NDC",
                                     startYr,"-",endYr,"DataSeries.Rdata", sep = "", collapse = NULL));



###################################<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<RAW RDATA FILE PATHS>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>########
##Data loading paths. Do not change the file names unless the file in which the full dataset is stored in is changed
lvtsTranche2BilateralTXNsNetFlowDataLoadPath <- 
  c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/tranche2BilateralTXNsNetFlowDataSeries",startYr,"-",endYr,".Rdata",
          sep = "", collapse = NULL));

##Used to load data manually
##load("C:/Projects/FundingDataTables/Cleaned Transactions Data/protectionSellerPrmiumAndDefaultLegData2005-2016.Rdata");

###################################<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<REQUIRED FUNCTIONS>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>########
##Load functions required during the analysis
movingAverageValue <- function(arr, n=15){
  res = arr
  for(i in n:length(arr)){
    res[i] = mean(arr[(i-n):i])
  }
  res
};



##########################<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<LOAD RAW DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#####################
##Get the tickers in order to import the data as a xts timesereis object
getSymbols("IBLSD678FRBCLE", src = 'FRED');
getSymbols("IRSTCI01CAM156N", src = 'FRED');

##Get interbank rate and other market data
tmp <- tempfile();
download.file(url = interbankRateURL, destfile = tmp);

##Convert downloaded xts file to a data.table object
IRSTCI01CAM156N_dt <- as.data.table(IRSTCI01CAM156N, keep.rownames = TRUE);
setnames(IRSTCI01CAM156N_dt, "index", "Date");
IRSTCI01CAM156N_dt <- IRSTCI01CAM156N_dt[,c("Year"):=year(Date), by=c("Date")];
IRSTCI01CAM156N_dt <- IRSTCI01CAM156N_dt[,c("Month"):=month(Date), by=c("Date")];

#rm(IRSTCI01CAM156N);
unlink(tmp);

###Pull in the liquidity premium data currenlty discontinued
## Contributions to the Cleveland Financial Stress Index: Interbank Liquidity Spread (DISCONTINUED)
tmp <- tempfile();
download.file(url = liquidityPremiumURL, destfile = tmp);

##Convert downloaded xts file to a data.table object
IBLSD678FRBCLE_dt <- as.data.table(IBLSD678FRBCLE, keep.rownames = TRUE);
setnames(IBLSD678FRBCLE_dt, "index", "Date");
IBLSD678FRBCLE_dt <- IBLSD678FRBCLE_dt[,c("Year"):=year(Date), by=c("Date")];
IBLSD678FRBCLE_dt <- IBLSD678FRBCLE_dt[,c("Month"):=month(Date), by=c("Date")];



#######<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<DOWNLOAD Bond Data>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>############
XBB <- na.omit(getSymbols(zeroCouponBondIndexID,auto.assign = FALSE, from = "2000-01-01"));

XSB <- na.omit(getSymbols(zeroCouponBondIndex2ID,auto.assign = FALSE, from = "2000-01-01"));
##Convert imported timeseries to dtata.table object
XBB_dt <- as.data.table(XBB, keep.rownames = TRUE);
XSB_dt <- as.data.table(XSB, keep.rownames = TRUE);
##Rename index columnt to date
setnames(XBB_dt, "index", "Date");
setnames(XSB_dt, "index", "Date");
##create year and month fields
#XBB_dt <- XBB_dt[,c("Year"):=year(Date), by=c("Date")];
#XBB_dt <- XBB_dt[,c("Month"):=month(Date), by=c("Date")];
#XSB_dt <- XSB_dt[,c("Year"):=year(Date), by=c("Date")];
#XSB_dt <- XSB_dt[,c("Month"):=month(Date), by=c("Date")];

#unlink(tmp)
####################<<<<<<<<<<<<<<<<<<<Print meta data>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>########################
#print(xtable(interbankRatemetadata, caption = 'Table 1: FRED metadata for Canadian Interbank Rates (IRSTCI01CAM156N)', 
#             align = rep('l', 3)), 
#      type = 'html', caption.placement = 'top', include.rownames = FALSE,
#      include.colnames = FALSE);


#########################<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<LOAD LVTS DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>############### 
load(lvtsTranche2BilateralTXNsNetFlowDataLoadPath);
load(lvtsT2NetDebitCapDataPath);
##drop unrequired fields from T2 net debit cap table (mergedLVTST2NDCYearsSpecYM)
mergedLVTST2NDCYearsSpecYM[ ,c("date", "time", "Year_Month","Year_Quarter",
                               "Year","MonthlyCount","QuarterlyCount","AnnualCount","DailyCount") := NULL];
mergedLVTST2NDCYearsSpecYM$Date <- as.Date(mergedLVTST2NDCYearsSpecYM$date.time);
mergedLVTST2NDCYearsSpecYM <- mergedLVTST2NDCYearsSpecYM[(hour(date.time)<18 & (T2NetDebitCcap > 0.0)),];##Remove BCLs after 6PM

#Check for NA values this is for data issue identification
NAIssue <- tranche2BilateralTXNsNetFlowData[(is.na(T2NetDebitCcap)),];


gc();
gc();
gc();
gc();
gc();
tranche2BilateralTXNsFlowRecyclingData <- tranche2BilateralTXNsNetFlowData;
rm(tranche2BilateralTXNsNetFlowData);
gc();
NAIssue <- tranche2BilateralTXNsFlowRecyclingData[(is.na(T2NetDebitCcap)),];

####Remove unrequired fields
tranche2BilateralTXNsFlowRecyclingData[ ,c("sequence", "t2NDCSettingDateTime", "sendLegVolume", "ExtendedVolume",
                                           "receiveLegVolume", "bilateralRiskControlPosition", "multilateralRiskControlPostion", 
                                           "RiskControlSpread", "dateTime", "RiskControlSpreadVolatility",
                                           "bilateralPosition", "cumulativeBilateralPosition", 
                                           "multilateralPosition", "cumulativeMultilateralPosition", "cumulativeValueSent", 
                                           "cumulativeValueRecieved") := NULL];
gc();
###Rename required fields to more appropriate names
setnames(tranche2BilateralTXNsFlowRecyclingData, "sender", "referenceEntity");
setnames(tranche2BilateralTXNsFlowRecyclingData, "receiver", "protectionSeller");
setnames(tranche2BilateralTXNsFlowRecyclingData, "receiverID", "protectionSellerID");
setnames(tranche2BilateralTXNsFlowRecyclingData, "receiverFullName", "protectionSellerFullName");
setnames(tranche2BilateralTXNsFlowRecyclingData, "senderID", "referenceEntityID");
setnames(tranche2BilateralTXNsFlowRecyclingData, "senderFullName", "referenceEntityFullName");
setnames(tranche2BilateralTXNsFlowRecyclingData, "sendLegValue", "valueRecieved");
setnames(tranche2BilateralTXNsFlowRecyclingData, "receiveLegValue", "valueSent");
gc();


###Remove all activity related to the Bank of Canada
tranche2BilateralTXNsFlowRecyclingData <- tranche2BilateralTXNsFlowRecyclingData[referenceEntity!="BCANCA",];
tranche2BilateralTXNsFlowRecyclingData <- tranche2BilateralTXNsFlowRecyclingData[protectionSeller!="BCANCA",];

tranche2BilateralTXNsFlowRecyclingData<-tranche2BilateralTXNsFlowRecyclingData%>%group_by(referenceEntity,protectionSeller,Date)%>%
  mutate(cumulativeValueSent=cumsum(valueRecieved));

tranche2BilateralTXNsFlowRecyclingData<-tranche2BilateralTXNsFlowRecyclingData%>%group_by(referenceEntity,protectionSeller,Date)%>%
  mutate(cumulativeValueRecieved=cumsum(valueSent));
gc();
tranche2BilateralTXNsFlowRecyclingData<-tranche2BilateralTXNsFlowRecyclingData%>%group_by(referenceEntity,protectionSeller,Date)%>%
  mutate(meanValueSent=mean(valueRecieved));

tranche2BilateralTXNsFlowRecyclingData<-tranche2BilateralTXNsFlowRecyclingData%>%group_by(referenceEntity,protectionSeller,Date)%>%
  mutate(meanValueRecieved=mean(valueSent));

gc();

gc();
####Initial attempt at getting actuallly liquidity recycled intraday
###To be revisited at a later date
tranche2BilateralTXNsFlowRecyclingData <- as.data.table(tranche2BilateralTXNsFlowRecyclingData);

#tranche2BilateralTXNsFlowRecyclingData[, c("recycledLiquidity"):=min(ExtendedValue,valueSent,valueRecieved), 
tranche2BilateralTXNsFlowRecyclingData[, c("recyclableLiquidity"):=min(ExtendedValue,valueRecieved), 
                                           by=c("date.time","referenceEntity","protectionSeller")];

tranche2BilateralTXNsFlowRecyclingData[, c("usedLiquidity"):=min(ExtendedValue,valueSent), 
                                       by=c("date.time","referenceEntity","protectionSeller")];

tranche2BilateralTXNsFlowRecyclingData[, c("usedLiquidity"):=min(ExtendedValue,valueSent), 
                                       by=c("date.time","referenceEntity","protectionSeller")];

gc();

##Computethe system levle T2NDC. This is assumed to be the sum of the maximum NDC a given FI had at its disposal on a
##given day in the LVTS. This calculation does not account for intraday changes in the NDC 
maxT2NDC <- mergedLVTST2NDCYearsSpecYM[,list(maxT2NetDebitCcap=max(T2NetDebitCcap)), 
                                       by=c("grantee","Date")];
dailySystemLevelMaxPotentialRecycling <- maxT2NDC[,list(sumT2NetDebitCcap=sum((maxT2NetDebitCcap))), 
                                                                                by=c("Date")];

##Remove day with coropt input data file. 
## This line will be removed once service desk provide the updated a extract file
dailySystemLevelMaxPotentialRecycling <- dailySystemLevelMaxPotentialRecycling[(Date !="2011-02-25"),];

##Compute moving average to make visualisation easier
dailySystemLevelMaxPotentialRecycling[, c("sumT2NetDebitCcapMovingAverage"):=movingAverageValue(sumT2NetDebitCcap),];


sumT2NetDebitCcapMovingAverageTS <- ts(dailySystemLevelMaxPotentialRecycling$sumT2NetDebitCcapMovingAverage, 
                                       start=as.Date(c("2005-1-1")), frequency=1);
plot(sumT2NetDebitCcapMovingAverageTS);
bp_sumT2NetDebitCcapMovingAverageTS <- breakpoints(sumT2NetDebitCcapMovingAverageTS  ~ 1);

plot(bp_sumT2NetDebitCcapMovingAverageTS);
plot(sumT2NetDebitCcapMovingAverageTS);
lines(bp_sumT2NetDebitCcapMovingAverageTS);

##create a summary data table and calculate key varriables
##calculate the average value sent and recieved by FI pairing  and day
tranche2BilateralTXNsFlowRecyclingDataSumary <- tranche2BilateralTXNsFlowRecyclingData[,list(expectedAvailableLiquidity=mean(recyclableLiquidity), 
                                                                                        expectedSentLiquidity=mean(usedLiquidity),
                                                                                        bilateralCreditLimitExtended=mean(ExtendedValue)), 
                                                                                       by=c("Date","referenceEntity","protectionSeller")];

##set the liquidity recycling as the minimum of the average value recieved and the avaerage value sent
tranche2BilateralTXNsFlowRecyclingDataSumary[, c("recycledLiquidity"):=min(expectedAvailableLiquidity,expectedSentLiquidity), 
                                       by=c("Date","referenceEntity","protectionSeller")];


##identify the max BCL
tranche2BilateralTXNsFlowRecyclingDataSumary[, maxBCL:=max(bilateralCreditLimitExtended),
                                             by=c("Date","protectionSeller")];


##Compute the max ASO
tranche2BilateralTXNsFlowRecyclingDataSumary[, maxASO:=systemWidePercentage*max(bilateralCreditLimitExtended),
                                          by=c("Date","protectionSeller")];


maxASOs <- tranche2BilateralTXNsFlowRecyclingDataSumary[, list(maxASO=max(maxASO)),
                                                        by=c("Date","protectionSeller")];

dailySystemLevelMaxCreditExposure <- maxASOs[,list(sumMaxASO=sum((maxASO))), 
                                                               by=c("Date")];

dailySystemLevelMaxCreditExposure <- dailySystemLevelMaxCreditExposure[(sumMaxASO>0 & Date !="2011-02-25"),];

sumMaxASOMovingAverageTS <- ts(dailySystemLevelMaxCreditExposure$sumMaxASOMovingAverage, 
                                       start=as.Date(c("2005-1-1")), frequency=1);
plot(sumMaxASOMovingAverageTS);
bp_sumMaxASOMovingAverageTS <- breakpoints(sumMaxASOMovingAverageTS  ~ 1);

plot(bp_sumMaxASOMovingAverageTS);
plot(sumMaxASOMovingAverageTS);
lines(bp_sumMaxASOMovingAverageTS);



##merge the system level liquidity leg and default leg data
protectionSellerPrmiumAndDefaultLegDataSeries <- merge(dailySystemLevelMaxCreditExposure, 
                                                       dailySystemLevelMaxPotentialRecycling);


protectionSellerPrmiumAndDefaultLegDataSeries <- protectionSellerPrmiumAndDefaultLegDataSeries[,c("Year"):=year(Date), by=c("Date")];
protectionSellerPrmiumAndDefaultLegDataSeries <- protectionSellerPrmiumAndDefaultLegDataSeries[,c("Month"):=month(Date), by=c("Date")];


setkey(IRSTCI01CAM156N_dt, Year, Month);
setkey(protectionSellerPrmiumAndDefaultLegDataSeries, Year, Month);

##Merging the premium and default leg data into the riskfree/interbank rate data
dailyProtectionSellingPricingDataSeries <- IRSTCI01CAM156N_dt[protectionSellerPrmiumAndDefaultLegDataSeries,];


dailyProtectionSellingPricingDataSeries[ ,c("Date") := NULL];
setnames(dailyProtectionSellingPricingDataSeries, "i.Date", "Date");
##Set key for merge
setkey(IBLSD678FRBCLE_dt, Date);
setkey(dailyProtectionSellingPricingDataSeries, Date);
##merge
dailyProtectionSellingPricingDataSeries <- IBLSD678FRBCLE_dt[dailyProtectionSellingPricingDataSeries,];
dailyProtectionSellingPricingDataSeries[ ,c("i.Year", "i.Month", "Year", "Month") := NULL];
##Replace the missing value liqudity premium rows with 1.44 (the last entry before the liquidity premium data was discontinued)
dailyProtectionSellingPricingDataSeries[is.na(dailyProtectionSellingPricingDataSeries)] <- 1.44;

##Merge pricing table with the bond data
setkey(XBB_dt, Date);
setkey(dailyProtectionSellingPricingDataSeries, Date);
dailyProtectionSellingPricingDataSeries <- XBB_dt[dailyProtectionSellingPricingDataSeries,];

for(i in globalDefaultProbability){
  
  ##Calculate the liquidity premium adjustment liquidityEffciencyRatio
  dailyProtectionSellingPricingDataSeries[, c("liquidityPremiumAdjustment"):=(1+(1/(liquidityEffciencyRatio))), by=c("Date")];
  
  ##Calculate the premium leg coupon 
  dailyProtectionSellingPricingDataSeries[, c("premiumLegCoupon"):=(liquidityPremiumAdjustment*(IBLSD678FRBCLE*0.01)*sumT2NetDebitCcap), by=c("Date")];
  
  ##Calculate the default leg coupon
  dailyProtectionSellingPricingDataSeries[, c(paste("defaultLegCoupon_",as.character(i), sep = "")):=(as.numeric(i)*(sumMaxASO)), by=c("Date")];
  
  
  ##Calculate the risk neutral discount factor
  dailyProtectionSellingPricingDataSeries[, c("discountFactor"):=(1/(1+(IRSTCI01CAM156N*0.01))), by=c("Date")];
  
  ##Calculate the neutral contingent leg valuation
#  dailyProtectionSellingPricingDataSeries[, c(paste("defaultLegValue_",as.character(i), sep = "")):=(defaultLegCoupon*discountFactor),
#                                          by=c("Date")];
  
  
  ##compute DV01 and PV01
  dailyProtectionSellingPricingDataSeries <- 
    dailyProtectionSellingPricingDataSeries[,c("DurationClose"):=(-1)*(-1*maturityPeriod*bondFaaceValue)*exp(-maturityPeriod*
                                                                                                               (IRSTCI01CAM156N/100))*(1/XBB.TO.Close),
                                            by=c("Date")];
  dailyProtectionSellingPricingDataSeries <- 
    dailyProtectionSellingPricingDataSeries[,c("PV01Close"):= (DurationClose/(1+((IRSTCI01CAM156N/100)/maturityPeriod)))*XBB.TO.Close*rateDelta,
                                            by=c("Date")];
  
  ##Next calculate the premium leg value
  dailyProtectionSellingPricingDataSeries[, c("premiumLegValue"):=(premiumLegCoupon*PV01Close), by=c("Date")];
  
  ##Next calculate the riusk nuetral CDS value
#  dailyProtectionSellingPricingDataSeries[, c(paste("Tranche2CDSValue_",i)):=(premiumLegValue-c(paste("defaultLegValue_",i))), by=c("Date")];
  
  
  ##Calculate the risk neutral breakeven defaut probability
  dailyProtectionSellingPricingDataSeries[, c("breakEvenPD"):=100*premiumLegValue/(sumMaxASO*discountFactor), by=c("Date")];

}


##Hard Coded till proper dynamic naming solution is figured out
dailyProtectionSellingPricingDataSeries[, c(paste("defaultLegValue_",0.0005, sep = "")):=(defaultLegCoupon_0.0005*discountFactor),by=c("Date")];
dailyProtectionSellingPricingDataSeries[, c(paste("defaultLegValue_",0.0008, sep = "")):=(defaultLegCoupon_0.0008*discountFactor),by=c("Date")];
dailyProtectionSellingPricingDataSeries[, c(paste("defaultLegValue_",0.0012, sep = "")):=(defaultLegCoupon_0.0012*discountFactor),by=c("Date")];
dailyProtectionSellingPricingDataSeries[, c(paste("defaultLegValue_",0.0016, sep = "")):=(defaultLegCoupon_0.0016*discountFactor),by=c("Date")];
dailyProtectionSellingPricingDataSeries[, c(paste("defaultLegValue_",0.0028, sep = "")):=(defaultLegCoupon_0.0028*discountFactor),by=c("Date")];
dailyProtectionSellingPricingDataSeries[, c(paste("defaultLegValue_",0.0064, sep = "")):=(defaultLegCoupon_0.0064*discountFactor),by=c("Date")];


dailyProtectionSellingPricingDataSeries[, c(paste("Tranche2CDSValue_",0.0005, sep = "")):=(premiumLegValue-defaultLegValue_0.0005),by=c("Date")];
dailyProtectionSellingPricingDataSeries[, c(paste("Tranche2CDSValue_",0.0008, sep = "")):=(premiumLegValue-defaultLegValue_0.0008),by=c("Date")];
dailyProtectionSellingPricingDataSeries[, c(paste("Tranche2CDSValue_",0.0012, sep = "")):=(premiumLegValue-defaultLegValue_0.0012),by=c("Date")];
dailyProtectionSellingPricingDataSeries[, c(paste("Tranche2CDSValue_",0.0016, sep = "")):=(premiumLegValue-defaultLegValue_0.0016),by=c("Date")];
dailyProtectionSellingPricingDataSeries[, c(paste("Tranche2CDSValue_",0.0028, sep = "")):=(premiumLegValue-defaultLegValue_0.0028),by=c("Date")];
dailyProtectionSellingPricingDataSeries[, c(paste("Tranche2CDSValue_",0.0064, sep = "")):=(premiumLegValue-defaultLegValue_0.0064),by=c("Date")];

dailyProtectionSellingPricingDataSeries <- na.omit(dailyProtectionSellingPricingDataSeries);

###########################<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<PLOT CHARTS>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>############################

plot_ly(dailySystemLevelMaxCreditExposure, x = ~Date, y = ~sumMaxASO, type = 'scatter', mode = 'lines',
        line=list(color='red')) %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Aggregate Value at Risk  (CA$)"));



dailySystemLevelMaxCreditExposure[, c("sumMaxASOMovingAverage"):=movingAverageValue(sumMaxASO), 
                                  ];

plot_ly(dailySystemLevelMaxCreditExposure, x = ~Date, y = ~sumMaxASOMovingAverage, type = 'scatter',
        mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Value at Risk (CA$)"));


plot_ly(dailySystemLevelMaxCreditExposure) %>%
  add_trace(x = ~Date, y = ~sumMaxASO, type = 'scatter', mode = 'lines', name = 'Aggregate Value at Risk',
            line = list(color = 'red'),
            hoverinfo = "text",
            text = ~paste(sumMaxASO)) %>%
  add_trace(x = ~Date, y = ~sumMaxASOMovingAverage, type = 'scatter', mode = 'lines', name = 'Moving Average', yaxis = 'y2',
            line = list(color = '#45171D'),
            hoverinfo = "text",
            text = ~paste(sumMaxASOMovingAverage)) %>%
  layout(title = "",
         xaxis = list(title = "Date"),
         yaxis = list(side = 'left', title = 'Value at Risk (CA$)', showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Moving Average (CA$)', showgrid = FALSE, zeroline = FALSE));


plot_ly(dailyProtectionSellingPricingDataSeries) %>%
  add_trace(x = ~Date, y = ~IBLSD678FRBCLE, type = 'bar', name = 'Liquidity Premium',
            marker = list(color = '#C9EFF9'),
            hoverinfo = "text",
            text = ~paste(IBLSD678FRBCLE, '%')) %>%
  add_trace(x = ~Date, y = ~IRSTCI01CAM156N, type = 'scatter', mode = 'lines', name = 'Interbank Rate', yaxis = 'y2',
            line = list(color = '#45171D'),
            hoverinfo = "text",
            text = ~paste(IRSTCI01CAM156N, '?F')) %>%
  layout(title = "",
         xaxis = list(title = "Date"),
         yaxis = list(side = 'left', title = 'Liquidity Premium %', showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Interbank Rate %', showgrid = FALSE, zeroline = FALSE));




plot_ly(dailySystemLevelMaxPotentialRecycling, x = ~Date, y = ~sumT2NetDebitCcap, type = 'scatter', mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Aggregate Maximum Possible Recycling (CA$)"));


plot_ly(dailySystemLevelMaxPotentialRecycling, x = ~Date, y = ~sumT2NetDebitCcapMovingAverage, type = 'scatter',
        mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Maximum Possible Recycling (CA$)"));


plot_ly(dailySystemLevelMaxPotentialRecycling) %>%
  add_trace(x = ~Date, y = ~sumT2NetDebitCcap, type = 'scatter', mode = 'lines', name = 'Aggregate Potential Liquidity Recycling',
            line = list(color = 'red'),
            hoverinfo = "text",
            text = ~paste(sumT2NetDebitCcap)) %>%
  add_trace(x = ~Date, y = ~sumT2NetDebitCcapMovingAverage, type = 'scatter', mode = 'lines', name = 'Moving Average', yaxis = 'y2',
            line = list(color = '#45171D'),
            hoverinfo = "text",
            text = ~paste(sumT2NetDebitCcapMovingAverage)) %>%
  layout(title = "",
         xaxis = list(title = "Date"),
         yaxis = list(side = 'left', title = 'Liquidity Recycling (CA$)', showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Moving Average (CA$)', showgrid = FALSE, zeroline = FALSE));



plot_ly(dailyProtectionSellingPricingDataSeries) %>%
  add_trace(x = ~Date, y = ~premiumLegValue, type = 'scatter', mode = 'lines', name = 'Value of Liquidity Recycling Leg',
            line = list(color = 'red'),
            hoverinfo = "text",
            text = ~paste(premiumLegValue)) %>%
  add_trace(x = ~Date, y = ~defaultLegValue, type = 'scatter', mode = 'lines', name = 'Value of Default Leg', yaxis = 'y2',
            line = list(color = '#45171D'),
            hoverinfo = "text",
            text = ~paste(defaultLegValue)) %>%
  layout(title = "",
         xaxis = list(title = "Date"),
         yaxis = list(side = 'left', title = 'Present Value (CA$)', showgrid = FALSE, zeroline = FALSE),
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Present Value (CA$)', showgrid = FALSE, zeroline = FALSE)
 );

plot_ly(dailyProtectionSellingPricingDataSeries, x = ~Date, y = ~(Tranche2CDSValue_0.0005), type = 'scatter',
        mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.05% A+)', line = list(color = 'black'), hoverinfo = "text") %>%
  add_trace(x = ~Date, y = ~(Tranche2CDSValue_0.0008), type = 'scatter', mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.08% A-)',
            line = list(color = 'purple'),
            hoverinfo = "text") %>%
  add_trace(x = ~Date, y = ~(Tranche2CDSValue_0.0012), type = 'scatter', mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.12% BBB+)',
            line = list(color = 'blue'),
            hoverinfo = "text") %>%
  add_trace(x = ~Date, y = ~(Tranche2CDSValue_0.0016), type = 'scatter', mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.16% BBB)',
            line = list(color = 'red'),
            hoverinfo = "text") %>%
  add_trace(x = ~Date, y = ~(Tranche2CDSValue_0.0028), type = 'scatter', mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.28% BBB-)',
            line = list(color = 'green'),
            hoverinfo = "text") %>%
  #  add_trace(x = ~Date, y = ~Tranche2CDSValue_0.0064, type = 'scatter', mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.64% BB+)',
  #            line = list(color = 'purple'),
  #            hoverinfo = "text") %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Value (CA$)")) %>% 
  layout(legend = list(orientation = 'h'));




plot_ly(dailyProtectionSellingPricingDataSeries, x = ~Date, y = ~movingAverageValue(Tranche2CDSValue_0.0005), type = 'scatter',
        mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.05% A+)', line = list(color = 'black'), hoverinfo = "text") %>%
  add_trace(x = ~Date, y = ~movingAverageValue(Tranche2CDSValue_0.0008), type = 'scatter', mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.08% A-)',
            line = list(color = 'purple'),
            hoverinfo = "text") %>%
  add_trace(x = ~Date, y = ~movingAverageValue(Tranche2CDSValue_0.0012), type = 'scatter', mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.12% BBB+)',
            line = list(color = 'blue'),
            hoverinfo = "text") %>%
  add_trace(x = ~Date, y = ~movingAverageValue(Tranche2CDSValue_0.0016), type = 'scatter', mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.16% BBB)',
            line = list(color = 'red'),
            hoverinfo = "text") %>%
  add_trace(x = ~Date, y = ~movingAverageValue(Tranche2CDSValue_0.0028), type = 'scatter', mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.28% BBB-)',
            line = list(color = 'green'),
            hoverinfo = "text") %>%
  #  add_trace(x = ~Date, y = ~Tranche2CDSValue_0.0064, type = 'scatter', mode = 'lines', name = 'Tranche 2 CDS Value (pd=0.64% BB+)',
  #            line = list(color = 'purple'),
  #            hoverinfo = "text") %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Value (CA$)")) %>% 
  layout(legend = list(orientation = 'h'));


plot_ly(dailyProtectionSellingPricingDataSeries, x = ~Date, y = ~premiumLegCoupon, type = 'scatter',
        mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Premium Leg Coupon (CA$)"));


plot_ly(dailyProtectionSellingPricingDataSeries, x = ~Date, y = ~PV01Close, type = 'scatter',
        mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "PV01 (CA$)"));

plot_ly(dailyProtectionSellingPricingDataSeries, x = ~Date, y = ~breakEvenPD, type = 'scatter',
        mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Breakeven Default Probability (%)"));

plot_ly(dailyProtectionSellingPricingDataSeries, x = ~Date, y = ~movingAverageValue(breakEvenPD), type = 'scatter',
        mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Breakeven Default Probability (%)"));


plot_ly(IBLSD678FRBCLE_dt, x = ~Date, y = ~(IBLSD678FRBCLE), type = 'scatter',
        mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Liquidity Premium (%)"));


plot_ly(IBLSD678FRBCLE_dt, x = ~Date, y = ~movingAverageValue(IBLSD678FRBCLE), type = 'scatter',
        mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Liquidity Premium (%)"));


plot_ly(IRSTCI01CAM156N_dt, x = ~Date, y = ~(IRSTCI01CAM156N), type = 'scatter',
        mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Interbank Rate (%)"));


plot_ly(IRSTCI01CAM156N_dt, x = ~Date, y = ~movingAverageValue(IRSTCI01CAM156N), type = 'scatter',
        mode = 'lines') %>%
  layout(#title = c(paste("System Level Liquidity Recycling (",startYr,"-",endYr,")",
    #               sep = "", collapse = NULL)),
    xaxis = list(title = "Date"),
    yaxis = list (title = "Interbank Rate (%)"));

###################################<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<END PLOT CHARTS>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>############################


##Create Data table with protection seller CDS leg information
protectionSellerPrmiumAndDefaultLegData <- tranche2BilateralTXNsFlowRecyclingDataSumary[, 
                                                                                        list(tranche2CDSPremiumLeg=sum(recycledLiquidity), 
                                                                                          tranche2CDSDefaultLeg=mean(maxASO),
                                                                                          maxBCL=mean(maxBCL)), 
                                                                                        by=c("Date","protectionSeller")];

protectionSellerPrmiumAndDefaultLegData$Date <- as.POSIXct.Date(protectionSellerPrmiumAndDefaultLegData$Date);
gc();
#, order = sample(seq_along(Date))
ggplotTranche2CDSPrmium <- ggplot(protectionSellerPrmiumAndDefaultLegData, 
                                                        aes(x=Date, y=(tranche2CDSPremiumLeg), color=protectionSeller)) +
                                                        geom_line(aes(group=protectionSeller)) + 
                                                        scale_x_datetime(breaks=date_breaks("25 weeks"), labels = date_format("%y/%m/%d")) + 
                                                        scale_y_continuous(label=dollar) +
                                                        facet_wrap(~ protectionSeller)+ ylab("Recycled TXN Value (CAD)")+ xlab("Day");

print(ggplotTranche2CDSPrmium);

save(tranche2BilateralTXNsFlowRecyclingData, file=tranche2BilateralTXNsFlowRecyclingDataSavePath);
save(protectionSellerPrmiumAndDefaultLegData, file=protectionSellerPrmiumAndDefaultLegDataSavePath);
save(tranche2BilateralTXNsFlowRecyclingDataSumary, file=tranche2BilateralTXNsFlowRecyclingSummaryDataSavePath);
save(dailyProtectionSellingPricingDataSeries, file=protectionSellerPrmiumAndDefaultLegDataSeriesSavePath);

write.csv(tranche2BilateralTXNsFlowRecyclingData, file=tranche2BilateralTXNsFlowRecyclingCSVDataSavePath);


