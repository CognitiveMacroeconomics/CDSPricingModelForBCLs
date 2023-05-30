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



mergedLVTSTXNsNameString <- NULL;

dataFrequency <- "dateTime"; #the options are monthly, quarterly, annual, daily, dateTime 
##(does not make sense since it does not distingush the year)
dateTimeNettingFrequency <- "1 min"; #This is used only if the dataFrequency is dateTime.
#Available options are "x min", "x hour", "x day" etc where x is the integer value of the interval

tranche2BilateralTXNsFlowRecyclingDataSavePath <- 
  c(paste("E:/Projects/FundingDataTables/Cleaned Transactions Data/tranche2BilateralTXNsFlowRecyclingDataSeries",startYr,"-",endYr,".Rdata",
          sep = "", collapse = NULL));


tranche2BilateralTXNsFlowRecyclingSummaryDataSavePath <- 
  c(paste("E:/Projects/FundingDataTables/Cleaned Transactions Data/tranche2BilateralTXNsFlowRecyclingSummaryDataSeries",startYr,"-",endYr,".Rdata",
          sep = "", collapse = NULL));

protectionSellerPrmiumAndDefaultLegDataSavePath <- 
  c(paste("E:/Projects/FundingDataTables/Cleaned Transactions Data/protectionSellerPrmiumAndDefaultLegDataSeries",startYr,"-",endYr,".Rdata",
          sep = "", collapse = NULL));


protectionSellerDailySummaryPrmiumAndDefaultLegDataSavePath <- 
  c(paste("E:/Projects/FundingDataTables/Cleaned Transactions Data/protectionSellerDailySummaryPrmiumAndDefaultLegData",startYr,"-",endYr,".Rdata",
          sep = "", collapse = NULL));

tranche2BilateralTXNsFlowRecyclingCSVDataSavePath <- 
  c(paste("E:/Projects/FundingDataTables/Cleaned Transactions Data/tranche2BilateralTXNsFlowRecyclingDataSeries",startYr,"-",endYr,".csv",
          sep = "", collapse = NULL));




###################################<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<RAW RDATA FILE PATHS>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>########
##Data loading paths. Do not change the file names unless the file in which the full dataset is stored in is changed
lvtsTranche2BilateralTXNsNetFlowDataLoadPath <- 
  c(paste("E:/Projects/FundingDataTables/Cleaned Transactions Data/tranche2BilateralTXNsNetFlowDataSeries",startYr,"-",endYr,".Rdata",
          sep = "", collapse = NULL));


load(lvtsTranche2BilateralTXNsNetFlowDataLoadPath);

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

tranche2BilateralTXNsFlowRecyclingData[ ,c("sequence", "t2NDCSettingDateTime", "sendLegVolume", "ExtendedVolume",
                                           "receiveLegVolume", "bilateralRiskControlPosition", "multilateralRiskControlPostion", 
                                           "RiskControlSpread", "dateTime", "RiskControlSpreadVolatility",
                                           "bilateralPosition", "cumulativeBilateralPosition", 
                                           "multilateralPosition", "cumulativeMultilateralPosition", "cumulativeValueSent", 
                                           "cumulativeValueRecieved") := NULL];
gc();
setnames(tranche2BilateralTXNsFlowRecyclingData, "sender", "referenceEntity");
setnames(tranche2BilateralTXNsFlowRecyclingData, "receiver", "protectionSeller");
setnames(tranche2BilateralTXNsFlowRecyclingData, "receiverID", "protectionSellerID");
setnames(tranche2BilateralTXNsFlowRecyclingData, "receiverFullName", "protectionSellerFullName");
setnames(tranche2BilateralTXNsFlowRecyclingData, "senderID", "referenceEntityID");
setnames(tranche2BilateralTXNsFlowRecyclingData, "senderFullName", "referenceEntityFullName");
setnames(tranche2BilateralTXNsFlowRecyclingData, "sendLegValue", "valueRecieved");
setnames(tranche2BilateralTXNsFlowRecyclingData, "receiveLegValue", "valueSent");
gc();

tranche2BilateralTXNsFlowRecyclingData <- tranche2BilateralTXNsFlowRecyclingData[referenceEntity!="BCANCA",];
tranche2BilateralTXNsFlowRecyclingData <- tranche2BilateralTXNsFlowRecyclingData[protectionSeller!="BCANCA",];

tranche2BilateralTXNsFlowRecyclingData<-tranche2BilateralTXNsFlowRecyclingData%>%group_by(referenceEntity,protectionSeller,Date)%>%
  mutate(cumulativeValueSent=cumsum(valueRecieved));

tranche2BilateralTXNsFlowRecyclingData<-tranche2BilateralTXNsFlowRecyclingData%>%group_by(referenceEntity,protectionSeller,Date)%>%
  mutate(cumulativeValueRecieved=cumsum(valueSent));
gc();
tranche2BilateralTXNsFlowRecyclingData<-tranche2BilateralTXNsFlowRecyclingData%>%group_by(referenceEntity,protectionSeller,Date)%>%
  mutate(meanValueRecieved=mean(valueRecieved));

tranche2BilateralTXNsFlowRecyclingData<-tranche2BilateralTXNsFlowRecyclingData%>%group_by(referenceEntity,protectionSeller,Date)%>%
  mutate(meanValueSent=mean(valueSent));
gc();

gc();

tranche2BilateralTXNsFlowRecyclingData <- as.data.table(tranche2BilateralTXNsFlowRecyclingData);

#tranche2BilateralTXNsFlowRecyclingData[, c("recycledLiquidity"):=min(ExtendedValue,valueSent,valueRecieved), 
tranche2BilateralTXNsFlowRecyclingData[, c("recyclableLiquidity"):=sum(meanValueRecieved), 
                                           by=c("Date","protectionSeller")];

tranche2BilateralTXNsFlowRecyclingData[, c("usedLiquidity"):=sum(meanValueSent), 
                                       by=c("Date","protectionSeller")];

tranche2BilateralTXNsFlowRecyclingData[, c("totalBCLs"):=sum(unique(ExtendedValue)), 
                                       by=c("Date","protectionSeller")];

tranche2BilateralTXNsFlowRecyclingData[, c("mndcCappedBLC"):=systemWidePercentage*max(totalBCLs),
                                       by=c("Date","protectionSeller")];


##Compute the max ASO
tranche2BilateralTXNsFlowRecyclingData[, c("maxASO"):=systemWidePercentage*max(ExtendedValue),
                                             by=c("Date","protectionSeller")];


gc();

##create a summary data table and calculate key varriables
##calculate the average value sent and recieved by FI pairing  and day
tranche2BilateralTXNsFlowRecyclingData <- tranche2BilateralTXNsFlowRecyclingData[,c("expectedAvailableLiquidity"):=mean(recyclableLiquidity),
                                                                                       by=c("Date","referenceEntity","protectionSeller")];

tranche2BilateralTXNsFlowRecyclingData <- tranche2BilateralTXNsFlowRecyclingData[,c("expectedSentLiquidity"):=mean(usedLiquidity),
                                                                                 by=c("Date","referenceEntity","protectionSeller")];


##set the liquidity recycling as the minimum of the average value recieved and the avaerage value sent
tranche2BilateralTXNsFlowRecyclingData[, c("recycledLiquidity"):=min(expectedAvailableLiquidity,expectedSentLiquidity), 
                                       by=c("Date","referenceEntity","protectionSeller")];


##Create Data table with protection seller CDS leg information
protectionSellerPrmiumAndDefaultLegData <- tranche2BilateralTXNsFlowRecyclingData[, list(tranche2CDSPremiumLeg=sum(recycledLiquidity), 
                                                                                             potentialTranche2CDSPremiumLeg=max(totalBCLs), 
                                                                                         potentialTranche2NDCCappedCDSPremiumLeg=max(mndcCappedBLC), 
                                                                                        tranche2CDSDefaultLeg=mean(maxASO)), 
                                                                                        by=c("Date","protectionSeller")];

protectionSellerPrmiumAndDefaultLegData$Date <- as.POSIXct.Date(protectionSellerPrmiumAndDefaultLegData$Date);
gc();
dailyProtectionSellersPrmiumAndDefaultLegData <- 
                      protectionSellerPrmiumAndDefaultLegData[,list(premiumLeg=sum(potentialTranche2NDCCappedCDSPremiumLeg), 
                                                defaultLeg=sum(tranche2CDSDefaultLeg)), by=c("Date")];
#, order = sample(seq_along(Date))
ggplotTranche2CDSPrmium <- ggplot(protectionSellerPrmiumAndDefaultLegData, 
                                                        aes(x=Date, y=(tranche2CDSPremiumLeg), color=protectionSeller)) +
                                                        geom_line(aes(group=protectionSeller)) + 
                                                        scale_x_datetime(breaks=date_breaks("25 weeks"), labels = date_format("%y/%m/%d")) + 
                                                        scale_y_continuous(label=dollar) +
                                                        facet_wrap(~ protectionSeller)+ ylab("Recycled TXN Value (CAD)")+ xlab("Day");


ggplotPotentialTranche2CDSPrmium <- ggplot(protectionSellerPrmiumAndDefaultLegData, 
                                  aes(x=Date, y=(potentialTranche2CDSPremiumLeg), color=protectionSeller)) +
                                  geom_line(aes(group=protectionSeller)) + 
                                  scale_x_datetime(breaks=date_breaks("25 weeks"), labels = date_format("%y/%m/%d")) + 
                                  scale_y_continuous(label=dollar) +
                                  facet_wrap(~ protectionSeller)+ ylab("Max Potnetial Recycled TXN Value (CAD)")+ xlab("Day");


ggplotPotentialTranche2MNDCCappedCDSPrmium <- ggplot(protectionSellerPrmiumAndDefaultLegData, 
                                           aes(x=Date, y=(potentialTranche2NDCCappedCDSPremiumLeg), color=protectionSeller)) +
                                            geom_line(aes(group=protectionSeller)) + 
                                            scale_x_datetime(breaks=date_breaks("25 weeks"), labels = date_format("%y/%m/%d")) + 
                                            scale_y_continuous(label=dollar) +
                                            facet_wrap(~ protectionSeller)+ ylab("Max Potnetial MNDC Capped Recycled TXN Value (CAD)") +
                                            xlab("Day");


ggplotTranche2CDSDefaultLeg <- ggplot(protectionSellerPrmiumAndDefaultLegData, 
                                  aes(x=Date, y=(tranche2CDSDefaultLeg), color=protectionSeller)) +
                                  geom_line(aes(group=protectionSeller)) + 
                                  scale_x_datetime(breaks=date_breaks("25 weeks"), labels = date_format("%y/%m/%d")) + 
                                  scale_y_continuous(label=dollar) +
                                  facet_wrap(~ protectionSeller)+ ylab("Recycled TXN Value (CAD)")+ xlab("Day");


print(ggplotTranche2CDSPrmium);
print(ggplotPotentialTranche2CDSPrmium);
print(ggplotPotentialTranche2MNDCCappedCDSPrmium);
print(ggplotTranche2CDSDefaultLeg);

save(tranche2BilateralTXNsFlowRecyclingData, file=tranche2BilateralTXNsFlowRecyclingDataSavePath);
save(protectionSellerPrmiumAndDefaultLegData, file=protectionSellerPrmiumAndDefaultLegDataSavePath);
save(dailyProtectionSellersPrmiumAndDefaultLegData, file=protectionSellerDailySummaryPrmiumAndDefaultLegDataSavePath);
write.csv(tranche2BilateralTXNsFlowRecyclingData, file=tranche2BilateralTXNsFlowRecyclingCSVDataSavePath);


