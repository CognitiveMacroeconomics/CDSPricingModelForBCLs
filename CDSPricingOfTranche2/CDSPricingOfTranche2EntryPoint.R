##This script is used to collect the data to price the CDS in Tranche 2
##
##Final input data tables that the code will work with are 
##        a: ACSSTransDataSeriesSentRecieved - The ACSS data file
##        b: 
##
##
##Final outut is a list containing the transaction volume/value time series data 
##        a: ACSSFILevelTransDataSeriesList - The list containing ACSS time series data for each FI. Each element of the list is a time series object for the FI
##        b: 
##
##
## Author : Segun Bewaji
## Creation Date : 12 May 2017
## Modified : Segun Bewaji
## Modifications Made: 
##        1)  
##           
##        2) 
##        3)  
##           
##        4)  
##           
##        5) 
##        6)  
## Modified :  
## Modifications Made:
##        7) 
##        8) 
##           
##        9)  
##           
##       10) 
##
##
## 
## $Id$

##the usual
gc();
rm(list=ls());
gc();


##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<IMPORT REQUIRED LIBRARIES>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##The use of require forces R to install the library if not already installed
require(plyr);# contains method and function calls to be used to filter and select subsets of the data
require(lubridate);# handles that date elements of the data
require(dplyr);# contains method and function calls to be used to filter and select subsets of the data
require(data.table);#used to store and manipiulate data in the form of a data table similar to a mySQL database table
require(xts);#used to manipulate date-time data into quarters
require(ggplot2);#used to plot data
require(zoo);#for date conventions used in financial analysis and quantitiative analysis
require(magrittr);#contains method/function calls to use for chaining e.g. %>%
require(scales);#component of ggplot2 that requires to be manually loaded to handle the scaling of various axies
require(foreach);#handles computations for each element of a vector/data frame/array/etc required the use of %do% or %dopar% operators in base R
require(iterators);#handles iteration through for each element of a vector/data frame/array/etc
require(stringr);#handles strings. Will be used to remove all quotation marks from the data
require(moments);#3rd-party library used to compute statistics such as skewness and kurtosis which are not provided in the base R package set
require(rlist);#3-party package to allow more flexible and java-collections like handling of lists in R. Allows you to save a list as a Rdata file
require(foreign);#to export data into formats readable by STATA and other statistical packages
require(xlsx);#3-party package to export data into excel xlsx format


startYr <- 2005;
endYr <- 2016;


inputdataPath <- c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/protectionSellerDailySummaryPrmiumAndDefaultLegData", startYr,"-",endYr,".Rdata", sep = ""));

##Load input data file
#load("C:/Projects/FundingDataTables/Cleaned Transactions Data/lvtsBilateralTXNsNetFlowDataSeries2005-2016.Rdata");
load(inputdataPath);
