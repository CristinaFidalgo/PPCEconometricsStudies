#-----------------------------------------------------------------------------#
#              Get raw data and prepare data for further analysis             #
#-----------------------------------------------------------------------------#

# Set new directory
setwd("C:/Users/crist/OneDrive/Ambiente de Trabalho/Master Thesis/Data")

startYear = 1993
EndYear = 2019

# Load time series library
if (!require("tis")) {install.packages("tis"); library("tis")}
if (!require("xts")) {install.packages("xts"); library("xts")}
if (!require("zoo")) {install.packages("zoo"); library("zoo")}
if (!require("Hmisc")) {install.packages("Hmisc"); library("Hmisc")}
if (!require("seasonal")) {install.packages("seasonal"); library("seasonal")}
if (!require("reshape2")) {install.packages("reshape2"); library("reshape2")}
if (!require("stats")) {install.packages("stats"); library("stats")}

# Load charts library
if (!require("ggplot2")) {install.packages("ggplot2"); library("ggplot2")} 
if (!require("lubridate")) {install.packages("lubridate"); library("lubridate")}
if (!require("dplyr")) {install.packages("dplyr"); library("dplyr")}
if (!require("dygraphs")) {install.packages("dygraphs"); library("dygraphs")}
if (!require("plyr")) {install.packages("plyr"); library("plyr")}
if (!require("gridExtra")) {install.packages("gridExtra"); library("gridExtra")}

# Load time series analysis library
if (!require("urca")) {install.packages("urca"); library("urca")}
if (!require("vars")) {install.packages("vars"); library("vars")}
if (!require("tsDyn")) {install.packages("tsDyn"); library("tsDyn")}
if (!require("forecast")) {install.packages("forecast"); library("forecast")}
if (!require("multDM")) {install.packages("multDM"); library("multDM")}

# Load data sources library
if (!require("imfr")) {install.packages("imfr"); library("imfr")}
#if (!require("devtools")) {install.packages("devtools"); library("devtools")}
#install_github("expersso/BIS")
if (!require("BIS")) {install.packages("BIS"); library("BIS")}

# Download data from IMF - CPI
CPIDataBase <- imf_data(database_id = 'IFS', indicator = 'PCPI_IX', country = c('BG', 'HR', 'CZ', 'HU', 'PL', 'RO', 'DE', 'US'), start = startYear, end = EndYear, freq = 'M')
CPIDataBase = dcast(CPIDataBase, year_month  ~ iso2c, value.var="PCPI_IX", fun.aggregate=sum)

# Download data from BIS - Rates
BISDataBase <- get_bis('https://www.bis.org/statistics/full_webstats_xru_current_dataflow_csv.zip')
BISDataBase2 = BISDataBase[BISDataBase$ref_area %in% c("BG", "HR", "CZ", "HU", "PL", "RO", "DE"), ]
BISDataBase2 = BISDataBase2[BISDataBase2$collection %in% c("E"), ]
BISDataBase2 = BISDataBase2[BISDataBase2$freq %in% c("M"), ]
BISDataBase3 = BISDataBase2[BISDataBase2$date >= "1993-01", ]
BISDataBase4 = BISDataBase3[BISDataBase3$date <= "2019-12", ]
BISDataBase <- NULL
BISDataBase2 <- NULL
BISDataBase3 <- NULL
BISDataBase = BISDataBase4
BISDataBase4 <- NULL
BISDataBase = dcast(BISDataBase, date ~ reference_area, value.var="obs_value", fun.aggregate=sum)

# Apply data transformations in order to obtain final data
BISDataBaseBackup = BISDataBase
BISDataBase[-(1:1)] <- log10(BISDataBase[-(1:1)])
BISDataBaseFirstOperation = BISDataBase
BISDataBase = BISDataBaseBackup
BISDataBaseBackup = NULL

names(BISDataBaseFirstOperation)[names(BISDataBaseFirstOperation) == "Bulgaria"] <- "BG"
names(BISDataBaseFirstOperation)[names(BISDataBaseFirstOperation) == "Croatia"] <- "HR"
names(BISDataBaseFirstOperation)[names(BISDataBaseFirstOperation) == "Czech Republic"] <- "CZ"
names(BISDataBaseFirstOperation)[names(BISDataBaseFirstOperation) == "Germany"] <- "DE"
names(BISDataBaseFirstOperation)[names(BISDataBaseFirstOperation) == "Hungary"] <- "HU"
names(BISDataBaseFirstOperation)[names(BISDataBaseFirstOperation) == "Poland"] <- "PL"
names(BISDataBaseFirstOperation)[names(BISDataBaseFirstOperation) == "Romania"] <- "RO"
names(BISDataBaseFirstOperation)[names(BISDataBaseFirstOperation) == "date"] <- "year_month"

#CPIDataBaseBackup = CPIDataBase
#CPIDataBase[-(1:1)] <- CPIDataBase[-(1:1)]/CPIDataBase[,'DE'][row(CPIDataBase[-(1:1)])]
CPIDataBaseBackup = CPIDataBase
CPIDataBase[-(1:1)] <- log10(CPIDataBase[-(1:1)]) - log10(CPIDataBase[,'US'][row(CPIDataBase[-(1:1)])])

CPIDataBaseFirstOperation = CPIDataBase
CPIDataBase = CPIDataBaseBackup
CPIDataBaseBackup = NULL

df12 <- left_join(CPIDataBaseFirstOperation, BISDataBaseFirstOperation, by = 'year_month')
df12$BG = df12$BG.x + df12$BG.y
df12$HR = df12$HR.x + df12$HR.y
df12$CZ = df12$CZ.x + df12$CZ.y
df12$DE = df12$DE.x + df12$DE.y
df12$HU = df12$HU.x + df12$HU.y
df12$PL = df12$PL.x + df12$PL.y
df12$RO = df12$RO.x + df12$RO.y

df12 = df12[,c("year_month", "BG", "HR", "CZ", "HU", "PL", "RO", "DE")]
RatesData = df12
df12 = NULL

#RatesData[,2:8] = log10(RatesData[,2:8])

# Set the start and end dates of the data used in the estimation
data.start1 <- "1993-01"
data.start  <- c(1993,1)
data.end    <- c(2019,12)

RatesData = RatesData[RatesData$year_month >= data.start1 , ]

################### Use to import data if IMF and/or BIS packages don't work ##########################

# Read data file and format date column (from character to date)
#rates.file  <- "NewData_EndHurn_Method.csv"
#rates.data  <- read.table(rates.file, skip = 0, header = TRUE, sep = ',', stringsAsFactors = FALSE)
#rates.data$Date <- ymd(rates.data$Date)

# Create time series
#rate.b      <- tis(rates.data$Bulgaria, start = data.start, tif='monthly')
#rate.cr     <- tis(rates.data$Croatia, start = data.start, tif='monthly')
#rate.cz     <- tis(rates.data$Czechia, start = data.start, tif='monthly')
#rate.h      <- tis(rates.data$Hungary, start = data.start, tif='monthly')
#rate.p      <- tis(rates.data$Poland, start = data.start, tif='monthly')
#rate.r      <- tis(rates.data$Romania, start = data.start, tif='monthly')
#rate.sl     <- tis(rates.data$Slovakia, start = data.start, tif='monthly')
#rate.g      <- tis(rates.data$Germany, start = data.start, tif='monthly')

# Create dataset
#rates.series.TFM <- cbind(rate.g, rate.h, rate.p, rate.cz)
#rates.series.data.TFM <- as.data.frame(rates.series.TFM)

#######################################################################################################

# Create time series
rate.b      <- tis(RatesData$BG, start = data.start, tif='monthly') 
rate.cr     <- tis(RatesData$HR, start = data.start, tif='monthly') 
rate.cz     <- tis(RatesData$CZ, start = data.start, tif='monthly') 
rate.h      <- tis(RatesData$HU, start = data.start, tif='monthly') 
rate.p      <- tis(RatesData$PL, start = data.start, tif='monthly') 
rate.r      <- tis(RatesData$RO, start = data.start, tif='monthly')
rate.g      <- tis(RatesData$DE, start = data.start, tif='monthly')

# Create dataframes
rate.b.df  = data.frame(rate.b=rate.b, date=format(as.Date(as.yearmon(time(rate.b))), "%Y-%m"))
rate.cr.df = data.frame(rate.cr=rate.cr, date=format(as.Date(as.yearmon(time(rate.cr))), "%Y-%m"))
rate.cz.df = data.frame(rate.cz=rate.cz, date=format(as.Date(as.yearmon(time(rate.cz))), "%Y-%m"))
rate.h.df  = data.frame(rate.h=rate.h, date=format(as.Date(as.yearmon(time(rate.h))), "%Y-%m"))
rate.p.df  = data.frame(rate.p=rate.p, date=format(as.Date(as.yearmon(time(rate.p))), "%Y-%m"))
rate.r.df  = data.frame(rate.r=rate.r, date=format(as.Date(as.yearmon(time(rate.r))), "%Y-%m"))
rate.g.df  = data.frame(rate.g=rate.g, date=format(as.Date(as.yearmon(time(rate.g))), "%Y-%m"))

rates.series2 = left_join(rate.b.df, rate.cr.df, by = 'date')
rates.series2 = left_join(rates.series2, rate.cz.df, by = 'date')
rates.series2 = left_join(rates.series2, rate.h.df, by = 'date')
rates.series2 = left_join(rates.series2, rate.p.df, by = 'date')
rates.series2 = left_join(rates.series2, rate.r.df, by = 'date')
rates.series2 = left_join(rates.series2, rate.g.df, by = 'date')

rates.series.data = rates.series2[, c(2,1,3,4,5,6,7,8)]
rates.series.dataNoData = rates.series2[, c(1,3,4,5,6,7,8)] 
rates.series.data.TFM = rates.series2[, c(8,5,6,4)]
rates.series.data.TFM.2 = rates.series2[, c(5,6,4)]

rates.series.data <- as.data.frame(rates.series.data)
rates.series.data.TFM  <- as.data.frame(rates.series.data.TFM)

rates.series.data.TFM.2  <- as.data.frame(rates.series.data.TFM.2)

rates.series.data$date <- as.Date(as.yearmon(rates.series.data$date))
rates.series.dataNoData <- as.data.frame(rates.series.dataNoData)

# Create first differences of time series
rate.b.d  <- diff(rate.b, lag = 1)
rate.cr.d <- diff(rate.cr, lag = 1)
rate.cz.d <- diff(rate.cz, lag = 1)
rate.h.d  <- diff(rate.h, lag = 1)
rate.r.d  <- diff(rate.r, lag = 1)
rate.p.d  <- diff(rate.p, lag = 1)
rate.g.d  <- diff(rate.g, lag = 1)
