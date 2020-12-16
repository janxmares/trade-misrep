# Transit trade
# Jan Mares, 190415

# Libraries
library(data.table)
library(ggplot2)  
library(fst)
library(Cairo)
library(RColorBrewer)
library(devEMF)
library(countrycode)
library(WDI)
library(stringr)

# set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/trade-misrep/data/")
# wd <- c("h:/COMTRADE/")
setwd(wd)

# years <- c(2005:2015)
years <- c(2015:2015)
# years <- c(2015)
x <- 2015

# read imports data
    imports <- data.table(read_fst(paste0("HS2/Import_", x,".fst")))
    
    # drop imports declared by the EU
    imports <- imports[reporter.ISO != "EU2",]
    
    # imports merging iso odes
    imports[, iso3c.m := reporter.ISO]
    imports[, iso3c.x := partner.ISO]
    
    #
    #
    
    # read exports data
    exports <- data.table(read_fst(paste0("HS2/Export_",x,".fst")))
    
    # drop exports reported by EU
    exports <- exports[reporter.ISO!="EU2",]

    # keep exports to countries, not other areas and "World", also, drop re-exports
    ##exports <- exports[!(is.na(partner.ISO)) & trade.flow=="Export" & partner.ISO!="" & !(partner %in% c("World")) & netweight.kg > 500,]
    exports <- exports[!(is.na(partner.ISO)) & trade.flow=="Export" & partner.ISO!="" & !(partner %in% c("World","EU2")),]
    
    # Only keep the flow where the partners report to COMTRADE
    exports <- exports[partner.ISO %in% unique(imports$reporter.ISO),]
    imports <- imports[partner.ISO %in% unique(exports$reporter.ISO),]

    # imports merging iso
    exports[, iso3c.m := partner.ISO]
    exports[, iso3c.x := reporter.ISO]
    
    # merge the data together on the matched trade flows
    # data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all = T)
    data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all = F)

    # read data on tarrifs
    tariffs <- data.table(read_fst(paste0("tariffs_",x,".fst")))

    # merge tariff data on the baseline data
    tariffs[, commodity.code := commodity.code.hs02]
    tariffs[, c('commodity.code.hs02','hs.classification') := NULL] # drop redundant columns

    data_k <- tariffs[data_k, on = c('commodity.code','iso3c.m')]
   
    # make the  observations null if they are NA, REDUNDANT AS ONLY MATCHED OBSERVATIONS ARE PRESENT
    # data_k[is.na(trade.value.USD.x), trade.value.USD.x := 0]
    # data_k[is.na(trade.value.USD.y), trade.value.USD.y := 0]
    
    # compute trade gaps at hs6 level
    # data_k[, hs6_gap := (trade.value.USD.x - trade.value.USD.y)/ (trade.value.USD.x + trade.value.USD.y)]

    # summarize the data across commodities
    data_hs6_sum <- data_k[, j = .(trade.value.mismatch = sum(abs(trade.value.USD.x - trade.value.USD.y), na.rm = T),
                                   trade.value.mismatch.costs = sum(trade.value.USD.y*0.1, na.rm = T),
                                   trade.value.mismatch.tariff = sum(trade.value.USD.y*(tariff.rate/100), na.rm = T),
                                   year=x), by = c("iso3c.m","iso3c.x")]

    data_hs6_sum <- data_hs6_sum[, j = .(trade.value.mismatch = sum(trade.value.mismatch, na.rm = T),
                                         trade.value.mismatch.costs = sum(trade.value.mismatch.costs, na.rm = T),
                                         trade.value.mismatch.tariff = sum(trade.value.mismatch.tariff, na.rm = T)),
                                         by = c("year","iso3c.m")]

head(data_hs6_sum)                                         