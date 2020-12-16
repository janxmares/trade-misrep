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
# wd <- c("c:/Users/JM/Dropbox/Research/GA CR Tax havens/")
wd <- c("h:/COMTRADE/")
setwd(wd)

# years <- c(2005:2015)
years <- c(2014)
# x <- 2014

#

# UNMATCHED EXPORTS

#
# aggregating at hs6 level, accounting for transit trade
system.time(
  data_hs6 <- lapply(years, function(x){
    
    # read imports data
    imports_1 <- data.table(read_fst(paste0("HS2/Import_",x,".fst")))
    imports_2 <- data.table(read_fst(paste0("HS2/Import_",x+1,".fst")))
    
    imports <- rbind(imports_1, imports_2)
    imports <- imports[, j = .(quantity = sum(quantity, na.rm = T), 
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)), 
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]

    # compute the unit value
    imports[, unit.value := trade.value.USD / netweight.kg]

    # drop imports declared by the EU
    imports <- imports[reporter.ISO != "EU2",]

    # imports merging iso odes
    imports[, iso3c.m := reporter.ISO]
    imports[, iso3c.x := partner.ISO]

    #
    #

    # read exports data
    exports_1 <- data.table(read_fst(paste0("HS2/Export_",x,".fst")))
    exports_2 <- data.table(read_fst(paste0("HS2/Export_",x+1,".fst")))

    exports <- rbind(exports_1, exports_2)
    exports <- exports[trade.flow=="Export", j = .(quantity = sum(quantity, na.rm = T),
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)),
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]

    # compute the unit value
    exports[, unit.value := trade.value.USD / netweight.kg]

    # drop exports reported by EU
    exports <- exports[reporter.ISO!="EU2",]

    # keep exports to countries, not other areas and "World", also, drop re-exports
    ##exports <- exports[!(is.na(partner.ISO)) & trade.flow=="Export" & partner.ISO!="" & !(partner %in% c("World")) & netweight.kg > 500,]
    exports <- exports[!(is.na(partner.ISO))  & partner.ISO!="" & !(partner %in% c("World","EU2")),]

    # Only keep the flow where the partners report to COMTRADE
    exports <- exports[partner.ISO %in% unique(imports$reporter.ISO),]
    imports <- imports[partner.ISO %in% unique(exports$reporter.ISO),]

    # imports merging iso
    exports[, iso3c.m := partner.ISO]
    exports[, iso3c.x := reporter.ISO]

    # merge the data together
    # data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all = T)

    # unmatched imports
    data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all.x = T, all.y = F)
    data_k <- data_k[is.na(trade.value.USD.y),]

    # sum across exporters
    data_k <- data_k[, j = .(trade.value.transit = sum(trade.value.USD.x, na.rm = T)), by = c("commodity.code","iso3c.x")]

    # take the transit trade (data_k) and merge it with exports data 
    # decrease the exports by transit trade proportionally to the trade volume with respective partners 
    # exports <- exports[data_k, on = c("iso3c.x","commodity.code")]

    # sum of the exports by countrya and commodity
    # exports[, x_sum := sum(trade.value.USD, na.rm = T), by = c("iso3c.x", "commodity.code")]

    # take of the proportional share of transit trade off the trade value
    # exports[, trade.value.USD.final := trade.value.USD-(i.trade.value.USD*(trade.value.USD/x_sum))]

    #
    #

    # take the transit trade and merge it with unmatched exports data
    data_um <- merge(imports, exports, by = c("commodity.code","iso3c.m","iso3c.x"), all.x = F, all.y = T)
    data_um <- data_um[is.na(trade.value.USD.x), j =.(commodity.code, iso3c.m, iso3c.x, trade.value.unmatched = trade.value.USD.y)]

    # compute unmatched trade by exporter and commodity
    data_um[, um_sum := sum(trade.value.unmatched, na.rm = T), by = c("iso3c.x","commodity.code")]
    
    # 
    # Merge with transit trade
    data_um <- merge(data_um, data_k, by = c("iso3c.x","commodity.code"), all.x = T, all.y = F)

    # replace NAs for transit trade by 0
    data_um[is.na(trade.value.transit), trade.value.transit := 0]

    # data_um <- data_um[!is.na(trade.value.unmatched), ] # drop claimed transit trade where we observe no unmatched trade

    # subtract proportional share of transit from trade from individual unmatched trade
    data_um[, trade.value.unmatched.final := trade.value.unmatched-(trade.value.unmatched/um_sum*trade.value.transit)]

    # only keep the observations with positive unmatched trade
    data_um <- data_um[trade.value.unmatched.final > 0, ] 

    # sum across commodities
    # data_um_f <- data_um[, j = .(trade.value.unmatched = sum(trade.value.unmatched.final, na.rm = T)), by = c("iso3c.m","iso3c.x")]

    # sum across importers
    data_um_f_im <- data_um[, j = .(trade.value.unmatched = sum(trade.value.unmatched.final, na.rm = T)), by = c("iso3c.m")]

    #
    #

    # get overall declared imports
    imports_sum <- imports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m")]

    # merge um exports with overall imports
    data_um_f_im <- data_um_f_im[imports_sum, on = c("iso3c.m")]

    # compute the share and order the data
    data_um_f_im[, unmatched.share := trade.value.unmatched / (trade.value.unmatched + trade.value.USD)]
    data_um_f_im <- data_um_f_im[order(-unmatched.share),]
    data_um_f_im[, country := countrycode(iso3c.m, "iso3c", "country.name")]
    data_um_f_im[, year := x]
    return(data_um_f_im)
  }
  )
)

# bind the individual years together
data_unmatched_transit <- do.call(rbind, data_hs6)

# set the names
setnames(data_unmatched_transit, names(data_unmatched_transit), paste0(names(data_unmatched_transit), ".transit"))
setnames(data_unmatched_transit, c("iso3c.m.transit","year.transit"),c("iso3c.m","year"))

# Write results into file
# write.csv(data_unmatched_transit, file = "Output/unmatched trade transit.csv", row.names = F)

#
#
#
#
#

# Same at HS4 level

# aggregating at hs6 level
system.time(
  data_hs4 <- lapply(years, function(x){
    
    # read imports data
    imports_1 <- data.table(read_fst(paste0("HS2/Import_",x,".fst")))
    imports_2 <- data.table(read_fst(paste0("HS2/Import_",x+1,".fst")))
    
    imports <- rbind(imports_1, imports_2)
    imports <- imports[, j = .(quantity = sum(quantity, na.rm = T), 
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)), 
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]

    # compute the unit value
    imports[, unit.value := trade.value.USD / netweight.kg]

    # drop imports declared by the EU
    imports <- imports[reporter.ISO != "EU2",]

    # create hs4 and hs2 levels
    imports[, hs4.cc := substr(commodity.code, 1,4)]
    
    # imports merging iso odes
    imports[, iso3c.m := reporter.ISO]
    imports[, iso3c.x := partner.ISO]

    #
    #

    # read exports data
    exports_1 <- data.table(read_fst(paste0("HS2/Export_",x,".fst")))
    exports_2 <- data.table(read_fst(paste0("HS2/Export_",x+1,".fst")))

    exports <- rbind(exports_1, exports_2)
    exports <- exports[trade.flow=="Export", j = .(quantity = sum(quantity, na.rm = T),
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)),
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]

    # compute the unit value
    exports[, unit.value := trade.value.USD / netweight.kg]

    # drop exports reported by EU
    exports <- exports[reporter.ISO!="EU2",]

    # create hs4 and hs2 levels
    exports[, hs4.cc := substr(commodity.code, 1,4)]

    # keep exports to countries, not other areas and "World", also, drop re-exports
    ##exports <- exports[!(is.na(partner.ISO)) & trade.flow=="Export" & partner.ISO!="" & !(partner %in% c("World")) & netweight.kg > 500,]
    exports <- exports[!(is.na(partner.ISO)) & partner.ISO!="" & !(partner %in% c("World","EU2")),]

    # Only keep the flow where the partners report to COMTRADE
    exports <- exports[partner.ISO %in% unique(imports$reporter.ISO),]
    imports <- imports[partner.ISO %in% unique(exports$reporter.ISO),]

    # imports merging iso
    exports[, iso3c.m := partner.ISO]
    exports[, iso3c.x := reporter.ISO]

    
    # unmatched imports
    data_k <- merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all.x = T, all.y = F)
    data_k <- data_k[is.na(trade.value.USD.y),]
    data_k[, hs4.cc := substr(commodity.code, 1, 4)]

    # sum across exporters
    data_k <- data_k[, j = .(trade.value.transit = sum(trade.value.USD.x, na.rm = T)), by = c("hs4.cc","iso3c.x")]

    # sum export and imports by hs4.cc
    imports <- imports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m","iso3c.x","hs4.cc")]
    exports <- exports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m","iso3c.x","hs4.cc")]

    # take the transit trade (data_k) and merge it with exports data 
    # decrease the exports by transit trade proportionally to the trade volume with respective partners 
    # exports <- exports[data_k, on = c("iso3c.x","hs4.cc")]

    # sum of the exports by countrya and commodity
    # exports[, x_sum := sum(trade.value.USD, na.rm = T), by = c("iso3c.x", "hs4.cc")]

    # take of the proportional share of transit trade off the trade value
    # exports[, trade.value.USD.final := trade.value.USD-(i.trade.value.USD*(trade.value.USD/x_sum))]

    #
    #

    # take the transit trade and merge it with unmatched exports data
    data_um <- merge(imports, exports, by = c("hs4.cc","iso3c.m","iso3c.x"), all.x = F, all.y = T)
    data_um <- data_um[is.na(trade.value.USD.x), j =.(hs4.cc, iso3c.m, iso3c.x, trade.value.unmatched = trade.value.USD.y)]

    # compute unmatched trade by exporter and commodity
    data_um[, um_sum := sum(trade.value.unmatched, na.rm = T), by = c("iso3c.x","hs4.cc")]

    # 
    # Merge with transit trade
    data_um <- merge(data_um, data_k, by = c("iso3c.x","hs4.cc"), all.x = T, all.y = F)

    data_um[is.na(trade.value.transit), trade.value.transit := 0]
    # data_um <- data_um[!is.na(trade.value.unmatched), ] # drop claimed transit trade where we observe no unmatched trade

    # subtract proportional share of transit from trade from individual unmatched trade
    data_um[, trade.value.unmatched.final := trade.value.unmatched-(trade.value.unmatched/um_sum*trade.value.transit)]

    # only keep the observations with positive unmatched trade
    data_um <- data_um[trade.value.unmatched.final > 0, ] 

    # sum across commodities
    # data_um_f <- data_um[, j = .(trade.value.unmatched = sum(trade.value.unmatched.final, na.rm = T)), by = c("iso3c.m","iso3c.x")]

    # sum across importers
    data_um_f_im <- data_um[, j = .(trade.value.unmatched = sum(trade.value.unmatched.final, na.rm = T)), by = c("iso3c.m")]

    #
    #

    # get overall declared imports
    imports_sum <- imports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m")]

    # merge um exports with overall imports
    data_um_f_im <- data_um_f_im[imports_sum, on = c("iso3c.m")]

    # compute the share and order the data
    data_um_f_im[, unmatched.share := trade.value.unmatched / (trade.value.unmatched + trade.value.USD)]
    data_um_f_im <- data_um_f_im[order(-unmatched.share),]
    data_um_f_im[, country := countrycode(iso3c.m, "iso3c", "country.name")]
    data_um_f_im[, year := x]
    return(data_um_f_im)
  }
  )
)

# Bind the data from individual years together
data_unmatched_transit_hs4 <- do.call(rbind, data_hs4)
names(data_unmatched_transit_hs4)

# Set the names
setnames(data_unmatched_transit_hs4, names(data_unmatched_transit_hs4), paste0(names(data_unmatched_transit_hs4), ".transit.hs4"))
setnames(data_unmatched_transit_hs4, c("iso3c.m.transit.hs4","year.transit.hs4"),c("iso3c.m","year"))

# Write results into file
# write.csv(data_unmatched_transit_hs4, file = "Output/unmatched trade transit hs4.csv", row.names = F)

#
#

# Unmatched exports without accounting for transit trade

#
#

system.time(
  data_hs6 <- lapply(years, function(x){
    
    # read imports data
    imports_1 <- data.table(read_fst(paste0("HS2/Import_",x,".fst")))
    imports_2 <- data.table(read_fst(paste0("HS2/Import_",x+1,".fst")))
    
    imports <- rbind(imports_1, imports_2)
    imports <- imports[, j = .(quantity = sum(quantity, na.rm = T), 
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)), 
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]

    # compute the unit value
    imports[, unit.value := trade.value.USD / netweight.kg]

    # drop imports declared by the EU
    imports <- imports[reporter.ISO != "EU2",]
    
    # imports merging iso odes
    imports[, iso3c.m := reporter.ISO]
    imports[, iso3c.x := partner.ISO]
    
    #
    #
    
    # read exports data
    exports_1 <- data.table(read_fst(paste0("HS2/Export_",x,".fst")))
    exports_2 <- data.table(read_fst(paste0("HS2/Export_",x+1,".fst")))

    exports <- rbind(exports_1, exports_2)
    exports <- exports[trade.flow=="Export", j = .(quantity = sum(quantity, na.rm = T),
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)),
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]

    # compute the unit value
    exports[, unit.value := trade.value.USD / netweight.kg]

    # drop exports reported by EU
    exports <- exports[reporter.ISO!="EU2",]

    # keep exports to countries, not other areas and "World", also, drop re-exports
    ##exports <- exports[!(is.na(partner.ISO)) & trade.flow=="Export" & partner.ISO!="" & !(partner %in% c("World")) & netweight.kg > 500,]
    exports <- exports[!(is.na(partner.ISO)) & partner.ISO!="" & !(partner %in% c("World","EU2")),]
    
    # Only keep the flow where the partners report to COMTRADE
    exports <- exports[partner.ISO %in% unique(imports$reporter.ISO),]
    imports <- imports[partner.ISO %in% unique(exports$reporter.ISO),]

    # imports merging iso
    exports[, iso3c.m := partner.ISO]
    exports[, iso3c.x := reporter.ISO]
    
    # merge the data together
    # data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all = T)
    data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all.x = F, all.y = T)
    
    # keep only the flows unmatched by the importers
    data_k <- data_k[is.na(trade.value.USD.x),]
    
    # make the trade value on importers side = 0 (they are NA)
    data_k[, trade.value.USD.x := 0]

    # compute trade gaps at hs6 level
    # data_k[, hs6_gap := (trade.value.USD.x - trade.value.USD.y) / (trade.value.USD.x + trade.value.USD.y)]

    # summarize the data across commodities
    data_hs6_sum <- data_k[, j = .(trade.volume.M = sum(trade.value.USD.x, na.rm = T),
                                   trade.volume.X = sum(trade.value.USD.y, na.rm = T),
                                   # hs6_gap_avg = mean(hs6_gap, na.rm = T),
                                   year=x), by = c("iso3c.m","iso3c.x")]
    return(data_hs6_sum)
  }
  )
)

# bind the individual years together
# data_hs6_dt <- do.call(rbind, data_hs6)
data_hs6_dt_unmatched <- do.call(rbind, data_hs6)

# create a summarizing column
data_hs6_dt_unmatched[, trade.value.unmatched := abs(trade.volume.M - trade.volume.X)]
data_hs6_dt_unmatched <- data_hs6_dt_unmatched[, j = .(trade.value.unmatched = sum(trade.value.unmatched, na.rm = T)),
                                                 by = c("iso3c.m","year")]

# Set the names
setnames(data_hs6_dt_unmatched, names(data_hs6_dt_unmatched), paste0(names(data_hs6_dt_unmatched), ".notransit"))
setnames(data_hs6_dt_unmatched, c("iso3c.m.notransit","year.notransit"),c("iso3c.m","year"))

#
#

# Merge the results together

#
#

# data_f <- data_unmatched_transit[data_unmatched_transit_hs4, on = c("year","iso3c.m")]
# data_f <- data_f[data_hs6_dt_unmatched, on = c("year","iso3c.m")]

data_f <- merge(data_unmatched_transit, data_unmatched_transit_hs4, by = c("year","iso3c.m"), all = T)
data_f <- merge(data_f, data_hs6_dt_unmatched, by = c("year","iso3c.m"), all = T)


#
#

# filter the required columns
data_f <- data_f[, j = .(iso3c.m, year, country = countrycode(iso3c.m, "iso3c", "country.name"),
                         trade.value.USD.all = trade.value.USD.transit,
                         trade.value.unmatched.notransit,
                         trade.value.unmatched.transit,
                         trade.value.unmatched.transit.hs4)]

# checks
data_f[trade.value.unmatched.notransit >= trade.value.unmatched.transit & 
       trade.value.unmatched.transit >= trade.value.unmatched.transit.hs4, check := TRUE ]

# compute the required categories
# trade value unmatched due to misreporting hs4 categories
data_f[, trade.value.unmatched.tt := trade.value.unmatched.notransit - trade.value.unmatched.transit]
data_f[, trade.value.unmatched.hs4 := trade.value.unmatched.transit - trade.value.unmatched.transit.hs4]
data_f[, trade.value.unmatched := trade.value.unmatched.notransit - trade.value.unmatched.tt - trade.value.unmatched.hs4]

# check
# sum(data_f$trade.value.unmatched + data_f$trade.value.unmatched.hs4 + data_f$trade.value.unmatched.tt)/1000000000

# check
data_f[(trade.value.unmatched+trade.value.unmatched.tt+trade.value.unmatched.hs4) == trade.value.unmatched.notransit, check2:=TRUE]

# final data on unmatched exports
data_f_unmatched <- data_f[, j = .(iso3c.m, year, trade.value.USD.all, trade.value.unmatched,
                                   trade.value.unmatched.tt,
                                   trade.value.unmatched.hs4)]

#
#

#

# UNMATCHED IMPORTS

#

#
#

# aggregating at hs6 level
system.time(
  data_hs6 <- lapply(years, function(x){
    
    # read imports data
    imports_1 <- data.table(read_fst(paste0("HS2/Import_",x,".fst")))
    imports_2 <- data.table(read_fst(paste0("HS2/Import_",x+1,".fst")))
    
    imports <- rbind(imports_1, imports_2)
    imports <- imports[, j = .(quantity = sum(quantity, na.rm = T), 
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)), 
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]

    # compute the unit value
    imports[, unit.value := trade.value.USD / netweight.kg]

    # drop imports declared by the EU
    imports <- imports[reporter.ISO != "EU2",]

    # imports merging iso odes
    imports[, iso3c.m := reporter.ISO]
    imports[, iso3c.x := partner.ISO]

    #
    #

    # read exports data
    exports_1 <- data.table(read_fst(paste0("HS2/Export_",x,".fst")))
    exports_2 <- data.table(read_fst(paste0("HS2/Export_",x+1,".fst")))

    exports <- rbind(exports_1, exports_2)
    exports <- exports[trade.flow=="Export", j = .(quantity = sum(quantity, na.rm = T),
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)),
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]

    # compute the unit value
    exports[, unit.value := trade.value.USD / netweight.kg]

    # drop exports reported by EU
    exports <- exports[reporter.ISO!="EU2",]

    # keep exports to countries, not other areas and "World", also, drop re-exports
    ##exports <- exports[!(is.na(partner.ISO)) & trade.flow=="Export" & partner.ISO!="" & !(partner %in% c("World")) & netweight.kg > 500,]
    exports <- exports[!(is.na(partner.ISO)) & partner.ISO!="" & !(partner %in% c("World","EU2")),]

    # Only keep the flow where the partners report to COMTRADE
    exports <- exports[partner.ISO %in% unique(imports$reporter.ISO),]
    imports <- imports[partner.ISO %in% unique(exports$reporter.ISO),]

    # imports merging iso
    exports[, iso3c.m := partner.ISO]
    exports[, iso3c.x := reporter.ISO]

    # merge the data together
    # data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all = T)

    # transit trade
    data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all.x = F, all.y = T)
    data_k <- data_k[is.na(trade.value.USD.x),]

    # sum across importers
    data_k <- data_k[, j = .(trade.value.transit = sum(trade.value.USD.y, na.rm = T)), by = c("commodity.code","iso3c.m")]

    # take the transit trade (data_k) and merge it with exports data 
    # decrease the exports by transit trade proportionally to the trade volume with respective partners 
    # exports <- exports[data_k, on = c("iso3c.x","commodity.code")]

    # sum of the exports by countrya and commodity
    # exports[, x_sum := sum(trade.value.USD, na.rm = T), by = c("iso3c.x", "commodity.code")]

    # take of the proportional share of transit trade off the trade value
    # exports[, trade.value.USD.final := trade.value.USD-(i.trade.value.USD*(trade.value.USD/x_sum))]

    #
    #

    # take the transit trade and merge it with unmatched imports data
    data_um <- merge(imports, exports, by = c("commodity.code","iso3c.m","iso3c.x"), all.x = T, all.y = F)
    data_um <- data_um[is.na(trade.value.USD.y), j =.(commodity.code, iso3c.m, iso3c.x, trade.value.unmatched = trade.value.USD.x)]

    # compute unmatched trade by exporter and commodity
    data_um[, um_sum := sum(trade.value.unmatched, na.rm = T), by = c("iso3c.m","commodity.code")]
    
    # 
    # Merge with transit trade
    data_um <- merge(data_um, data_k, by = c("iso3c.m","commodity.code"), all.x = T, all.y = F)

    # replace NAs for transit trade by 0
    data_um[is.na(trade.value.transit), trade.value.transit := 0]

    # data_um <- data_um[!is.na(trade.value.unmatched), ] # drop claimed transit trade where we observe no unmatched trade

    # subtract proportional share of transit from trade from individual unmatched trade
    data_um[, trade.value.unmatched.final := trade.value.unmatched-(trade.value.unmatched/um_sum*trade.value.transit)]

    # only keep the observations with negative unmatched trade
    data_um <- data_um[trade.value.unmatched.final > 0, ] 

    # sum across exporters
    data_um_ex <- data_um[, j = .(trade.value.unmatched = sum(abs(trade.value.unmatched.final), na.rm = T)), by = c("iso3c.x")]

    #
    #

    # get overall declared imports
    
    # compute the share and order the data
    data_um_ex[, country := countrycode(iso3c.x, "iso3c", "country.name")]
    data_um_ex[, year := x]
    return(data_um_ex)
  }
  )
)

# bind the individual years together
data_unmatched_imports_transit <- do.call(rbind, data_hs6)

# set the names
setnames(data_unmatched_imports_transit, names(data_unmatched_imports_transit), paste0(names(data_unmatched_imports_transit), ".im.transit"))
setnames(data_unmatched_imports_transit, c("iso3c.x.im.transit","year.im.transit"),c("iso3c.x","year"))

# Write results into file
# write.csv(data_unmatched_imports, file = "Output/unmatched trade imports.csv", row.names = F)

#
#
#
#
#

# Same at HS4 level

# aggregating at hs4 level
system.time(
  data_hs4 <- lapply(years, function(x){
    
    # read imports data
    imports_1 <- data.table(read_fst(paste0("HS2/Import_",x,".fst")))
    imports_2 <- data.table(read_fst(paste0("HS2/Import_",x+1,".fst")))
    
    imports <- rbind(imports_1, imports_2)
    imports <- imports[, j = .(quantity = sum(quantity, na.rm = T), 
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)), 
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]
   
    # compute the unit value
    imports[, unit.value := trade.value.USD / netweight.kg]
   
    # drop imports declared by the EU
    imports <- imports[reporter.ISO != "EU2",]

    # create hs4 and hs2 levels
    imports[, hs4.cc := substr(commodity.code, 1,4)]
    
    # imports merging iso odes
    imports[, iso3c.m := reporter.ISO]
    imports[, iso3c.x := partner.ISO]

    #
    #

    # read exports data
    exports_1 <- data.table(read_fst(paste0("HS2/Export_",x,".fst")))
    exports_2 <- data.table(read_fst(paste0("HS2/Export_",x+1,".fst")))

    exports <- rbind(exports_1, exports_2)
    exports <- exports[trade.flow=="Export", j = .(quantity = sum(quantity, na.rm = T),
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)),
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]
    
    # compute the unit value
    exports[, unit.value := trade.value.USD / netweight.kg]

    # drop exports reported by EU
    exports <- exports[reporter.ISO!="EU2",]

    # create hs4 and hs2 levels
    exports[, hs4.cc := substr(commodity.code, 1,4)]

    # keep exports to countries, not other areas and "World", also, drop re-exports
    ##exports <- exports[!(is.na(partner.ISO)) & trade.flow=="Export" & partner.ISO!="" & !(partner %in% c("World")) & netweight.kg > 500,]
    exports <- exports[!(is.na(partner.ISO)) & partner.ISO!="" & !(partner %in% c("World","EU2")),]

    # Only keep the flow where the partners report to COMTRADE
    exports <- exports[partner.ISO %in% unique(imports$reporter.ISO),]
    imports <- imports[partner.ISO %in% unique(exports$reporter.ISO),]

    # imports merging iso
    exports[, iso3c.m := partner.ISO]
    exports[, iso3c.x := reporter.ISO]

    
    # transit trade
    data_k <- merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all.x = F, all.y = T)
    data_k <- data_k[is.na(trade.value.USD.x),]
    data_k[, hs4.cc := substr(commodity.code, 1, 4)]

    # sum across importers
    data_k <- data_k[, j = .(trade.value.transit = sum(trade.value.USD.y, na.rm = T)), by = c("hs4.cc","iso3c.m")]

    # sum export and imports by hs4.cc
    imports <- imports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m","iso3c.x","hs4.cc")]
    exports <- exports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m","iso3c.x","hs4.cc")]

    # take the transit trade (data_k) and merge it with exports data 
    # decrease the exports by transit trade proportionally to the trade volume with respective partners 
    # exports <- exports[data_k, on = c("iso3c.x","hs4.cc")]

    # sum of the exports by countrya and commodity
    # exports[, x_sum := sum(trade.value.USD, na.rm = T), by = c("iso3c.x", "hs4.cc")]

    # take of the proportional share of transit trade off the trade value
    # exports[, trade.value.USD.final := trade.value.USD-(i.trade.value.USD*(trade.value.USD/x_sum))]

    #
    #

    # take the transit trade and merge it with unmatched exports data
    data_um <- merge(imports, exports, by = c("hs4.cc","iso3c.m","iso3c.x"), all.x = T, all.y = F)
    data_um <- data_um[is.na(trade.value.USD.y), j =.(hs4.cc, iso3c.m, iso3c.x, trade.value.unmatched = trade.value.USD.x)]

    # compute unmatched trade by importer and commodity
    data_um[, um_sum := sum(trade.value.unmatched, na.rm = T), by = c("iso3c.m","hs4.cc")]

    # Merge with transit trade
    data_um <- merge(data_um, data_k, by = c("iso3c.m","hs4.cc"), all.x = T, all.y = F)

    #
    data_um[is.na(trade.value.transit), trade.value.transit := 0]

    # data_um <- data_um[!is.na(trade.value.unmatched), ] # drop claimed transit trade where we observe no unmatched trade

    # subtract proportional share of transit from trade from individual unmatched trade
    data_um[, trade.value.unmatched.final := trade.value.unmatched-(trade.value.unmatched/um_sum*trade.value.transit)]

    # only keep the observations with positive unmatched trade
    data_um <- data_um[trade.value.unmatched.final > 0, ] 

    # sum across importers
    data_um_ex <- data_um[, j = .(trade.value.unmatched = sum(abs(trade.value.unmatched.final), na.rm = T)), by = c("iso3c.x")]

    # compute the share and order the data
    data_um_ex[, country := countrycode(iso3c.x, "iso3c", "country.name")]
    data_um_ex[, year := x]
    return(data_um_ex)
  }
  )
)

# Bind the data from individual years together
data_unmatched_imports_transit_hs4 <- do.call(rbind, data_hs4)

# Set the names
setnames(data_unmatched_imports_transit_hs4, names(data_unmatched_imports_transit_hs4), paste0(names(data_unmatched_imports_transit_hs4), ".im.transit.hs4"))
setnames(data_unmatched_imports_transit_hs4, c("iso3c.x.im.transit.hs4","year.im.transit.hs4"), c("iso3c.x","year"))

# Write results into file
# write.csv(data_unmatched_imports_transit_hs4, file = "Output/unmatched trade transit hs4.csv", row.names = F)

#
#
#
#
#

# Unmatched imports without accounting for transit trade

#
#

system.time(
  data_hs6 <- lapply(years, function(x){
    
    # read imports data
    imports_1 <- data.table(read_fst(paste0("HS2/Import_",x,".fst")))
    imports_2 <- data.table(read_fst(paste0("HS2/Import_",x+1,".fst")))
    
    imports <- rbind(imports_1, imports_2)
    imports <- imports[, j = .(quantity = sum(quantity, na.rm = T), 
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)), 
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]
    
    # compute the unit value
    imports[, unit.value := trade.value.USD / netweight.kg]

    # drop imports declared by the EU
    imports <- imports[reporter.ISO != "EU2",]
    
    # imports merging iso odes
    imports[, iso3c.m := reporter.ISO]
    imports[, iso3c.x := partner.ISO]
    
    #
    #
    
    # read exports data
    exports_1 <- data.table(read_fst(paste0("HS2/Export_",x,".fst")))
    exports_2 <- data.table(read_fst(paste0("HS2/Export_",x+1,".fst")))

    exports <- rbind(exports_1, exports_2)
    exports <- exports[trade.flow=="Export", j = .(quantity = sum(quantity, na.rm = T),
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)),
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]

    # compute the unit value
    exports[, unit.value := trade.value.USD / netweight.kg]

    # drop exports reported by EU
    exports <- exports[reporter.ISO!="EU2",]

    # keep exports to countries, not other areas and "World", also, drop re-exports
    ##exports <- exports[!(is.na(partner.ISO)) & trade.flow=="Export" & partner.ISO!="" & !(partner %in% c("World")) & netweight.kg > 500,]
    exports <- exports[!(is.na(partner.ISO)) & partner.ISO!="" & !(partner %in% c("World","EU2")),]
    
    # Only keep the flow where the partners report to COMTRADE
    exports <- exports[partner.ISO %in% unique(imports$reporter.ISO),]
    imports <- imports[partner.ISO %in% unique(exports$reporter.ISO),]

    # imports merging iso
    exports[, iso3c.m := partner.ISO]
    exports[, iso3c.x := reporter.ISO]
    
    # merge the data together
    data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all.x = T, all.y = F)
    
    # keep only the flows unmatched by the importers
    data_k <- data_k[is.na(trade.value.USD.y),]
    
    # make the trade value on exporters side = 0 (they are NA)
    data_k[, trade.value.USD.y := 0]


    # summarize the data across commodities
    data_hs6_sum <- data_k[, j = .(trade.volume.M = sum(trade.value.USD.x, na.rm = T),
                                   trade.volume.X = sum(trade.value.USD.y, na.rm = T),
                                   # hs6_gap_avg = mean(hs6_gap, na.rm = T),
                                   year=x), by = c("iso3c.m","iso3c.x")]
    
    # summarize across exporters
    data_hs6_sum <- data_hs6_sum[, j = .(trade.value.unmatched = sum(abs(trade.volume.M-trade.volume.X), na.rm = T),
                                   year=x), by = c("iso3c.x")]
    return(data_hs6_sum)
  }
  )
)

# bind the individual years together
# data_hs6_dt <- do.call(rbind, data_hs6)
data_hs6_dt_unmatched_imports <- do.call(rbind, data_hs6)
# head(data_hs6_dt_unmatched_imports)

# set the names
setnames(data_hs6_dt_unmatched_imports, names(data_hs6_dt_unmatched_imports), paste0(names(data_hs6_dt_unmatched_imports), ".im.notransit"))
setnames(data_hs6_dt_unmatched_imports, c("iso3c.x.im.notransit","year.im.notransit"),c("iso3c.x","year"))

#
#

# Merge the results together

#
#

data_f <- data_unmatched_imports_transit[data_unmatched_imports_transit_hs4, on = c("year","iso3c.x")]
data_f <- data_f[data_hs6_dt_unmatched_imports, on = c("year","iso3c.x")]

# sum(data_f$trade.value.unmatched.im.notransit)/1000000000

#
#

# filter the required columns
data_f <- data_f[, j = .(iso3c.x, year, country = countrycode(iso3c.x, "iso3c", "country.name"),
                         trade.value.unmatched.im.notransit,
                         trade.value.unmatched.im.transit,
                         trade.value.unmatched.im.transit.hs4)]

# checks
data_f[trade.value.unmatched.im.notransit >= trade.value.unmatched.im.transit &
      trade.value.unmatched.im.transit >= trade.value.unmatched.im.transit.hs4, check := TRUE ]

# compute the required categories
# trade value unmatched due to misreporting hs4 categories
data_f[, trade.value.unmatched.im.hs4 := trade.value.unmatched.im.transit - trade.value.unmatched.im.transit.hs4]
data_f[, trade.value.unmatched.im.tt := trade.value.unmatched.im.notransit - trade.value.unmatched.im.transit]
data_f[, trade.value.unmatched.im := trade.value.unmatched.im.notransit - trade.value.unmatched.im.tt - trade.value.unmatched.im.hs4]

# check
#sum(data_f$trade.value.unmatched.im + data_f$trade.value.unmatched.im.tt + data_f$trade.value.unmatched.im.hs4)/1000000000
data_f[(trade.value.unmatched.im+trade.value.unmatched.im.tt+trade.value.unmatched.im.hs4) == trade.value.unmatched.im.notransit, check2:=TRUE]

# final data on unmatched exports (change the name of iso column for merging final data)
data_f_unmatched_im <- data_f[, j = .(iso3c.m = iso3c.x, year, trade.value.unmatched.im,
                                   trade.value.unmatched.im.tt,
                                   trade.value.unmatched.im.hs4)]

# check
# sum(data_f_unmatched_im$trade.value.unmatched.im + data_f_unmatched_im$trade.value.unmatched.im.tt + data_f_unmatched_im$trade.value.unmatched.im.hs4)/1000000000

# 
#

# MATCHED TRADE FLOWS

#

#
#

# matched trades, extreme price deviations

#
#

# aggregating at hs6 level
system.time(
  data_hs6 <- lapply(years, function(x){
    
    # read imports data
    imports_1 <- data.table(read_fst(paste0("HS2/Import_",x,".fst")))
    imports_2 <- data.table(read_fst(paste0("HS2/Import_",x+1,".fst")))
    
    imports <- rbind(imports_1, imports_2)
    imports <- imports[, j = .(quantity = sum(quantity, na.rm = T), 
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)), 
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]

    # compute the unit value
    imports[, unit.value := trade.value.USD / netweight.kg]

    # drop imports declared by the EU
    imports <- imports[reporter.ISO != "EU2",]
    
    # imports merging iso odes
    imports[, iso3c.m := reporter.ISO]
    imports[, iso3c.x := partner.ISO]
    
    #
    #
    
    # read exports data
    exports_1 <- data.table(read_fst(paste0("HS2/Export_",x,".fst")))
    exports_2 <- data.table(read_fst(paste0("HS2/Export_",x+1,".fst")))

    exports <- rbind(exports_1, exports_2)
    exports <- exports[trade.flow=="Export", j = .(quantity = sum(quantity, na.rm = T),
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)),
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]
    
    # compute the unit value
    exports[, unit.value := trade.value.USD / netweight.kg]

    # drop exports reported by EU
    exports <- exports[reporter.ISO!="EU2",]

    # keep exports to countries, not other areas and "World", also, drop re-exports
    ##exports <- exports[!(is.na(partner.ISO)) & trade.flow=="Export" & partner.ISO!="" & !(partner %in% c("World")) & netweight.kg > 500,]
    exports <- exports[!(is.na(partner.ISO)) & partner.ISO!="" & !(partner %in% c("World","EU2")),]
    
    # Only keep the flow where the partners report to COMTRADE
    exports <- exports[partner.ISO %in% unique(imports$reporter.ISO),]
    imports <- imports[partner.ISO %in% unique(exports$reporter.ISO),]

    # imports merging iso
    exports[, iso3c.m := partner.ISO]
    exports[, iso3c.x := reporter.ISO]
    
    # merge the data together
    # data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all = T)
    data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all = F)

    # compute the weighted world pricesof exports for individual commodities (based on exports (FOB))
    data_p_world <- data_k[is.finite(unit.value.x), j = .(avg_p_world = weighted.mean(unit.value.x, netweight.kg.x, na.rm = T),
                                    p.sd = sd(unit.value.x, na.rm = T)),
                            by = c("commodity.code")]

    # select required columns
    data_k <- data_k[, j = .(iso3c.m, iso3c.x, commodity.code, unit.value.x, unit.value.y, trade.value.USD.x, trade.value.USD.y)]

    
    # Merge the prices together
    data_k <- data_p_world[data_k, on = c("commodity.code")]
    data_k[, ratio := abs(unit.value.x - avg_p_world)/p.sd]
    
    # keep the observations with ratio more than 1.96, that is the price is almost 2 standard deviations off
    # the world weighted average (computed on matched observations)

    data_k <- data_k[ratio > 1.96, ]
    # make the  observations null if they are NA, REDUNDANT AS ONLY MATCHED OBSERVATIONS ARE PRESENT
    # data_k[is.na(trade.value.USD.x), trade.value.USD.x := 0]
    # data_k[is.na(trade.value.USD.y), trade.value.USD.y := 0]
    

    # summarize the data across commodities
    data_hs6_sum <- data_k[, j = .(trade.value.mismatch.extreme.p = sum(abs(trade.value.USD.x - trade.value.USD.y), na.rm = T),
                                   trade.value.mismatch.extreme.p.costs = sum(trade.value.USD.y*0.1), 
                                   year=x), by = c("iso3c.m","iso3c.x")]
    
    data_hs6_sum <- data_hs6_sum[, j = .(trade.value.mismatch.extreme.p = sum(trade.value.mismatch.extreme.p, na.rm = T),
                                         trade.value.mismatch.extreme.p.costs = sum(trade.value.mismatch.extreme.p.costs, na.rm = T)),
                                         by = c("year","iso3c.m")]

    return(data_hs6_sum)
  }
  )
)

# remove redundant elements to clear memory
# rm(list=c("imports","exports","data_k"))
# gc()

# bind the individual years together
# data_hs6_dt <- do.call(rbind, data_hs6)
data_hs6_dt_mismatch_p <- do.call(rbind, data_hs6)

# Set the names
# setnames(data_hs6_dt_match_p, names(data_hs6_dt_match_p), paste0(names(data_hs6_dt_match_p), ".match.p"))

#
#
#
#
#

#
#

# matched trade, overall

# aggregating at hs6 level
system.time(
  data_hs6 <- lapply(years, function(x){
    
    # read imports data
    imports_1 <- data.table(read_fst(paste0("HS2/Import_",x,".fst")))
    imports_2 <- data.table(read_fst(paste0("HS2/Import_",x+1,".fst")))
    
    imports <- rbind(imports_1, imports_2)
    imports <- imports[, j = .(quantity = sum(quantity, na.rm = T), 
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)), 
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]
    
    # drop imports declared by the EU
    imports <- imports[reporter.ISO != "EU2",]
    
    # imports merging iso odes
    imports[, iso3c.m := reporter.ISO]
    imports[, iso3c.x := partner.ISO]
    
    #
    #
    
    # read exports data
    exports_1 <- data.table(read_fst(paste0("HS2/Export_",x,".fst")))
    exports_2 <- data.table(read_fst(paste0("HS2/Export_",x+1,".fst")))

    exports <- rbind(exports_1, exports_2)
    exports <- exports[trade.flow=="Export", j = .(quantity = sum(quantity, na.rm = T),
                               netweight.kg = sum(netweight.kg, na.rm = T),
                               trade.value.USD = sum(trade.value.USD, na.rm = T)),
                               by = c('reporter','partner','reporter.ISO','partner.ISO','commodity.code')]
    
    
    # drop exports reported by EU
    exports <- exports[reporter.ISO!="EU2",]

    # keep exports to countries, not other areas and "World", also, drop re-exports
    ##exports <- exports[!(is.na(partner.ISO)) & trade.flow=="Export" & partner.ISO!="" & !(partner %in% c("World")) & netweight.kg > 500,]
    exports <- exports[!(is.na(partner.ISO)) & partner.ISO!="" & !(partner %in% c("World","EU2")),]
    
    # Only keep the flow where the partners report to COMTRADE
    exports <- exports[partner.ISO %in% unique(imports$reporter.ISO),]
    imports <- imports[partner.ISO %in% unique(exports$reporter.ISO),]

    # imports merging iso
    exports[, iso3c.m := partner.ISO]
    exports[, iso3c.x := reporter.ISO]
    
    # merge the data together on the matched trade flows
    # data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all = T)
    data_k <-  merge(imports, exports, by = c("commodity.code", "iso3c.m", "iso3c.x"), all = F)

    # make the  observations null if they are NA, REDUNDANT AS ONLY MATCHED OBSERVATIONS ARE PRESENT
    # data_k[is.na(trade.value.USD.x), trade.value.USD.x := 0]
    # data_k[is.na(trade.value.USD.y), trade.value.USD.y := 0]
    
    # compute trade gaps at hs6 level
    # data_k[, hs6_gap := (trade.value.USD.x - trade.value.USD.y)/ (trade.value.USD.x + trade.value.USD.y)]

    # summarize the data across commodities
    data_hs6_sum <- data_k[, j = .(trade.value.mismatch = sum(abs(trade.value.USD.x - trade.value.USD.y), na.rm = T),
                                   trade.value.mismatch.costs = sum(trade.value.USD.y*0.1, na.rm = T),
                                   year=x), by = c("iso3c.m","iso3c.x")]

    data_hs6_sum <- data_hs6_sum[, j = .(trade.value.mismatch = sum(trade.value.mismatch, na.rm = T),
                                         trade.value.mismatch.costs = sum(trade.value.mismatch.costs, na.rm = T)),
                                         by = c("year","iso3c.m")]

    return(data_hs6_sum)
  }
  )
)

# remove redundant elements to clear memory
# rm(list=c("imports","exports","data_k"))
# gc()

# bind the individual years together
# data_hs6_dt <- do.call(rbind, data_hs6)
data_hs6_dt_mismatch <- do.call(rbind, data_hs6)

# sum(data_hs6_dt_mismatch$trade.value.mismatch + data_hs6_dt_mismatch$trade.value.mismatch.costs) / 1000000000
# sum(data_hs6_dt_mismatch$trade.value.mismatch) / 1000000000

# Set the names
# setnames(data_hs6_dt_mismatch, names(data_hs6_dt_mismatch), paste0(names(data_hs6_dt_mismatch), ".mismatch"))

#
#

# merge the mismatch trades together
data_f_mismatch <- data_hs6_dt_mismatch_p[data_hs6_dt_mismatch, on = c("iso3c.m","year")]

data_f_mismatch[trade.value.mismatch >= trade.value.mismatch.extreme.p, check := T]
data_f_mismatch[is.na(trade.value.mismatch.extreme.p), trade.value.mismatch.extreme.p := 0]
data_f_mismatch[is.na(trade.value.mismatch.extreme.p.costs), trade.value.mismatch.extreme.p.costs := 0]

# compute the 4 and 6 categories (normal prices + deliberate misreporting)
data_f_mismatch[, trade.value.mismatch.residual := trade.value.mismatch - trade.value.mismatch.costs - (trade.value.mismatch.extreme.p - trade.value.mismatch.extreme.p.costs)]
data_f_mismatch[, trade.value.mismatch.extreme.p := trade.value.mismatch.extreme.p - trade.value.mismatch.extreme.p.costs]

# keep only required columns
data_f_mismatch <- data_f_mismatch[, j = .(year, iso3c.m, trade.value.mismatch.costs, 
                                           trade.value.mismatch.extreme.p,
                                           trade.value.mismatch.residual)]

# Check 
# View(data_f_mismatch) for 1 year only
# sum(data_f_mismatch$trade.value.mismatch.costs + data_f_mismatch$trade.value.mismatch.extreme.p + data_f_mismatch$trade.value.mismatch.residual)/1000000000

#
#

# merge unmatched exports, imports and mismatched data together
data_final <- merge(data_f_unmatched, data_f_mismatch, by = c("iso3c.m","year"), all = T)
data_final <- merge(data_final, data_f_unmatched_im, by = c("iso3c.m", "year"), all = T)

# checks (for 1 year only)
# sum(data_final$trade.value.mismatch.costs + data_final$trade.value.mismatch.extreme.p + data_final$trade.value.mismatch.residual, na.rm=T)/1000000000
# sum(data_final$trade.value.unmatched.im + data_final$trade.value.unmatched.im.tt + data_final$trade.value.unmatched.im.hs4, na.rm = T)/1000000000
# sum(data_final$trade.value.unmatched + data_final$trade.value.unmatched.hs4 + data_final$trade.value.unmatched.tt, na.rm = T)/1000000000

#
#

# read the world bank income grouping and regional grouping
wb_reg <- data.table(read.csv("c:/Users/JM/Documents/GitHub/trade-misrep/wb regions.csv", header=T, stringsAsFactors = F))

# reset problematic country names
setnames(wb_reg, c("country","iso3c.m", "region"))
wb_reg[, country:=NULL]

#
#

wb_inc <- data.table(read.csv("c:/Users/JM/Documents/GitHub/trade-misrep/wb income grouping.csv", header=T, stringsAsFactors = F))

# reset problematic column names
setnames(wb_inc, c("iso3c.m","country",names(wb_inc)[3:length(names(wb_inc))]))

wb_inc <- melt(wb_inc, id.vars=c("iso3c.m"), measure.vars = names(wb_inc)[3:length(names(wb_inc))], 
               value.name="i.group", variable.name = "year")

# substitute year
wb_inc[, year:=as.numeric(substr(year,2,5))]

# only relevant years
wb_inc <- wb_inc[year>2009,]

#
#

# merge the misreported trade data with world bank regions and income groups
data_final <- wb_inc[data_final, on = c("iso3c.m","year"), nomatch = 0]
data_final <- wb_reg[data_final, on = c("iso3c.m"), nomatch = 0]

# iso3c
# View(data_final)
# write.csv(data_final, file="Output/Reported trade gap decomposition 2015 final.csv", row.names = F)
write.csv(data_final, file="c:/Users/JM/Documents/GitHub/trade-misrep/reported trade gap decomposition 2y overlap (14-15).csv", row.names = F)

#
#
#
#
#

# Analysis and figures

#
#
#
#
#

# read the data
data_final <- data.table(read.csv("c:/Users/JM/Documents/GitHub/trade-misrep/reported trade gap decomposition 2y overlap (14-15).csv", header = T, stringsAsFactors= F))

# GDP data from the WB
wdi.gdp <- data.table(WDI(indicator="NY.GDP.MKTP.CD", start=2010, end=2015))
wdi.gdp[, iso3c.m:=countrycode(iso2c,"iso2c","iso3c")] # add iso3 column to WB data
wdi.gdp <- wdi.gdp[, j = .(year, ngdp = NY.GDP.MKTP.CD, iso3c.m)]

# merge final data with GDP
data_final <- wdi.gdp[data_final, on = c("iso3c.m","year")]

# explore 
# names(data_final)
# nrow(data_final)

data_final[is.na(region), region := "Latin America & Caribbean"] # (fix for Montserrat)
data_final[is.na(i.group), i.group := "H"] # (fix for Montserrat)

# values of overall commodity trade 2010-2015
# check <- data_final[, j=.(total_x=sum(trade.value.all.x, na.rm = T), total_m = sum(trade.value.all.m, na.rm = T),
#                           total_all = sum(trade.value.all, na.rm = T)), by = c("year")]

#
#

#
# merge the values for transit trade for unmatched imports and exports
# also add up the hs4 misclassifications and unmatched in general

data_final[, trade.value.unmatched.all := trade.value.unmatched + trade.value.unmatched.im]
data_final[, trade.value.unmatched.all.tt := trade.value.unmatched.tt + trade.value.unmatched.im.tt]
data_final[, trade.value.unmatched.all.hs4 := trade.value.unmatched.hs4 + trade.value.unmatched.im.hs4]

# drop the redundant columns 
data_final[, c("trade.value.unmatched","trade.value.unmatched.im","trade.value.unmatched.tt",
              "trade.value.unmatched.im.tt", "trade.value.unmatched.hs4","trade.value.unmatched.im.hs4") := NULL]

# compute overall gap
data_final[, trade.gap := trade.value.mismatch.costs + trade.value.mismatch.extreme.p + trade.value.mismatch.residual+ 
                          trade.value.unmatched.all + trade.value.unmatched.all.tt + trade.value.unmatched.all.hs4]

#
#

# Figures

#
#

# gap decomposition, yearly
gap_yearly <- data_final[, j = .(trade.value.mismatch.costs = sum(trade.value.mismatch.costs, na.rm = T)/1000000000,
                          trade.value.mismatch.extreme.p = sum(trade.value.mismatch.extreme.p, na.rm = T)/1000000000,
                          trade.value.mismatch.residual = sum(trade.value.mismatch.residual, na.rm = T)/1000000000,
                          trade.value.unmatched.all = sum(trade.value.unmatched.all, na.rm = T)/1000000000,
                          trade.value.unmatched.all.tt = sum(trade.value.unmatched.all.tt, na.rm = T)/1000000000,
                          trade.value.unmatched.all.hs4 = sum(trade.value.unmatched.all.hs4, na.rm = T)/1000000000,
                          trade.value.all = sum(trade.value.USD.all, na.rm = T)/1000000000,
                          trade.gap = sum(trade.gap, na.rm = T)/1000000000), by = c("year")]

# check
# gap_yearly

#
# Gap decomposition
#

# melt for plot
gap_yearly_l <- data.table(melt(gap_yearly, id.vars = c("year"), measure.vars = names(gap_yearly)[2:length(names(gap_yearly))],
                     variable.name = "component", value.name = "value"))

# redefine the series' names
gap_yearly_l[component == "trade.value.mismatch.costs", component:="Trade costs"]
gap_yearly_l[component == "trade.value.mismatch.extreme.p", component:="Abnormal prices"]
gap_yearly_l[component == "trade.value.mismatch.residual", component:="Residual"]
gap_yearly_l[component == "trade.value.unmatched.all", component:="Unmatched trade"]
gap_yearly_l[component == "trade.value.unmatched.all.hs4", component:="Product misclassification"]
gap_yearly_l[component == "trade.value.unmatched.all.tt", component:="Country misclassification"]

# make component into factor - ordered
gap_yearly_l[, component := factor(component, c("trade.value.all","trade.gap","Country misclassification","Product misclassification","Unmatched trade","Abnormal prices","Trade costs","Residual"),
                                   ordered = T)]


# define series to plot
plot_series <- c("Trade costs", "Abnormal prices", "Residual", "Unmatched trade",
                 "Product misclassification", "Country misclassification")

# set the colour pallette
cpal <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#333333")

fig_gap_yearly <- ggplot(gap_yearly_l[component %in% plot_series, ], aes(x = year, y = value, fill = component))
fig_gap_yearly + geom_bar(stat = "identity") + coord_flip()+ xlab("") + ylab("USD bil.") + scale_fill_manual(values = cpal, name = "") +
                 scale_x_continuous(breaks=c(2010:2015), labels = c("2010","2011","2012","2013","2014","2015")) + theme_bw() + theme(legend.position="bottom")

# write emf
emf(file = "Final/figures/gap_decomposition_2010-2015.emf", width = 9, height = 6)
fig_gap_yearly + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("USD bil.") + scale_fill_manual(values = cpal, name = "") +
                 scale_x_continuous(breaks=c(2010:2015), labels = c("2010","2011","2012","2013","2014","2015")) + theme_bw() + theme(legend.position="bottom")
dev.off()

cairo_ps(file = "Final/figures/gap_decomposition_2010-2015.eps", width = 9, height = 6)
fig_gap_yearly + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("USD bil.") + scale_fill_manual(values = cpal, name = "") +
                 scale_x_continuous(breaks=c(2010:2015), labels = c("2010","2011","2012","2013","2014","2015")) + theme_bw() + theme(legend.position="bottom")
dev.off()


#
# Gap decomposition, shares of the gap
#

# gap decomposition, yearly, shares of gap
gap_yearly <- data_final[, j = .(trade.value.mismatch.costs = sum(trade.value.mismatch.costs, na.rm = T)/1000000000,
                          trade.value.mismatch.extreme.p = sum(trade.value.mismatch.extreme.p, na.rm = T)/1000000000,
                          trade.value.mismatch.residual = sum(trade.value.mismatch.residual, na.rm = T)/1000000000,
                          trade.value.unmatched.all = sum(trade.value.unmatched.all, na.rm = T)/1000000000,
                          trade.value.unmatched.all.tt = sum(trade.value.unmatched.all.tt, na.rm = T)/1000000000,
                          trade.value.unmatched.all.hs4 = sum(trade.value.unmatched.all.hs4, na.rm = T)/1000000000,
                          trade.value.all = sum(trade.value.USD.all, na.rm = T)/1000000000,
                          trade.gap = sum(trade.gap, na.rm = T)/1000000000), by = c("year")]

# check
gap_yearly[, trade.value.mismatch.costs.share := trade.value.mismatch.costs/trade.gap*100]
gap_yearly[, trade.value.mismatch.extreme.p.share := trade.value.mismatch.extreme.p/trade.gap*100]
gap_yearly[, trade.value.mismatch.residual.share := trade.value.mismatch.residual/trade.gap*100]
gap_yearly[, trade.value.unmatched.all.share := trade.value.unmatched.all/trade.gap*100]
gap_yearly[, trade.value.unmatched.all.tt.share := trade.value.unmatched.all.tt/trade.gap*100]
gap_yearly[, trade.value.unmatched.all.hs4.share := trade.value.unmatched.all.hs4/trade.gap*100]

gap_yearly_shares <- gap_yearly[, j=.(year, trade.value.mismatch.costs.share, trade.value.mismatch.extreme.p.share,
                                      trade.value.mismatch.residual.share, trade.value.unmatched.all.share,
                                      trade.value.unmatched.all.tt.share, trade.value.unmatched.all.hs4.share)]

# melt for plot
gap_yearly_shares <- data.table(melt(gap_yearly_shares, id.vars = c("year"), measure.vars = names(gap_yearly_shares)[2:length(names(gap_yearly_shares))],
                     variable.name = "component", value.name = "value"))

# redefine the series' names
gap_yearly_shares[component == "trade.value.mismatch.costs.share", component:="Trade costs"]
gap_yearly_shares[component == "trade.value.mismatch.extreme.p.share", component:="Abnormal prices"]
gap_yearly_shares[component == "trade.value.mismatch.residual.share", component:="Residual"]
gap_yearly_shares[component == "trade.value.unmatched.all.share", component:="Unmatched trade"]
gap_yearly_shares[component == "trade.value.unmatched.all.hs4.share", component:="Product misclassification"]
gap_yearly_shares[component == "trade.value.unmatched.all.tt.share", component:="Country misclassification"]

# make component into factor - ordered
gap_yearly_shares[, component := factor(component, c("Country misclassification","Product misclassification","Unmatched trade","Abnormal prices", "Trade costs", "Residual"),
                    ordered = T)]

# define series to plot
plot_series <- c("Trade costs", "Abnormal prices", "Residual", "Unmatched trade",
                 "Product misclassification", "Country misclassification")

# set the colour pallette
cpal <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#333333")

fig_gap_yearly_shares <- ggplot(gap_yearly_shares[component %in% plot_series, ], aes(x = year, y = value, fill = component))
fig_gap_yearly_shares + geom_bar(position = "fill", stat = "identity") + coord_flip() + xlab("") + ylab("%") + scale_fill_manual(values = cpal, name = "") +
                 scale_x_continuous(breaks=c(2010:2015), labels = c("2010","2011","2012","2013","2014","2015")) + theme_bw() + theme(legend.position="bottom")

# write emf
emf(file = "Final/figures/gap_shares_2010-2015.emf", width = 9, height = 6)
fig_gap_yearly_shares + geom_bar(position = "fill", stat = "identity") +  coord_flip() + xlab("") + ylab("%") + scale_fill_manual(values = cpal, name = "") +
                 scale_x_continuous(breaks=c(2010:2015), labels = c("2010","2011","2012","2013","2014","2015")) + theme_bw() + theme(legend.position="bottom")
dev.off()

cairo_ps(file = "Final/figures/gap_shares_2010-2015.eps", width = 9, height = 6)
fig_gap_yearly_shares + geom_bar(position = "fill", stat = "identity") +  coord_flip() + xlab("") + ylab("%") + scale_fill_manual(values = cpal, name = "") +
                 scale_x_continuous(breaks=c(2010:2015), labels = c("2010","2011","2012","2013","2014","2015")) + theme_bw() + theme(legend.position="bottom")
dev.off()

#
# Gap decomposition, shares of overall trade
#

# TBD

#
#

#
# Gap by region
#
gap_yearly <- data_final[, j = .(trade.value.mismatch.costs = sum(trade.value.mismatch.costs, na.rm = T)/1000000000,
                          trade.value.mismatch.extreme.p = sum(trade.value.mismatch.extreme.p, na.rm = T)/1000000000,
                          trade.value.mismatch.residual = sum(trade.value.mismatch.residual, na.rm = T)/1000000000,
                          trade.value.unmatched.all = sum(trade.value.unmatched.all, na.rm = T)/1000000000,
                          trade.value.unmatched.all.tt = sum(trade.value.unmatched.all.tt, na.rm = T)/1000000000,
                          trade.value.unmatched.all.hs4 = sum(trade.value.unmatched.all.hs4, na.rm = T)/1000000000,
                          trade.value.all = sum(trade.value.USD.all, na.rm = T)/1000000000,
                          trade.gap = sum(trade.gap, na.rm = T)/1000000000), by = c("year","region")]

# melt for plot
gap_yearly_l <- data.table(melt(gap_yearly, id.vars = c("year","region"), measure.vars = names(gap_yearly)[3:length(names(gap_yearly))],
                     variable.name = "component", value.name = "value"))

# redefine the series' names
gap_yearly_l[component == "trade.value.mismatch.costs", component:="Trade costs"]
gap_yearly_l[component == "trade.value.mismatch.extreme.p", component:="Abnormal prices"]
gap_yearly_l[component == "trade.value.mismatch.residual", component:="Residual"]
gap_yearly_l[component == "trade.value.unmatched.all", component:="Unmatched trade"]
gap_yearly_l[component == "trade.value.unmatched.all.hs4", component:="Product misclassification"]
gap_yearly_l[component == "trade.value.unmatched.all.tt", component:="Country misclassification"]

# make component into factor - ordered
gap_yearly_l[, component := factor(component, c("trade.value.all","trade.gap","Country misclassification","Product misclassification","Unmatched trade","Abnormal prices","Trade costs","Residual"),
                                   ordered = T)]


# define series to plot
plot_series <- c("Trade costs", "Abnormal prices", "Residual", "Unmatched trade",
                 "Product misclassification", "Country misclassification")

# set the colour pallette
cpal <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#333333")

fig_gap_region <- ggplot(gap_yearly_l[component %in% plot_series & year==2015, ], aes(x = region, y = value, fill = component))
fig_gap_region + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("USD bil.") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")

# write emf
emf(file = "Final/figures/gap_region_2015.emf", width = 9, height = 6)
fig_gap_region + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("USD bil.") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")
dev.off()

cairo_ps(file = "Final/figures/gap_region_2015.eps", width = 9, height = 6)
fig_gap_region + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("USD bil.") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")
dev.off()

#
#

#
# Gap by income group
#
gap_yearly <- data_final[, j = .(trade.value.mismatch.costs = sum(trade.value.mismatch.costs, na.rm = T)/1000000000,
                          trade.value.mismatch.extreme.p = sum(trade.value.mismatch.extreme.p, na.rm = T)/1000000000,
                          trade.value.mismatch.residual = sum(trade.value.mismatch.residual, na.rm = T)/1000000000,
                          trade.value.unmatched.all = sum(trade.value.unmatched.all, na.rm = T)/1000000000,
                          trade.value.unmatched.all.tt = sum(trade.value.unmatched.all.tt, na.rm = T)/1000000000,
                          trade.value.unmatched.all.hs4 = sum(trade.value.unmatched.all.hs4, na.rm = T)/1000000000,
                          trade.value.all = sum(trade.value.USD.all, na.rm = T)/1000000000,
                          trade.gap = sum(trade.gap, na.rm = T)/1000000000), by = c("year","i.group")]

# melt for plot
gap_yearly_l <- data.table(melt(gap_yearly, id.vars = c("year","i.group"), measure.vars = names(gap_yearly)[3:length(names(gap_yearly))],
                     variable.name = "component", value.name = "value"))

# redefine the series' names
gap_yearly_l[component == "trade.value.mismatch.costs", component:="Trade costs"]
gap_yearly_l[component == "trade.value.mismatch.extreme.p", component:="Abnormal prices"]
gap_yearly_l[component == "trade.value.mismatch.residual", component:="Residual"]
gap_yearly_l[component == "trade.value.unmatched.all", component:="Unmatched trade"]
gap_yearly_l[component == "trade.value.unmatched.all.hs4", component:="Product misclassification"]
gap_yearly_l[component == "trade.value.unmatched.all.tt", component:="Country misclassification"]

# make component into factor - ordered
gap_yearly_l[, component := factor(component, c("trade.value.all","trade.gap","Country misclassification","Product misclassification","Unmatched trade","Abnormal prices","Trade costs","Residual"),
                                   ordered = T)]

# rename the income groups
gap_yearly_l[i.group=="H", i.group:="High"]
gap_yearly_l[i.group=="UM", i.group:="Upper-middle"]
gap_yearly_l[i.group=="LM", i.group:="Lower-middle"]
gap_yearly_l[i.group=="L", i.group:="Low"]

# make income into factors
gap_yearly_l[, i.group := factor(i.group, levels = c("High","Upper-middle","Lower-middle","Low"), ordered = T)]


# define series to plot
plot_series <- c("Trade costs", "Abnormal prices", "Residual", "Unmatched trade",
                 "Product misclassification", "Country misclassification")

# set the colour pallette
cpal <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#333333")

fig_gap_igroup <- ggplot(gap_yearly_l[component %in% plot_series & year==2015, ], aes(x = i.group, y = value, fill = component))
fig_gap_igroup + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("USD bil.") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")

# write emf
emf(file = "Final/figures/gap_igroup_2015.emf", width = 9, height = 6)
fig_gap_igroup + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("USD bil.") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")
dev.off()

cairo_ps(file = "Final/figures/gap_igroup_2015.eps", width = 9, height = 6)
fig_gap_igroup + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("USD bil.") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")
dev.off()

#
#
#
# Gap by income group as a share of GDP
#
#
#

gap_yearly <- data_final[!is.na(ngdp), j = .(trade.value.mismatch.costs = sum(trade.value.mismatch.costs, na.rm = T)/1000000000,
                          trade.value.mismatch.extreme.p = sum(trade.value.mismatch.extreme.p, na.rm = T)/1000000000,
                          trade.value.mismatch.residual = sum(trade.value.mismatch.residual, na.rm = T)/1000000000,
                          trade.value.unmatched.all = sum(trade.value.unmatched.all, na.rm = T)/1000000000,
                          trade.value.unmatched.all.tt = sum(trade.value.unmatched.all.tt, na.rm = T)/1000000000,
                          trade.value.unmatched.all.hs4 = sum(trade.value.unmatched.all.hs4, na.rm = T)/1000000000,
                          ngdp = sum(ngdp, na.rm = T)/1000000000,
                          trade.value.all = sum(trade.value.USD.all, na.rm = T)/1000000000,
                          trade.gap = sum(trade.gap, na.rm = T)/1000000000), by = c("year","i.group")]

# check
gap_yearly[, trade.value.mismatch.costs.share := trade.value.mismatch.costs/ngdp*100]
gap_yearly[, trade.value.mismatch.extreme.p.share := trade.value.mismatch.extreme.p/ngdp*100]
gap_yearly[, trade.value.mismatch.residual.share := trade.value.mismatch.residual/ngdp*100]
gap_yearly[, trade.value.unmatched.all.share := trade.value.unmatched.all/ngdp*100]
gap_yearly[, trade.value.unmatched.all.tt.share := trade.value.unmatched.all.tt/ngdp*100]
gap_yearly[, trade.value.unmatched.all.hs4.share := trade.value.unmatched.all.hs4/ngdp*100]

gap_yearly_shares <- gap_yearly[, j=.(year, i.group, trade.value.mismatch.costs.share, trade.value.mismatch.extreme.p.share,
                                      trade.value.mismatch.residual.share, trade.value.unmatched.all.share,
                                      trade.value.unmatched.all.tt.share, trade.value.unmatched.all.hs4.share)]

# melt for plot
gap_yearly_shares <- data.table(melt(gap_yearly_shares, id.vars = c("year","i.group"), measure.vars = names(gap_yearly_shares)[3:length(names(gap_yearly_shares))],
                     variable.name = "component", value.name = "value"))

# redefine the income groups
gap_yearly_shares[i.group=="H", i.group:="High"]
gap_yearly_shares[i.group=="UM", i.group:="Upper-middle"]
gap_yearly_shares[i.group=="LM", i.group:="Lower-middle"]
gap_yearly_shares[i.group=="L", i.group:="Low"]

gap_yearly_shares[, i.group := factor(i.group, levels = c("High","Upper-middle","Lower-middle","Low"), ordered = T)]

# redefine the series' names
gap_yearly_shares[component == "trade.value.mismatch.costs.share", component:="Trade costs"]
gap_yearly_shares[component == "trade.value.mismatch.extreme.p.share", component:="Abnormal prices"]
gap_yearly_shares[component == "trade.value.mismatch.residual.share", component:="Residual"]
gap_yearly_shares[component == "trade.value.unmatched.all.share", component:="Unmatched trade"]
gap_yearly_shares[component == "trade.value.unmatched.all.hs4.share", component:="Product misclassification"]
gap_yearly_shares[component == "trade.value.unmatched.all.tt.share", component:="Country misclassification"]

# make component into factor - ordered
gap_yearly_shares[, component := factor(component, c("Country misclassification","Product misclassification","Unmatched trade","Abnormal prices", "Trade costs", "Residual"),
                    ordered = T)]

# define series to plot
plot_series <- c("Trade costs", "Abnormal prices", "Residual", "Unmatched trade",
                 "Product misclassification", "Country misclassification")

# set the colour pallette
cpal <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#333333")

#

fig_gap_igroup <- ggplot(gap_yearly_shares[component %in% plot_series & year==2015, ], aes(x = i.group, y = value, fill = component))
fig_gap_igroup + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("Share of GDP (%)") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")

# write emf
emf(file = "Final/figures/gap_igroup_ngdp_share_2015.emf", width = 9, height = 6)
fig_gap_igroup + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("Share of GDP (%)") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")
dev.off()

cairo_ps(file = "Final/figures/gap_igroup_ngdp_share_2015.eps", width = 9, height = 6)
fig_gap_igroup + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("Share of GDP (%)") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")
dev.off()

#
#
#
#
#
# Gap by region as a share of GDP
#
#
#

gap_yearly <- data_final[!is.na(ngdp), j = .(trade.value.mismatch.costs = sum(trade.value.mismatch.costs, na.rm = T)/1000000000,
                          trade.value.mismatch.extreme.p = sum(trade.value.mismatch.extreme.p, na.rm = T)/1000000000,
                          trade.value.mismatch.residual = sum(trade.value.mismatch.residual, na.rm = T)/1000000000,
                          trade.value.unmatched.all = sum(trade.value.unmatched.all, na.rm = T)/1000000000,
                          trade.value.unmatched.all.tt = sum(trade.value.unmatched.all.tt, na.rm = T)/1000000000,
                          trade.value.unmatched.all.hs4 = sum(trade.value.unmatched.all.hs4, na.rm = T)/1000000000,
                          ngdp = sum(ngdp, na.rm = T)/1000000000,
                          trade.value.all = sum(trade.value.USD.all, na.rm = T)/1000000000,
                          trade.gap = sum(trade.gap, na.rm = T)/1000000000), by = c("year","region")]

# check
gap_yearly[, trade.value.mismatch.costs.share := trade.value.mismatch.costs/ngdp*100]
gap_yearly[, trade.value.mismatch.extreme.p.share := trade.value.mismatch.extreme.p/ngdp*100]
gap_yearly[, trade.value.mismatch.residual.share := trade.value.mismatch.residual/ngdp*100]
gap_yearly[, trade.value.unmatched.all.share := trade.value.unmatched.all/ngdp*100]
gap_yearly[, trade.value.unmatched.all.tt.share := trade.value.unmatched.all.tt/ngdp*100]
gap_yearly[, trade.value.unmatched.all.hs4.share := trade.value.unmatched.all.hs4/ngdp*100]

gap_yearly_shares <- gap_yearly[, j=.(year, region, trade.value.mismatch.costs.share, trade.value.mismatch.extreme.p.share,
                                      trade.value.mismatch.residual.share, trade.value.unmatched.all.share,
                                      trade.value.unmatched.all.tt.share, trade.value.unmatched.all.hs4.share)]

# melt for plot
gap_yearly_shares <- data.table(melt(gap_yearly_shares, id.vars = c("year","region"), measure.vars = names(gap_yearly_shares)[3:length(names(gap_yearly_shares))],
                     variable.name = "component", value.name = "value"))

# redefine the income groups
# gap_yearly_shares[i.group=="H", i.group:="High"]
# gap_yearly_shares[i.group=="UM", i.group:="Upper-middle"]
# gap_yearly_shares[i.group=="LM", i.group:="Lower-middle"]
# gap_yearly_shares[i.group=="L", i.group:="Low"]

# redefine the series' names
gap_yearly_shares[component == "trade.value.mismatch.costs.share", component:="Trade costs"]
gap_yearly_shares[component == "trade.value.mismatch.extreme.p.share", component:="Abnormal prices"]
gap_yearly_shares[component == "trade.value.mismatch.residual.share", component:="Residual"]
gap_yearly_shares[component == "trade.value.unmatched.all.share", component:="Unmatched trade"]
gap_yearly_shares[component == "trade.value.unmatched.all.hs4.share", component:="Product misclassification"]
gap_yearly_shares[component == "trade.value.unmatched.all.tt.share", component:="Country misclassification"]

# make component into factor - ordered
gap_yearly_shares[, component := factor(component, c("Country misclassification","Product misclassification","Unmatched trade","Abnormal prices", "Trade costs", "Residual"),
                    ordered = T)]

# define series to plot
plot_series <- c("Trade costs", "Abnormal prices", "Residual", "Unmatched trade",
                 "Product misclassification", "Country misclassification")

# set the colour pallette
cpal <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#333333")

#

fig_gap_reg_gdp <- ggplot(gap_yearly_shares[component %in% plot_series & year==2015, ], aes(x = region, y = value, fill = component))
fig_gap_reg_gdp + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("Share of GDP (%)") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")

# write emf
emf(file = "Final/figures/gap_reg_ngdp_share_2015.emf", width = 9, height = 6)
fig_gap_reg_gdp + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("Share of GDP (%)") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")
dev.off()

cairo_ps(file = "Final/figures/gap_reg_ngdp_share_2015.eps", width = 9, height = 6)
fig_gap_reg_gdp + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("Share of GDP (%)") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")
dev.off()

#
#
#
#
#

#
#
#
# Gap by income group, shares of overall trade gap
#
#
#

gap_yearly <- data_final[, j = .(trade.value.mismatch.costs = sum(trade.value.mismatch.costs, na.rm = T)/1000000000,
                          trade.value.mismatch.extreme.p = sum(trade.value.mismatch.extreme.p, na.rm = T)/1000000000,
                          trade.value.mismatch.residual = sum(trade.value.mismatch.residual, na.rm = T)/1000000000,
                          trade.value.unmatched.all = sum(trade.value.unmatched.all, na.rm = T)/1000000000,
                          trade.value.unmatched.all.tt = sum(trade.value.unmatched.all.tt, na.rm = T)/1000000000,
                          trade.value.unmatched.all.hs4 = sum(trade.value.unmatched.all.hs4, na.rm = T)/1000000000,
                          ngdp = sum(ngdp, na.rm = T)/1000000000,
                          trade.value.all = sum(trade.value.USD.all, na.rm = T)/1000000000,
                          trade.gap = sum(trade.gap, na.rm = T)/1000000000), by = c("year","i.group")]

# check
gap_yearly[, trade.value.mismatch.costs.share := trade.value.mismatch.costs/trade.gap*100]
gap_yearly[, trade.value.mismatch.extreme.p.share := trade.value.mismatch.extreme.p/trade.gap*100]
gap_yearly[, trade.value.mismatch.residual.share := trade.value.mismatch.residual/trade.gap*100]
gap_yearly[, trade.value.unmatched.all.share := trade.value.unmatched.all/trade.gap*100]
gap_yearly[, trade.value.unmatched.all.tt.share := trade.value.unmatched.all.tt/trade.gap*100]
gap_yearly[, trade.value.unmatched.all.hs4.share := trade.value.unmatched.all.hs4/trade.gap*100]

gap_yearly_shares <- gap_yearly[, j=.(year, i.group, trade.value.mismatch.costs.share, trade.value.mismatch.extreme.p.share,
                                      trade.value.mismatch.residual.share, trade.value.unmatched.all.share,
                                      trade.value.unmatched.all.tt.share, trade.value.unmatched.all.hs4.share)]

# melt for plot
gap_yearly_shares <- data.table(melt(gap_yearly_shares, id.vars = c("year","i.group"), measure.vars = names(gap_yearly_shares)[3:length(names(gap_yearly_shares))],
                     variable.name = "component", value.name = "value"))

# redefine the income groups
gap_yearly_shares[i.group=="H", i.group:="High"]
gap_yearly_shares[i.group=="UM", i.group:="Upper-middle"]
gap_yearly_shares[i.group=="LM", i.group:="Lower-middle"]
gap_yearly_shares[i.group=="L", i.group:="Low"]

gap_yearly_shares[, i.group := factor(i.group, levels = c("High","Upper-middle","Lower-middle","Low"), ordered = T)]

# redefine the series' names
gap_yearly_shares[component == "trade.value.mismatch.costs.share", component:="Trade costs"]
gap_yearly_shares[component == "trade.value.mismatch.extreme.p.share", component:="Abnormal prices"]
gap_yearly_shares[component == "trade.value.mismatch.residual.share", component:="Residual"]
gap_yearly_shares[component == "trade.value.unmatched.all.share", component:="Unmatched trade"]
gap_yearly_shares[component == "trade.value.unmatched.all.hs4.share", component:="Product misclassification"]
gap_yearly_shares[component == "trade.value.unmatched.all.tt.share", component:="Country misclassification"]

# make component into factor - ordered
gap_yearly_shares[, component := factor(component, c("Country misclassification","Product misclassification","Unmatched trade","Abnormal prices", "Trade costs", "Residual"),
                    ordered = T)]

# define series to plot
plot_series <- c("Trade costs", "Abnormal prices", "Residual", "Unmatched trade",
                 "Product misclassification", "Country misclassification")

# set the colour pallette
cpal <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#333333")

#

fig_gap_igroup <- ggplot(gap_yearly_shares[component %in% plot_series & year==2015, ], aes(x = i.group, y = value, fill = component))
fig_gap_igroup + geom_bar(position="stack", stat = "identity") + coord_flip() + xlab("") + ylab("Share of overall trade gap") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")

# write emf
emf(file = "Final/figures/gap_igroup_gap_share_2015.emf", width = 9, height = 6)
fig_gap_igroup + geom_bar(position="stack", stat = "identity") + coord_flip() + xlab("") + ylab("Share of overall trade gap") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")
dev.off()

cairo_ps(file = "Final/figures/gap_igroup_gap_share_2015.eps", width = 9, height = 6)
fig_gap_igroup + geom_bar(position="stack", stat = "identity") + coord_flip() + xlab("") + ylab("Share of overall trade gap") + scale_fill_manual(values = cpal, name = "") + theme_bw() + theme(legend.position="bottom")
dev.off()