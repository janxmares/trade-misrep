# Get tariffs data from TRAINS database by  the World bank
# Jan Mares, IES FSV UK
# 16 Sep 2020

# Libraries
library(here)
library(data.table)
library(rsdmx)
library(readr)
library(readxl)
library(fst)
library(tidyverse)

# WITS codelist
clURL <- "http://wits.worldbank.org/API/V1/SDMX/V21/rest/codelist/all/"
cl <- readSDMX(clURL)

#
#

# get all the codelis ids
cls <- slot(cl, 'codelists')
cl_df <- sapply(cls, function(x) slot(x, 'id'))

#
#

# get all the conutry codes
countries.df <- as.data.table(cl, codelistId = 'CL_COUNTRY_WITS')
# countries.df <- as.data.table(cl, codelistId = 'CL_TS_COUNTRY_WITS')
# countries.df <- countries.df[, j = .(id, country = name.en)]
countries.df <- countries.df[2:197, j = .(id, country = name.en)]

View(countries.df)
#
#

# get all the product codes
# pl.df <- as.data.table(cl, codelistId = 'CL_PRODUCTCODE_WITS')
# pl.df <- pl.df[, j = .(id)]

#
#

# one reporter, one partner, one year all products query
system.time(
tariff.list <- lapply(countries.df$id[151:nrow(countries.df)], function(x){
    # prod.list <- lapply(countries.df$id[1:196], function(y){
        tryCatch({
        # url <- paste0('http://wits.worldbank.org/API/V1/SDMX/V21/rest/data/DF_WITS_Tariff_TRAINS/.',x,'.',y,'..reported/?startperiod=2015&endperiod=2015')
        url <- paste0('http://wits.worldbank.org/API/V1/SDMX/V21/rest/data/DF_WITS_Tariff_TRAINS/.',x,'.','000','..reported/?startperiod=2015&endperiod=2015')
        query <- readSDMX(url)
        query.df <- as.data.table(query)
        # print(paste0('Read data for country ', x))
        # write.fst(query.df, paste0('tariffs_',x,'_',y,'.fst'), 80)
        write.fst(query.df, paste0('data/2015/tariffs_',x,'_','000','.fst'), 80)
        # print('Tarrifs for',x,' and ',y,' read OK.')
        print('Tarrifs for',x,' and ','000',' read OK.')
        return('Tarrifs for',x,' and ','000',' read OK.')
        # return(paste0('Tarrifs for',x,' and ',y,' read OK.'))
        # return(paste0('Tarrifs for',x,' and ','000',' read OK.'))
    }, error = function(err){
       print('The data is not available')
       return(NULL)
        },
       warning = function(war){
       print('The data is not available')
       return(NULL)
       })
    #    })
    # prod.list.df <- do.call(rbind, prod.list)
    # return(prod.list.df)
    })
)

# get all the tarrif data from external HDD, where they're permanently stored
folder_2010 <- "h:/UN TRAINS/tariffs 2010/"
files_2010 <- dir(folder_2010, pattern = "*.fst")

tariffs_2010 <- paste0(folder_2010, files_2010) %>%
                    map(read.fst) %>%
                    reduce(rbind)
head(tariffs_2010)

# read the country code correspondece
country_codes <- data.table(read_xls('conversion tables/AllCountries.xls', sheet='Sheet1', range = 'A1:B276'))
setnames(country_codes, c('iso3c','REPORTER'))

# merge tariff data with iso3 codes
tariffs_2010 <- country_codes[tariffs_2010, on = c('REPORTER')]

# filter required columns
tariffs_f <- tariffs_2010[, j = .(commodity.code = PRODUCTCODE, iso3c.m = iso3c, tariff.rate = obsValue, hs.classification = NOMENCODE)]

#
#

# H4 adjustment
# read the conversion table
conversion.02.12 <- data.table(read_xls('conversion tables/HS 2015 to HS 2002 Correlation and conversion tables.xls', sheet = 'Conversion HS 2015-HS 2002', range = 'C7:D5213'))

# rename the columns
setnames(conversion.02.12, c('commodity.code','commodity.code.hs02'))

# merge with tariff data
tariffs_h4 <- tariffs_f[hs.classification == 'H4',]
tariffs_h4 <- tariffs_h4[conversion.02.12, on = c('commodity.code')]

# H3 adjustment
# read the conversion table
conversion.02.07 <- data.table(read_xls('conversion tables/HS 2007 to HS 2002 Correlation and conversion tables.xls', sheet = 'Conversion Tables', range = 'A2:B5054'))

# rename the columns
setnames(conversion.02.07, c('commodity.code','commodity.code.hs02'))

# merge with tariff data
tariffs_h3 <- tariffs_f[hs.classification == 'H3',]
tariffs_h3 <- tariffs_h3[conversion.02.07, on = c('commodity.code')]

# Bind the tariffs data back together
tariffs_f <- rbind(tariffs_f[hs.classification == 'H2',], tariffs_h3, tariffs_h4, fill = T)

# adjust the hs02 commodity column for the HS2 observations (original codes are already HS2)
tariffs_f[is.na(commodity.code.hs02), commodity.code.hs02 := commodity.code]

# drop the duplicate observations which appear due to merging more categories in HS4 to one HS2,
# essentially, all have the same tariff rate
tariffs_f <- tariffs_f[!duplicated(tariffs_f[, j = .(commodity.code.hs02,iso3c.m)]), ]

# write the fst file with tariffs 2015
write.fst(tariffs_f, 'tariffs_2010.fst', 80)