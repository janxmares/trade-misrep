# Plot, trade gap - HS aggregation comparison in 2015
# Jan Mares, 201129

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
library(here)

#
#


# read imports data
imports <- data.table(read_fst(paste0("Data/HS2/Import_",2015,".fst")))

# drop imports declared by the EU
imports <- imports[reporter.ISO != "EU2",]

# imports merging iso odes
imports[, iso3c.m := reporter.ISO]
imports[, iso3c.x := partner.ISO]

#
#

# read exports data
exports <- data.table(read_fst(paste0("Data/HS2/Export_",2015,".fst")))

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

# merge imports and exports, keep all the observations
data_final <- merge(imports, exports, by = c("iso3c.m","iso3c.x","commodity.code"), all=T)

# replace the NA values
data_final[is.na(trade.value.USD.x), trade.value.USD.x := 0]
data_final[is.na(trade.value.USD.y), trade.value.USD.y := 0]

# compute the absolute trade gap for all the observations
data_final[, trade.gap := abs(trade.value.USD.x - trade.value.USD.y)]

trade_gap_w <- sum(data_final$trade.gap)
trade_gap_w/1000000000

trade_w <- sum(data_final$trade.value.USD.y+data_final$trade.value.USD.x)

trade_ex_w <- sum(data_final$trade.value.USD.y)
trade_im_w <- sum(data_final$trade.value.USD.x)

trade_w/1000000000
trade_ex_w/1000000000
trade_im_w/1000000000

#
#
#
#
#

# HS4 level
exports[, hs4.cc := substr(commodity.code, 1, 4)]
imports[, hs4.cc := substr(commodity.code, 1, 4)]

exports <- exports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m","iso3c.x","hs4.cc")]
imports <- imports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m","iso3c.x","hs4.cc")]

# merge imports and exports, keep all the observations
data_final <- merge(imports, exports, by = c("iso3c.m","iso3c.x","hs4.cc"), all = T)

# replace the NA values
data_final[is.na(trade.value.USD.x), trade.value.USD.x := 0]
data_final[is.na(trade.value.USD.y), trade.value.USD.y := 0]

# compute the absolute trade gap for all the observations
data_final[, trade.gap := abs(trade.value.USD.x - trade.value.USD.y)]

trade_gap_hs4_w <- sum(data_final$trade.gap)
trade_gap_hs4_w/1000000000

trade_hs4_w <- sum(data_final$trade.value.USD.y + data_final$trade.value.USD.x)

trade_ex_hs4_w <- sum(data_final$trade.value.USD.y)
trade_im_hs4_w <- sum(data_final$trade.value.USD.x)

trade_hs4_w/1000000000
trade_ex_hs4_w/1000000000
trade_im_hs4_w/1000000000

#
#
#
#
#

# HS2 level
exports[, hs2.cc := substr(hs4.cc, 1, 2)]
imports[, hs2.cc := substr(hs4.cc, 1, 2)]

exports <- exports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m","iso3c.x","hs2.cc")]
imports <- imports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m","iso3c.x","hs2.cc")]

# merge imports and exports, keep all the observations
data_final <- merge(imports, exports, by = c("iso3c.m","iso3c.x","hs2.cc"), all = T)

# replace the NA values
data_final[is.na(trade.value.USD.x), trade.value.USD.x := 0]
data_final[is.na(trade.value.USD.y), trade.value.USD.y := 0]

# compute the absolute trade gap for all the observations
data_final[, trade.gap := abs(trade.value.USD.x - trade.value.USD.y)]

trade_gap_hs2_w <- sum(data_final$trade.gap)
trade_gap_hs2_w/1000000000

#
#
#
#
#

# HS0 level
exports <- exports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m","iso3c.x")]
imports <- imports[, j = .(trade.value.USD = sum(trade.value.USD, na.rm = T)), by = c("iso3c.m","iso3c.x")]

# merge imports and exports, keep all the observations
data_final <- merge(imports, exports, by = c("iso3c.m","iso3c.x"), all = T)

# replace the NA values
data_final[is.na(trade.value.USD.x), trade.value.USD.x := 0]
data_final[is.na(trade.value.USD.y), trade.value.USD.y := 0]

# compute the absolute trade gap for all the observations
data_final[, trade.gap := abs(trade.value.USD.x - trade.value.USD.y)]

trade_gap_hs0_w <- sum(data_final$trade.gap)
trade_gap_hs0_w/1000000000

#
#
#
#
#

# read the data
data_final <- data.table(read.csv(here("output","Reported trade gap decomposition 2010-2015 tariff.csv"), header = T, stringsAsFactors= F))


data_final[, trade.value.unmatched.all := trade.value.unmatched + trade.value.unmatched.im]
data_final[, trade.value.unmatched.all.tt := trade.value.unmatched.tt + trade.value.unmatched.im.tt]
data_final[, trade.value.unmatched.all.hs4 := trade.value.unmatched.hs4 + trade.value.unmatched.im.hs4]

# drop the redundant columns 
data_final[, c("trade.value.unmatched","trade.value.unmatched.im","trade.value.unmatched.tt",
              "trade.value.unmatched.im.tt", "trade.value.unmatched.hs4","trade.value.unmatched.im.hs4") := NULL]

# compute overall gap
data_final[, trade.gap := trade.value.mismatch.costs + trade.value.mismatch.extreme.p + trade.value.mismatch.residual +
                          trade.value.mismatch.tariff + 
                          trade.value.unmatched.all + trade.value.unmatched.all.tt + trade.value.unmatched.all.hs4]



# gap decomposition, yearly
gap_yearly <- data_final[, j = .(trade.value.mismatch.costs = sum(trade.value.mismatch.costs, na.rm = T)/1000000000,
                          trade.value.mismatch.tariff = sum(trade.value.mismatch.tariff, na.rm = T)/1000000000,
                          trade.value.mismatch.extreme.p = sum(trade.value.mismatch.extreme.p, na.rm = T)/1000000000,
                          trade.value.mismatch.residual = sum(trade.value.mismatch.residual, na.rm = T)/1000000000,
                          trade.value.unmatched.all = sum(trade.value.unmatched.all, na.rm = T)/1000000000,
                          trade.value.unmatched.all.tt = sum(trade.value.unmatched.all.tt, na.rm = T)/1000000000,
                          trade.value.unmatched.all.hs4 = sum(trade.value.unmatched.all.hs4, na.rm = T)/1000000000,
                          trade.value.all = sum(trade.value.USD.all, na.rm = T)/1000000000,
                          trade.gap = sum(trade.gap, na.rm = T)/1000000000), by = c("year")]
# round(sapply(gap_yearly[year == 2015, ], function(x){x/gap_yearly[year == 2015, trade.gap]*100}),2)

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
gap_yearly_l[component == "trade.value.mismatch.tariff", component:="Tariff costs"]
gap_yearly_l[component == "trade.value.mismatch.extreme.p", component:="Abnormal prices"]
gap_yearly_l[component == "trade.value.mismatch.residual", component:="Residual"]
gap_yearly_l[component == "trade.value.unmatched.all", component:="Unmatched trade"]
gap_yearly_l[component == "trade.value.unmatched.all.hs4", component:="Product misclassification"]
gap_yearly_l[component == "trade.value.unmatched.all.tt", component:="Country misclassification"]

# make component into factor - ordered
gap_yearly_l[, component := factor(component, c("trade.value.all","trade.gap","Country misclassification","Product misclassification","Unmatched trade","Abnormal prices","Trade costs",'Tariff costs',"Residual"),
                                   ordered = T)]

# define series to plot
plot_series <- c("Trade costs",'Tariff costs',"Abnormal prices", "Residual", "Unmatched trade",
                 "Product misclassification", "Country misclassification")
#
#
#
#
#

# define series to plot
plot_series <- c("Trade costs", "Tariff costs", "Abnormal prices", "Residual", "Unmatched trade",
                 "Product misclassification", "Country misclassification")

# set the colour 
cpal <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffe033", "#333333", "#666666", "#666666", "#666666","#666666","#666666")
# cpal <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#000000")

# trade gaps in vector
trade.gap <- c(trade_gap_hs4_w, trade_gap_hs2_w, trade_gap_hs0_w)/1000000000

trade.gap.dt <- data.table(year=2015, component=c("HS4 overall gap","HS2 overall gap","HS0 overall gap"), 
                           value=trade.gap, group=c("HS4","HS2","HS0"))

# filter only the total value of gap
gap_yearly_l <- gap_yearly_l[year == 2015, j = .(year, component, value, group = 'HS6')]

# add the different HS categories into yearly decomposed data
gap_yearly_l_fig <- rbind(gap_yearly_l, trade.gap.dt)
gap_yearly_l_fig <- gap_yearly_l_fig[!(component %in% c('trade.value.all', 'trade.gap')),]

gap_yearly_l_fig[, group:=factor(group, levels = c("HS0","HS2","HS4","HS6"), ordered = T )]

# figure
fig1 <- ggplot(data=gap_yearly_l_fig, aes(x = group, y = value, fill=component))
fig1 + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("Trade gap (bil. USD)") + theme_bw() + scale_fill_manual(values = cpal, name = "") + theme(legend.position="bottom")


emf(file = here("figures/HS0_categories.emf"), width = 9, height = 6)
fig1 + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("Trade gap (bil. USD)") + theme_bw() + scale_fill_manual(values = cpal, name = "") + theme(legend.position="bottom")
dev.off()

# ps file for beamer
cairo_ps(file = here("figures/HS0_categories.eps"), width = 9, height = 6)
fig1 + geom_bar(stat = "identity") + coord_flip() + xlab("") + ylab("Trade gap (bil. USD)") + theme_bw() + scale_fill_manual(values = cpal, name = "") + theme(legend.position="bottom")
dev.off()
