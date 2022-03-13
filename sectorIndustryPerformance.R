# devtools::install_github("talgalili/d3heatmap")
require("d3heatmap");require("shiny");require("miniUI");require("data.table");require("pbapply")
source("getSymbolsDB.R")

# function to group tickers by major group (similar to sector)
getTickersbyMajorGroup = function(x){
  subset(ALL, ALL$majorGroup == x)
}

# read in list
ALL <- readRDS("sectorIndustryList.rds")
# as data frame
ALL <- as.data.frame(ALL)
ALL <- subset(ALL, ALL$exhanges != "OTC")
ALL <- subset(ALL, ALL$exhanges != "")


# get unique TICKERS
TICKERS <- as.character(unique(ALL$currentTicker))
# get unique SECTORS 
MAJORGRP <- as.character(unique(ALL$majorGroup))
# get unique INDUSTRIES
INDUSTRY <- as.character(unique(ALL$industry))
# ********************************************************************************************************************************
# ********************************************************************************************************************************
# by major group
pctMAJORGRP <- rbindlist(pblapply(as.list(MAJORGRP), function(x){
  # group by sector/industry
  df <- subset(ALL, ALL$majorGroup == x)
  # get unique tickers for that group
  tickers <- as.character(unique(df$currentTicker))
  # get data and returns for selected group
  data <- pblapply(as.list(tickers), function(i){
    # get data from DB
    tmp <- try(getSymbolsDB(i)) # gets data from database
    #cat("\n",i)
    # if no errors : 
    if(nrow(tmp)>2){
      # subset performance
      fiveyr <- try(round(sum(ROC(tmp[paste0(Sys.Date()- years(5),"::")],type = "discrete"),na.rm = TRUE),4), silent = TRUE)
      oneyr  <- try(round(sum(ROC(tmp[paste0(Sys.Date()- years(1),"::")],type ="discrete"),na.rm = TRUE),4), silent = TRUE)
      ytd    <- try(round(sum(ROC(tmp[paste0(format(Sys.Date(), "%Y"))], type ="discrete"),na.rm = TRUE),4), silent = TRUE)
      sixMo  <- try(round(sum(ROC(tmp[paste0(Sys.Date()- months(6),"::")],type ="discrete"),na.rm = TRUE),4), silent = TRUE)
      threeMo<- try(round(sum(ROC(tmp[paste0(Sys.Date()- months(3),"::")],type ="discrete"),na.rm = TRUE),4), silent = TRUE)
      oneMo  <- try(round(sum(ROC(tmp[paste0(Sys.Date()-months(1),"::")], type ="discrete"),na.rm = TRUE),4), silent = TRUE)
      oneWk  <- try(round(sum(ROC(tmp[paste0(Sys.Date()-weeks(1),"::")], type = "discrete"),na.rm = TRUE),4), silent = TRUE)
      oneDay <- try(round(sum(ROC(tmp[(nrow(tmp)-1):nrow(tmp),], type = "discrete"),na.rm = TRUE),4), silent = TRUE)
      
      fiveyr <- ifelse(inherits(fiveyr, 'try-error'), NA, fiveyr)
      oneyr <- ifelse(inherits(oneyr, 'try-error'), NA, oneyr)
      ytd <- ifelse(inherits(ytd, 'try-error'), NA, ytd)
      sixMo <- ifelse(inherits(sixMo, 'try-error'), NA, sixMo)
      threeMo <- ifelse(inherits(threeMo, 'try-error'), NA, threeMo)
      oneMo <- ifelse(inherits(oneMo, 'try-error'), NA, oneMo)
      oneWk <- ifelse(inherits(oneWk, 'try-error'), NA, oneWk)
      oneDay <- ifelse(inherits(oneDay, 'try-error'), NA, oneDay)
      
      STATS <- as.data.frame(cbind(i, fiveyr, oneyr, ytd, sixMo, threeMo, oneMo, oneWk, oneDay, x))
      colnames(STATS)[1] <- c("ticker")
      colnames(STATS)[ncol(STATS)] <- c("majorGroup")
      
    }else{
      STATS <- NULL
    }
    STATS
  })
  # row bind all the data
  data <- rbindlist(data,use.names = TRUE, fill = TRUE)
  
  data
}), use.names = TRUE, fill = TRUE)
# summary
grpSummary = rbindlist(pblapply(as.list(unique(pctMAJORGRP$majorGroup)), function(x){
  
  tmp = as.data.frame(subset(pctMAJORGRP, pctMAJORGRP$majorGroup == x))
  N <- as.numeric(nrow(tmp))
  tmp$fiveyr <- as.numeric(tmp$fiveyr)
  tmp$oneyr <- as.numeric(tmp$oneyr)
  tmp$ytd <- as.numeric(tmp$ytd)
  tmp$sixMo <- as.numeric(tmp$sixMo)
  tmp$threeMo <- as.numeric(tmp$threeMo)
  tmp$oneMo <- as.numeric(tmp$oneMo)
  tmp$oneWk <- as.numeric(tmp$oneWk)
  tmp$oneDay <- as.numeric(tmp$oneDay)  
  tmp <- tmp[,2:(ncol(tmp)-1)]
  tmp <- as.data.frame(t(round(apply(tmp, 2, function(x) median(x, na.rm = TRUE)),4)))
  tmp$Ntickers <- N
  tmp$majorGroup <- x
  as.data.frame(tmp)
}), use.names = TRUE, fill = TRUE)
# subset by N tickers
grpSummary <- subset(grpSummary, grpSummary$Ntickers >=5)

View(getTickersbyMajorGroup(x= "Miscellaneous Services"))
# ********************************************************************************************************************************
# ********************************************************************************************************************************
# group selected columns only
df <- grpSummary[,c("oneDay","oneWk","oneMo","majorGroup")]
df <- data.frame(cbind(df[,1:3]), row.names = df$majorGroup)
df<- round(df*100,2)

# html plot also interactive
gadget <- d3heatmapGadget(df, col="RdYlGn") 
print(gadget)
save(gadget, file = "heatmap.html")
gadget <- d3heatmapGadget(gadget)
