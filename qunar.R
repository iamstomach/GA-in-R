# get data
setwd("E:/CP")
library(RCurl)
library(XML)
library(RJSONIO)
library(xlsx)

#please copy the txt into excel file
#read excel file
a <- read.xlsx(header=F, "rank_json.xlsx", sheetName = "Sheet1")
a$X1 <- as.character(a$X1) #转文本
Encoding(a$X1) <- "UTF-8" #转换编码
a1 <- fromJSON(a$X1)
attributes(a1)

do.call(cbind, lapply(a1, t))
is.pairlist(a1$hotels)


json_file <- do.call(rbind, lapply(a1, function(x) {
  if (sum(unlist(lapply(x, is.list)), na.rm=TRUE) == 0){
    t(x)
  } else {
    x
  }
})
)




#serverip
serverip <- data.frame(serverip=a1$serverip)

#info
sc_name <- paste(a1$info$tradings, "|", collapse = "")
a1$info$tradings  <- ""
info <- as.data.frame(t(do.call(rbind, a1$info)))
info[, "tradings"] <- sc_name

#queryInfo
queryInfo_0 <- do.call(cbind, a1$queryInfo)
queryInfo <- as.data.frame(t(apply(queryInfo_0, 1, unlist)))


#hotels
hotels_0 <- do.call(rbind, a1$hotels)
hotels <- as.data.frame(t(apply(hotels_0, 1, unlist)))
names(hotels)[names(hotels)=="cityName"] <- "HcityName"    #same name "cityName" in queryInfo and hotels

#output excel file
final_table <- merge(merge(merge(serverip, info), queryInfo), hotels)
write.xlsx(final_table, "qunar_rank.xls")
