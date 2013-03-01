library(RCurl)
library(rjson)
library(XLConnect)

##############    step1: 定义函数     #############################
client_id = "862341168163-qtefv92ckvn2gveav66im725c3gqj728.apps.googleusercontent.com" ;
client_secret="orSEbf0-S76VZv6RMHe46z_N";
redirecturi = ('urn:ietf:wg:oauth:2.0:oob')
### 取得动态口令token的函数
getToken <- function(client.id, client.secret) {
  url = paste('https://accounts.google.com/o/oauth2/auth?',
            'scope=https://www.googleapis.com/auth/analytics.readonly&',
            'state=%2Fprofile&',
            'redirect_uri=',redirecturi,'&',
            'response_type=code&',
            'client_id=',client_id,'&',
            'approval_prompt=force&',
            'access_type=offline', sep='', collapse='')

getURL(url, curl = getCurlHandle(ssl.verifypeer = FALSE))
#give the permissions here with the account you want to give permissions to
browseURL(url)
cat(paste('Browse URL:', url, '\n')); # in case of server
#here's where you paste in the "code" from the resulting url
cat(paste('Browse URL:', url, '\n')); # in case of server
code <- readline('将页面出现的代码黏贴在此处: ');
opts = list(verbose=T )
accesstoken = fromJSON(postForm('https://accounts.google.com/o/oauth2/token', .opts=opts, code=code, client_id=client_id,
                                client_secret=client_secret, redirect_uri=redirecturi, grant_type="authorization_code", 
                                style="POST", curl = getCurlHandle(ssl.verifypeer = FALSE) ))
  return(accesstoken)
}
refresh2accesstoken = function(accesstoken){
  opts = list(verbose=T );
  rt = fromJSON(postForm('https://accounts.google.com/o/oauth2/token', .opts=opts,  refresh_token=accesstoken$refresh_token, client_id=client_id,
                         client_secret=client_secret,  grant_type="refresh_token", 
                         style="POST" , curl = getCurlHandle(ssl.verifypeer = FALSE)))
  accesstoken$access_token = rt$access_token
  accesstoken$expires_in = rt$expires_in
  return(accesstoken)
}
### 数据获取和导出的函数
exportData <- function(ids, start.date = format(Sys.time(), "%Y-%m-%d"), 
                       end.date = format(Sys.time(), "%Y-%m-%d"), metrics = 'ga:visits',
                       dimensions = 'ga:date', sort = '', filters = '', segment = '', fields = '', 
                       start = 1, max = 1000, last.days, date.format = '%Y-%m-%d', 
                       output.raw, output.formats, rbr = FALSE, envir = .GlobalEnv) {
  
  if (missing(ids)) { stop('please enter a profile id'); }
  
  # ensure that profile id begings with 'ga:'
  if (!as.logical(length(as.numeric(grep('ga:', ids))))) { 
    ids <- paste('ga:', ids, sep = ''); 
  }
  
  url <- paste('https://www.googleapis.com/analytics/v3/data/ga',
               '?access_token=', accesstoken$access_token,
               '&ids=', ids,
               '&start-date=', start.date,
               '&end-date=', end.date,
               '&metrics=', metrics,
               '&dimensions=', dimensions,
               '&start-index=', start,
               '&max-results=', max,
               sep = '', collapse = '');
  
  if (sort != '') { url <- paste(url, '&sort=', sort, sep='', collapse=''); }
  if (segment != '') { url <- paste(url, '&segment=', segment, sep='', collapse=''); }
  if (fields != '') { url <- paste(url, '&fields=', fields, sep='', collapse=''); }
  
  if (filters != '') { 
    url <- paste(url, '&filters=', curlEscape(filters), sep='', collapse=''); 
  }  				
  
  # get data and convert from json to list-format
  ga.data <- getURL(url, curl = getCurlHandle(ssl.verifypeer = FALSE));
  #assign('testing', ga.data, envir = envir);
  
  ga.data <- fromJSON(ga.data);
  
  # get column names
  ga.headers <- as.data.frame(do.call(rbind, ga.data$columnHeaders));
  # convert to data.frame
  ga.data.df <- as.data.frame(do.call(rbind, ga.data$rows)); 
  ga.data.df <- data.frame(lapply(ga.data.df, as.character), stringsAsFactors = F); # convert to characters
  ga.headers$name <- sub('ga:', '', ga.headers$name); # remove ga: from column headers
  
  names(ga.data.df) <- ga.headers$name; # insert column names
  
  # add sum
#  ALL <- as.data.frame(do.call(cbind,ga.data$totalsForAllResults))
#  names(ALL) <- sub('ga:', '', names(ALL))
#  sum.dimension <- ga.headers[ga.headers$columnType == "DIMENSION",]
#  for (i in 1:nrow(sum.dimension)){
#    dime <- sum.dimension$name[i];
#    ALL[[dime]] = "ALL"
#  }
#  ga.data.df <- rbind(ga.data.df, ALL)
  
  # find formats
  formats <- as.data.frame(do.call(rbind, ga.data$columnHeaders));
  
  # convert to r friendly
  formats$name <- sub('ga:', '', formats$name);
  formats$columnType <- tolower(formats$columnType);
  # for percent
  for (i in 1:nrow(formats)) {
    if (formats$dataType[[i]] == 'PERCENT') {
      name.percent <- formats$name[[i]];
      ga.data.df[[name.percent]] <- as(ga.data.df[[name.percent]], "numeric")/100; 
    }
  }
  formats$dataType[formats$dataType == 'STRING'] <- 'character';
  formats$dataType[formats$dataType == 'INTEGER'] <- 'numeric';
  formats$dataType[formats$dataType == 'FLOAT'] <- 'numeric';
  formats$dataType[formats$dataType == 'PERCENT'] <- 'numeric';
  formats$dataType[formats$dataType == 'TIME'] <- 'numeric';
  formats$dataType[formats$dataType == 'CURRENCY'] <- 'numeric';
  formats$dataType[formats$name == 'date'] <- 'Date';
  
  for (i in 1:nrow(formats)) {
    column <- formats$name[i];
    class <- formats$dataType[[i]];
    ga.data.df[[column]] <- as(ga.data.df[[column]], class);
  }
  return(ga.data.df)
}


##############    step2: 获取动态口令token      #############################
setwd("M:/CP/GA Test")
# 下面的命令一行一行运行哦
accesstoken <- getToken(client_id, client_secret)

##############  step3: 更新动态口令，开始获得“所有流量转化XX”的数据    ###############

## 自定义表名称：所有流量转化XX
  # ga:58559351              亿佰（除内网）
  # Sys.Date()-1               开始时间
  # Sys.Date()-1               结束时间
  # segment=""               高级细：无
  # ga:visits                访问次数
  # ga:pageviewsPerVisit     每次访问的网页浏览量
  # ga:avgTimeOnSite         平均访问持续时间
  # ga:percentNewVisits      新访问次数百分比
  # ga:visitBounceRate,      跳出率
  # ga:goal16ConversionRate  详情页（目标 16 的转化率）
  # ga:goal11ConversionRate  购物车（目标 11 的转化率）
  # ga:goal7ConversionRate   结算页（目标 7 的转化率）
  # ga:goal6ConversionRate   购物转化（目标 6 的转化率）
  # ga:transactionsPerVisit  电子商务转化率

  
## step3.1  设定参数：指标和起始时间
indicatorList <- c("ga:visits,ga:pageviewsPerVisit,ga:avgTimeOnSite,ga:percentNewVisits,ga:visitBounceRate,ga:goal16ConversionRate,ga:goal11ConversionRate,ga:goal7ConversionRate,ga:goal6ConversionRate,ga:transactionsPerVisit")
startDate <- "2013-01-02"  # 前一天
endDate <- "2013-01-02"    # 前一天

segmentList <- read.table(header=TRUE, text="
segmentId;name
gaid::-4;付费搜索
gaid::-5;非付费搜索
gaid::753897166;自有EDM
gaid::1281244306;BD
gaid::1723046398;银行链接
gaid::2069598548;cps
gaid::1989676528;社交
", sep=";");

## step3.2  定义导出csv文件的函数
easybuy.Data.segment  <- function(segmentId) {
  easybuy.Data.seg <- exportData(ids="ga:58559351", start.date = startDate, end.date = endDate,
                                     metrics = indicatorList , dimensions = "ga:source,ga:medium", 
                                     sort = "-ga:visits", filters = "", segment = segmentId,
                                     start = 1, max = 1000);
#  source.medium <- paste(easybuy.Data.seg[,1], easybuy.Data.seg[,2], sep = " / ");
#  names(source.medium) = "source_medium"
#  easybuy.Data.seg <- cbind(source.medium, easybuy.Data.seg[,-1])
#   easybuy.Data.seg[, 2] <- startDate
#   names(easybuy.Data.seg)[2]  <- "Date"
#   easybuy.Data.seg <- cbind(easybuy.Data.seg[,2], easybuy.Data.seg[,-2])
   
  source.medium <- paste(easybuy.Data.seg[,1], easybuy.Data.seg[,2], sep = " / ");
  download.time= source.medium
  easybuy.Data.seg2 <- easybuy.Data.seg[,-(1:2)]
  easybuy.Data.seg3 <- cbind(download.time, source.medium, easybuy.Data.seg2)
  easybuy.Data.seg3[,1]= startDate
#  header <- c("DATE", "sourcemedium","访问次数", "每次访问的网页浏览量","平均访问持续时间"," 新访问次数百分比","跳出率","详情页目标 16 的转化率","购物车目标 11 的转化率","结算页目标 7 的转化率","购物转化目标 6 的转化率","电子商务转化率")
  header <- c("DATE", "sourcemedium", "FWCS", "MCFWWYLLL","PJFWCXSC"," XFWCSBFB","TCL","XQYDZH","GWCDZH","JSYDZH","GWCZHDZH","DZSWZHL")
  names(easybuy.Data.seg3) =  header 
  
  if (dim(easybuy.Data.seg)[1] == 1000){
    stop("记录数可能大于1000，请调高max参数值")
  }
# export data to csv file
  file.name <- paste(startDate, ".csv", sep = "")
  write.csv(easybuy.Data.seg3, file.name, row.names = F)

  # export data to excel file
#   file.name <- paste(startDate, ".xlsx", sep = "")
#   if (segmentId == ''){
#     sheet.name <- paste("all", sep = "");
#   } else {
#     sheet.name <- paste(segmentList$name[segmentList$segmentId == segmentId], sep = "");
#   }
#   xls <- loadWorkbook(file.name, create=TRUE)
#   createSheet(xls, name=sheet.name)
#   writeWorksheet(xls, easybuy.Data.seg, sheet.name, header=TRUE)
#   saveWorkbook(xls)
}

## step3.3  更新和保存动态口令
load("M:/CP/GA Test/accesstoken.RData")
accesstoken = refresh2accesstoken(accesstoken)
# 将口令保存到为工作空间存在本地，这个可能可以下次再运行的时候直接用load("work目录/accesstoken.RData")来调用，还没试；
save(accesstoken, file='accesstoken.RData')


## step3.4 测试能否成功导出文件，逐个导出文件到本地目录

######## 先看看第一个能否成功运行，不能的话你先看看能不能打开google主页，不能正常打开的话可能和google的连接中断了
######## 一直到google能正常打开之后再运行上面的step3.3两条命令获得动态口令，然后继续运行下面的命令


easybuy.Data.segment(segmentId = 'gaid::-4') # Paid Search Traffic 付费搜索流量 
easybuy.Data.segment(segmentId = 'gaid::-5') # Non-paid Search Traffic 非付费搜索流量
easybuy.Data.segment(segmentId = 'gaid::753897166') # 自有EDM
easybuy.Data.segment(segmentId = 'gaid::1281244306') # BD
easybuy.Data.segment(segmentId = 'gaid::1723046398') # 银行链接
easybuy.Data.segment(segmentId = 'gaid::2069598548') # cps
easybuy.Data.segment(segmentId = 'gaid::1989676528') # 社交
easybuy.Data.segment(segmentId = '') # 所有



#################这是你要的程序，可以先看看上面的easybuy.Data.segment的自定义函数######################################################
Daysforward <- 1
mmm <- seq(Sys.Date(), Sys.Date()-Daysforward, -1)
for (mm in 1:length(mmm)) {
  startDate <- as.character(mmm[mm])  # 前一天
  endDate <- startDate    # 前一
  easybuy.Data.segment(segmentId = '')
}
#######################################################################################











##############  step4: 更新动态口令，开始获得“虚拟页面”的数据    ###############

## 自定义表名称：所有流量转化XX
# ga:63758150             虚拟页面(默认)
# Sys.Date()-1             开始时间
# Sys.Date()-1             结束时间
# segment=""               高级细：无
# ga:pagePath              主要维度：网页
# ga:pageviews             页浏览量
# ga:uniquePageviews       唯一身份浏览量
# ga:avgTimeOnPage         平均页面停留时间
# ga:entrances             进入次数
# ga:entranceBounceRate    跳出率
# ga:exitRate              退出百分比

## step4.1  设定参数：指标和起始时间
indicatorList <- c("ga:pageviews,ga:uniquePageviews,ga:uniquePageviews,ga:avgTimeOnPage,ga:entrances,ga:entranceBounceRate,ga:exitRate")
startDate <- Sys.Date()-1  # 前一天
endDate <- Sys.Date()-1    # 前一天


## step3.2  定义导出csv文件的函数
easybuy.Data.dummyPage  <- function(segmentId) {
  easybuy.Data.Page <- exportData(ids="ga:63758150", start.date = startDate, end.date = endDate,
                                 metrics = indicatorList , dimensions = "ga:pagePath", 
                                 sort = "-ga:pageviews", filters = "ga:pagePath=@科技", segment = "",
                                 start = 1, max = 1000);
  if (dim(easybuy.Data.seg)[1] == 1000){
    stop("记录数可能大于1000，请调高max参数值")
  }
  tablename <- paste(startDate, "虚拟页面 - ", segmentList$name[segmentList$segmentId == segmentId], ".csv", sep = "");
  write.csv(easybuy.Data.seg, tablename)
}

accesstoken = refresh2accesstoken(accesstoken)
# 将口令保存到为工作空间存在本地，这个可能可以下次再运行的时候直接用load("work目录/accesstoken.RData")来调用，还没试；
save(accesstoken, file='accesstoken.RData')



