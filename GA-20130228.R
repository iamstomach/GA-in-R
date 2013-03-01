library(RCurl)
library(rjson)
library(XLConnect)

##############    step1: ���庯��     #############################
client_id = "862341168163-qtefv92ckvn2gveav66im725c3gqj728.apps.googleusercontent.com" ;
client_secret="orSEbf0-S76VZv6RMHe46z_N";
redirecturi = ('urn:ietf:wg:oauth:2.0:oob')
### ȡ�ö�̬����token�ĺ���
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
code <- readline('��ҳ����ֵĴ�������ڴ˴�: ');
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
### ���ݻ�ȡ�͵����ĺ���
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


##############    step2: ��ȡ��̬����token      #############################
setwd("M:/CP/GA Test")
# ���������һ��һ������Ŷ
accesstoken <- getToken(client_id, client_secret)

##############  step3: ���¶�̬�����ʼ��á���������ת��XX��������    ###############

## �Զ�������ƣ���������ת��XX
  # ga:58559351              �ڰۣ���������
  # Sys.Date()-1               ��ʼʱ��
  # Sys.Date()-1               ����ʱ��
  # segment=""               �߼�ϸ����
  # ga:visits                ���ʴ���
  # ga:pageviewsPerVisit     ÿ�η��ʵ���ҳ�����
  # ga:avgTimeOnSite         ƽ�����ʳ���ʱ��
  # ga:percentNewVisits      �·��ʴ����ٷֱ�
  # ga:visitBounceRate,      ������
  # ga:goal16ConversionRate  ����ҳ��Ŀ�� 16 ��ת���ʣ�
  # ga:goal11ConversionRate  ���ﳵ��Ŀ�� 11 ��ת���ʣ�
  # ga:goal7ConversionRate   ����ҳ��Ŀ�� 7 ��ת���ʣ�
  # ga:goal6ConversionRate   ����ת����Ŀ�� 6 ��ת���ʣ�
  # ga:transactionsPerVisit  ��������ת����

  
## step3.1  �趨������ָ�����ʼʱ��
indicatorList <- c("ga:visits,ga:pageviewsPerVisit,ga:avgTimeOnSite,ga:percentNewVisits,ga:visitBounceRate,ga:goal16ConversionRate,ga:goal11ConversionRate,ga:goal7ConversionRate,ga:goal6ConversionRate,ga:transactionsPerVisit")
startDate <- "2013-01-02"  # ǰһ��
endDate <- "2013-01-02"    # ǰһ��

segmentList <- read.table(header=TRUE, text="
segmentId;name
gaid::-4;��������
gaid::-5;�Ǹ�������
gaid::753897166;����EDM
gaid::1281244306;BD
gaid::1723046398;��������
gaid::2069598548;cps
gaid::1989676528;�罻
", sep=";");

## step3.2  ���嵼��csv�ļ��ĺ���
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
#  header <- c("DATE", "sourcemedium","���ʴ���", "ÿ�η��ʵ���ҳ�����","ƽ�����ʳ���ʱ��"," �·��ʴ����ٷֱ�","������","����ҳĿ�� 16 ��ת����","���ﳵĿ�� 11 ��ת����","����ҳĿ�� 7 ��ת����","����ת��Ŀ�� 6 ��ת����","��������ת����")
  header <- c("DATE", "sourcemedium", "FWCS", "MCFWWYLLL","PJFWCXSC"," XFWCSBFB","TCL","XQYDZH","GWCDZH","JSYDZH","GWCZHDZH","DZSWZHL")
  names(easybuy.Data.seg3) =  header 
  
  if (dim(easybuy.Data.seg)[1] == 1000){
    stop("��¼�����ܴ���1000�������max����ֵ")
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

## step3.3  ���ºͱ��涯̬����
load("M:/CP/GA Test/accesstoken.RData")
accesstoken = refresh2accesstoken(accesstoken)
# ������浽Ϊ�����ռ���ڱ��أ�������ܿ����´������е�ʱ��ֱ����load("workĿ¼/accesstoken.RData")�����ã���û�ԣ�
save(accesstoken, file='accesstoken.RData')


## step3.4 �����ܷ�ɹ������ļ�����������ļ�������Ŀ¼

######## �ȿ�����һ���ܷ�ɹ����У����ܵĻ����ȿ����ܲ��ܴ�google��ҳ�����������򿪵Ļ����ܺ�google�������ж���
######## һֱ��google��������֮�������������step3.3���������ö�̬���Ȼ������������������


easybuy.Data.segment(segmentId = 'gaid::-4') # Paid Search Traffic ������������ 
easybuy.Data.segment(segmentId = 'gaid::-5') # Non-paid Search Traffic �Ǹ�����������
easybuy.Data.segment(segmentId = 'gaid::753897166') # ����EDM
easybuy.Data.segment(segmentId = 'gaid::1281244306') # BD
easybuy.Data.segment(segmentId = 'gaid::1723046398') # ��������
easybuy.Data.segment(segmentId = 'gaid::2069598548') # cps
easybuy.Data.segment(segmentId = 'gaid::1989676528') # �罻
easybuy.Data.segment(segmentId = '') # ����



#################������Ҫ�ĳ��򣬿����ȿ��������easybuy.Data.segment���Զ��庯��######################################################
Daysforward <- 1
mmm <- seq(Sys.Date(), Sys.Date()-Daysforward, -1)
for (mm in 1:length(mmm)) {
  startDate <- as.character(mmm[mm])  # ǰһ��
  endDate <- startDate    # ǰһ
  easybuy.Data.segment(segmentId = '')
}
#######################################################################################











##############  step4: ���¶�̬�����ʼ��á�����ҳ�桱������    ###############

## �Զ�������ƣ���������ת��XX
# ga:63758150             ����ҳ��(Ĭ��)
# Sys.Date()-1             ��ʼʱ��
# Sys.Date()-1             ����ʱ��
# segment=""               �߼�ϸ����
# ga:pagePath              ��Ҫά�ȣ���ҳ
# ga:pageviews             ҳ�����
# ga:uniquePageviews       Ψһ���������
# ga:avgTimeOnPage         ƽ��ҳ��ͣ��ʱ��
# ga:entrances             �������
# ga:entranceBounceRate    ������
# ga:exitRate              �˳��ٷֱ�

## step4.1  �趨������ָ�����ʼʱ��
indicatorList <- c("ga:pageviews,ga:uniquePageviews,ga:uniquePageviews,ga:avgTimeOnPage,ga:entrances,ga:entranceBounceRate,ga:exitRate")
startDate <- Sys.Date()-1  # ǰһ��
endDate <- Sys.Date()-1    # ǰһ��


## step3.2  ���嵼��csv�ļ��ĺ���
easybuy.Data.dummyPage  <- function(segmentId) {
  easybuy.Data.Page <- exportData(ids="ga:63758150", start.date = startDate, end.date = endDate,
                                 metrics = indicatorList , dimensions = "ga:pagePath", 
                                 sort = "-ga:pageviews", filters = "ga:pagePath=@�Ƽ�", segment = "",
                                 start = 1, max = 1000);
  if (dim(easybuy.Data.seg)[1] == 1000){
    stop("��¼�����ܴ���1000�������max����ֵ")
  }
  tablename <- paste(startDate, "����ҳ�� - ", segmentList$name[segmentList$segmentId == segmentId], ".csv", sep = "");
  write.csv(easybuy.Data.seg, tablename)
}

accesstoken = refresh2accesstoken(accesstoken)
# ������浽Ϊ�����ռ���ڱ��أ�������ܿ����´������е�ʱ��ֱ����load("workĿ¼/accesstoken.RData")�����ã���û�ԣ�
save(accesstoken, file='accesstoken.RData')


