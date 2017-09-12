TAIFEX_F_TbyT_ZIPs_URL = "http://www.taifex.com.tw/eng/eng3/eng3_1_3.asp"
TAIFEX_FC_TbyT_ZIPs_URL = "http://www.taifex.com.tw/eng/eng3/eng3_1_5.asp"

library(httr)
library(rvest)
library(stringr)
library(xts)

res = GET(TAIFEX_F_TbyT_ZIPs_URL)
#print(res)
downloadUrls = html(res) %>% 
  html_nodes(".table_c input") %>% 
  html_attr("onclick")
print(downloadUrls)
downloadUrls = sapply(downloadUrls,function(xx) str_replace_all(xx,"window.open[(]'../..","http://www.taifex.com.tw"))
print(downloadUrls)
downloadUrls = sapply(downloadUrls,function(xx) str_replace_all(xx,"'[)]",""))
print(downloadUrls)
print(names(downloadUrls))
names(downloadUrls) = NULL

downloadUrls = downloadUrls[grep("DataInformation.doc",downloadUrls,invert = T)]
downloadUrls = downloadUrls[grep("CSV",downloadUrls,invert = T)]
print(downloadUrls) #下載的檔案網址與名稱

dir1 <- dir(path = "./TAIFEX/zip", pattern = NULL, 
            all.files = FALSE,full.names = FALSE, 
            recursive = FALSE,ignore.case = FALSE, 
            include.dirs = FALSE, no.. = FALSE)
downloadUrls = downloadUrls[grep(dir1[4],downloadUrls,invert = T)]

downloadUrls = apply(dir1,1,downloadUrls[grep(dir1[],downloadUrls,invert = T)])

downloadFilenames = sapply(downloadUrls, function(url){
  xx = unlist(strsplit(url,"/"))
  xx[length(xx)]
})
print(downloadFilenames) #下載的[檔案網址,檔案名稱]
print(names(downloadFilenames))
names(downloadFilenames) = NULL

dnFilesDF = data.frame(url=downloadUrls,dest=downloadFilenames,stringsAsFactors = F)
#View(downloadUrls)
#print(dnFilesDF)

DownloadPATH = "TAIFEX"
setwd('D://R/prog/taifex-download')
dnFilesDF$dest = sprintf("./TAIFEX/zip/%s",dnFilesDF$dest)
#View(dnFilesDF)

outPath <- "./TAIFEX/rpt"

rptFiles = apply(dnFilesDF,1,function(xx){
  do.call(download.file,as.list(xx))
  unzip(xx[2], exdir = outPath)
})

print(rptFiles)

sapply(rptFiles,function(rptF){
  TaifexFutureTByT_df = read.csv(rptF, na.strings = "-",stringsAsFactors=FALSE)  
  TaifexFutureTByT_df$Time.of.Trades = sapply(TaifexFutureTByT_df$Time.of.Trades,function(time){
    ifelse(str_length(time) < 6,sprintf("0%s",time),time)
  })
  
  TaifexFutureTByT_df$Time = apply(TaifexFutureTByT_df,1,function(row){
    str_replace_all(paste(row[1],row[4],collapse = "")," ","")
  })
  
  TaifexFutureTByT_df$Time = strptime(TaifexFutureTByT_df$Time,"%Y%m%d%H%M%S",tz = "CST")
  
  rowData = cbind(time = TaifexFutureTByT_df$Time,
                  price = TaifexFutureTByT_df$Trade.Price, 
                  pcode = str_replace_all(TaifexFutureTByT_df$Product.Code," ",""),
                  exMW = str_replace_all(TaifexFutureTByT_df$Contract.Month.Week.," ",""),
                  volume=TaifexFutureTByT_df$Volume.Buy.Sell./2,
                  pNM = TaifexFutureTByT_df$Price.for.Nearer.Delivery.Month.Contract,
                  pFM = TaifexFutureTByT_df$Price.for.Nearer.Delivery.Month.Contract,
                  OCA = TaifexFutureTByT_df$Opening.Call.Auction)
  
  rowDataX = cbind(price = TaifexFutureTByT_df$Trade.Price, 
                   pcode = str_replace_all(TaifexFutureTByT_df$Product.Code," ",""),
                   exMW = str_replace_all(TaifexFutureTByT_df$Contract.Month.Week.," ",""),
                   volume=TaifexFutureTByT_df$Volume.Buy.Sell./2,
                   pNM = TaifexFutureTByT_df$Price.for.Nearer.Delivery.Month.Contract,
                   pFM = TaifexFutureTByT_df$Price.for.Nearer.Delivery.Month.Contract,
                   OCA = TaifexFutureTByT_df$Opening.Call.Auction)
  
  #Xt = xts(rowDataX,TaifexFutureTByT_df$Time)
  
  #dest = str_replace_all(rptF,"rpt","RData")
  #save(TaifexFutureTByT_df, rowData, Xt ,file = dest)
})
