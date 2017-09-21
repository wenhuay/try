TAIFEX_F_TbyT_ZIPs_URL = "http://www.taifex.com.tw/eng/eng3/eng3_1_3.asp"
TAIFEX_FC_TbyT_ZIPs_URL = "http://www.taifex.com.tw/eng/eng3/eng3_1_5.asp"

library(httr)
library(rvest)
library(stringr)
library(xts)
library(dplyr)
library(quantmod)

res = GET(TAIFEX_F_TbyT_ZIPs_URL)
downloadUrls = html(res) %>% 
  html_nodes(".table_c input") %>% 
  html_attr("onclick")
downloadUrls = sapply(downloadUrls,function(xx) str_replace_all(xx,"window.open[(]'../..","http://www.taifex.com.tw"))
downloadUrls = sapply(downloadUrls,function(xx) str_replace_all(xx,"'[)]",""))
names(downloadUrls) = NULL

downloadUrls = downloadUrls[grep("DataInformation.doc",downloadUrls,invert = T)]
downloadUrls = downloadUrls[grep("CSV",downloadUrls,invert = T)]
print(downloadUrls) #下載的檔案網址與名稱
setwd('D://R/prog/taifex-download')
dir1 <- dir(path = "./TAIFEX/zip", pattern = NULL, 
            all.files = FALSE,full.names = FALSE, 
            recursive = FALSE,ignore.case = FALSE, 
            include.dirs = FALSE, no.. = FALSE)
for(i in 1:length(dir1)){
  downloadUrls = downloadUrls[grep(dir1[i],downloadUrls,invert = T)]
  print(i)
}

downloadFilenames = sapply(downloadUrls, function(url){
  xx = unlist(strsplit(url,"/"))
  xx[length(xx)]
})
print(downloadFilenames) #下載的[檔案網址,檔案名稱]
print(names(downloadFilenames))
names(downloadFilenames) = NULL

dnFilesDF = data.frame(url=downloadUrls,dest=downloadFilenames,stringsAsFactors = F)

DownloadPATH = "TAIFEX"
dnFilesDF$dest = sprintf("./TAIFEX/zip/%s",dnFilesDF$dest)
outPath <- "./TAIFEX/rpt"

rptFiles = apply(dnFilesDF,1,function(xx){
  do.call(download.file,as.list(xx))
  unzip(xx[2], exdir = outPath)
})

print(rptFiles)

sapply(rptFiles,function(rptF){
  AA = read.csv(rptF, na.strings = "-",stringsAsFactors=FALSE)
  print("triger 1")
  TaifexFutureTByT_df <- filter(AA , AA[2] >= "MTX" & AA[3] >= "201710")
  print("triger 1.5")
  TaifexFutureTByT_df$Time.of.Trades = sapply(TaifexFutureTByT_df$Time.of.Trades,function(time){
    ifelse(str_length(time) < 6,sprintf("0%s",time),time)
  })
  print("triger 1.6")
  TaifexFutureTByT_df$Time = apply(TaifexFutureTByT_df,1,function(row){
    str_replace_all(paste(row[1],row[4],collapse = "")," ","")
  })
  print("triger 2")
  TaifexFutureTByT_df$Time = strptime(TaifexFutureTByT_df$Time,"%Y%m%d%H%M%S")
  print("triger 3")

  rowData1 <- cbind(
    price = TaifexFutureTByT_df$Trade.Price, 
    #pcode = TaifexFutureTByT_df$Product.Code,
    #exMW = TaifexFutureTByT_df$Contract.Month.Week.,
    volume=TaifexFutureTByT_df$Volume.Buy.Sell./2
    )
  print("triger 3.1")
  print(rowData1)
 
#  rowData <- xts(rowData1,TaifexFutureTByT_df$Time)
  rowData <- na.omit(xts(rowData1,TaifexFutureTByT_df$Time))  
  print("triger 3.2")

  print(rowData[1:10])

  bars <- period.apply(rowData,
                       endpoints(rowData,"secs",60),
                       function(xx){
                         ticks=coredata(xx$price)
                         c( first(ticks),max(ticks), min(ticks),
                            last(ticks), sum(xx$volume))
                       })
  print("triger 3.3")
  colnames(bars) <- c("Open","High","Low","Close","Volume")
  align.time(bars,60)
  chartSeries(bars)
  print("triger 3.4")
  
  
#  Xt = xts(rowDataX,TaifexFutureTByT_df$Time)
  
#  dest = str_replace_all(rptF,"rpt","RData")
#  save(TaifexFutureTByT_df, rowData, Xt ,file = dest)
})
