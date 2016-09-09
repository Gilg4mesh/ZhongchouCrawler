library(XML)  #--提供許多C剖析問題工具(libxml2)的介面
library(rjson)
library(stringr)
totalPage <- 1




url <- "http://www.zhongchou.com/browse"
link <- xpathSApply(doc = htmlParse(file = url), path = "/html/body/div[@class='mainInnerBox']/div[@class='sousuoListBox clearfix']/div[@class='ssCardItem']/a/@href")
link <- as.data.frame(link)
colnames(link) <- c("pageLink")

for ( i in 1:totalPage ) {
  cat("nowpage : ", i, "\n")
  topicUrl <- paste0(url,"/re-p",i)
  pageLink <- xpathSApply(doc = htmlParse(file = topicUrl), path = "/html/body/div[@class='mainInnerBox']/div[@class='sousuoListBox clearfix']/div[@class='ssCardItem']/a/@href")
  link <- rbind(link, as.data.frame(pageLink))
}


linkID <- as.data.frame(as.numeric(gsub("\\D", "", link$pageLink)))
colnames(linkID) <- "linkID"




url <- "http://www.zhongchou.com/deal-show/id-"

crawlInfo <- function(id) {
  urlID <- paste0(url,id)
  projectURL <- htmlParse(file = urlID, encoding = "UTF-8")
  Sys.sleep(runif(1,2,3)) # 不延遲會被鎖IP!!!
  
  projectName <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[1]/div[1]/div[1]/h3",xmlValue)) # 標題(未算字數)
  projectLauncher <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[1]/div[1]/div[2]/span[3]",xmlGetAttr,"title")) # 發起人身份
  if ( is.null(projectLauncher) ) projectLauncher <- "undefined" # 發起人無標簽
  followers <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[2]/div[1]/a",xmlGetAttr,"title")) # 關注數
  sup <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[2]/div[2]/div[1]/div[1]/p/span[1]",xmlValue)) # 支持數
  got <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[2]/div[2]/div[1]/div[2]/p/span[1]",xmlValue)) # 已募款
  percent <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[2]/div[2]/div[2]/p",xmlValue)) # 百分比
  wanted <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[2]/div[2]/div[2]/div[2]/span[2]/b",xmlValue)) # 目標
  projectType <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[2]/div[2]/div[3]/div[2]/div/span[1]/a",xmlValue)) # 行業
  projectCity <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[2]/div[2]/div[3]/div[2]/div/span[2]/a",xmlValue)) # 地點
  projectCity <- projectCity[1]
  fresh <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[3]/div[1]/div[1]/ul[1]/li[2]/b",xmlValue)) # 更新數
  comment <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[3]/div[1]/div[1]/ul[1]/li[3]/b",xmlValue)) # 評論數
  report <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[4]/div[1]/div[2]/div[1]/div[1]/div[1]/div[1]/div[3]/div[3]/p[2]/b",xmlValue)) # 項目回報
  report <- report[1]
  haveMedia <- unlist(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[4]/div[1]/div[1]/a/@href")) # 影片網址
  ifelse( is.null(haveMedia), haveMedia <- FALSE, haveMedia <- TRUE )  # 是否有影片
  picNums <- length(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[4]/div[1]/div[1]/div[3]/img")) # 圖片數量
  priceType <- length(xpathSApply(doc = projectURL, path = "/html/body/div[@class='jlxqOuterBox']/div/div[1]/div[4]/div[1]/div[2]/div[1]/div[1]/div[1]/div[1]/div/h3/b",xmlValue))-1
  
  info <- cbind( id, projectName, projectLauncher, haveMedia, picNums, followers, sup, got, percent, wanted, projectType, projectCity, fresh, comment, priceType, report )
  if ( ncol(info) != 16 ) {
    info <- cbind(info, "no report")
    colnames(info)[16] <- "report"
  }
  return( as.data.frame(info) )
}




JSONurl <- paste0("http://www.zhongchou.com/deal-march_list?id=", linkID$linkID[1], "&offset=0&page_size=50")
JSONlink <- xpathSApply(doc = htmlParse(file = JSONurl), path = "/html/body/p/text()")
JSONlinktext <- sapply(JSONlink, xmlValue)
x <- JSONlinktext
write(x, file = "x.json")
json_data <- fromJSON(paste(readLines("x.json"), collapse=""))
json_data_sub <- json_data$data$march_list
log_info <- as.data.frame(json_data_sub[[length(json_data_sub)]]$create_time )
enddate <- str_extract(json_data_sub[[length(json_data_sub)]]$log_info, "[:digit:]{1,4}年[:digit:]{1,2}月[:digit:]{1,2}日")
enddate <- gsub("年", "-", enddate)
enddate <- gsub("月", "-", enddate)
enddate <- gsub("日", "", enddate)
craw <- crawlInfo(linkID$linkID[1])
log_info <- cbind(linkID$linkID[1],log_info, enddate, craw)
colnames(log_info)[c(1,2,3)] <- c("linkID","create_time","end_time")




skipped <- c()
skip <- 1
for ( i in 2:nrow(linkID) ) {
  cat("nowrow : ", i, "\n")
  
  tryCatch(
    {
      JSONurl <- paste0("http://www.zhongchou.com/deal-march_list?id=", linkID$linkID[i], "&offset=0&page_size=50")
      # http://www.zhongchou.com/deal-topic_list?id=362590&offset=10&page_size=10 評論
      JSONlink <- xpathSApply(doc = htmlParse(file = JSONurl), path = "/html/body/p/text()")
      JSONlinktext <- sapply(JSONlink, xmlValue)
      x <- JSONlinktext
      write(x, file = "x.json")
      json_data <- fromJSON(paste(readLines("x.json"), collapse=""))
      json_data_sub <- json_data$data$march_list
      
      temp <- as.data.frame(json_data_sub[[length(json_data_sub)]]$create_time )
      enddate <- str_extract(json_data_sub[[length(json_data_sub)]]$log_info, "[:digit:]{1,4}年[:digit:]{1,2}月[:digit:]{1,2}日")
      enddate <- gsub("年", "-", enddate)
      enddate <- gsub("月", "-", enddate)
      enddate <- gsub("日", "", enddate)
      craw <- crawlInfo(linkID$linkID[i])
      temp <- cbind(linkID$linkID[i], temp, enddate, craw)
      colnames(temp)[c(1,2,3)] <- c("linkID","create_time","end_time")
      log_info <- rbind(log_info, temp)
      Sys.sleep(runif(1,10,11)) # 不延遲會被鎖IP!!!
      write.csv(log_info, file = "log_info.csv")
    }, error=function(e){
      cat( "SKIP : " , linkID$linkID[i], "\n" )
      skipped[skip] <- linkID$linkID[i]
      skip <- skip+1
      Sys.sleep(runif(1,10,11)) # 不延遲會被鎖IP!!!
    } # error
    
  ) # tryCatch
}
