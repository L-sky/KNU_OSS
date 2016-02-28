OSSComposePage <- function(type="rds"){
    if(type=="rds"){
        oss_frame <- readRDS("oss_frame.rds")
    } else if(type=="csv"){
        oss_frame <- read.csv("oss_frame.csv",sep="|")
        oss_frame$text <- as.character(oss_frame$text) 
    } else {
        return ("Wrong type")
    }
    page <- character()
    page <- append(page,"<!DOCTYPE html>")
    page <- append(page,"<html>")
    
    page <- append(page,"<head>")
    page <- append(page,"<meta charset=\"utf-8\">")
    page <- append(page,"<title>Положення про студентське самоврядування у Київському національному університеті імені Тараса Шевченка</title>")
    page <- append(page,"<style>")
    page <- append(page,"h1, h2, h3, h4, h5, h6 {text-align: center;}")
    page <- append(page,".wrapper {width: 800px; margin-left: 20px;}")
    page <- append(page,".chapter {margin-bottom:0px;}")
    page <- append(page,".act {font-size: 20px; font-weight: bold;}")
    page <- append(page,".part { }")
    page <- append(page, ".subpart {margin-left: 40px;}")
    page <- append(page,"</style>")
    page <- append(page,"</head>")
    
    page <- append(page,"<body>")
    page <- append(page,"<div class=\"wrapper\">")
    page <- append(page,"<h2>ПОЛОЖЕННЯ ПРО СТУДЕНТСЬКЕ САМОВРЯДУВАННЯ У КИЇВСЬКОМУ НАЦІОНАЛЬНОМУ УНІВЕРСИТЕТІ ІМЕНІ ТАРАСА ШЕВЧЕНКА</h2><br>")
    page <- append(page,"<h2 class=\"chapter\">Зміст</h2><br>")
    for(i in 1:dim(oss_frame)[1]){
        subpart <- oss_frame$subpart[i]
        part <- oss_frame$part[i]
        act <- oss_frame$act[i]
        chapter <- oss_frame$chapter[i]
        text <- oss_frame$text[i]
        if(subpart==0 & part==0){
            if(act!=0){
                page <- append(page,paste0("<a href=\"#",chapter,".",act,"\">","<div>","Глава ",chapter,".",act,". ",text,"</div></a><br>"))
            } else if(chapter!=0){
                page <- append(page,paste0("<a href=\"#",chapter,"\">","<h4 class=\"chapter\">","Розділ ",chapter,". ",text,"</h4></a><br>"))
            }
        }
    }
    
    for(i in 1:dim(oss_frame)[1]){
        subpart <- oss_frame$subpart[i]
        part <- oss_frame$part[i]
        act <- oss_frame$act[i]
        chapter <- oss_frame$chapter[i]
        text <- oss_frame$text[i]
        if(subpart!=0){
            page <- append(page,paste0("<div class=\"subpart\">",subpart,") ",text,"</div><br>",collapse=""))
        } else if(part!=0){
            page <- append(page,paste0("<div class=\"part\">",chapter,".",act,".",part,". ",text,"</div><br>",collapse=""))
        } else if(act!=0){
            page <- append(page,paste0("<div class=\"act\" id=\"",chapter,".",act,"\">","Глава ",chapter,".",act,". ",text,"</div><hr>",collapse=""))
        } else if(chapter!=0){
            page <- append(page,paste0("<h2 class=\"chapter\" id=\"",chapter,"\">","Розділ ",chapter,". ",text,"</h2><br>",collapse=""))
        }
    }
    page <- append(page,"</div>")
    page <- append(page,"</body>")
    page <- append(page,"</html>")
    writeLines(iconv(page,from = "windows-1251",to="utf-8"),"index.html",useBytes = TRUE)
    return(page)
}