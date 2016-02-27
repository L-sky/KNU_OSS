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
    page <- append(page,"<title>онкнфеммъ опн ярсдемряэйе яюлнбпъдсбюммъ с йх╞бяэйнлс мюж╡нмюкэмнлс см╡бепяхрер╡ ╡лем╡ рюпюяю ьебвемйю</title>")
    page <- append(page,"</head>")
    page <- append(page,"<body>")
    page <- append(page,"<div style=\"width:800px\">")
    page <- append(page,"<h2 style=\"text-align:center\">онкнфеммъ опн ярсдемряэйе яюлнбпъдсбюммъ с йх╞бяэйнлс мюж╡нмюкэмнлс см╡бепяхрер╡ ╡лем╡ рюпюяю ьебвемйю</h2><br>")
    page <- append(page,"<h2 style=\"text-align:center; margin-bottom:0px\">гЛЁЯР</h2><br>")
    for(i in 1:dim(oss_frame)[1]){
        subpart <- oss_frame$subpart[i]
        part <- oss_frame$part[i]
        act <- oss_frame$act[i]
        chapter <- oss_frame$chapter[i]
        text <- oss_frame$text[i]
        if(subpart==0 & part==0){
            if(act!=0){
                page <- append(page,paste0("<a href=\"#",chapter,".",act,"\">","<div style=\"margin-left=40xp\">","цКЮБЮ ",chapter,".",act,". ",text,"</div></a><br>"))
            } else if(chapter!=0){
                page <- append(page,paste0("<a href=\"#",chapter,"\">","<h4>","пНГДЁК ",chapter,". ",text,"</h4></a><br>"))
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
            page <- append(page,paste0("<div class=\"subpart\" style=\"margin-left:40px\">",subpart,") ",text,"</div><br>",collapse=""))
        } else if(part!=0){
            page <- append(page,paste0("<div class=\"part\">",chapter,".",act,".",part,". ",text,"</div><br>",collapse=""))
        } else if(act!=0){
            page <- append(page,paste0("<div class=\"act\" style=\"font-size:20px\" id=\"",chapter,".",act,"\">","<b>цКЮБЮ ",chapter,".",act,". ",text,"</b></div><hr>",collapse=""))
        } else if(chapter!=0){
            page <- append(page,paste0("<h2 class=\"chapter\" style=\"text-align:center;margin-bottom:0px;\" id=\"",chapter,"\">","пНГДЁК ",chapter,". ",text,"</h2><br>",collapse=""))
        }
    }
    page <- append(page,"</div>")
    page <- append(page,"</body>")
    page <- append(page,"</html>")
    writeLines(iconv(page,from = "windows-1251",to="utf-8"),"index.html",useBytes = TRUE)
    return(page)
}