# oss_list <- OSSDecomposetoList()
# oss_frame <- OSSListToFrame(oss_list)
# OSSFrameSave(oss_frame)

OSSDecomposetoList <- function(){
    oss <- readLines("knuOSSraw.txt")
    chapter <- unlist(strsplit(oss,split = "Розділ[ ]*[0-9]+\\.?[ ]*",perl=T))
    chapter <- chapter[-1]
    act <- strsplit(chapter,split="[ ]*Глава[ ]*[0-9]+\\.[0-9]+\\.?[ ]*",perl=T)
    part <- sapply(act,function(part){return(strsplit(part,split="(?!([ ]*[0-9]+\\.[0-9]+\\.[0-9]+\\.?[ ]*(цього Положення)))([ ]*[0-9]+\\.[0-9]+\\.[0-9]+\\.?[ ]*)",perl=T))})
    subpart <- sapply(part, function(subpart){sapply(subpart,function(internal){return(strsplit(internal,split="[ ]*[0-9]+\\)[ ]*",perl=T))})})
    # subpart = nested list representation
    return (subpart)
}

OSSListToFrame <- function(oss_list){
    chapter_vec <- numeric()
    act_vec <- numeric()
    part_vec <- numeric()
    subpart_vec <- numeric()
    text_vec <- character()
    for(chapter in 1:length(oss_list)){
        for(act in 1:length(oss_list[[chapter]])){
            for(part in 1:length(oss_list[[chapter]][[act]])){
                for(subpart in 1:length(oss_list[[chapter]][[act]][[part]])){
                    chapter_vec <- c(chapter_vec,chapter)
                    act_vec <- c(act_vec,act-1)
                    part_vec <- c(part_vec,part-1)
                    subpart_vec <- c(subpart_vec,subpart-1)
                    text_vec <- c(text_vec,oss_list[[chapter]][[act]][[part]][[subpart]])
                }
            }
        }
    }
    oss_frame <- data.frame(chapter=chapter_vec,act=act_vec,part=part_vec,subpart=subpart_vec,text=text_vec)
    oss_frame$text <- as.character(oss_frame$text)
    return(oss_frame)
}

OSSFrameSave <- function(oss_frame,type="rds"){
    if(type=="rds"){
        saveRDS(oss_frame,"oss_frame.rds")
    } else if(type=="csv"){
        write.table(oss_frame,"oss_frame.csv",sep="|")
    } else {
        print("Wrong type")
    }
    
}