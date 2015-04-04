saveFileAndUniqueWords <- function(filename, rdataName, lines = 0, path, remove=TRUE){
    
    if (lines == 0)
        a <- readLines(filename, encoding="UTF-8")
    else
        a <- readLines(filename, n=lines, encoding="UTF-8")
    
    a <- iconv(a, from="UTF-8", to="latin1", sub=" ")
    a_list <- unlist(strsplit(tolower(paste(a, collapse = " "))," "))
    a_list <- paste(unique(a_list), collapse=" ")
    
    save(a, file=paste(c(path,rdataName,".Rdata"), collapse=""))
    save(a_list, file=paste(c(path,rdataName,"_unique.Rdata"), collapse=""))
    
    if(remove){
        rm (a)
        rm (a_list)
    }
}