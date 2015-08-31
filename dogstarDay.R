#author Ryan Inghilterra

dogStarDay <- function(date, month) {
    
    library(XML)
    library(xlsx)
    library(dplyr)

    date <- paste("date=", date, sep="")
    month <- paste("month=", month, sep="")
    url <- "http://www.dogstarradio.com/search_playlist.php?artist=&title=&channel=51&month=1&date=1&shour=&sampm=&stz=&ehour=&eampm="
    url <- gsub("date=[123456789][0123456789]*", date, url)
    url <- gsub("month=[123456789][012]*", month, url)
    dogstar.table <- readHTMLTable(url, header=T, which=2)
    fullDay.table <- readHTMLTable(url, header=T, which=2)
    totMatchesStr <- readHTMLTable(url, header=T, which=2)$V2[1]
    totMatchesStr <- lapply(totMatchesStr, as.character)
    totMatchesStr <- totMatchesStr[[1]]
    totMatchPattern <- regexpr( "[[:digit:]]{3}[[:space:]]total", totMatchesStr)
    totMatchesStr <- regmatches(totMatchesStr,totMatchPattern)
    totMatchPattern <- regexpr("[[:digit:]]+", totMatchesStr)
    totMatchesStr <- regmatches(totMatchesStr, totMatchPattern)
    totalMatches <- as.numeric(totMatchesStr) #parses out total matches
    url <- paste("http://www.dogstarradio.com/search_playlist.php?artist=&title=&channel=51&month=1&date=1&shour=&sampm=&stz=&ehour=&eampm=&resultcount=", totMatchesStr, sep="")
    url <- gsub("date=[0123][0123456789]*", date, url)
    url <- gsub("month=[123456789][012]*", month, url)
    url2 <- paste(url, "&page=", sep="")
    pageCount <- 1
    while(nrow(fullDay.table) < totalMatches) {
        url <- paste(url2, pageCount, sep="")
        dogstar.table <- readHTMLTable(url, header=T, which=2, skip.rows=2)
        fullDay.table <- rbind(fullDay.table, dogstar.table)
        fullDay.table <- fullDay.table[complete.cases(fullDay.table),] #rid of na's
        channelLogical <- grepl("Channel", fullDay.table$V1)
        fullDay.table <- fullDay.table[!channelLogical,] #get rid of header rows
        pageCount = pageCount + 1
    }

    names(fullDay.table) <- c("Channel", "Artist", "Title", "Date", "Time")
    i <- sapply(fullDay.table, is.factor)
    fullDay.table[] <- lapply(fullDay.table[i], as.character)
    sxmElectroLogical <- grepl("sxmElectro", fullDay.table$Title)
    fullDay.table <- fullDay.table[!sxmElectroLogical,]
    sxmElectroLogical <- grepl("sxmElectro", fullDay.table$Artist)
    fullDay.table <- fullDay.table[!sxmElectroLogical,]
    row.names(fullDay.table)<-NULL

    #write.xlsx(fullDay.table, "dogstar.xlsx")

    return(fullDay.table)

}

dogStarMonth <- function(month) {
    
    month.table <- dogStarDay(1, month)
    i <- 2
    while(i <= 24 ) {
        day.table <- dogStarDay(i,month)
        #print(i)
        month.table <- rbind(month.table, day.table)
        i = i + 1
    }
    
    return(month.table)
}