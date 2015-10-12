#author Ryan Inghilterra

dogStarDay <- function(month, date) {

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
    if(length(totalMatches) == 0) {
        fullDay.table <- data.frame(data.frame("EMPTY","DATE",month," / ",date))
        names(fullDay.table) <- c("Channel", "Artist", "Title", "Date", "Time")
        return(fullDay.table)
    }

    url <- paste("http://www.dogstarradio.com/search_playlist.php?artist=&title=&channel=51&month=1&date=1&shour=&sampm=&stz=&ehour=&eampm=&resultcount=", totMatchesStr, sep="")
    url <- gsub("date=[0123][0123456789]*", date, url)
    url <- gsub("month=[123456789][012]*", month, url)
    url2 <- paste(url, "&page=", sep="")
    pageCount <- 1
    while(nrow(fullDay.table) < totalMatches) {
        url <- paste(url2, pageCount, sep="")
        #print(url)
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
    fullDay.table <- removeDuplicateRows(fullDay.table)
    row.names(fullDay.table)<-NULL

    #write.xlsx(fullDay.table, "dogstar.xlsx")

    return(fullDay.table)

}

dogStarMonth <- function(month) {
    daysInMonth <- 31
    if(month == 2) {
        daysInMonth <- 28
    }
    if(month == 4 | month == 6 | month == 9 | month == 11 ) {
        daysInMonth <- 30
    }

    month.table <- dogStarDay(month,1)
    i <- 2
    while(i <= daysInMonth ) {
        day.table <- dogStarDay(month,i)
        month.table <- rbind(month.table, day.table)
        i = i + 1
    }

    write.xlsx(month.table, "dogstar.xlsx")

    return(month.table)
}


removeDuplicateRows <- function(table) {
    rowsToRemove <- vector('numeric')
    rowNumber <- 1
    while(rowNumber < nrow(table) - 1) {
        if(table[rowNumber, 'Title'] == table[rowNumber+1, 'Title']) {
            rowsToRemove <- append(rowsToRemove, rowNumber)
        }

        rowNumber = rowNumber + 1
    }
    if(length(rowsToRemove) != 0) {
        finalTable <- table[-rowsToRemove,]
    }
    else {
        finalTable <- table
    }
    return(finalTable)
}