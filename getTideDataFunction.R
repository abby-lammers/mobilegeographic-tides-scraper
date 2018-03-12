require(jsonlite)
require(XML)
require(dplyr)
require(RCurl)
require(stringi)
require(tidyr)

## source: https://rpubs.com/shyambv/extract_htmljsonxml_data

bookshtml.link <- getURL("https://msdn.microsoft.com/en-us/library/ms762258(v=vs.85).aspx") %>%  htmlParse(asText = TRUE)

bookshtml.xml <- xpathSApply(bookshtml.link,"//div[@class='codeSnippetContainerCode']",xmlValue)

booksexact <- bookshtml.xml[1] %>%  str_replace_all("[\r\n]","") %>%  str_trim(side = "both")

books.xml.parse <- xmlParse(booksexact) %>% xmlToDataFrame()

books.json <- toJSON(books.xml.parse)

tides_url <-"http://tides.mobilegeographics.com/calendar/month/493.html?y=2016&m=8&d=1"

tidesLink <- getURL(tides_url) %>% htmlParse(asText = TRUE)
tidesXml <- xpathApply(tidesLink, "//table")
tidesExact <- tidesXml[1] %>%  str_replace_all("[\r\n]","") %>%  str_trim(side = "both")
tidesParsed <- xmlParse(tidesXml[1]) %>% xmlToDataFrame()

### !!!!!
rawTable <- XML::readHTMLTable(tides_url) %>% data.frame()

### start function planning
# params 
month <- 8
year <- 2016
sitenum <- 493 ##   TODO: fetch site? 

# eventually, in shinyApp, have ability to search for site by (a) name and (b) coords
# rawSiteIndex <- XML::readHTMLTable('http://tides.mobilegeographics.com/index.html')
# SiteIndexTable <- dplyr::bind_rows(rawSiteIndex)
# given "siteName," listed name of site
# sitenum = which(SiteIndexTable$Location == siteName) - 1

GetTidesData <- function(year, month, sitenum) {
  # actually fetch table 
  tides_url <- paste0('http://tides.mobilegeographics.com/calendar/month/',sitenum,'.html?y=',year,'&m=',month,'&d=1')
  RawTidesTable <- XML::readHTMLTable(tides_url) %>% data.frame()
  
  ## reshape data
  
  # stack columns labeled "high" (number may vary)
  onlyHighData <- RawTidesTable[ ,append(1, which(grepl('High',colnames(RawTidesTable))))] %>% 
    tidyr::gather(key = NULL.Day)
  colnames(onlyHighData) <- c('Date','Tide','TimeFt')
  onlyHighData$Tide <- 'High'
  
  # stack columns labeled "low" (number may vary)
  onlyLowData <- RawTidesTable[ ,append(1, which(grepl('Low',colnames(RawTidesTable))))] %>% 
    tidyr::gather(key = NULL.Day)
  colnames(onlyLowData) <- c('Date','Tide','TimeFt')
  onlyLowData$Tide <- 'Low'
  
  # merge high tide and low tide data
  RawTidesLong <- bind_rows(onlyHighData,onlyLowData)
  # remove blank rows
  RawTidesLong <- dplyr::filter(RawTidesLong, TimeFt != "")
  
  ## convert from time / ft format to actual viable measurements
  # observation example: "12:12 AM CST / 0.6 ft"
  # Extract timestamp before the slash
  RawTidesLong$Time <- substr(RawTidesLong$TimeFt, 1, regexpr("/", RawTidesLong$TimeFt) - 2) 
  # Extract feet after slash and before units; convert to number
  RawTidesLong$Feet <- substr(RawTidesLong$TimeFt, regexpr("/", RawTidesLong$TimeFt) + 2, regexpr("ft", RawTidesLong$TimeFt) - 2) %>% as.numeric
  
  # Convert date from weird unidentifiable R date object to just some characters
  RawTidesLong$Date <- as.character(RawTidesLong$Date)
  # extract day, trim whitespace, convert to number
  RawTidesLong$Day <- substr(RawTidesLong$Date, 5, nchar(RawTidesLong$Date)) %>% trimws() %>% as.numeric()
  
  # make and parse a single datetime object
  RawTidesLong$Datetime <- lubridate::ymd_hm(paste0(year,'-',month,'-',RawTidesLong$Day,' ',RawTidesLong$Time))
  
  TidesData <- select(RawTidesLong, Datetime, Tide, Feet)
  TidesData$Tide <- as.factor(TidesData$Tide)
  
  # reorder data?
  TidesData <- TidesData[order(TidesData$Datetime),]
  
  return(TidesData)
}

# get full set of tide data for Oddessy !
FullTideData <- GetTidesData(year = 2016, month = 3, sitenum = 493)
for (mo in 4:12) {
  newData <- GetTidesData(year = 2016, month = mo, sitenum = 493)
  FullTideData <- bind_rows(FullTideData, newData)
}

FullTideData <- FullTideData[!is.na(FullTideData$Datetime),]
write.csv(FullTideData, 'Belize City Tide Data 2016.csv')
