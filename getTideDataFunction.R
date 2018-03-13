require(XML)
require(dplyr)
require(lubridate)

## Inspired by: https://rpubs.com/shyambv/extract_htmljsonxml_data

# eventually, in shinyApp, have ability to search for site by (a) name and (b) coords
# rawSiteIndex <- XML::readHTMLTable('http://tides.mobilegeographics.com/index.html')
# SiteIndexTable <- dplyr::bind_rows(rawSiteIndex)
# given "siteName," listed name of site
# sitecode = which(SiteIndexTable$Location == siteName) - 1

MakeDate <- function(dateString) {
  if (class(dateString) == 'character') {
    # if character string, try to convert to ymd. If fails, stop function. 
    dateString <- tryCatch(expr = lubridate::ymd(dateString), 
      warning = function(w) stop('Error in MakeDate(): must provide either Date object or "yyyy-mm-dd" character string.')
    )
    
  } else if (class(dateString) != 'Date') {
    stop("Date object not provided.")
  }
  
  return(dateString)
}

GetTidesData_Month <- function(year, month, sitecode) {
  # actually fetch table 
  tides_url <- paste0('http://tides.mobilegeographics.com/calendar/month/',sitecode,'.html?y=',year,'&m=',month,'&d=1')
  
  # sometimes the function fails a few times, so make this little loop to wait
  RawTidesTable <- data.frame()
  while(NROW(RawTidesTable) < 1) {
    RawTidesTable <- XML::readHTMLTable(tides_url) 
  }
  RawTidesTable <- as.data.frame(RawTidesTable) # convert to dataframe
  
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
  RawTidesLong$HeightUnit <- ifelse(grepl("ft",RawTidesLong$TimeFt[1]), 'ft','m') %>% as.factor()
  RawTidesLong$Height <- substr(RawTidesLong$TimeFt, regexpr("/", RawTidesLong$TimeFt) + 2, regexpr(RawTidesLong$HeightUnit[1], RawTidesLong$TimeFt) - 2) %>% as.numeric
  
  # Convert date from weird unidentifiable R date object to just some characters
  RawTidesLong$Date <- as.character(RawTidesLong$Date)
  # extract day, trim whitespace, convert to number
  RawTidesLong$Day <- substr(RawTidesLong$Date, 5, nchar(RawTidesLong$Date)) %>% trimws() %>% as.numeric()
  
  # make and parse a single datetime object
  RawTidesLong$Datetime <- lubridate::ymd_hm(paste0(year,'-',month,'-',RawTidesLong$Day,' ',RawTidesLong$Time))
  
  # new table with only relevant columns
  TidesData <- select(RawTidesLong, Datetime, Tide, Height, HeightUnit)
  TidesData$Tide <- as.factor(TidesData$Tide) # convert to factor for better summary stats
  
  # reorder data
  TidesData <- TidesData[order(TidesData$Datetime),]
  
  return(TidesData)
}


# Parameters:
# firstDate, a string in ymd format or date object
# lastDate, a string in ymd format or date object
# sitecode, the mobilegeographic site number
GetTidesData_Range <- function(firstDate, lastDate, sitecode) {
  firstDate <- MakeDate(firstDate)
  lastDate <- MakeDate(lastDate)

  firstMonth <- lubridate::ceiling_date(firstDate, 'month')
  lastMonth <- lubridate::floor_date(lastDate, 'month')
  
  DateVec <- seq.Date(from = firstMonth, to = lastMonth, by = 'month') %>% floor_date('month') %>% as.character()
  
  FullTideData <- GetTidesData_Month(year = year(firstDate), month = month(firstDate), sitecode = sitecode)
  for (dt in DateVec) {
    dt <- ymd(dt)
    newData <- GetTidesData_Month(year = lubridate::year(dt), month = lubridate::month(dt), sitecode = sitecode)
    FullTideData <- bind_rows(FullTideData, newData)
  }
  
  TideData <- filter(FullTideData, Datetime >= floor_date(firstDate,'day') & Datetime <= ceiling_date(lastDate,'day'))
  
  return(TideData)
}


# GetTidesData_Date
# date: a Date object or string in "yyyy-mm-dd" format.
# sitecode: a mobilegeographic site number 
GetTidesData_Date <- function(date, sitecode) {
  # ensure correct date format, convert to date
  date <- MakeDate(date)
  
  WholeMonthData <- GetTidesData_Month(lubridate::year(date), lubridate::month(date), sitecode)
  
  DateData <- filter(WholeMonthData, date(Datetime) == date)
  
  return(DateData)
}
