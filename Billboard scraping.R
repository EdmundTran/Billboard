# This function takes a recent year (2013+) and writes a csv file containing
# all the songs that were on the Billboard Hot 100 in that year.

# May not work for older years because of differences in chart formatting
# (lack of "Chart Highlights" before xxxx, 2 highlights, 3 highlights,
# then introduction of 4th highlight [Pacesetter/Streaming Gainer] in mid-2012,
# so different rows to clean up)

scrape <- function(year) {
  library(httr)
  library(XML)
  library(lubridate)
  
  # Find the first Saturday of the year and initialize current_date with it
  day1 <- as.POSIXlt(paste(year, "01-01", sep = "-"))
  week1 <- as.POSIXlt(seq(day1, length.out = 7, by = "day"))
  saturday1 <- week1[week1$wday == 6]
  current_date <- saturday1
  
  # create an empty matrix to hold year's charts
  # the inital row of this matrix will be removed later
  year_chart <- matrix(ncol = 2)
  
  # If current year, only loop for number of past weeks this year
  # Otherwise, loop for 52 weeks
  if (year == year(Sys.Date())) {
    num_weeks <- week(Sys.Date())
  } else {
    num_weeks <- 52
  }
  
  # Loop for each week
  for (i in 1:num_weeks) {
    url_to_scrape <- paste0("http://www.billboard.com/charts/hot-100/", 
                            year(current_date), "-",
                            sprintf("%02d", month(current_date)), "-",
                            sprintf("%02d", day(current_date)))
    httr_get <- GET(url_to_scrape)
    httr_content <- content(httr_get, as = "text")
    httr_htmlparse <- htmlParse(httr_content, asText = TRUE)
    
    # 116 values
    songs <- xpathSApply(httr_htmlparse, "//h2", xmlValue)
    
    # clean up by removing 16 values
    songs <- songs[c(-1, -(3:7), -17, -(83:86), -(112:116))]
    
    # 110 values
    artists <- xpathSApply(httr_htmlparse, "//h3", xmlValue)
    
    # clean up by removing 10 values
    artists <- artists[c(-(2:9), -19, -85)]
    
    # create weekly chart
    week_chart <- cbind(songs, artists)
    
    # add weekly chart to yearly chart
    year_chart <- rbind(year_chart, week_chart)
    
    # update current_date to next week
    current_date <- current_date + days(7)
  }
  
  # remove initial empty row
  year_chart <- year_chart[-1, ]
  
  # remove duplicate rows (doesn't catch typos and differences in case)
  unique_chart <- unique(year_chart)
  
  write.csv(unique_chart, file = paste0(year,"BillboardHot100.csv"))
}




