library( lubridate )
library( dplyr )
library( plotly )
library( shiny ) 




minsSinceMidnight <- function( x ) {
### Recode TIME to "DAY"(07:00 - 18:59) /"NIGHT (19:00 - 06:59)"
  as.numeric( difftime( as.POSIXct( x, format = '%H:%M'),
                          as.POSIXct('00:00', format = '%H:%M'),
                          units = 'min'
                        )
              )
}




timeGrouper <- function( x, timeTable ) {
### convertes time (mins since midnight) to one of many groups defined in the
### timeTable argument
### format is:   START   END   LABEL
    y <- c()
    for( t in 1:dim(timeTable)[1] ){
        if( x >= timeTable[t,1] && x <= timeTable[t,2] ){ y <- timeTable[t,3] }
    }
    return( y )
}




restateNA <- function( x ) {
### replace spaces, "unspecified" and blanks with NA
    x <- sub( "^(\\s*|unspecified|)$", NA, x, ignore.case = TRUE )
}




addFactor <- function( daColName, daDF, daLookups ) {
    f <- rep( 0, dim(daDF)[1] )
    for(j in 1:length(daLookups) ){
        f <- f + ( daDF[ , daLookups[ j ] ] %in% daColName )
    }
    f[ f >= 1 ] <- 1
    f <- as.factor( f )
    return( f )
}





minsToClock <- function( x ){
### takes a number and converts it to string clock
### e.g. 182 into "03:02"
    res <- NA
    if( x > 0 || x < 1440 ){
        res <- paste0( sprintf( "%02d", x %/% 60 ), ":", sprintf( "%02d", x %% 60 ) )
    }
    return( res )
}




makeYLabels <- function( ival ) {
### returns a vector with size 1440 / ival
### labelled minutes
    ret <- minsToClock( seq( 0, 1439, by=ival ) )
    return( ret )
}




createMissingRows <- function( x, daWeekday ) {
    allMins <- 0:1439
    missingMins <- !(allMins %in% x)
    newMins <- allMins[missingMins]
    nn <- length( newMins )
    newRows <- data.frame(WEEKDAYS=rep( daWeekday, nn),
                          TIME = minsToClock( newMins ),
                          COUNT = rep(0, nn),
                          TIME.MINS = newMins
                         )
    newRows$WEEKDAYS <- as.factor(newRows$WEEKDAYS)
    newRows$TIME <- as.character(newRows$TIME)
    newRows$COUNT <- as.integer(newRows$COUNT)
    newRows$TIME.MINS <- as.numeric(newRows$TIME.MINS)
    return( newRows )
}




addMissingMins <- function( daDF, daWeekday ) {
    newRows <- createMissingRows( subset(daDF, daDF$WEEKDAYS == daWeekday)$TIME.MINS, daWeekday )
    for(i in 1:nrow(newRows)){
        aRow <- as.list(newRows[i,])
        daDF <- daDF %>% rbind( aRow )
    }
    return(daDF)
}




textDF <- data.frame( tInterval <- c( 1, 5, 15, 30, 60, 120, 180 ),
                         eText <- c( "This is the finest level, it is too noisy to see any clear pattern",
                                    "Notice the 'stripes', which may be due to eye witnesses mentally rounding up the time of the incident to the nearest quarter (15 min) of the hour",
                                    "We can see the 'stripes' here",
                                    "The stripes are still pretty strong here",
                                    "This seems to be the 'ideal' level of granularity. Notice how the difference between Mon-Fri and Sat-Sun is clearly visible. Also notice that on Mon-Fri, there are two 'peaks', at 08:00-09:00 and 16:00-17:00, this would disappear at lower levels of granularity",
                                    "Similar to the hourly (60 mins) interval, but less resolution",
                                    "At this very low level of granularity, the 'two-peaks' pattern on Mon-Fri is no longer visible"
                                   )
                        )


makeExplanatoryText <- function( intvl ){
    intvl <- as.numeric( intvl )
    idx <- which(textDF$tInterval == intvl)
    return( as.character( textDF[ idx, 2 ] ) )
}




shinyServer(function(input, output) {
  ### Load the data
  inData <- read.table( './inData.csv',
                      header = TRUE,
                      stringsAsFactor = FALSE,
                      fill = TRUE,
                      quote = '"',
                      sep = ","
                    ) 
  
  ### For HEATMAP of day x time:
  ### narrow down to only columns of interest
  dat <- inData[,c( 1,2,3, 31:35 )]
  names(dat) <- gsub("[\\/\\s\\(\\)\\_\\-\\']", ".", names(dat), perl=TRUE, )
  
  ### group and summarize
  datHeatMap <- dat %>% group_by( WEEKDAYS, TIME ) %>%
    summarize( COUNT=n() )

  
  ### ensure each weekdays have complete range of minutes
  datHeatMap$TIME.MINS <- minsSinceMidnight( datHeatMap$TIME )
  datHeatMap <- addMissingMins(datHeatMap, "Monday")
  datHeatMap <- addMissingMins(datHeatMap, "Tuesday")
  datHeatMap <- addMissingMins(datHeatMap, "Wednesday")
  datHeatMap <- addMissingMins(datHeatMap, "Thursday")
  datHeatMap <- addMissingMins(datHeatMap, "Friday")
  datHeatMap <- addMissingMins(datHeatMap, "Saturday")
  datHeatMap <- addMissingMins(datHeatMap, "Sunday")

  ### recode WEEKDAYS from words into numbers
  datHeatMap[ datHeatMap$WEEKDAYS=="Monday",1 ] <- 1
  datHeatMap[ datHeatMap$WEEKDAYS=="Tuesday",1 ] <- 2
  datHeatMap[ datHeatMap$WEEKDAYS=="Wednesday",1 ] <- 3
  datHeatMap[ datHeatMap$WEEKDAYS=="Thursday",1 ] <- 4
  datHeatMap[ datHeatMap$WEEKDAYS=="Friday",1 ] <- 5
  datHeatMap[ datHeatMap$WEEKDAYS=="Saturday",1 ] <- 6
  datHeatMap[ datHeatMap$WEEKDAYS=="Sunday",1 ] <- 7

  ### sort by day and timeTable
  datHeatMap <- datHeatMap %>% arrange( WEEKDAYS, TIME )
  

  dm2 <- reactive({
    datHeatMap$TIME.SEG <- datHeatMap$TIME.MINS %/% as.numeric(input$tInterval)
    datHeatMapSeg <- datHeatMap %>% group_by( WEEKDAYS, TIME.SEG ) %>%
      summarize( COUNT = sum(COUNT) )%>%
      arrange( WEEKDAYS, TIME.SEG )

    matrix( datHeatMapSeg$COUNT, nrow=(1440/as.numeric(input$tInterval)), ncol=7 )
  })
  
  output$plot1 <- renderPlotly({

    plot_ly(
      x = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
      y = makeYLabels(as.numeric(input$tInterval)),
      z = ~dm2(),
      type = "heatmap",
      text = c('Apples', 'Pears', 'Peaches', 'Bananas', 'Pineapples', 'Cherries')
  ) %>%
    layout(
      title = 'Based on data from 01/01/2017 to 12/31/2017',
      xaxis = list(
        type = 'category',
        title = 'Day'
      ),
      yaxis = list(
        title = 'Hour',
        type='bar'
      )
    )
  })
  
  output$text1 <- renderText({
      makeExplanatoryText( input$tInterval )
  })
})
