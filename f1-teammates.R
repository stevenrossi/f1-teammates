library(dplyr)
library(ggvis)

dat <- read.csv("f1-teammate-data.csv") %>%
       filter( !is.na(Teammate) )

getDriverInfo <- function( driver1 = "Gasly, Pierre" )
{
  dat0 <- filter(dat,driverName==driver1)

  z <- c("Name:",driver1)

  if( dat0$number[1] != "NULL" )
    z <- rbind( z, c("Number:",dat0$number[1]) )

  z <- rbind( z,
              c("DOB:",dat0$dob[1]),
              c("Nationality:",dat0$nationality[1]),
              c("First Race:",paste0(dat0$year[1]," ",dat0$raceName[1]," (",dat0$date[1],")")),
              c("Most Recent Race:",paste0(dat0$year[nrow(dat0)]," ",dat0$raceName[nrow(dat0)]," (",dat0$date[nrow(dat0)],")")),
              c("Number of entries:",nrow(dat0)),
              c("Number of wins:",sum(dat0$raceWinner==driver1)),
              c("Wiki:",dat0$url[1])
               )

  colnames(z) <- c(" "," ")
  z
}

getTeammateTable <- function( driver1 = "Gasly, Pierre" )
{
  dat0 <- filter(dat,driverName==driver1)

  dat1 <- dat0 %>%
          group_by(Teammate) %>%
          summarize(Team=first(constructorName),
                    firstR=min(yearx),
                    firstRace=min(year),
                    lastRace=max(year),
                    nRaces=length(relPosition),
                    pWin=mean(relPosition>0),
                    avgPositionDiff=mean(relPosition),
                    totalPointsDiff=sum(points)-sum(teammatePoints)) %>%
          arrange(firstR) %>%
          select(Teammate,Team,firstRace,lastRace,nRaces,pWin,avgPositionDiff,totalPointsDiff)

  dat1$Teammate <- gsub(",.*","",dat1$Teammate)

  dat1 <- rbind( dat1,
                 c("Total","",
                   min(dat0$year),
                   max(dat0$year),
                   nrow(dat0),
                   mean(dat0$relPosition>0),
                   mean(dat0$relPosition),
                   sum(dat0$points)-sum(dat0$teammatePoints)) ) %>%
          mutate(pWin=format(round(as.numeric(pWin),3),nsmall=3),
                 avgPositionDiff=format(round(as.numeric(avgPositionDiff),2),nsmall=2))

  dat1

}




getTeammateData <- function()
{
  dat <- read.csv(file="f1-teammate-data-raw.csv")

  dat <- dat %>%
         mutate( Teammate    = NA,
                 teammatePosition = NA,
                 teammatePoints = NA,
                 relPosition = NA,
                 raceWinner  = "Neither" )

  # Loop over every race result
  for( i in 1:nrow(dat) )
  {
    # Get 
    dat2 <- dat %>%
            filter( constructorId == dat[i,"constructorId"],
                    driverName    != dat[i,"driverName"],
                    year          == dat[i,"year"],
                    round         == dat[i,"round"]
                  )

    if( nrow(dat2) == 1  )
    {  
      dat[i,"relPosition"] <- dat2[1,"positionOrder"]-dat[i,"positionOrder"]
      dat[i,"Teammate"] <- dat2[1,"driverName"]
      dat[i,"teammatePosition"] <- dat2[1,"positionOrder"]
      dat[i,"teammatePoints"] <- dat2[1,"points"]
    }
  }

  dat <- dat %>%
         mutate( date = as.Date(date),
                 julian = as.POSIXlt(date)$yday,
                 yearx  = year + julian/365 ) %>%
         arrange( year, round )

  dat$x <- 1:nrow(dat)

  driverWins <- dat[ ,"positionOrder"]==1
  dat[driverWins,"raceWinner"] <- dat[driverWins,"driverName"]
  
  teammateWins <- dat[ ,"teammatePosition"]==1
  dat[teammateWins,"raceWinner"] <- "Teammate"

  write.csv(dat,file="f1-teammate-data.csv")

}


