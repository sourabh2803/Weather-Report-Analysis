library(lubridate) #package to parse date and time
library(stringr)   #package to split the string variable

#---------------------------------DATA IMPORTING AND CLEANING--------------------------------------------------------------------------------------

path <- "E:/Analytics/Weather Report"
setwd(path)

#loading the text file into R using readlines function
weather_data <- paste(readLines("input_temp.txt"))

#converting the data into dataframe
weather_data <- as.data.frame(weather_data) 

#changing the column name
names(weather_data)[names(weather_data) == 'weather_data'] <- 'Observations'

#changing the variable type of column
weather_data$Observations <- as.character(weather_data$Observations)


#--------------------------------CREATING A USER FUNCTION TO GET MAXIMUM AND MINIMUM TEMPERATURE FROM THE DATA--------------------------------------


#By putting date and time range, we can calculate minimum and maximum temperature within that range. Also, country is an optional argument.
weather_country_null <- function(weather_data,date_min, date_max, time_min, time_max, Country)
{  
  #converting given string variable into date format, becuase to compare two dates, we need to convert them into date format,
  #otherwise we can not compare two dates, which are originally string variable
  date_min <- lubridate::dmy(date_min) 
  date_max <- lubridate::dmy(date_max)
  time_min <- lubridate::hms(time_min) #converting given time into time format
  time_max <- lubridate::hms(time_max)
  
  store <- data.frame()   #creating an empty dataframe to store values
  
  #follow this loop, if country argument is not given
  if( missing(Country)) 
  {
    for( i in 1:nrow(weather_data))
	{
	  #spliting the row whereever there is "\t" and "_". so that country, date, time and temperature can be distinguished.
      a <- strsplit(weather_data[i,], split = '["\t","_"]') 
      b <- lubridate::dmy(a[[1]][2])
      if (date_min <= b & b <= date_max) #comparing the date with given date range
	  {
        c <- seq(3, length(a[[1]]), by = 2) #creating odd numbered sequence starting with 3, becuase all odd indices contain time variable.
        for (j in 1: length(c))
		{
          d <- lubridate::hms(a[[1]][c[j]])
          if (time_min <= d & d <= time_max) #comparing the time with given time range
		  {
            #store <- a[[1]][c[j]+1]
            store <- rbind(store, as.numeric(a[[1]][c[j]+1]) )  #storing the temperature into dataframe using rbind function, so the new entry could be added as new row.
            #store <- do.call("rbind", sapply(1:length(c), FUN = function(i) as.numeric(a[[1]][c[j]+1]), simplify = FALSE))
		   }
            j <- j+1              
		 }   
       }
        i <- i+1       
     } 
   }
  
  #follow this loop is country argument is given
  else {
         index <- grep(Country, weather_data$Observations) #finding indices where given country is present
         for( i in 1:length(index))
		 {
           a <- strsplit(weather_data[index[i],], split = '["\t","_"]')
           b <- lubridate::dmy(a[[1]][2])
           if (date_min <= b & b <= date_max)
		   {
             c <- seq(3, length(a[[1]]), by = 2)
             for (j in 1: length(c))
			 {
               d <- lubridate::hms(a[[1]][c[j]])
               if (time_min <= d & d <= time_max)
			   {
                 #store <- a[[1]][c[j]+1]
                 store <- rbind(store, as.numeric(a[[1]][c[j]+1]) )
                 #store <- do.call("rbind", sapply(1:length(c), FUN = function(i) as.numeric(a[[1]][c[j]+1]), simplify = FALSE))
                }  
               j <- j+1              
			   }   
            }
             i <- i+1                                            
		  }  
        }
  temp_max <- max(store)
  temp_min <- min(store)
  print(paste("Max Temp =",temp_max,"    Min Temp =",temp_min ))
  #system.time(do.call("rbind", sapply(1:length(c)*nrow(weather_data), FUN = function(i) as.numeric(a[[1]][c[j]+1]), simplify = FALSE)))
  print(store)
}