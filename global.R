library(shiny)
library(xts)
library(leaflet)
library(dplyr)
library(RColorBrewer)

#df <- readRDS("tracking_old.RDS")
#df$carid = gsub(" ", "", df$carid)




interpolate_data <- function(df){
  #' create time series by min
  newdf <- df[0,]
  for( car in unique(df$carid)){
    
    thisdf = df[df$carid == car,]
    
    min_ts = seq(min(thisdf$timestp), max(thisdf$timestp), by='min')
    min_ts = cbind.data.frame(timestp = min_ts)
    
    min_ts = min_ts[seq(1, nrow(min_ts), 10), ]
    min_ts = cbind.data.frame(timestp = min_ts)
    
    thisdf <- merge(min_ts, thisdf, by = 'timestp', all.x = TRUE)
    thisdf$carid <- car
    thisdf <- thisdf[order(thisdf$timestp),]
    
    thisdf$latitude <- na.approx(thisdf$latitude, thisdf$timestp, na.rm = FALSE, maxgap = nrow(thisdf))
    thisdf$longitude <- na.approx(thisdf$longitude, thisdf$timestp, na.rm = FALSE, maxgap = nrow(thisdf))
    
    newdf <- rbind(newdf, thisdf)
    
    
  }
  
  newdf <- newdf[!is.na(newdf$lat),]
  
  return(newdf)
}


if(FALSE){
  #df <- readRDS("pathdf2.RDS")
  carmap = read.table("car_mapping", stringsAsFactors = F, header = T)
  
  for( i in 1:nrow(carmap) ){
    motusid = carmap$MOTUS[i]
    iwireid = carmap$IWIRE[i]
    df$carid[df$carid == iwireid] = motusid
    
  }
}

#df <- readRDS("iwire2.RDS")

df <- readRDS("pathdf.RDS")

df <- df %>% filter(timestp >= as.Date("2019-03-17"))
# df <- df %>% filter(carid == "EV7")
# 
# df <- df %>%
#   mutate( lat_diff = abs(latitude - lag(latitude))*1000 ) %>%
#   mutate( lng_diff = abs(longitude - lag(longitude))*1000 )
# 
# df <- df %>% filter(lat_diff < 50) %>% filter(lng_diff < 50)


pal = colorFactor(palette = "Dark2", domain = df$carid)

#df <- interpolate_data(df)

#busdata <- readRDS("busdata.RDS")

# x = read.csv("bus2.csv", stringsAsFactors = FALSE)
# x$carid = "EV 8 "
# 
# x$latitude = as.numeric(na.approx(zoo(c(x$latitude))))
# x$longitude = as.numeric(na.approx(zoo(c(x$longitude))))
# 
# x$timestp = as.POSIXct("17/03/2019 15:03",format="%d/%m/%Y %H:%M")
# x$timestp = x$timestp + seq(from=5, to=nrow(x)*5,by = 5)
# 
# x2 <- x
# x2$latitude = rev(x$latitude)
# x2$longitude = rev(x$longitude)
# x2$timestp <- x2$timestp + 120
# 
# x3 <- x
# x3$timestp <- x2$timestp + 120
# 
# x4 <- x2
# x4$timestp <- x3$timestp + 120
# 
# x5 <- x
# x5$timestp <- x4$timestp + 120
# 
# x6 <- x2
# x6$timestp <- x5$timestp + 120
# 
# x7 <- x
# x7$timestp <- x6$timestp + 120
# 
# x8 <- x2
# x8$timestp <- x7$timestp + 120
# 
# x9 <- x
# x9$timestp <- x8$timestp + 120
# 
# x10 <- x2
# x10$timestp <- x9$timestp + 120



#df = rbind.data.frame(df,x,x2,x3,x4,x5,x6,x7,x8,x9,x10)

#df = rbind(df, busdata)

df$carid <- as.character(df$carid)

#saveRDS(df, "tracking.RDS")

step_const = 20

icons <- awesomeIcons(
  icon = 'car-side',
  iconColor = 'black',
  library = 'fa'
)

carIcon <- makeIcon(
  iconUrl = "car_icon5.png",
  iconWidth = 30, iconHeight = 30
)

busIcon <- makeIcon(
  iconUrl = "bus_icon5.png",
  iconWidth = 40, iconHeight = 40
)


#' manipulations
# ev5 = df %>%
#   filter(carid == "EV 5 ")
# 
# ev7 = df %>%
#   filter(carid == "EV 7 ")
# 
# ev3 = df %>%
#   filter(carid == "EV 3 ")
# 
# ev2 = ev5[order(ev5$timestp, decreasing = T), ]
# ev2$timestp = ev5$timestp
# ev2$carid = "EV 2 "
# 
# ev4 = ev7[order(ev7$timestp, decreasing = T), ]
# ev4$timestp = ev7$timestp
# ev4$carid = "EV 4 "
# 
# ev6 = ev3[order(ev3$timestp, decreasing = T), ]
# ev6$timestp = ev3$timestp
# ev6$carid = "EV 6 "
# 
# df <- rbind(df, ev2, ev4, ev6)
#

#df <- df[order(df$timestp),]

df_1_last = df %>%
  filter(carid == "EV1" & row_number()==1)

df_2_last = df %>%
  filter(carid == "EV2"& row_number()==1)


df_3_last = df %>%
  filter(carid == "EV3"& row_number()==1)


df_4_last = df %>%
  filter(carid == "EV4"& row_number()==1)


df_5_last = df %>%
  filter(carid == "EV5"& row_number()==1)

df_6_last = df %>%
  filter(carid == "EV6"& row_number()==1)

df_7_last = df %>%
  filter(carid == "EV7"& row_number()==1)

df_8_last = df %>%
  filter(carid == "EV8"& row_number()==1)

