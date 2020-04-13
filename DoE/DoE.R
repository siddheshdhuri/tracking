library(readxl)
library(xlsx)

if(FALSE){
  
  setwd("~/SOTracker")
  #A_C ID 8603465968 MN-I1C18D002572 D-19-03-2019
  water_dir = "DoE/water/"
  electric_dir = "DoE/electricity/"
  
  water_files = list.files(water_dir)
  electric_files = list.files(electric_dir)
  
  col_names = c("time", "profile_status", "active_energy_import",
                "active_energy_export","reactive_energy_import","reactive_energy_export", "ac_id")
  
  electric_df <- readxl::read_excel(paste0(electric_dir, electric_files[1]))
  ac_id <- substr(electric_files[1],1,nchar(file)-15)
  filename <- electric_files[1]
  electric_df['ac_id'] <- ac_id
  electric_df['filename'] <- filename
  electric_df['block'] <- "1"
  
  electric_df <- electric_df[0,c(1,2,3,5,6,7,8,9,10)]
  colnames(electric_df) <- c(col_names, 'filename', 'block')
  
  
  
  
  for(file in electric_files){
    
    ac_id <- substr(file,1,nchar(file)-15)
    
    
    
    tryCatch({
      
      x <- readxl::read_excel(paste0(electric_dir, file), col_types = "text")
      x['ac_id'] <- ac_id
      
      x <- x[1:25,c(1,2,3,5,6,7,8)]
      colnames(x) <- col_names
      
      x['filename'] <- file
      x['block'] <- "1"
      
      electric_df <- rbind(electric_df, x)
      
    }, error = function(e) {
      
      tryCatch({
        
        x <- readxl::read_excel(paste0(electric_dir, file), col_types = "text")
        x['ac_id'] <- ac_id
        
        colnames(x) <- col_names
        x['filename'] <- file
        x['block'] <- "2"
        
        electric_df <- rbind(electric_df, x)
        
      }, error = function(e) {
        
        tryCatch({
          
          x <- readxl::read_excel(paste0(electric_dir, file), col_types = "text")
          
          colnames(x) <- c("time", "active_energy_import",
                           "active_energy_export", "delete")
          
          x['ac_id'] <- ac_id
          x['filename'] <- file
          x['block'] <- "3"
          
          x["delete"] <- NULL
          
          x["profile_status"] <- ""
          x["reactive_energy_import"] <- 0
          x["reactive_energy_export"] <- 0
          
          electric_df <- rbind(electric_df, x)
          
          
        }, error = function(e){
          print(file)
        })
        
      })
      
      
    })
    
    
    
  }
  
  conv <- function(x){
    val <- x
    tryCatch({
      val <- if (nchar(x) > 18 ) lubridate::dmy_hms(x) else openxlsx::convertToDateTime(as.numeric(x), origin = "1900-01-01")
    }, error = function(e) {
    })
    return(as.character(val))
  }
  
  electric_df$time <- unlist(lapply(electric_df$time, FUN = conv))
  
  electric_df$date <- substr(electric_df$time,1,10)
  electric_df$date <- as.Date(electric_df$date, "%Y-%m-%d")
  electric_df$hour <- substr(electric_df$time,12,13)
  electric_df$hour <- sub("^$", "00", electric_df$hour)
  electric_df$hour <- as.integer(electric_df$hour)
  
  
  electric_df$active_energy_import <- as.integer(electric_df$active_energy_import)
  
}

if(FALSE) {
  
  col_names = c("time", "profile_status", "active_energy_import",
                "active_energy_export","reactive_energy_import","reactive_energy_export")
  
  
  water_df <- readxl::read_excel(paste0(water_dir, water_files[1]))
  #water_df <- electric_df[0,c(1,2,3,5,6,7)]
  #colnames(electric_df) <- col_names
  col_names <- colnames(water_df)
  
  ac_id <- unlist(strsplit(water_files[1], " "))[3]
  water_df['ac_id'] <- ac_id
  
  water_df <- water_df[0,]
  
  for(file in water_files){
    
    tryCatch({
      
      tryCatch({
        
        x <- readxl::read_excel(paste0(water_dir, file))
        
        
        ac_id <- unlist(strsplit(file, " "))[3]
        x['ac_id'] <- ac_id
        
        water_df <- rbind(water_df, x)
        
      }, error= function(e){
        x <- readxl::read_excel(paste0(water_dir, file), col_names = col_names)
        
        ac_id <- unlist(strsplit(file, " "))[3]
        x['ac_id'] <- ac_id
        
        water_df <- rbind(water_df, x)
      })
      
    }, error= function(e){
      print(file)
      print(e)
    })
    
  }
  
  
  water_df$Date <- as.Date(water_df$Date, "%b %d, %Y")
  water_df$Time <- as.integer(substr(water_df$Time,1,2))
  water_df$`Total volume` <- as.double(sub(" .*", "", water_df$`Total volume`))
  water_df$`Temperature (environment)` <- as.double(sub(" .*", "", water_df$`Temperature (environment)`))
  water_df$`Temperature (medium)` <- as.double(sub(" .*", "", water_df$`Temperature (medium)`))
  
  
}

library(ggplot2)


c <- ggplot(electric_df, aes(x = date, y = active_energy_import))
c + geom_bar(stat = "identity")


p1 <- ggplot(data = electric_df, aes(x = date, y = active_energy_import)) + geom_bar(stat = "identity", fill = "#552683") +
  ylab("Energy consumption") + xlab("Day") + facet_grid(. ~ date) +
  ggtitle("TITLE OF THE FIGURE")
p1



ggplot(data=electric_df, aes(x=hour, y=active_energy_import, group=1)) +
  geom_line(color="red")+
  geom_point()


x <- electric_df[,c('hour', 'active_energy_import')] %>% 
      filter(!is.na(active_energy_import)) %>%
      group_by(hour) %>%
        summarise(active_energy_import = mean(as.numeric(active_energy_import), na.rm = TRUE))

ggplot(electric_df) + 
  stat_summary(aes(x = hour, y = active_energy_import), 
               fun.y = function(x) mean(as.numeric(x), na.rm=TRUE), 
               geom = "line") + 
  geom_point(aes(x = hour, y = active_energy_import))


ggplot(x) + 
  geom_point(aes(x = hour, y = active_energy_import, colour = active_energy_import), size = 3) +
  stat_smooth(aes(x = hour, y = active_energy_import), method = "lm",
              formula = y ~ poly(x, 21), se = FALSE)





x <- electric_df[,c('date','hour', 'active_energy_import')] %>% 
  filter(!is.na(active_energy_import)) %>%
  group_by(date, hour) %>%
  summarise(active_energy_import = sum(as.numeric(active_energy_import), na.rm = TRUE))

p2 <- ggplot(data = x, aes(x = hour, y = hour, group = factor(date))) +
  geom_line(stat = "identity", aes(linetype = factor(date)), size = 0.7, colour = "#552683") +
  ylab("Y LABEL") + xlab("X LABEL") + ggtitle("TITLE OF THE FIGURE")
p2

