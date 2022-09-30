library(tidyverse)
library(janitor)
detach("package:dplyr")
#detach("package:plyr") # only run if already attached, else it will result in an error!
library(plyr) # has to be loaded before dplyr to prevent any issues
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggcorrplot)
library(wesanderson)
library(RColorBrewer)
library(sqldf)
library(scales)
library(ggpubr)
library(ggplot2)
library(ggcorrplot)


library(rmd2jupyter)
rmd2jupyter("US_Flight_Analysis.md")

local_path <- "/Users/markuskofler/Google Data Analytics/Portfolio Project/Flight_delay.csv"
flights_df <- read_csv(local_path)



names(flights_df) <- tolower(names(flights_df %>% 
                                     dplyr::rename(weekday = DayOfWeek,
                                            dep_time = DepTime,
                                           arr_time = ArrTime,
                                           scheduled_arr_time = CRSArrTime, 
                                           uniq_carrier_code = UniqueCarrier,
                                           flight_num = FlightNum,
                                           tail_num = TailNum,
                                           actual_flight_time_min = ActualElapsedTime,
                                           estimate_flight_time_min = CRSElapsedTime,
                                           air_time_min = AirTime,
                                           arr_delay = ArrDelay,
                                           dep_delay = DepDelay,
                                           dep_airport_code = Origin,
                                           dep_airport = Org_Airport,
                                           dest_airport_code = Dest,
                                           dest_airport = Dest_Airport,
                                           distance_miles = Distance, 
                                           landing_to_gate_min = TaxiIn,
                                           gate_to_takeoff_min =TaxiOut,
                                           cancellation_cause_code = CancellationCode,
                                           carrier_delay = CarrierDelay,
                                           weather_delay = WeatherDelay,
                                           nas_delay = NASDelay,
                                           security_delay = SecurityDelay,
                                           late_aircraft_delay = LateAircraftDelay)))

vector <- c()
for (i in names(flights_df)) {
  if (is_double(flights_df[[i]][2]) == TRUE) {
    if (sum(flights_df[i]) == 0 ) {
      vector <- append(vector, i)
    }
  }
}

#method 1: selecting all except from elements of vector
flights_df <- select(flights_df, -all_of(vector))


#method 2: dropping useless columns
flights_df <- flights_df[!(names(flights_df) %in% vector)]



flights_df <- mutate(flights_df,
                     total_delay = (carrier_delay + weather_delay + nas_delay + 
                                    security_delay + late_aircraft_delay))


library(lubridate)

flights_df <- flights_df %>% mutate(month = month(dmy(date)))


flights_df <- flights_df %>% 
  mutate(degree_delay =
           ifelse(total_delay <= 15, "no delay", 
                  ifelse(total_delay >= 45, "large delay", "medium delay")))
           
flights_df$degree_delay[1:10]

# inefficient !!!
#######################################################
#vec <- c()                                            #
#for (t in flights_df$total_delay) {                   #
#  if (t <= 15) {                                      #
#    vec <- append(vec, "No delay")                    #
#}                                                     #
#  if (t >= 45) {                                      #
#    vec <- append(vec, "Large delay")                 #
#  }                                                   #
#  else {                                              #
#    vec <- append(vec, "Medium delay")                #
#  }                                                   #
#}                                                     #
#######################################################
#flights_df["delay_degree"] <- vec

flights_df <- flights_df %>% 
  mutate(compensation =
           ifelse(total_delay < 180, "no compensation", 
                  ifelse(total_delay >= 300, "full refund", "up to 600€")))

flights_df$compensation[1:10]

# inefficient !!!
#################################################
#vect <- c()                                     # 
#for (c in flights_df$total_delay){              #
#  if (c < 180){                                 #
#    vect <- append(vect, "no compensation")     #
#  }                                             #
#  if (c >= 300){                                #
#    vect <- append(vect, "full refund")         #
#  }                                             #
#  else {                                        #
#    vect <- append(vect, "up to 600€")          #
#  }                                             #
#}                                               #
#################################################
#flights_df["compensation"] <- vect

flights_df %>% 
  group_by(airline) %>% 
  drop_na() %>% 
  summarize(accumulated_delay = sum(total_delay)) %>% 
  arrange(-accumulated_delay)


as.data.frame(table(flights_df$airline)) %>% arrange(-Freq)


ggplot(flights_df) +
  geom_bar(aes(x = airline), fill = "#00CC99", color = "#009933", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  labs(title = "Number of Flights US Airlines",
        x = "Airline", y = "Number of Flights")


ggplot(flights_df) +
  geom_bar(aes(x = airline), fill = "#00CC99", color = "#009933", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  labs(title = "Number of Flights US Airlines",
        x = "Airline", y = "Number of Flights") +
  facet_wrap(~month)


flights_df %>% 
  group_by(airline) %>% 
  drop_na() %>% 
  summarize(delay = mean(total_delay)) %>% 
  arrange(-delay)


flights_df %>%  summarize(total_carrier = sum(carrier_delay),
                          total_weather = sum(weather_delay),
                          total_nas = sum(nas_delay),
                          total_security = sum(security_delay),
                          total_late_aircraft = sum(late_aircraft_delay)) %>% 
  pivot_longer(cols=1:5, names_to = 'Delay_Type', values_to = 'Accumulated_Delay') %>% 
  arrange(-Accumulated_Delay)


  

df1 <- flights_df %>%  summarize(carrier = sum(carrier_delay),
                                 weather = sum(weather_delay),
                                 nas = sum(nas_delay),
                                 security = sum(security_delay),
                                 late_aircraft = sum(late_aircraft_delay)) %>% 
  pivot_longer(cols=1:5, names_to = 'Delay_Type', values_to = 'Accumulated_Delay') %>% 
  arrange(-Accumulated_Delay)

df2 <- flights_df %>% summarize(carrier = mean(carrier_delay),
                                weather = mean(weather_delay),
                                nas = mean(nas_delay),
                                security = mean(security_delay),
                                late_aircraft = mean(late_aircraft_delay)) %>% 
  pivot_longer(cols=1:5, names_to = 'Delay_Type', values_to = 'Average_Delay') %>% 
  arrange(-Average_Delay)

#inner join of both data frames by the primary key 'Delay_Type'
merge(df1, df2) %>% arrange(-Average_Delay) 


merge(df1, df2) %>% 
  arrange(-Average_Delay) %>%  
  pivot_longer(cols = c("Accumulated_Delay", "Average_Delay"), 
               names_to ="Method", values_to = "Value") %>% 
  ggplot() + 
  geom_bar(aes(x = reorder(Delay_Type, -Value), y = Value, fill = Delay_Type), 
           color = "dark grey", alpha = 0.9, stat="identity", position = "dodge") + 
  facet_wrap(~Method, scale = "free") + 
  scale_y_continuous(labels = format_format(big.mark = ",", scientific = FALSE)) +
  labs(x = "Delay Type", y = "Delay (min)", fill = "") +
  theme(legend.position="top", axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_brewer(palette = 14)


avg <- flights_df %>% 
  group_by(airline) %>% 
  drop_na() %>% 
  summarize(delay = mean(total_delay)) %>% 
  arrange(-delay)

avg



startdate <-  min(flights_df$date)
enddate <-  max(flights_df$date)

ggplot(data=avg) +
  geom_bar(aes(x = reorder(airline, -delay), y = delay, fill = airline), 
           stat = "identity", width = 0.6) +
  labs(title = "Average Delay per Airline", subtitle = paste("From", startdate, "to", enddate),
       caption = "by Markus Köfler", x = "Airlines", y = "Average Delay (min)") +
  theme(axis.text.x = element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


ag <- flights_df %>% 
  group_by(airline, month) %>% 
  drop_na() %>% 
  summarize(delay = mean(total_delay)) 


ggplot(data=ag) +
  geom_bar(aes(x = reorder(airline, -delay), y = delay, fill = airline), 
           stat = "identity", width = 0.6) +
  labs(title = "Average Delay per Airline", subtitle = paste("From", startdate, "to", enddate),
       caption = "by Markus Köfler", x = "Airlines", y = "Average Delay (min)") +
  theme(axis.text.x = element_blank()) + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  facet_wrap(~month)


nrow(filter(flights_df, airline=="Alaska Airlines Inc." & month==6))


ggplot(flights_df) +
  geom_jitter(aes(distance_miles, total_delay), alpha = 0.1, shape = "o", color = "navy") +
  geom_smooth(aes(distance_miles, total_delay), color = "red", method = "lm") + 
  facet_wrap(~airline, scale = "free", shrink = FALSE) + #adjusted x- and y-axis 
  stat_cor(aes(distance_miles, total_delay), color = "red", geom = "label", fill = "transparent") +
  labs(title = "Corellation between Flight Distance and Total Delay", subtitle = "Individual Airlines",
       caption = "by Markus Köfler", x = "Distance (miles)", y = "Delay (min)") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) 


ggplot(flights_df) +
  geom_jitter(aes(distance_miles, total_delay), shape = "+", alpha = 0.9) +
  geom_smooth(aes(distance_miles, total_delay), color = "red") + 
  labs(title = "Overall Correlation between Flight Distance and Total Delay", 
       subtitle = paste("Correlation:",
                        toString(cor(flights_df$distance_miles, flights_df$total_delay)), 
                        sep = " "))


dep_airport_df <- dplyr::rename(as.data.frame(table(flights_df$dep_airport)) %>% 
                                  arrange(-Freq), dep_airport = Var1, departures = Freq)

dest_airport_df <- dplyr::rename(as.data.frame(table(flights_df$dest_airport)) %>% 
                                   arrange(-Freq), dest_airport = Var1, arrivals = Freq)

dep_dest_airports <- cbind(dep_airport_df, dest_airport_df)

head(dep_dest_airports, n = 10)


len_of_df <- length(dep_dest_airports$dep_airport)

# assigning integers from 1 to 260
rank <- c(1:len_of_df) 

# adding ranking to each individual data frame
dep_rank_df <- mutate(dplyr::rename(dep_airport_df, airport = dep_airport), rank_dep = rank)
dest_rank_df <- mutate(dplyr::rename(dest_airport_df, airport = dest_airport), rank_dest = rank)


# joining the data frames based on a common key which is the column "airport" 
dep_dest_rank <- arrange(plyr::join(dep_rank_df, 
                                    dest_rank_df, type = "full", 
                                    by = "airport"), 
                         + rank_dep)

top_n(dep_dest_rank, -10)



# computing the correlation
# function which iterates through a vector containing
# the 3 correlation methods used in data science
cor_methods <- c("pearson", "kendall", "spearman")

for (cor_method in cor_methods) {
     print(paste(cor_method, sep = ": ", 
                cor(dep_dest_rank$rank_dep, dep_dest_rank$rank_dest, method = cor_method)
                )
           )
}


cor_calculator <- function (method_vector = c("pearson", "kendall", "spearman")
                            , var1, var2) { 
  result <- c()
  for (cor_method in method_vector) {
    result <- append(result, paste(cor_method, sep = ": ", 
                cor(dep_dest_rank$rank_dep, dep_dest_rank$rank_dest, method = cor_method)))
    }
  return(result)
}


variable_1 <- dep_dest_rank$rank_dep
variable_2 <- dep_dest_rank$rank_dest

cor_calculator(var1 = variable_1, var2 = variable_2)


flights_df["dep_dest_airports"] <- paste("FROM:", flights_df$dep_airport, 
                                         "TO:", flights_df$dest_airport, 
                                         sep = " ")

flights_df$dep_dest_airports[1:5]

routes_df <- as.data.frame(table(flights_df["dep_dest_airports"])) %>% arrange(-Freq)

# display the top 10 mosth frequent travel routes 
top_n(routes_df, 10)

#filtering for columns that are numeric only

flights_numeric <- select_if(flights_df, is.numeric)

# Computing correlation matrix
cor_matrix <- round(cor(flights_numeric),3)

# Visualizing and reordering correlation matrix
ggcorrplot(cor_matrix, hc.order =FALSE, tl.cex = 8,
           outline.color ="#808080", method = "square", colors = c("#FF007F", "white", "#0000FF")) +
  labs(title= "Correlation Matrix") +
  theme(plot.title = element_text(size = 22, hjust = 1)) 




corrp.mat <- cor_pmat(flights_numeric)
corrp.mat

flights_df <- mutate(flights_df, 
                     route = paste(flights_df$dep_airport_code, 
                                 flights_df$dest_airport_code, 
                                 sep = "-"))

flights_df$route[1:5]


#### SQL query 


sqldf("
       SELECT 
          route,
          airline,
          avg(actual_flight_time_min) AS average_travel_time,
          avg(total_delay) AS average_delay
       
       FROM
          flights_df
      
      WHERE 
        route = 'ORD-LAX' OR route = 'MDW-LAX' 
      
      GROUP BY 
          airline
      
      ORDER BY
          average_delay ASC
      ")
     


df <- sqldf("
       SELECT 
          airline,
          route,
          avg(actual_flight_time_min) AS average_travel_time,
          avg(total_delay) AS average_delay
       
       FROM
          flights_df
      
      WHERE 
          route = 'DFW-JFK' OR 
          route = 'DFW-LGA' OR 
          route = 'DFW-EWR' OR 
          route = 'DAL-JFK' OR
          route = 'DAL-LGA' OR
          route = 'DAL-EWR' 
      
      GROUP BY 
          route 
      
      ORDER BY 
          average_travel_time ASC
      ") 

sum((flights_df$dep_airport_code == "DAL" & flights_df$dest_airport_code == "JFK") |
    (flights_df$dep_airport_code == "DAL" & flights_df$dest_airport_code == "LGA") |
    (flights_df$dep_airport_code == "DAL" & flights_df$dest_airport_code == "EWR"))

