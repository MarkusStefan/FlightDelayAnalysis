Flight Delay Analysis
================
by Markus
2022-09-25

## Introduction

This project is intended to demonstrate the skills acquired from the
Google Data Analytics Certificate Course hosted on Coursera. The dataset
was retrived from
[Kaggle](%22https://www.kaggle.com/datasets/undersc0re/flight-delay-and-causes/metadata?select=Flight_delay.csv%22).
Originally, the dataset comes form the [U.S. Department of
Transportation’s (DOT) Bureau of Transportation Statistics
(BTS)](https://www.bts.gov/).

The attempt to analyze the data set in a Spreadsheet (Excel) failed due
to its high volume. I personally decided to use R over SQL because R is
more functional and also allows me to visualize the data.

## General Analysis

### Data Preparation

#### **1** Loading the required packages for the analysis

If the packages are not installed yet, use the install.packages()
function first!

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(janitor)
```

    ## 
    ## Attache Paket: 'janitor'
    ## 
    ## Die folgenden Objekte sind maskiert von 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
```

    ## 
    ## Attache Paket: 'lubridate'
    ## 
    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     date, intersect, setdiff, union

#### **2** Opening the dataset

``` r
local_path <- ".../Flight_delay.csv"
local_path <- "/Users/markuskofler/Google Data Analytics/Portfolio Project/Flight_delay.csv"
local_path_windows_os <- "~/A R_scripts/project/Flight_delay.csv"
flights_df <- read_csv(local_path_windows_os)
```

    ## Rows: 484551 Columns: 29
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): Date, UniqueCarrier, Airline, TailNum, Origin, Org_Airport, Dest, ...
    ## dbl (20): DayOfWeek, DepTime, ArrTime, CRSArrTime, FlightNum, ActualElapsedT...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## # A tibble: 6 × 29
    ##   DayOfW…¹ Date  DepTime ArrTime CRSAr…² Uniqu…³ Airline Fligh…⁴ TailNum Actua…⁵
    ##      <dbl> <chr>   <dbl>   <dbl>   <dbl> <chr>   <chr>     <dbl> <chr>     <dbl>
    ## 1        4 03-0…    1829    1959    1925 WN      Southw…    3920 N464WN       90
    ## 2        4 03-0…    1937    2037    1940 WN      Southw…     509 N763SW      240
    ## 3        4 03-0…    1644    1845    1725 WN      Southw…    1333 N334SW      121
    ## 4        4 03-0…    1452    1640    1625 WN      Southw…     675 N286WN      228
    ## 5        4 03-0…    1323    1526    1510 WN      Southw…       4 N674AA      123
    ## 6        4 03-0…    1416    1512    1435 WN      Southw…      54 N643SW       56
    ## # … with 19 more variables: CRSElapsedTime <dbl>, AirTime <dbl>,
    ## #   ArrDelay <dbl>, DepDelay <dbl>, Origin <chr>, Org_Airport <chr>,
    ## #   Dest <chr>, Dest_Airport <chr>, Distance <dbl>, TaxiIn <dbl>,
    ## #   TaxiOut <dbl>, Cancelled <dbl>, CancellationCode <chr>, Diverted <dbl>,
    ## #   CarrierDelay <dbl>, WeatherDelay <dbl>, NASDelay <dbl>,
    ## #   SecurityDelay <dbl>, LateAircraftDelay <dbl>, and abbreviated variable
    ## #   names ¹​DayOfWeek, ²​CRSArrTime, ³​UniqueCarrier, ⁴​FlightNum, …

\####**3** For the sake of visual appeal, I renamed the columnnames and
converted them all to lowercase

``` r
names(flights_df) <- tolower(names(flights_df %>% 
                                     rename(weekday = DayOfWeek,
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
```

    ##  [1] "weekday"                  "date"                    
    ##  [3] "dep_time"                 "arr_time"                
    ##  [5] "scheduled_arr_time"       "uniq_carrier_code"       
    ##  [7] "airline"                  "flight_num"              
    ##  [9] "tail_num"                 "actual_flight_time_min"  
    ## [11] "estimate_flight_time_min" "air_time_min"            
    ## [13] "arr_delay"                "dep_delay"               
    ## [15] "dep_airport_code"         "dep_airport"             
    ## [17] "dest_airport_code"        "dest_airport"            
    ## [19] "distance_miles"           "landing_to_gate_min"     
    ## [21] "gate_to_takeoff_min"      "cancelled"               
    ## [23] "cancellation_cause_code"  "diverted"                
    ## [25] "carrier_delay"            "weather_delay"           
    ## [27] "nas_delay"                "security_delay"          
    ## [29] "late_aircraft_delay"

#### **4** Next, we remove columns that don’t give us any information (due to a lack of data)

``` r
vector <- c()
for (i in names(flights_df)) {
  if (is_double(flights_df[[i]][2]) == TRUE) {
    if (sum(flights_df[i]) == 0 ) {
      vector <- append(vector, i)
    }
  }
}
```

    ## the vector contains columns:
    ## -cancelled
    ## -diverted

#### **5** Removing the two unnecessary columns (2 methods)

``` r
#method 1: selecting all except from elements of vector
flights_df <- select(flights_df, -all_of(vector))
```

``` r
#method 2: dropping useless columns
flights_df <- flights_df[!(names(flights_df) %in% vector)]
```

    ##  [1] "weekday"                  "date"                    
    ##  [3] "dep_time"                 "arr_time"                
    ##  [5] "scheduled_arr_time"       "uniq_carrier_code"       
    ##  [7] "airline"                  "flight_num"              
    ##  [9] "tail_num"                 "actual_flight_time_min"  
    ## [11] "estimate_flight_time_min" "air_time_min"            
    ## [13] "arr_delay"                "dep_delay"               
    ## [15] "dep_airport_code"         "dep_airport"             
    ## [17] "dest_airport_code"        "dest_airport"            
    ## [19] "distance_miles"           "landing_to_gate_min"     
    ## [21] "gate_to_takeoff_min"      "cancellation_cause_code" 
    ## [23] "carrier_delay"            "weather_delay"           
    ## [25] "nas_delay"                "security_delay"          
    ## [27] "late_aircraft_delay"

#### **6** Creating a new column that contains values of the total delay for each specific flight

``` r
flights_df <- mutate(flights_df,
                     total_delay = (carrier_delay + weather_delay + nas_delay + 
                                    security_delay + late_aircraft_delay))
```

#### **7** Creating a new column that contains the month each individual flight took place

``` r
library(lubridate)

flights_df <- flights_df %>% mutate(month = month(dmy(date)))
```

#### **8** Determining, in which delay category each flight falls

classification according to the Federal Aviation Administration (FAA)
that considers an actual arrival less than 15 min after the scheduled
arrival as not delayed, an arrival between 15 and 45 min after the
scheduled arrival as “medium delay” and beyond 45 min as “large delay”.
Source:
[Wikipedia](https://en.wikipedia.org/wiki/Flight_cancellation_and_delay#:~:text=A%20flight%20delay%20is%20when,all%20for%20a%20certain%20reason)

``` r
vec <- c()
for (t in flights_df$total_delay) {
  if (t <= 15) {
    vec <- append(vec, "No delay")
  }
  if (t >= 45) {
    vec <- append(vec, "Large delay")
  }
  else {
    vec <- append(vec, "Medium delay")
  }
}
```

    ##  [1] "Medium delay" "Large delay"  "Large delay"  "No delay"     "Medium delay"
    ##  [6] "Medium delay" "Medium delay" "Medium delay" "Large delay"  "Large delay" 
    ## [11] "Large delay"  "Medium delay" "Medium delay" "Large delay"  "Medium delay"
    ## [16] "Large delay"  "Medium delay" "Medium delay" "Medium delay" "Medium delay"
    ## [21] "Medium delay"

#### **9** creating a new column from the vector containing the categorization of each flight

``` r
flights_df["delay_degree"] <- c(1:484551)
```

    ##  [1] "weekday"                  "date"                    
    ##  [3] "dep_time"                 "arr_time"                
    ##  [5] "scheduled_arr_time"       "uniq_carrier_code"       
    ##  [7] "airline"                  "flight_num"              
    ##  [9] "tail_num"                 "actual_flight_time_min"  
    ## [11] "estimate_flight_time_min" "air_time_min"            
    ## [13] "arr_delay"                "dep_delay"               
    ## [15] "dep_airport_code"         "dep_airport"             
    ## [17] "dest_airport_code"        "dest_airport"            
    ## [19] "distance_miles"           "landing_to_gate_min"     
    ## [21] "gate_to_takeoff_min"      "cancellation_cause_code" 
    ## [23] "carrier_delay"            "weather_delay"           
    ## [25] "nas_delay"                "security_delay"          
    ## [27] "late_aircraft_delay"      "total_delay"             
    ## [29] "month"                    "delay_degree"

#### **10** this step is mainly for the sake of(this case does not apply to the US since it is an EU law):

creating a new column which states whether the passenger are potentially
subject to compensation according to EU261 law. Passengers are eligible
to claim up to 600€ as soon as the flight is delayed for 3 hours, and
receive a full refund, if delayed for 5 hours or longer.

``` r
vect <- c()
for (c in flights_df$total_delay){
  if (c < 180){
    vect <- append(vect, "no compensation")
  }
  if (c >= 300){
    vect <- append(vect, "full refund")
  }
  else {
    vect <- append(vect, "up to 600€")
  }
}
```

## Data Exploration

## The Business Task

1)  A business consultancy company is sending their consultants to their
    customers within the US area (domestic flights). The head of the HR
    team wants to know what are the most reliable airlines, what
    airports cause the most frequent and longest delays and cause the
    most frequent and logest delays in order to cut costs and minimize
    travel times for employees.

2)  While working on the analysis, the CEO wants to find out \[clients
    should be served on time \]

3)  The consultancy company is located in Chicago (IL), Austin (TX),
    Denver (CL) , Jersey City (NJ) and Seattle (WA)

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](GitHub-R_markdown_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
