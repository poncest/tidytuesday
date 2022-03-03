
## Challenge: #TidyTuesday 2022 week 09
## Author: Steven Ponce
## Date: 2022-03-01


## 1. Load packages and data ----
library(pacman)
p_load(tidyverse, tidytuesdayR, skimr, showtext, ggtext, janitor, ggrepel)

tt <- tidytuesdayR::tt_load('2022-03-01')
stations <- tt$stations %>% 
    clean_names()


# OFFLINE - In case of VPN issues
# stations_csv <- write_csv(stations, '2022/Week_09/stations_csv.csv')
# stations <- read_csv('2022/Week_09/stations_csv.csv')

## 2. Examine the data ----

glimpse(stations_df)
skim(stations_df)

names(stations)
names(stations_df)

distinct(stations_df, country)
unique(stations_df$fuel_type_code)
unique(stations_df$access_days_time)


## 3. Tidydata ----

# States in the NE region - stations dataset
ne_region_abb <- c('ME', 'NH', 'VT', 'MA', 'RI', 'CT', 'NJ', 'NY', 'PA', 'MD', 'DE') 

stations_df <- stations %>% 
    
    # select columns of interest
    select(country, state, city, fuel_type_code, status_code, 
           latitude, longitude, access_days_time, access_code) %>% 

    # clean 24 hours string
    mutate(access_days_time = access_days_time  %>% 
               str_extract(pattern = "^[24 hours]+") %>%
               str_trim())  %>% 
    
    # keep electric stations & 24 hr available
    filter(fuel_type_code == 'ELEC', access_days_time == '24 hours') %>% 
    
    # filter NE region
    filter(state %in% ne_region_abb) %>% 
    
    # remove outliers (longitude = 0)
    filter(longitude != 0)


## Northeast (NE) basic plot

# States in the NE region
ne_region <- c('maine', 'new hampshire', 'vermont', 'massachusetts', 'rhode island', 'connecticut', 'new jersey', 'new york', 'pennsylvania', 'maryland', 'delaware') 

# Load state information
states <- map_data("state")

# NE region 
ne_states <- states %>% 
    filter(region %in% ne_region)

# top 10 cities with EV stations that are available 24 hrs
station_count_top_10_tbl <- stations_df %>% 
    select(country, state, city) %>% 
    group_by(city) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count)) %>%
    top_n(10) %>% 
    ungroup()

# top 10 cities 
city_names <- c("Boston", "New York", "Baltimore", "Cambridge", "Pittsburgh", 
                "Albany", 'Buffalo', 'Rochester', 'Worcester', 'Brooklyn')

cities_tbl <- stations_df %>% 
    filter(city %in% city_names) %>% 
    select(state, city, latitude, longitude) %>% 
    group_by(city) %>% 
    # getting the lat and long of cities for labels
    mutate(
        lat = first(latitude),
        long = first(longitude)
    ) %>% 
    arrange(city)

# labels for map
labels_tbl <- cities_tbl %>% 
    select(city, lat, long) %>% 
    distinct(city, lat, long)


## 4. US Northeast map ----

## add google fonts
font_add_google(family='PT Serif', 'PT Serif')
font_add_google(family='Lato', 'Lato')
showtext_auto(enable = TRUE) 

ggplot(ne_states) +
    geom_polygon(aes(x = long, y = lat, group = group), 
                 color = "grey", fill = "grey", alpha = .15) +
    
    coord_map() +
    
    # adding the EV stations that service 24 hrs 
    geom_point(data = stations_df, 
               mapping = aes(x = longitude, y = latitude), 
               size = .3, shape = 20, color = '#f60456', alpha = .85
               )  +                                    
    
    # adding city labels
    geom_text_repel(data = labels_tbl,
          mapping = aes(x = long, y = lat, label = city),
          size = 5, color = 'snow') +
  
    # labels
    labs(
        title    = "EV Charging Stations across the US Northeast",
        subtitle = "Top 10 citites with <span style='font-size:25pt; color:#f60456'>**24 hours**</span> service available",
        caption =paste0("#TidyTuesday Wk 9 • Data: U.S. Department of Transportation • Visualization: Steven Ponce (@sponce1)")) +
    
    # theme
    theme_minimal(base_family = "PT Serif") +
    theme(
        plot.title.position = "plot",
        legend.key = element_blank(),
       
        plot.background = element_rect(fill = "#262a33", color = "#262a33"),
        panel.background = element_rect(fill = "#262a33", color = "#262a33"),
        
        panel.grid = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text  = element_blank(),
        
        plot.margin = margin(t = 10, r = 5, b = 10, l = 5),
        
        plot.title = element_text(
            color = "#fafcfb",
            face = "bold",
            size = 25,  
            margin = margin(t = 10)),
        
        plot.subtitle = element_markdown(
            color = "#fafcfb",
            size = 20,  
            margin = margin(t = 5, b = 5)),
        
        plot.caption = element_text(
            color = "#8a8b91",
            family = 'Lato',
            size = 12,
            hjust = .5,
            margin = margin(t = 10, b = 5)) 
    ) 


## 5. Save final figure ----
ggsave("2022/Week_09/2022_09_alternative_fuel_stations.png", plot = last_plot(),
       height = 8, width = 6,  dpi = 320)

