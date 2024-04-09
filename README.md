# Zomato-Data-Cleaning
This project is focused on cleaning and analyzing datasets of restaurants that are registered on Zomato in Bengaluru City, India.  

## key tasks

### Data Cleaning:
- Deleting redundant columns.
- Renaming the columns.
- Dropping duplicates.
- Cleaning individual columns.
- Remove the NaN values from the dataset

### Data Visualization: 
- Restaurants delivering Online or not.
- Restaurants allowing table booking or not.
- Table booking Rate vs Rate.
- Best Location.
- Relation between Location and Rating.
- Restaurant Type.
- Gaussian Rest type and Rating.
- Types of Services.
- Relation between Type and Rating.
- Cost of Restaurant.
- No. of restaurants in a Location.
- Restaurant type.
- Most famous restaurant chains in Bengaluru.

## install and load all relevant packages
```{R}
install.packages("tidyverse")
library(tidyverse)
install.packages("stringr")
library(stringr)
```

## Data Cleaning  
Deleting redundant/unwanted columns
```{R}
zomato <- zomato %>%
  select(-listed_in.city.,-listed_in.type.,-approx_cost.for.two.people.,-reviews_list,-menu_item)
glimpse(zomato)
```

Dropping duplicates 
```{R}
zomato_duplicate_count <- zomato$url[duplicated(zomato$url)]
```

changed the values in rate to ensure consistency and the column name to rating
```{R}
zomato$rate <- sub("/5","", zomato$rate)
zomato$rating <- (zomato$rate)
```

Renaming the columns
```{R}
zomato <- filter(zomato, !is.na(url))
zomato <- filter(zomato, !is.na(listed_in_city))
zomato <- filter(zomato, !is.na(listed_in_type))
zomato <- filter(zomato, !is.na(price_for_2))
zomato <- filter(zomato, !is.na(cuisines))
zomato <- filter(zomato, !is.na(dish_liked))
zomato <- filter(zomato, !is.na(rest_type))
zomato <- filter(zomato, !is.na(location()))
zomato <- filter(zomato, !is.na(location))
zomato <- filter(zomato, !is.na(phone))
zomato <- filter(zomato, !is.na(votes))
zomato <- filter(zomato, !is.na(book_table))
zomato <- filter(zomato, !is.na(online_order))
zomato <- filter(zomato, !is.na(name))
zomato <- filter(zomato, !is.na(address))
```

changed the values of strings in online_order and book_table to enable format change.
```{R}
zomato$online_order <- str_replace(zomato$online_order, "Yes", "TRUE")
zomato$online_order <- str_replace(zomato$online_order, "No", "FALSE")
zomato$book_table <- str_replace(zomato$book_table, "No", "FALSE")
zomato$book_table <- str_replace(zomato$book_table, "Yes", "TRUE")
```

change column format for book_table, online_order, rate
```{R}
zomato$rate <- as.numeric(zomato$rate)
zomato$book_table <- as.logical(zomato$book_table)
zomato$online_order <- as.logical(zomato$online_order)
```
## Data analysis and visualizations

```{R}
#counting the number of online orders
zomato %>%
  count(online_order)

# geom_bar plot to count the number of restaurants that deliver online  
ggplot(data = zomato) +
  geom_bar(mapping= aes(x=online_order), fill = "red") +
  labs(title = "restaurants that take online order")
```
![online delivers](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/b4e51127-4083-4a79-9630-d44c95b792bc)


```{R}
# visual count of restaurants that allow booking
ggplot(data = zomato) +
  geom_bar(mapping= aes(x=book_table), fill = "violet") +
  labs(title = "restaurants that allow booking")
```
![restaurants that allow booking](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/c584a7f6-76ea-4bef-9ca8-38faddf495bb)

```{R}
#best location by to rating.
location_rating <- zomato %>%
  select("location", "rating","name") 
arrange(location_rating, desc(rating))
```

```{R}
#best restaurant location by average rating
avg_rating_table <- location_rating %>% 
  group_by(location) %>%
  summarise(avg_rating = mean(rating)) %>%
  arrange(desc(avg_rating))
avg_rating_table
```

```{R}
#unique restaurant types
unique(zomato$rest_type)
```
![Capture](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/55c7b99f-5616-4aa7-9904-8b4de1aa8cd4)

```{R}
#listing the number of service type according to the most desired
zomato %>%
  count(listed_in_type) %>%
  arrange(desc(n))
```
![Capture](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/18c055c6-f293-4fbe-b71c-11aec144e825)

```{R}
#listing the number of service type according to their rating
zomato %>%
  group_by(listed_in_type) %>%
  summarise(avg_rating = mean(rating)) %>%
  arrange(desc(avg_rating))
```
![Capture](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/650666ea-6253-4aa3-b123-d2984c747430)

```{R}
#top 10 costliest restaurants on average (price for 2 guests)
zomato %>%
  group_by(name) %>%
  summarise(avg_price = mean(price_for_2))%>%
  arrange(desc(avg_price))
```

```{R}
#number of restaurants in a location
zomato %>%
  count(location) %>%
  arrange(desc(n))
```

```{R}

```
