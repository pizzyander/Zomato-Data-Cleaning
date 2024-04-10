# Zomato-Dataset
This project is focused on cleaning and analyzing datasets of restaurants that are registered on Zomato in Bengaluru City, India.  

## key tasks

### Data Cleaning and EDA:
- Deleting redundant columns.
- Renaming the columns.
- Dropping duplicates.
- Remove the null observations from the dataset.
- check for data distributiong to identify skews and outliers.
- make plots to identify relationships between fields.


## install and load all relevant packages
```{R}
install.packages("tidyverse")
library(tidyverse)
install.packages("stringr")
library(stringr)
```
first of all, i will import the dataset into Rstudio before we proceed.
```{R}
zomato <- read.csv("C:/Users/PIZZY/Downloads/zomato.csv")
```
Let us take a glimpse at my data and see what it looks like. 

![Capture](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/9b528e08-b7cd-4ae5-979f-277ca2492f32)

## Data Cleaning  
I will rename some columns to snake case. This is done as a standard practice.
```{R}
#rename columns
zomato$price_for_2 <- (zomato$approx_cost.for.two.people.)
zomato$listed_in_type <- (zomato$listed_in.type.)
zomato$listed_in_city <- (zomato$listed_in.city.)
zomato$rating <- (zomato$rate)
glimpse(zomato)
```

At this point, I have to delete redundant/unwanted columns to enable me have more view of my data at one glance and save me the stress of working on unwanted data.  
i also stored the resultant data in a variable named zomata. and that's what we'll be working with as we proceed.
```{R}
zomata <- zomato %>%
  select(-listed_in.city.,-listed_in.type.,-approx_cost.for.two.people.,-reviews_list,-menu_item, -rate)
glimpse(zomata)
```
![Capture](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/394a0536-87b4-4dbf-a906-883c251a38ca)

My next task will be to eliminate all the null observations from the dataset. 
note: we are using the url column as our unique values in this task. we are sure that no 2 url can be thesame.  
if we are wrong then this command will eliminate the duplicate values

```{R}
zomata_duplicate_count <- zomata$url[duplicated(zomata$url)]
zomata_duplicate_count
```
there are no null characters.
character(0)

changed the values in rate to ensure consistency and the column name to rating
```{R}
zomata$rating <- sub("/5","", zomata$rating)
glimpse(zomata)
```
![Capture](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/98d618b8-1ba7-4c4a-89eb-f84b397c9eeb)

now i will drop all observations with null values from the dataframe.
notice the change in the number of observations after dropping the null observations.
```{R}
zomata <- na.omit(zomata)
glimpse(zomata)
```

The "Yes" and "No" values in the online_order and book_table fields are supposed to be Logical instead of Character.
Converting the fields directly to logical fields will result in null values. 
So I have to change the "Yes" and "No" to "TRUE" and "FALSE". this would enable a valid conversion to logical field.
```{R}
zomata$online_order <- str_replace(zomata$online_order, "Yes", "TRUE")
zomata$online_order <- str_replace(zomata$online_order, "No", "FALSE")
zomata$book_table <- str_replace(zomata$book_table, "No", "FALSE")
zomata$book_table <- str_replace(zomata$book_table, "Yes", "TRUE")
glimpse(zomata)
```

change column type for book_table, online_order, rate
```{R}
#now I can convert from character to Logical values 
zomata$book_table <- as.logical(zomata$book_table)
zomata$online_order <- as.logical(zomata$online_order)
zomata$rating <- as.numeric(zomata$rating)
zomata$price_for_2 <- as.integer(zomata$price_for_2)
glimpse(zomata)
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
