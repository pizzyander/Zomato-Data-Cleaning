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

now i will drop all observations with null values from the dataframe.
notice the change in the number of observations after dropping the null observations.
```{R}
zomata <- na.omit(zomata)
glimpse(zomata)
```

The "Yes" and "No" values in the online_order and book_table fields are supposed to be Logical instead of Character format.
Converting the fields directly to logical fields will result in null values. 
So I have to change the "Yes" and "No" to "TRUE" and "FALSE". this would enable a valid conversion to logical field.
```{R}
zomata$online_order <- str_replace(zomata$online_order, "Yes", "TRUE")
zomata$online_order <- str_replace(zomata$online_order, "No", "FALSE")
zomata$book_table <- str_replace(zomata$book_table, "No", "FALSE")
zomata$book_table <- str_replace(zomata$book_table, "Yes", "TRUE")
glimpse(zomata)
```

At this point, I can convert some colunms to their appropriate formats.
```{R}
#now I can convert from character to Logical values 
zomata$book_table <- as.logical(zomata$book_table)
zomata$online_order <- as.logical(zomata$online_order)
zomata$rating <- as.numeric(zomata$rating)
zomata$price_for_2 <- as.integer(zomata$price_for_2)
glimpse(zomata)
```
## Data analysis and visualizations
Let us check the distribution of the numeric data in the dataframe.   
This would enable us to identify skews and outliers.
```{R}
#Now i will plot a histogram and boxplot to check their distribution of the numeric fields in the dataframe.
hist(zomata$rating)
hist(zomata$price_for_2)
boxplot(zomata$rating, main="rating_boxplot",ylab="rating")
boxplot(zomata$price_for_2, main="price_for_2_boxplot",ylab="price_for_2")
```
![Rplot](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/46abb787-ac41-42c5-b26e-ea9769054c78)

![Rplot01](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/7915ae6e-55ef-44f7-b52f-a75693169152)
The rating histogram shows an even distribution with 3.7 as the hightest number of rating.  


Let us count the number of restaurants that allow customers order items online.
```{R}
#counting the number of retsaurants that allow customers to order items online
zomata %>%
  count(online_order)

# visual count of restaurants that allow customers to order items online
ggplot(data = zomata) +
  geom_bar(mapping= aes(x=online_order), fill = "red") +
  labs(title = "restaurants that take online order")
```
According to the bar plot below, about ~31,000 restaurants allow online orders, that is more than the ones that dont allow online orders(~20,000).
![Rplot03](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/eee64992-aeec-4ab0-ae7f-cb2486bcb2ca)  

Now we can count the number of restaurants that allow booking tables.
```{R}
#counting the number of restaurants that allow booking tables upfront
zomata %>%
  count(book_table)

# visual bar plot of restaurants that allow booking
ggplot(data = zomata) +
  geom_bar(mapping= aes(x=book_table), fill = "violet") +
  labs(title = "restaurants that allow booking")
```
![Rplot05](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/4e02baa5-f820-4b44-b93e-5191f099d9f0)
According to the bar plot above, there are far more walk-in retaurants than restaurants that allow booking.  


let's check for the relationship between the numeric data in the dataframe
```{R}
#point plot of price_for_2 and votes
ggplot(data = zomata) +
  geom_point(mapping= aes(x = price_for_2, y = votes), color = "blue") +
  labs(title = "price_for 2 X votes")
```
![Rplot06](https://github.com/pizzyander/Zomato-Data-Cleaning/assets/141561016/e3d002fd-113f-4826-866f-ed6c88ab5635)

## Analysis

lets check the best location for restaurants by rating
```{R}
#best location by total rating.
location_rating <- zomata %>%
  select("location", "rating","name") 
arrange(location_rating, desc(rating))
```
it shows that the restaurants in "Sarjapur Road" are the highest rated @ 4.9.

```{R}
#listing the number of service type according to the most desired
zomato %>%
  count(listed_in_type) %>%
  arrange(desc(n))
```

```{R}
#listing the number of service type according to their rating
zomato %>%
  group_by(listed_in_type) %>%
  summarise(avg_rating = mean(rating)) %>%
  arrange(desc(avg_rating))
```

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
