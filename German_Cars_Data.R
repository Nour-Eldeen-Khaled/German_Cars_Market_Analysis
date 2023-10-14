# the needed libraries till now
install.packages("plotrix")
library(plotrix)
library(tidyverse)
library(readr)

# reading my CSV file and printing it 

my_data <- read.csv("D:/coursera google data analysis/professional/Course 8- Capstone/Data sets downloaded/germany cars data set/autoscout24-germany-dataset.csv")
my_data

# exploring the data 
dim(my_data)

View(my_data)

names(my_data)

glimpse(my_data)

unique(my_data$make)
unique(my_data$fuel)  
unique(my_data$gear)  
unique(my_data$offerType)  
unique(my_data$year)

# Checking for the nulls 
my_data %>% 
  select(everything()) %>% 
  filter(!complete.cases(.)) %>%    # ma3na keda eno 7aygeblak kol el NA fy el data ely enta e5tartaha
  view()                        

# checking for duplicates 
View(my_data[duplicated(my_data),])

# choosing the distinct values 
data_distinct <- distinct(my_data)
View(data_distinct)

# deleting the Na in the data  in the hp column because it doesn't matter 
sum(is.na(data_distinct))

colSums(is.na(data_distinct))

My_cleaned_data <- na.omit(data_distinct)
View(My_cleaned_data)




# now I think i've finished exploring and cleaning 
# now it's time for insights and some answers for the questions 
#Questions

#What are the most popular German car brands and models in the dataset and what are the least popular?
counts <- My_cleaned_data %>%
  group_by(make, model) %>%
  summarize(count = n())

# most common cars in germany 
counts2 <- counts %>% 
  arrange(desc(counts$count)) %>% 
  head(10)
# Plot for the most common cars
ggplot(data = counts2)+
  geom_col(mapping = aes(x= model, y= count, fill = model))+
  labs(x="model",y="count",xlab="model",ylab="count" )+
  theme_classic()+
  geom_text(aes(x = model, y = count - 50, label = count), size = 3) +
  ggtitle("The Most Common Cars In Germany ")


# least common cars in germany 
counts3 <- counts %>% 
  arrange(desc(counts$count)) %>% 
  tail(10) %>% 
  View()



#  What is the average price of German cars by year, make type?

# the average price for a german car over years

CarPriceByYear <- My_cleaned_data %>% 
  group_by(year) %>%
  summarize(average_price = round(mean(price)))  

ggplot(data = CarPriceByYear)+
  geom_point(mapping = aes(x= year, y= average_price), size = 4, alpha =0.9) +
  geom_line(mapping = aes(x= year, y= average_price) ) +
  geom_text(mapping = aes(x=year,y=average_price + 1000,label=average_price))+
  labs(y = "Average Price", x = "Year", ylab = "Average Price") +
  # Change the theme to make the plot more readable
  theme_bw() +
  ggtitle("Average price of cars over the years")




# the average price for a german car by company the most expensive 15 and the cheapest 15
# the most Expensive
CarsPriceByMake1 <- My_cleaned_data %>% 
  group_by(make) %>% 
  summarize(average_price2 = round(mean(price))) %>% 
  arrange(desc(average_price2)) %>% 
  head(15)

ggplot(data = CarsPriceByMake1)+
  geom_col(mapping = aes(x=make, y=average_price2, fill = make))+
  geom_text(aes(x=make,y=average_price2 - 30000, label = average_price2))+
  labs(title = "Average Price by model")+
  coord_flip()

# the cheapest
CarsPriceByMake2 <- My_cleaned_data %>% 
  group_by(make) %>% 
  summarize(average_price2 = round(mean(price))) %>% 
  arrange(average_price2) %>% 
  head(15)

ggplot(data = CarsPriceByMake2)+
  geom_col(mapping = aes(x=make, y=average_price2, fill = make))+
  geom_text(aes(x=make,y=average_price2 - 500, label = average_price2))+
  coord_flip()

  


#  What is the average horsepower of German cars by Brand the 10 most and the least 9 ? 
AverageHpByMake1 <- My_cleaned_data %>% 
  group_by(make) %>% 
  summarize(average_hp = round(mean(hp))) %>% 
  arrange(desc(average_hp)) %>% 
  head(10) 



AverageHpByMake2 <- My_cleaned_data %>% 
  group_by(make) %>% 
  summarize(average_hp = round(mean(hp))) %>% 
  arrange(average_hp) %>% 
  head(9) 



# how does the Horse Power affect the price ?
Hp_and_Price <- My_cleaned_data %>% 
  group_by(hp) %>% 
  summarize(hp_price = round(mean(price))) %>% 
  arrange(hp_price)

ggplot(data = Hp_and_Price)+
  geom_line(mapping = aes(x=hp,y=hp_price))
# as we have seen that the Horse Power does slightly affect on the price but it really depend on the Brand 





# Here i  chose not to delete the blank values in the gear and instead i just filtered the column only 
# what is the most common type of gear in cars in germany and what is its average price ?? 
avg_gear_price <- My_cleaned_data %>%
  group_by(gear) %>% 
  filter(gear == "Manual" | gear == "Automatic" | gear == "Semi-automatic" ) %>% 
  summarize(avg_gear_pr = round(mean(price)), count = n()) %>% 
  View()

# as we viewed in the last command that the semi-automatic count is very small --> (54)
# so we won't visualize it or put it in the filter 
gear_only <- My_cleaned_data %>%
  group_by(gear) %>% 
  filter(gear == "Manual" | gear == "Automatic") %>% 
  summarize(count = n()) %>% 
  arrange(count)
# the semi-automatic is very small number nearly neglected so we didn't put it in the pie chart 
percentage = round((gear_only$count / sum(gear_only$count)) * 100)
the_label = paste(gear_only$gear,"-",percentage,"%")
pie3D(gear_only$count,labels = the_label ,
      main="the most common gear type in the german market",
      explode = 0.0)


##what are the question that are in my head right now 
## fuel 
## mileage
## offerType
unique(My_cleaned_data$fuel)
unique(My_cleaned_data$offerType)

fuel_Type <- My_cleaned_data %>% 
  group_by(fuel) %>%
  summarize(count = n() ) %>% 
  arrange(desc(count)) %>% 
  View()

# as we saw in the previuos View that the most common 3 fuel type in germany are :
# Gasoline   27,372
# Diesel     14,749
# Electric/Gasoline  1,051

# the least common fuel types are :
# Ethanol   2
# Hydrogen  1   


# what is the most common offer type in germany 
offerType_data <- My_cleaned_data %>% 
  group_by(offerType) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  View()
# so as we saw that the most common offer type is : Used 
# because most of this data set is for Used cars 

# what is the relationship between mileage and other columns :
# at first we saw that most of the cars in the dataset is USED 
# now we are gonna filter the offerType by USED and see the average mileage 
# and also we should see if the mileage affect the price or not 

mileage_data <- My_cleaned_data %>% 
  group_by(mileage) %>% 
  filter(offerType == "Used") %>% 
  summarize(av_pr = round(mean(price))) %>%
  filter(mileage %in% seq(10000, nrow(My_cleaned_data) * 10000, by = 10000))


ggplot(data = mileage_data)+
  geom_smooth(mapping =  aes(x=mileage,y=av_pr), color = "green")+
  theme_minimal()
