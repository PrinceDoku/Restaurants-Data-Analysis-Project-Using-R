# installation of packages
install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("maps")
install.packages("mapdata")
install.packages("corrplot")


# Importing of libraries into R
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(mapdata)
library(corrplot)

# Data exploration and pre-processing

## loading of data set

data <-read.csv("C:/Users/Elwin/Downloads/Dataset.csv")
data

## View 10 rows of the data set
head(data,10)

## Explore the data set to identify the number of rows and columns
cat("The number of rows :",nrow(data) ,"\n")
cat("The number of columns :",ncol(data))


## Identify missing values in each column and handle them

### Check for missing values
missing_values <- sum(is.na(data))
missing_values

### Check for empty values
empty_values <- sum(data=='')
empty_values

cat("Missing values count:",missing_values,'\n')
cat("Empty values count:",empty_values)

### Check for empty values columns 
Empty_col_values_count <- colSums(data=='')
Empty_col_values_count

cat("Empty Values Count:\n")
print(Empty_col_values_count)


# Removal of empty values columns by dropping it(Cuisines)
data <- data[!(data$Cuisines==""),,drop=FALSE]


## Checking of duplicates 
dup <- sum(duplicated(data))
cat("The Number of duplicate rows:" ,dup)

# Display basic information about the data set to check data types 
str(data)


# Analyse the distribution of the target variable ("Aggregate rating") and identify any class imbalances
Check_for_balance <- summary(data$Aggregate.rating)
print(Check_for_balance)

## Check if distribution is balance
target_variable <- table(data$Aggregate.rating)
is_balance <- all(target_variable >= mean(target_variable))
if (is_balance){
  print("The distribution of the target variable is balanced.")
} else{
  print("The distribution of the target variable is imbalanced.")
}


# Descriptive Analysis
## Calculate basic statistical measures (mean, median, standard deviation, etc.) for numerical columns
### select nummeric columns from data set
numeric_column <- data[,sapply(data,is.numeric)]
numeric_column

### Calculate basic statistical measures
summary_stats <- summary(numeric_column)
print(summary_stats)

### Calculate the standard deviation for numeric_columns
sds <- sapply(data[,sapply(data,is.numeric)],sd)
sds

# Explore the distribution of categorical variables like "Country Code," "City," and "Cuisines."

## Count Plot for the Country Code
ggplot(data=data,aes(x=factor(Country.Code))) +
  geom_bar(fill = 'orange')+
  labs(
    title = "Distribution of Restaurant by Country Code",
    x= "Country Codes",
    y='Number Of Restauarants'
  )

## Create a subset of the data containing only top 10 cities
Top_10 <- head(names(sort(table(data$City),decreasing = TRUE)),10)
Top_10
data_top_10 <- data[data$City %in% Top_10,]
data_top_10

# Create a Count Plot for Top_10 Cities
ggplot(data=data_top_10,aes(y=factor(City,levels=rev(Top_10))))+
  geom_bar(fill="blue",width =0.5,stat='count')+
  labs(
    title ="Top 10 cities with highest number of restaurants",
    x = "Number of Restaurants",
    y = 'Name of cities'
  )


## Top 10 Cuisines and cities with the highest number of restaurants

### Identify top 10 cuisines and their count
top_cuisines <- head(sort(table(data$Cuisines),decreasing = TRUE),10 )
 
### Create a data frame with the top 10 cuisines names and and count
top_cuisines_df<- data.frame(Cuisines = names(top_cuisines), Count=as.numeric(top_cuisines))
print("Top 10 cuisines with the highest number of restaurants:")
print(top_cuisines_df)



### Identify top 10 cities and their count
top_cities<- head(sort(table(data$City),decreasing = TRUE),10 )

### Create a data frame with the top 10 cities names and and count
top_cities_df<- data.frame(Cities = names(top_cities), Count=as.numeric(top_cities))
print("Top 10 cities with the highest number of restaurants:")
print(top_cities_df)


# Geo-spatial Analysis

## Visualize the locations of restaurants on a map using latitude and longitude information.
###  Create a map of the world
world_map <- map_data('world')
world_map


## Plot of restaurants on map
ggplot()+
  geom_polygon(data=world_map,aes(x=long,y=lat,group=group),fill='lightgreen',color='skyblue')+
  geom_point(data=data,aes(x=Longitude,y=Latitude,color="Restaurants"),size=2)+
  scale_color_manual(name='legend',values=c(Restaurants='yellow'))+
  labs(
    title='Restaurants Locations on map',
    x='Longitude',
    y='Latitude'
  )

# Analyse the distribution of restaurants across different cities or countries.

## Create a subset of the data containing only top 10 cities
Top_10 <- head(names(sort(table(data$City),decreasing = TRUE)),10)
Top_10
data_top_10 <- data[data$City %in% Top_10,]
data_top_10

# Create a Plot of restaurants across cities
ggplot(data=data_top_10,aes(y=factor(City,levels=rev(Top_10))))+
  geom_bar(fill="blue",width =0.5,stat='count')+
  labs(
    title ="Distribution of Restaurants across cities",
    x = "Number of Restaurants",
    y = 'Name of cities'
  )


# Determine if there is any correlation between the restaurant's location and its rating.

## Calculate the correlation
cor_matrix <- cor(data[c('Longitude','Latitude','Aggregate.rating')])
cor_matrix


## Create a Heat map to visualize the correlation
library(corrplot)
corrplot(cor_matrix,method='color',col=colorRampPalette(c('blue','green','orange'))(20),
        type='upper',order='hclust',tl.col='black',tl.srt=45,
        title ="Correlation Between Restaurant's Loaction and Rating",
        mar=c(0,0,3,1)
        )
