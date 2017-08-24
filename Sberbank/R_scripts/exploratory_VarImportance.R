library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(corrplot)
library(DT)

dtrain = read_csv("~/data/train.csv")

# a look at the missing data:

miss_pct = map_dbl(dtrain, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

miss_pct = miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# state should be discrete values between 1 and 4. 
# 33 is a data entry error. replace it with the mode.
dtrain$state[dtrain$state == 33] = which.max(table(dtrain$state))
# build_year has an larve value error of: 20052009. 
#replace with 2007
dtrain$build_year[dtrain$build_year == 20052009] = 2007

# Alook for correlation among the internal home characteristics and price.

internal_chars <- c('full_sq', 'life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 
                    'kitch_sq', 'state', 'price_doc')


corrplot(cor(dtrain[, ..internal_chars], use="complete.obs")) ##Not able to get corrplot to work:

###  LOOKING AT HOW SQ_FOOTAGE CORRESPONDES TO PRICE
#Area of Home and Number of Rooms

#the corrplot showed that full_sq is correlated with price:

ggplot(aes(x=full_sq, y=price_doc), data= dtrain) +
  geom_point(color='red')
#shows a strong outlier, remove it for now.

#removing outlier:
dtrain %>% 
  filter(full_sq < 2000) %>% 
  ggplot(aes(x = full_sq, y=price_doc)) +
  geom_point(color='red', alpha=0.5) +
  labs(x='Area', y='price', title='Price by area in sq_meters')
#new graph givesa much better representation of a strong correlation b/w sq_ft & price.

# a look at the number of rooms distribution:
table(dtrain$num_room)


ggplot(aes(x=num_room), data=dtrain) +
  geom_histogram(fill='red', bins=20, stat='count') +
  ggtitle('Distribution of number of rooms')
#There is a large number of NA's and the x-axis order is off, need to fix.
# majority of room numbers are 3 rooms or less.

### SALE TYPE: (Ownership/investment/etc.)

##what makes up product_type:
unique(dtrain$product_type)

#A difference in price for homes bought by an owner-occupier or homes for investment?

ggplot(aes(x=price_doc), data=dtrain) +
  geom_density(fill='red', color='red') +
  facet_grid(~product_type) +
  scale_x_continuous(trans='log')
#the density plots look pretty similar

dtrain %>%  group_by(product_type) %>%  summarise(median(price_doc))
#The median price shows investment properties sell for more than Ownerships

### BUILD YEAR:

# a look at build_year, and the affect it has on price:
table(dtrain$build_year)

#clean-up the data to only include years that make sense:

dtrain %>% 
  filter(build_year > 1691 & build_year < 2018) %>% 
  ggplot(aes(x=build_year)) + 
  geom_histogram(fill='red', stat='count') + 
  ggtitle('Distribution of build year')

#Are build_year and prices related? group the data by year and 
#take the mean of price_doc.

dtrain %>% 
  filter(build_year > 1691 & build_year < 2018) %>%
  group_by(build_year) %>% 
  summarize(mean_build_price=mean(price_doc)) %>%
  ggplot(aes(x=build_year, y=mean_build_price)) +
  geom_line(stat='identity', color='red') + 
  geom_smooth(color='darkgrey') +
  ggtitle('Mean price by year of build')


dtrain %>% 
  select(price_doc) %>% 
  ggplot(aes(x=price_doc)) +
  geom_histogram(fill='red')





# To get an idea of how many sales transactions are in each district:

dtrain %>% 
  group_by(sub_area) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=reorder(sub_area, n), y=n)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  labs(y='Number of transactions', x='', title='Number of Transactions by District')

#Top 20 sub_area (districts) by number of transactions by districts 
dtrain %>% 
  group_by(sub_area) %>%
  summarize(n=n()) %>%
  top_n(n, n=20) %>% 
  ggplot(aes(x=reorder(sub_area, n), y=n)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  labs(y='Number of transactions', x='', title='Number of Transactions by District')


## School Characteristics
#How much does school quality affect housing prices?

school_chars = c('children_preschool', 'preschool_quota', 'preschool_education_centers_raion',
                  'children_school', 'school_quota', 'school_education_centers_raion', 
                  'school_education_centers_top_20_raion', 'university_top_20_raion',
                  'additional_education_raion', 'additional_education_km', 'university_km',
                  'price_doc')

corrplot(cor(dtrain[, ..school_chars], use='complete.obs'))


#little correlation between price & school variables.
# Schools do appear to be highly correlated with eachother However, need to be careful of
#multicollinearity if using any of these variables.

table(dtrain$university_top_20_raion)

dtrain %>%
  ggplot(aes(x=as.factor(university_top_20_raion), y=price_doc)) + 
  geom_jitter(color='grey') + 
  geom_boxplot(fill='red', color='gray', alpha=0.5) + 
  ggtitle('Distribution of home price by # of top universities in Raion')


unique(dtrain %>% filter(university_top_20_raion==3) %>% select(sub_area))
#There are very few homes with 3 top universites in their raion. Let's see how many 
#districts there are with three universities.


###########----MODELING-----#########

## Variable Importance W/ random forest to get an initial/rough overiew:

library(caret)

#complete cases for dtrain
completes = complete.cases(dtrain)

# Set training control so that we only 1 run forest on the entire set of complete cases
trControl = trainControl(method='none')

# Run random forest on complete cases of dtrain. Exclude incineration_raion since it
# only has 1 factor level
rfmod <- train(price_doc ~ . - id - timestamp - incineration_raion,
               method='rf',
               data=dtrain[completes, ],
               trControl=trControl,
               tuneLength=1,
               importance=TRUE)

varImp(rfmod)

rfmod = train(price_doc ~ . - id - timestamp - incineration_raion,
               method='rf',
               data=dtrain[completes, ],
               trControl=trControl,
               tuneLength=1,
               importance=TRUE)

varImp(rfmod)


























