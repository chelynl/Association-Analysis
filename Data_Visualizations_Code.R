library(ggplot2)
library(data.table)
library(stringr)
library(dplyr)


#----------------------------- Wine Pairings: R code and Data Visualizations --------------------------#

#use file.choose to find the file
association_rules <- fread(file.choose())

#Pulls the food and wine names out, drops the unformatted columns 
rules <- association_rules %>% 
  mutate(Meats = word(antecedents,2,sep = "'"),
         Wines = word(consequents,2,sep = "'")) %>% 
  select(-c(antecedents, consequents)) 

#Get a list of the distinct wines and pair with Red or white
wines = distinct(rules,Wines)
WineColor <- c("Red", "Red","White","Red", "White" , "Red", 
           "White" , "Red", "Red", "White" ,"White" ,"White" ,"White"  )
wines = cbind(wines, WineColor)

#Inner join to add the colors
rules <- rules %>% 
  inner_join(wines, by = "Wines")

#Create the plot
ggPairs <- rules %>% ggplot(aes(x = confidence, y = Wines, fill = WineColor)) + 
  geom_col() + 
  facet_wrap(~Meats, nrow = 2)  + 
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c('#B11226', '#f3e5ab')) + 
  scale_x_continuous(labels = scales::percent) + 
  labs(x = "Percentage of Orders", #could change to confidence
       y = "", 
       fill = "Wine Color",
       title = "Popular Wine Pairings")
  

# Orders ####

#NOTE: the factor mutation is meant to specify the order of the levels in the charts

#choose order data csv with line numbers
orders <- fread(file.choose())

#filters the wines, groups by item, gets count and sorts
wineOrder <- orders %>% 
  filter(line_number == 2) %>% 
  group_by(item) %>% 
  tally(sort = T) %>% 
  #mutate(item = factor(item, levels = item[order(n)])) %>% 
  rename(Wines = item)

wineOrder <-  wineOrder %>% 
  inner_join( wines, by = 'Wines') %>% 
  mutate(Wines = factor(Wines,  levels = Wines[order(n)]))

#same process for mains
mainOrder <- orders %>% 
  filter(line_number == 1) %>% 
  group_by(item) %>% 
  tally(sort = T) %>% 
  mutate(item = factor(item, levels = item[order(n)])) %>% 
  rename(Mains = item)

#adding colors
mains <- cbind(distinct(mainOrder, Mains), 
               'Meat Type' = c("Beef", "Fish", 'Pork', 'Pork', 'Fish', 'Poultry', 'Fish','Poultry'))

mainOrder <- inner_join(mainOrder, mains, by = 'Mains')


#same process for sides
sideOrder <- orders %>% 
  filter(line_number == 3) %>% 
  group_by(item) %>% 
  tally(sort = T) %>% 
  mutate(item = factor(item, levels = item[order(n)])) %>% 
  rename(Sides = item) 

#adding colors
sides <- cbind(distinct(sideOrder, Sides), 
               'Side Type' = c('Vegetables', 'Beans', 'Vegetables', 'Salad', 'Potatoes', 'Salad', 'Potatoes'))
sideOrder <- inner_join(sideOrder, sides, by = 'Sides')

#simple bar charts
ggWine <- wineOrder %>% ggplot(aes(x = Wines, y = n, fill = WineColor)) + 
  geom_col() + 
  theme_minimal() +
  scale_fill_manual(values = c('#B11226', '#f3e5ab')) + #handpicked colors
  labs(x = "Wine", y = "Frequency", title = "Count of Wine Orders") +
  coord_flip()
ggWine

#For sides
ggSide <- sideOrder %>% ggplot(aes(x = Sides, y = n, fill = `Side Type`)) + 
  geom_col() + 
  theme_minimal() +
  scale_fill_manual(values = c('#672422', '#b79268', '#97be11', '#28590c')) + 
  labs(x = "Side Item", y = "Frequency", title = "Count of Side Item Orders") +
  coord_flip()
ggSide

#For mains
ggMain <- mainOrder %>% ggplot(aes(x = Mains, y = n, fill = `Meat Type`)) + 
  geom_col() + 
  theme_minimal() +
  scale_fill_manual(values = c('#B11226', '#add8e6', '#fcd7de', '#f4e8a4')) + 
  labs(x = "Main Item", y = "Frequency", title = "Count of Main Item Orders") +
  coord_flip()
ggMain
