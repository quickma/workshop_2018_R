# reading in the data

gapminder <- read.csv("gapminder-FiveYearData.csv")
head (gapminder)
str(gapminder)
summary (gapminder)

# group_by    function
gapminder %>% group_by(country)   #commond+shif+m  gives %>%
gapminder %>% group_by(country) %>% tally()  #count using tally

# summarize
gapminder %>% group_by(country) %>% summarise(avg = mean(pop), std = sd(pop), total =n())
names(gapminder)
gapminder %>% group_by(country) %>% summarise(avg = mean(pop), std = sd(pop), total =n()) %>% arrange(avg)
gapminder %>% group_by(country) %>% summarise(avg = mean(pop), std = sd(pop), total =n()) %>% arrange(desc(avg))
# after modify, it is not in the data frame, now put it into a new dataframe

# mutate the data frame by using "mutate" function
gapminder_mod <- gapminder
gapminder_mod %>% mutate(gdp = pop * gdpPercap)
gapminder_mod %>% mutate(gdp = pop * gdpPercap) %>% head()
gapminder_mod <- gapminder_mod %>% mutate(gdp = pop * gdpPercap)
# exercise

gapminder %>% group_by(country) %>% tally() 
gapminder %>% group_by(country) %>% 
  summarise (life_ave = mean(lifeExp)) %>% 
  arrange (life_ave) %>% head(1)

# base R plotting
plot( x= gapminder_mod$gdpPercap, y = gapminder_mod$lifeExp)

# ggplot2
library(ggplot2)
ggplot(gapminder_mod, aes(x= gdpPercap, y = lifeExp)) # need to use + to add geom
ggplot(gapminder_mod, aes(x= gdpPercap, y = lifeExp)) + geom_point()
ggplot(gapminder_mod, aes(x= log10(gdpPercap), y = lifeExp)) + geom_point()
ggplot(gapminder_mod, aes(x= log10(gdpPercap), y = lifeExp)) + geom_point(alpha =1/3, size =3)
# alpha is the transperancy

# add color
# first to get the idea how many continents are there
summary(gapminder)
ggplot(gapminder_mod, aes(x= log10(gdpPercap), y = lifeExp, color = continent)) + geom_point(alpha =1/3, size =3)

#assign that to send to a variable
p <- ggplot(gapminder_mod, aes(x= log10(gdpPercap), y = lifeExp, color = continent)) + geom_point(alpha =1/3, size =3)
p  # this will be more easy by adding more geoms
p + facet_wrap(~ continent)
p <- p + facet_wrap(~ continent)
p2 <- p + geom_smooth(color  ="orange")
p2

# combine dplyr with ggplot2
gapminder %>% mutate(gdp = pop * gdpPercap) %>% 
  ggplot(aes(gdp, lifeExp)) + geom_point()

#exercise 10
p3 <- ggplot(gapminder_mod, aes(lifeExp, fill = continent)) + geom_histogram(binwidth = 1) +
  ggtitle("Histogram_gapminder")
p3

# saving plots
ggsave(p3, file = "histogram_lifeExp.png")
# if want save to another file
ggsave(p3, file = "~/scw_2018/advanced_R/histogram_lifeExp.png") # define the path (relative or complet path)
? ggplot2
? ggtitile

# line plot
gapminder_mod %>% filter (country == "Afghanistan") %>% head()
gapminder_mod %>% filter (country == "Afghanistan") %>% summary()
gapminder_mod %>% filter (country == "Afghanistan") %>% 
  ggplot(aes(x = year, y = lifeExp)) + geom_line()

gapminder_mod %>% filter (country == "Afghanistan") %>% 
  ggplot(aes(x = year, y = lifeExp)) + geom_line() +
  
# exercise 10.2
p4<- ggplot(gapminder_mod, aes(x = lifeExp, y = year, color = continent)) + 
  facet_wrap(~ continent) +
  geom_smooth(color  ="orange", lwd = 2, se = FALSE) +
  geom_smooth(color = "blue", lwd = 2, se = FALSE, method = "lm") +
  geom_point(alpha =1/3, size =3) 
p4
ggsave(p4, file = "geom_smooth_type.png")

# density plots
ggplot(gapminder_mod, aes(gdpPercap, lifeExp)) +
  geom_point(size = 0.25) +
  geom_density_2d() + 
  scale_x_log10()

# combine plots
install.packages("gridExtra")
library(gridExtra)

#  loops
gapminder_mod %>% filter (continent == "Asia") %>% 
  summarise (avg = mean (lifeExp))
continents <- unique(gapminder_mod$continent) # unique is to remove the repeat
continents

for (variable in list) {
  do something
}

for (c in continents) {
  #print (c)
  res <- gapminder_mod %>% filter (continent == c) %>% 
    summarise (avg = mean (lifeExp))
  print (paste0("The avg life expectancy of " , c, " is  ", res))
}

gapminder_mod %>% group_by(continent, year) %>% summarise (avg = mean(lifeExp))

# functions
mean (2,3)
# so how to write a funciton to do that, eg, add 2 numbers
adder <- function(x,y){
  return(x+y)
}
adder (2, 3)
#
adder <- function(x,y){
  print (paste0("the sum of ", x, " and ", y, " is ", x+y))
  #return(x+y)
}
adder (2, 3)
