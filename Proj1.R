rm(list = ls())
library(here)
library(gapminder)
library(tidyverse)

usethis::use_git()
usethis::use_github()
# tests if a directory named "data" exists locally
if (!dir.exists(here("data"))) {
  dir.create(here("data"))
}

# saves data only once (not each time you knit a R Markdown)
if (!file.exists(here("data", "chocolate.RDS"))) {
  url_csv <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv"
  chocolate <- readr::read_csv(url_csv)

  # save the file to RDS objects
  saveRDS(chocolate, file = here("data", "chocolate.RDS"))
}

chocolate <- readRDS(here("data", "chocolate.RDS"))
chocolate <- as_tibble(chocolate)
glimpse(chocolate)
par(mfrow = c(2, 2))
hist(chocolate$rating, breaks = 10, main = "Histogram of Chocolate Ratings \n with 10 Bins",
     xlab = "Rating")
hist(chocolate$rating, breaks = 15, main = "Histogram of Chocolate Ratings \n with 15 Bins",
     xlab = "Rating")
hist(chocolate$rating, breaks = 20, main = "Histogram of Chocolate Ratings \n with 20 Bins",
     xlab = "Rating")
hist(chocolate$rating, breaks = 25, main = "Histogram of Chocolate Ratings \n with 10 Bins",
     xlab = "Rating")
# Make a histogram of the rating scores to visualize the overall distribution of scores.
# Change the number of bins from the default to 10, 15, 20, and 25. Pick on the one that
# you think looks the best. Explain what the difference
# is when you change the number of bins and explain why you picked the one you did.

par(mfrow = c(1,1))
length(unique(chocolate$country_of_bean_origin))
chocolate %>% group_by(country_of_bean_origin) %>% summarize(num_reviews = length(rating))

ecuador <- chocolate %>% filter(country_of_bean_origin == "Ecuador") %>% summarize(mean= mean(rating),
                                                                        sd = sd(rating),
                                                                        total = length(rating))
tail(chocolate %>% filter(country_of_bean_origin == "Ecuador") %>% group_by(company_manufacturer) %>%
             summarize(mean = mean(rating)) %>% arrange(mean))
# Calculate the average rating across all country of origins for beans. Which top 3 countries (for bean origin) have the highest ratings on average?

head(chocolate %>% group_by(country_of_bean_origin) %>%summarize(mean= mean(rating)) %>% arrange(desc(mean)), 3)
head(chocolate %>% group_by(country_of_bean_origin) %>% filter(length(ref) >=10) %>% summarize(mean= mean(rating)) %>% arrange(desc(mean)), 3)


#Identify the countries of bean origin with at least 50 reviews. Remove reviews from countries are not in this list.

chocplus50 <- chocolate%>% group_by(country_of_bean_origin) %>%
  summarize(review_num=length(ref)) %>%
  filter(review_num>=50)

#Using the variable describing the chocolate percentage for each review, create a new column that groups chocolate percentages
#into one of four groups: (i) <60%, (ii) >=60 to <70%, (iii) >=70 to <90%, and
#(iii) >=90% (Hint check out the substr() function in base R and the case_when() function from dplyr – see example below).

chocplus50 <- right_join(chocolate, chocplus50, by = "country_of_bean_origin")

chocplus50 <- chocplus50 %>% mutate(chocpercent = case_when(
  cocoa_percent <"60%" ~ 1,
  cocoa_percent >= "60%" & cocoa_percent < "70%" ~ 2,
  cocoa_percent >= "70%" & cocoa_percent < "90%" ~ 3,
  cocoa_percent >= "90%" ~ 4
))
#don't need to relevel but making a factor column
chocplus50$chocpercent <- factor(chocplus50$chocpercent, levels = c(1, 2, 3, 4))

#For each country, make a set of four side-by-side boxplots plotting the groups on the x-axis and the
#ratings on the y-axis. These plots should be faceted by country
ggplot(chocplus50, aes(x=chocpercent, y = rating))+
  geom_boxplot() +
  facet_wrap(~country_of_bean_origin) +
  labs(x = "Cocoa Percentage Level", y = "Rating",
       title = "Boxplots of Ratings by Cocoa Percentage Group for Each Country")

# On average, which category of chocolate percentage is most highly rated? Do these
# countries mostly agree or are there disagreements?

chocplus50 %>% group_by(chocpercent) %>% summarize(mean_rating = mean(rating))

#Do these countries mostly agree or are there disagreements?

chocplus50 %>%
  group_by(country_of_bean_origin, chocpercent) %>%
  summarize(mean_rating = mean(rating, na.rm = TRUE))

#Only keep reviews that have reviews from countries of bean origin with at least 10 reviews.
head(chocolate)
head(gapminder)

gapminder_unique <- gapminder %>%
  group_by(country) %>%
  summarize(continent = first(continent))

chocolate_data <- chocolate %>%
  left_join(gapminder_unique, by = c("country_of_bean_origin" = "country"))


chocplus10 <- chocolate_data%>% group_by(country_of_bean_origin) %>%
  summarize(review_num=n()) %>%
  filter(review_num>=10)



chocplus.10 <- inner_join(chocolate_data, chocplus10, by = "country_of_bean_origin")
chocplus.10 <- filter(chocplus.10, country_of_bean_origin != "Blend")
na_check <- chocplus.10 %>%
  filter(is.na(continent))

unique(na_check$country_of_bean_origin)

continent_data <- tibble(
  country_of_bean_origin = unique(na_check$country_of_bean_origin),
  continent = c("Oceania", "Oceania", "Africa", "Oceania", "South America",
                "North America", "North America", "Africa", "Oceania",
                "North America", "North America")
)

chocplus_10 <- left_join(chocplus.10, continent_data, by="country_of_bean_origin") %>%
  mutate(continent = coalesce(continent.x, continent.y)) %>%
  select(-continent.x, -continent.y)

sort(unique(chocplus_10$country_of_bean_origin))

ggplot(chocplus_10, aes(x = continent, y = rating)) +
  geom_violin()+
  labs(x = "Continent", y = "Rating", title = "Violin Plots of Chocolate Ratings by Continent")


view(chocolate)
#
chocplus_10<- chocplus_10 %>%
  mutate(
    beans = if_else(str_detect(ingredients, "B"), 1, 0),
    sugar = if_else(str_detect(ingredients, "S"), 1, 0),
    cocoa_butter = if_else(str_detect(ingredients, "C"), 1, 0),
    vanilla = if_else(str_detect(ingredients, "V"), 1, 0),
    lecthin = if_else(str_detect(ingredients, "L"), 1, 0),
    salt = if_else(str_detect(ingredients, "Sa"), 1, 0)
  )

chocplus_10<- chocplus_10 %>%
  mutate(
    char_cocoa = if_else(str_detect(most_memorable_characteristics, "cocoa"), 1, 0),
    char_sweet = if_else(str_detect(most_memorable_characteristics, "sweet"), 1, 0),
    char_nutty = if_else(str_detect(most_memorable_characteristics, "nutty"), 1, 0),
    char_creamy = if_else(str_detect(most_memorable_characteristics, "creamy"), 1, 0),
    char_roasty = if_else(str_detect(most_memorable_characteristics, "roasty"), 1, 0),
    char_earthy = if_else(str_detect(most_memorable_characteristics, "earthy"), 1, 0)
  )
#

# Calculate mean values by year
choc_reviews<- chocplus_10 %>%
  group_by(review_date) %>%
  summarize(across(c("beans", "sugar", "cocoa_butter", "vanilla",
                                 "lecthin", "salt", "char_cocoa", "char_sweet",
                                 "char_nutty", "char_creamy", "char_roasty",
                                 "char_earthy"), mean, na.rm = TRUE))

choc_long <- choc_reviews %>%
  pivot_longer(cols = c("beans", "sugar", "cocoa_butter", "vanilla",
                        "lecthin", "salt", "char_cocoa", "char_sweet",
                        "char_nutty", "char_creamy", "char_roasty",
                        "char_earthy"),
               names_to = "feature",
               values_to = "mean_score")

# Print the long dataset
print(choc_long)

ggplot(choc_long, aes(x = review_date, y = mean_score)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
         facet_wrap(~ feature) +
         labs(x = "Year", y = "Mean Score",
              title = "Annual Trend in Mean Score of Cocoa Features",
              subtitle = "This plot shows the average scores of various chocolate features over the years.
              Trends indicate a U shaped presence of cocoa character, declining creaminess, and limited change
              in other taste aspects (i.e., earthym nutty, roasty, and sweet). Use of beans, sugar, and salt
              is constant, but lecthin use has declined, as has the use of vanilla and cocoa butter.",
              caption = "Plot created by Mary de Boer")

names(choc_long)

ggplot(choc_long, aes(x = feature, y = mean_score)) +
  geom_bar(stat = "identity") +  # Incorrect use of geom_bar for continuous y
  ggtitle("The Worst Chocolate Plot Ever") +  # Uninformative title
  labs(x = "Chocolate Countries", y = "Chocolate Ratings", fill = "Percentage")

names(chocplus50)
# Plot with numeric fill
chocplus50 %>%
  ggplot(aes(
   x = (chocpercent),
    y = rating,
    fill =chocpercent
  )) +
  geom_violin() +
  facet_wrap(~review_date) +
  ggtitle("Chocolate Ratings by Review Date and Cocoa Percentage") +
  labs(x = "Percent Cocoa", y = "Rating", fill = "Cocoa Percentage",
       subtitle= "Cocoa Percentage Levels include: (1) <60%, (2) >=60 to <70%, (3) >=70 to <90%, and (4) >=90%",
       caption = "Plot improved by Mary de Boer")

chocolate %>%
  ggplot(aes(
    x = as.factor(review_date),
    y = rating,
    fill = (cocoa_percent)
  )) +
  geom_violin()+
  ggtitle("Rating by Review Date") +  # Uninformative title
  labs(x = "Review Dates", y = "Chocolate Ratings", fill = "Percentage")
