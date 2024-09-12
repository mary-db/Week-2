library(here)
library(tidyverse)

list.files(here::here())
list.files(here("data"))
df <- read.csv(here("data", "promax_results1 copy.csv"))
df$Factor.1
here("data", "promax_results1 copy.csv")
x <- 1:5
save(x, file = here("data", "x.Rda"))
saveRDS(x, file = here("data", "x.Rds"))
list.files(path = here("data"))
new_x1 <- readRDS(here("data", "x.Rds"))
new_x1
new_x2 <- load(here("data", "x.Rda"))
new_x2
x
file.remove(here("data", "x.Rda"))
file.remove(here("data", "x.Rds"))
rm(x)
x <- 1:5
y <- x^2
save(x, y, file = here("data", "x.Rda"))
new_x2 <- load(here("data", "x.Rda"))
new_x2
file.remove(here("data", "x.Rda"))
rm(x,y, new_x1, new_x2)
vignette("tibble")
as_tibble(df) %>%
  print(n = 5, width = Inf)
df[[4]]
df$Factor.3
df[["Factor.3"]]
df <- as_tibble(df)
str(df)
names(df)[1:3]
subset <- select(df, X:Factor.2)
subset <- select(df, starts_with("Factor"))
str(subset)
df.f <- filter(df, Uniqueness > 0.9)
df.f <- filter(df, X %in% "lpchix")
df <- arrange(df, Communality)

tail(select(df, X, Communality), 3)
head(df[, 1:4], 3)
df <- mutate(df, sum = Communality + Uniqueness)
df$sum
transmute(df, sum = Communality + Uniqueness)
slice_head(df, n = 3)
print(df, max_extra_cols = 10)
tree <- as_tibble(trees) %>% filter(Height > 70) %>% arrange(Volume)

library("gapminder")
print(gapminder)
# If we wanted to make lifeExp, pop and gdpPercap (all measurements that we observe)
# go from a wide table into a long table, what would we do?
gapminder %>%
  pivot_longer(cols = c(lifeExp, pop, gdpPercap),
               names_to = "measurement",
               values_to = "value")

gapminder_long <- gapminder %>%
  pivot_longer(cols = c(lifeExp, pop, gdpPercap), # Columns to gather into long format
               names_to = "measurement",         # Name of new column for variable names
               values_to = "value")              # Name of new column for values

# View the transformed data
head(gapminder_long)

gapminder %>%
  unite(
    col = "country_continent_year",
    country:year,
    sep = "_"
  ) %>%
  separate(
    col = "country_continent_year",
    into = c("country", "continent", "year"),
    sep = "_"
  )


##
df <- tibble(
  "company" = rep(1:3, each = 4),
  "year" = rep(2006:2009, 3),
  "Q1" = sample(x = 0:100, size = 12),
  "Q2" = sample(x = 0:100, size = 12),
  "Q3" = sample(x = 0:100, size = 12),
  "Q4" = sample(x = 0:100, size = 12),
)

head(df)

df_longer <- df %>%
  pivot_longer(cols = c(Q1, Q2, Q3, Q4),
               names_to = "quarter",
               values_to = "revenue")
df_longer %>% group_by(company) %>% summarize(ave_revenue = mean(revenue))
df_longer %>% group_by(company, year) %>% summarize(ave_revenue = mean(revenue))

#

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "merge")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "right")

df <- tibble(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df %>% extract(x, "A")
df %>% extract(x, c("A", "B"), "([[:alnum:]]+)-([[:alnum:]]+)")

data <- data.frame(id = c("123456", "987654"))
separated_data <- data %>%
  separate(id, into = c("first_part", "second_part"), sep = c(3))

print(separated_data)


outcomes <- tibble(
  id = rep(c("a", "b", "c"), each = 3),
  visit = rep(0:2, 3),
  outcome = rnorm(3 * 3, 3)
)
subjects <- tibble(
  id = c("a", "b", "c"),
  house = c("detached", "rowhouse", "rowhouse")
)
left_join(x = outcomes, y = subjects, by = "id")

subjects <- tibble(
  id = c("b", "c"),
  visit = c(1, 0),
  house = c("rowhouse", "rowhouse"),
)

inner_join(x = outcomes, y = subjects, by = c("id", "visit"))

right_join(x = outcomes, y = subjects, by = c("id", "visit"))

# Create first example data frame
df1 <- data.frame(
  ID = 1:3,
  X1 = c("a1", "a2", "a3")
)
# Create second example data frame
df2 <- data.frame(
  ID = 2:4,
  X2 = c("b1", "b2", "b3")
)

inner_join(df1, df2) # everything that is in both, keeping all that matches
semi_join(df1, df2) # the Ids that are in both but only the variables from df1
anti_join(df1, df2) # the IDs and associated variables that are in df1 and not in df2
inner_join(df2, df1) #same as above, just the order of variables changes
semi_join(df2, df1)# the Ids that are in both but only the variables from df2
anti_join(df2, df1)#the IDs and associated variables that are in df2 and not in df1
