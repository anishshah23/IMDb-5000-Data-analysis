## Import the information of USA movies
setwd("/Users/Administrator/Documents/UMD/s2/DataScience/imdb")
data <- read.csv("movie_metadata.csv", header = TRUE)
head(data)
mydata <- subset(data, country == "USA", select  = -country)

summary(mydata)

# How many movies are in our dataset?
nrow(mydata)
install.packages("dplyr")
install.packages("ggrepel")
library("ggplot2", lib.loc = "/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("dplyr")
library("ggrepel")

# What does the distribution of imdb ratings look like?
ggplot(mydata, aes(imdb_score)) + geom_histogram(bins = 100)
summary(mydata$imdb_score)

#Create object mplot (movie year and score)
mplot <- ggplot(mydata, aes(title_year, imdb_score))

mplot + geom_point(aes(colour = imdb_score)) + scale_colour_gradient(low = "yellow", high =
                                                                       "red")

qplot(imdb_score, gross, data = mydata, color = actor_1_name)

# How are the average ratings of stars distributed?
# Is it related to the number of films they have starred in?
actors <- mydata %>% group_by(actor_1_name) %>%
  summarise(
    count = n(),
    avg_score = mean(imdb_score),
    gross = mean(gross)
  ) %>%
  arrange(desc(count))

ggplot(actors, aes(count, avg_score)) + geom_point(alpha = 1 / 5)

# Zoom in on the most experienced stars
# We choose 24 starring roles as an arbitrary cut-off, since it leaves us the top 10 experienced stars
top_actors <- actors %>% filter(count > 23)
ggplot(top_actors) +
  geom_point(aes(count, avg_score, size = count), color = 'red') +
  geom_text_repel(aes(count, avg_score, label = actor_1_name)) +
  theme_classic(base_size = 16)

# How are the gross revenue of stars distributed?
# Is it related to the number of films they have starred in?
ggplot(actors, aes(count, gross)) + geom_point(alpha = 1 / 5)

# Zoom in on the most profitable stars
# We choose gross larger than 222701282 as an arbitrary cut-off, since it leaves us the top 10 profitable stars
top_actors_gross <- actors %>% filter(gross > 222701282)
ggplot(top_actors_gross) +
  geom_point(aes(count, gross, size = count), color = 'red') +
  geom_text_repel(aes(count, gross, label = actor_1_name)) +
  theme_classic(base_size = 16)

## Use Gross, Count, Facebook likes of the movie, Facebook likes of the actor
## to forecast if the actor is highly scored or low scored(make it a categorical variable).

## random forest
setwd("/Users/Administrator/Documents/UMD/s2/DataScience/imdb")
mydata <- read.csv("USA_string_adjusted.csv", header = TRUE)
library(xlsx)
write.xlsx(
  mydata,
  "/Users/Administrator/Documents/UMD/s2/DataScience/string_adjusted.xlsx"
)
mydata <- na.omit(mydata)
head(mydata)
barplot(table(mydata$imdb_score))

##classify movies into bad, good and normal quality
mydata$quality <-
  ifelse(mydata$imdb_score <= 5.5,
         "bad",
         ifelse(mydata$imdb_score >= 7, "good", "normal"))

barplot(table(mydata$quality))

table(mydata$quality)

## Separate data into testing and training sets
set.seed(123)
samp <- sample(nrow(mydata), 0.8 * nrow(mydata))
train <- mydata[samp,]
test <- mydata[-samp,]

sample(1:nrow(train), replace = TRUE)

## Random forest model
install.packages("randomForest")
> library(randomForest)
> fit <-
randomForest(
as.factor(quality) ~ duration + director_facebook_likes + adj_budg + actor_1_facebook_likes + actor_2_facebook_likes + actor_3_facebook_likes + title_year +  Mystery + Romance + Biography + History + Music + War + Western + Action + Adventure + Animation + Comedy + Crime + Family + Fantasy + Thriller + Sci_Fi + Drama +  Horror + Sport + Documentary + Film_Noir,
data = usa_train,
importance = TRUE,
ntree = 9560
)
> fit
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(Movie = test$movie_title, Quality = Prediction)
submit <- cbind2(submit, test[,32])
as.factor(submit$Quality)
as.factor(submit$y)
table(submit$Quality,submit$y)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)