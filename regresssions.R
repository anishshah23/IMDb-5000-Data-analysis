library(leaps)
library(lars)
usa <- read.csv('usa_string_genre_dummies.csv',header=TRUE)
usa_train <- usa[1:1000,]
usa_test <- usa[1001:1494,]
y = usa_train$imdb_score
x1 = cbind(usa_train$duration, usa_train$director_facebook_likes, usa_train$adj_gross, usa_train$cast_total_facebook_likes, usa_train$facenumber_in_poster, usa_train$adj_budg, usa_train$title_year)
x2 = cbind(usa_train$duration, usa_train$director_facebook_likes, usa_train$adj_gross, usa_train$cast_total_facebook_likes, usa_train$facenumber_in_poster, usa_train$adj_budg, usa_train$title_year, usa_train$Action, usa_train$Adventure, usa_train$Animation, usa_train$Comedy, usa_train$Crime, usa_train$Family, usa_train$Fantasy, usa_train$Thriller, usa_train$Sci_Fi, usa_train$Drama, usa_train$Mystery, usa_train$Romance, usa_train$Biography, usa_train$History, usa_train$Music, usa_train$War, usa_train$Western, usa_train$Horror, usa_train$Sport, usa_train$Documentary, usa_train$Film_Noir)
x3 = cbind(usa_train$duration, usa_train$director_facebook_likes, usa_train$adj_gross, usa_train$cast_total_facebook_likes, usa_train$facenumber_in_poster, usa_train$adj_budg, usa_train$title_year, 
           usa_train$Action, usa_train$Adventure, usa_train$Animation, usa_train$Comedy, usa_train$Crime, usa_train$Family, usa_train$Fantasy, usa_train$Thriller, usa_train$Sci_Fi, usa_train$Drama, usa_train$Mystery, usa_train$Romance, usa_train$Biography, usa_train$History, usa_train$Music, usa_train$War, usa_train$Western, usa_train$Horror, usa_train$Sport, usa_train$Documentary, usa_train$Film_Noir,
           usa_train$Approved, usa_train$G, usa_train$M, usa_train$NC_17, usa_train$Not_Rated, usa_train$PG, usa_train$PG_13, usa_train$Passed, usa_train$R, usa_train$Unrated, usa_train$rating_x)

#forward stepwise search
res1 = lars(x1, y, type="stepwise")
print(summary(res1))
res1
res2 = lars(x2, y, type="stepwise")
print(summary(res2))
res2
res3 = lars(x3, y, type="stepwise")
print(summary(res3))
res3
#regression
yvar <- usa_test[,"imdb_score"]
xvar1 <- usa_test[,c("duration","director_facebook_likes","adj_gross","cast_total_facebook_likes","facenumber_in_poster")]
xvar2 <- usa_test[,c("duration", "Animation", "Drama", "director_facebook_likes", "adj_gross", "cast_total_facebook_likes", "Horror", "Comedy", "title_year", "Fantasy", "History")]
xvar3 <- usa_test[,c("duration", "Animation", "Drama", "director_facebook_likes", "adj_gross", "cast_total_facebook_likes", "Horror", "G", "Not_Rated", "Comedy", "Approved", "Passed", "Fantasy", "Music", "NC_17")]
reg1 <- lm(imdb_score ~ duration+director_facebook_likes+adj_gross+cast_total_facebook_likes+facenumber_in_poster, data=usa_train)
reg2 <- lm(imdb_score ~ duration+Animation+Drama+director_facebook_likes+adj_gross+cast_total_facebook_likes+Horror+Comedy+title_year+Fantasy, data = usa_train)
reg3 <- lm(imdb_score ~ duration+Animation+Drama+director_facebook_likes+adj_gross+cast_total_facebook_likes+Horror+G+Not_Rated+Comedy+Approved+Passed+Fantasy+Music+NC_17, data = usa_train)
pred1 <- predict(reg1, newdata=data.frame(xvar1),type="response")
pred1
pred2 <- predict(reg2, newdata=data.frame(xvar2),type="response")
pred2
pred3 <- predict(reg3, newdata=data.frame(xvar3),type="response")
pred3
Error1 <- (sum(abs(yvar-pred1)))/nrow(usa_test)
Error1 #0.749
Error2 <- (sum(abs(yvar-pred2)))/nrow(usa_test)
Error2 #0.6937
Error3 <- (sum(abs(yvar-pred3)))/nrow(usa_test)
Error3 #0.6967
summary(reg1) #adj R2: 22.64%
summary(reg2) #adj R2: 29.85%
summary(reg3) #adj R2: 31.27
