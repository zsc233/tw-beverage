East <- read_excel("East.xlsx")
View(East)
East <- East[,-c(1,3,4)]
install.packages("ISLR")
install.packages("rpart.plot")
install.packages("rpart")
library(rpart.plot)

set.seed(123)

pairs(~ avg.sale + sun + den + tem + dummy, pch = 19,
      data = East,
      main="Scatterplot Matrix")

model1 <- lm(avg.sale ~ sun + den + tem + dummy, data = East)
summary(model1)

model2 <- lm(avg.sale ~ den + tem + dummy, data = East)
summary(model2)

model3 <- lm(avg.sale ~ den + sun + dummy, data = East)
summary(model3)

model4 <- lm(avg.sale ~ den + dummy, data = East)
summary(model4)

model5 <- lm(avg.sale ~ den + dummy + young, data = East)
summary(model5)

install.packages("stargazer")
library(stargazer)
stargazer(model1, model2, model3, model4, model5, title = "North Result", align = TRUE, type = "text", omit.stat = c("ll", "aic", "ser"), no.space = TRUE, out = "result.html")

#�M����
training <- sample(1:nrow(East), 0.8 * nrow(East))
East.train <- East[training,]
East.test <- East[-training,]

mlb.east <- rpart(avg.sale ~ den + dummy , data = East,
                     method = "anova",
                     control = list(minsplit = 10, maxdepth = 3))
rpart.plot(mlb.east)

#�M����װ�
hyper_grid <- expand.grid(
  minsplit = seq(1, 20, 1),
  maxdepth = seq(1, 30, 1))
head(hyper_grid)

models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    avg.sale ~ .,
    data = East,
    method = "anova",
    control = list(minsplit = minsplit,
                   maxdepth = maxdepth))
}

get_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"]
}

get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

tuning_result <- cbind(hyper_grid,
                       unlist(lapply(models, get_cp)),
                       unlist(lapply(models, get_min_error)))
colnames(tuning_result) <- c("minsplit", "maxdepth", "cp", "xerror")
dim(tuning_result)

head(tuning_result[order(tuning_result[,"xerror"]),], 10)

model.tuned <- rpart(avg.sale~.,
                          method = "anova",
                          data = East,
                          control = list(minsplit = 19,
                                         maxdepth = 30))

min <- which.min(model.tuned$cptable[, "xerror"])

model.tuned$cptable[min,]

plotcp(model.tuned)

model.tunedpruned <- rpart(avg.sale~.,
                                  East.train, method = "anova",
                                 control = list(
                                   minsplit = 4,
                                   maxdepth = 24,
                                   cp = model.tuned$cptable[3, "CP"]))
rpart.plot(model.tunedpruned)

pred <- predict(model.tunedpruned, newdata = East.test)
test.mse <- mean((East.test$avg.sale - pred)^2)
test.mse
sqrt(test.mse)



library(readxl)
Middle <- read_excel("~/Downloads/Middle.xlsx")
View(Middle)
Middle <- Middle[,-c(1,3,4)]
install.packages("ISLR")
install.packages("rpart.plot")
library(rpart.plot)

pairs(~ avg.sale + sun + den + tem + dummy, pch = 19,
      data = Middle,
      main="Scatterplot Matrix")

model1 <- lm(avg.sale ~ sun + den + tem + dummy, data = Middle)
summary(model1)

model2 <- lm(avg.sale ~ den + tem + dummy, data = Middle)
summary(model2)

model3 <- lm(avg.sale ~ sun + tem + dummy, data = Middle)
summary(model3)

model4 <- lm(avg.sale ~ sun + den + dummy, data = Middle)
summary(model4)

model5 <- lm(avg.sale ~ sun + den + dummy + young, data = Middle)
summary(model5)

install.packages("stargazer")
library(stargazer)
stargazer(model1, model2, model3, model4, model5, title = "Middle Result", align = TRUE, type = "text", omit.stat = c("ll", "aic", "ser"), no.space = TRUE, out = "result.html")

#決策樹
set.seed(123)
training <- sample(1:nrow(Middle), 0.8 * nrow(Middle))
Middle.train <- Middle[training,]
Middle.test <- Middle[-training,]

mlb.salary0 <- rpart(avg.sale ~ dummy + young , data = Middle,
                     method = "anova",
                     control = list(minsplit = 20, maxdepth = 30))
rpart.plot(mlb.salary0)

pred <- predict(mlb.salary0, newdata = Middle.test)
test.mse <- mean((Middle.test$avg.sale - pred)^2)
test.mse
sqrt(test.mse)

#調教
hyper_grid <- expand.grid(
  minsplit = seq(1, 20, 1),
  maxdepth = seq(1, 30, 1))
head(hyper_grid)

models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    avg.sale ~ .,
    data = Middle,
    method = "anova",
    control = list(minsplit = minsplit,
                   maxdepth = maxdepth))
}

get_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"]
}

get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

tuning_result <- cbind(hyper_grid,
                       unlist(lapply(models, get_cp)),
                       unlist(lapply(models, get_min_error)))
colnames(tuning_result) <- c("minsplit", "maxdepth", "cp", "xerror")
dim(tuning_result)

head(tuning_result[order(tuning_result[,"xerror"]),], 10)

model.tuned <- rpart(avg.sale~.,
                          method = "anova",
                          data = Middle,
                          control = list(minsplit = 14,
                                         maxdepth = 25))

min <- which.min(model.tuned$cptable[, "xerror"])

model.tuned$cptable[min,]

plotcp(model.tuned)

model.tuned.pruned <- rpart(avg.sale~.,
                                 Middle.train, method = "anova",
                                 control = list(
                                   minsplit = 14,
                                   maxdepth =25,
                                   cp = model.tuned$cptable[3, "CP"]))
rpart.plot(model.tuned.pruned)

pred <- predict(model.tuned.pruned, newdata = Middle.test)
test.mse <- mean((Middle.test$avg.sale - pred)^2)
test.mse
sqrt(test.mse)


library(rpart.plot)
library(readxl)
library(stargazer)
library(ISLR)

North <- read_excel("North.xlsx")
View(North)
North <- North[,-c(1,3,4)]

pairs(~ avg.sale + sun + den + tem + dummy, pch = 19,
      data = North,
      main="Scatterplot Matrix")

model1 <- lm(avg.sale ~ sun + den + tem + dummy, data = North)
summary(model1)

model2 <- lm(avg.sale ~ den + tem + dummy, data = North)
summary(model2)

model3 <- lm(avg.sale ~ den + sun + dummy, data = North)
summary(model3)

model4 <- lm(avg.sale ~ den + dummy, data = North)
summary(model4)

model5 <- lm(avg.sale ~ den + dummy + young, data = North)
summary(model5)

install.packages("stargazer")
library(stargazer)
stargazer(model1, model2, model3, model4, model5, title = "North Result", align = TRUE, type = "text", omit.stat = c("ll", "aic", "ser"), no.space = TRUE, out = "result.html")

#決策樹
training <- sample(1:nrow(North), 0.8 * nrow(North))
North.train <- North[training,]
North.test <- North[-training,]

mlb.salary0 <- rpart(avg.sale ~ den + dummy , data = North,
                     method = "anova",
                     control = list(minsplit = 10, maxdepth = 3))
rpart.plot(mlb.salary0)

#調教
hyper_grid <- expand.grid(
  minsplit = seq(1, 20, 1),
  maxdepth = seq(1, 30, 1))
head(hyper_grid)

models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    avg.sale ~ .,
    data = North,
    method = "anova",
    control = list(minsplit = minsplit,
                   maxdepth = maxdepth))
}

get_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"]
}

get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

tuning_result <- cbind(hyper_grid,
                       unlist(lapply(models, get_cp)),
                       unlist(lapply(models, get_min_error)))
colnames(tuning_result) <- c("minsplit", "maxdepth", "cp", "xerror")
dim(tuning_result)

head(tuning_result[order(tuning_result[,"xerror"]),], 10)

model.tuned <- rpart(avg.sale~.,
                          method = "anova",
                          data = North,
                          control = list(minsplit = 19,
                                         maxdepth = 30))

min <- which.min(model.tuned$cptable[, "xerror"])

model.tuned$cptable[min,]

plotcp(model.tuned)

model.tuned.pruned <- rpart(avg.sale~.,
                                 North.train, method = "anova",
                                 control = list(
                                   minsplit = 4,
                                   maxdepth = 24,
                                   cp = model.tuned$cptable[3, "CP"]))
rpart.plot(model.tuned.pruned)

pred <- predict(model.tuned.pruned, newdata = North.test)
test.mse <- mean((North.test$avg.sale - pred)^2)
test.mse
sqrt(test.mse)


library(rpart.plot)
library(readxl)
library(stargazer)
library(ISLR)

South <- read_excel("South.xlsx")
View(South)
South <- South[,-c(1,3,4)]

pairs(~ avg.sale + sun + den + tem + dummy, pch = 19,
      data = South,
      main="Scatterplot Matrix")

model1 <- lm(avg.sale ~ sun + den + tem + dummy , data = South)
summary(model1)

model2 <- lm(avg.sale ~ den + tem + dummy, data = South)
summary(model2)

model3 <- lm(avg.sale ~ den + sun + dummy, data = South)
summary(model3)

model4 <- lm(avg.sale ~ den + dummy, data = South)
summary(model4)

model5 <- lm(avg.sale ~ den + dummy + young, data = South)
summary(model5)

stargazer(model1, model2, model3, model4, model5, title = "South Result", align = TRUE, type = "text", omit.stat = c("ll", "aic", "ser"), no.space = TRUE, out = "Sresult.html")

#決策樹
training <- sample(1:nrow(South), 0.8 * nrow(South))
South.train <- South[training,]
South.test <- South[-training,]

model <- rpart(avg.sale ~ den + dummy , data = South,
                     method = "anova",
                     control = list(minsplit = 10, maxdepth = 3))
rpart.plot(model)

#調教
hyper_grid <- expand.grid(
  minsplit = seq(1, 20, 1),
  maxdepth = seq(1, 30, 1))
head(hyper_grid)

models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    avg.sale ~ .,
    data = South,
    method = "anova",
    control = list(minsplit = minsplit,
                   maxdepth = maxdepth))
}

get_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"]
}

get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

tuning_result <- cbind(hyper_grid,
                       unlist(lapply(models, get_cp)),
                       unlist(lapply(models, get_min_error)))
colnames(tuning_result) <- c("minsplit", "maxdepth", "cp", "xerror")
dim(tuning_result)

head(tuning_result[order(tuning_result[,"xerror"]),], 10)

model.tuned <- rpart(avg.sale~.,
                     method = "anova",
                     data = South,
                     control = list(minsplit = 19,
                                    maxdepth = 30))

min <- which.min(model.tuned$cptable[, "xerror"])

model.tuned$cptable[min,]

plotcp(model.tuned)

model.tuned.pruned <- rpart(avg.sale~.,
                            South.train, method = "anova",
                            control = list(
                              minsplit = 4,
                              maxdepth = 24,
                              cp = model.tuned$cptable[3, "CP"]))
rpart.plot(model.tuned.pruned)

pred <- predict(model.tuned.pruned, newdata = South.test)
test.mse <- mean((South.test$avg.sale - pred)^2)
test.mse
sqrt(test.mse)


library(readxl)
df <- read_excel("D:/code/R/Island.xlsx")
library(stargazer)
library(rpart)
library(rpart.plot)

pairs(~ avg_sale + sun + population_density + avg_temp + dummy, pch = 19,
      data = df,
      main="Scatterplot Matrix")

model1 <- lm(avg_sale ~ sun + population_density + avg_temp + dummy, data = df)

model2 <- lm(avg_sale ~ population_density + avg_temp+ dummy, data = df)

model3 <- lm(avg_sale ~ population_density + sun + dummy, data =df)

model4 <- lm(avg_sale ~ population_density + dummy, data = df)

model5 <- lm(avg_sale ~ population_density + dummy + youth_density, data =df)

stargazer(model1, model2, model3, model4, model5, title = "Island Result", align = TRUE, type = "text", omit.stat = c("ll", "aic", "ser"), no.space = TRUE, out = "result.html")
##
set.seed(123)
select <- sample(1:nrow(df),nrow(df)*0.8)

train <- df[select,]

test <- df[-select,]

CART.tree <- rpart(avg_sale ~ population_density +dummy, data=train,
                   method = "anova",
                   control = list(minsplit = 10, maxdepth = 3))
rpart.plot(CART.tree)
#�{��
hyper_grid <- expand.grid(
  minsplit = seq(1, 20, 1),
  maxdepth = seq(1, 30, 1))
head(hyper_grid)

models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    avg_sale ~ .,
    data = df,
    method = "anova",
    control = list(minsplit = minsplit,
                   maxdepth = maxdepth))
}

plotcp(CART.tree)
get_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"]
}

get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

tuning_result <- cbind(hyper_grid,
                       unlist(lapply(models, get_cp)),
                       unlist(lapply(models, get_min_error)))
colnames(tuning_result) <- c("minsplit", "maxdepth", "cp", "xerror")
dim(tuning_result)

head(tuning_result[order(tuning_result[,"xerror"]),], 10)

model.tuned <- rpart(avg_sale~.,
                     method = "anova",
                     data = df,
                     control = list(minsplit = 19,
                                    maxdepth = 30))

min <- which.min(model.tuned$cptable[, "xerror"])

model.tuned$cptable[min,]

plotcp(model.tuned)

model.tuned.pruned <- rpart(avg_sale~.,
                            train, method = "anova",
                            control = list(
                              minsplit = 4,
                              maxdepth = 24,
                              cp = model.tuned$cptable[3, "CP"]))
rpart.plot(model.tuned.pruned)

pred <- predict(model.tuned.pruned, newdata =test)
test.mse <- mean((test$avg_sale - pred)^2)
test.mse
sqrt(test.mse)


