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


