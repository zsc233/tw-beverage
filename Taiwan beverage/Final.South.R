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


