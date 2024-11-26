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

#¨Mµ¦¾ğ
training <- sample(1:nrow(East), 0.8 * nrow(East))
East.train <- East[training,]
East.test <- East[-training,]


mlb.east <- rpart(avg.sale ~ den + dummy , data = East,
                     method = "anova",
                     control = list(minsplit = 10, maxdepth = 3))
rpart.plot(mlb.east)

#¨Mµ¦¾ğ­×°Å
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



