library(readxl)
library(rpart.plot)
library(readxl)
library(stargazer)
library(ISLR)

df <- read_excel("D:/code/R/Island.xlsx")

df <- df [,-c(1,3,4)]


pairs(~ avg.sale + sun + den + tem + dummy, pch = 19,
      data = df,
      main="Scatterplot Matrix")

model1 <- lm(avg.sale ~ sun + den + tem + dummy, data = df)
summary(model1)

model2 <- lm(avg.sale ~ den + tem + dummy, data = df)
summary(model2)

model3 <- lm(avg.sale ~ den + sun + dummy, data = df)
summary(model3)

model4 <- lm(avg.sale ~ den + dummy, data = df)
summary(model4)


model5 <- lm(avg.sale ~ den + dummy + young, data = df)
summary(model5)


library(stargazer)
stargazer(model1, model2, model3, model4, model5, title = "Island Result", align = TRUE, type = "text", omit.stat = c("ll", "aic", "ser"), no.space = TRUE, out = "result2.html")
