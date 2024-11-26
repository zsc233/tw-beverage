library(readr)
one <- read_excel("young103.xlsm",sheet = "01")
two <- read_excel("young103.xlsm",sheet = "02")
three <- read_excel("young103.xlsm",sheet = "03")
four <- read_excel("young103.xlsm",sheet = "04")
five <- read_excel("young103.xlsm",sheet = "05")
six <- read_excel("young103.xlsm",sheet = "06")
seven <- read_excel("young103.xlsm",sheet = "07")
eight <- read_excel("young103.xlsm",sheet = "08")
nine <- read_excel("young103.xlsm",sheet = "09")
ten <- read_excel("young103.xlsm",sheet = "10")
ele <- read_excel("young103.xlsm",sheet = "11")
twl <- read_excel("young103.xlsm",sheet = "12")
View(four)


  A <-read_excel("104.xls", sheet = 1)
  A <- A[-c(1:3,76),]
  A$...22 <- as.numeric(A$...22)
  A$...28 <- as.numeric(A$...28)
  A$...34 <- as.numeric(A$...34)
  A$...3 <- as.numeric(A$...3)
  
  B <- data.frame(
    B$young = A$...22+ A$...28 + A$...34,
    B$TTL = A$...3,
    B$ratio = B$young/B$TTL)



for (i in 1:12) {
  A <-read_excel("104.xls", sheet = i)
  A <- A[-c(1:3,76),]
  A$...22 <- as.numeric(A$...22)
  A$...28 <- as.numeric(A$...28)
  A$...34 <- as.numeric(A$...34)
  A$...3 <- as.numeric(A$...3)
  
  B <- data.frame(
    B$young = A$...22+ A$...28 + A$...34,
    B$TTL = A$...3,
    B$ratio = B$young/B$TTL)
  
  #C <- data.frame()
  #C[,i]  <- B$young
  #C[,12+i] <- B$TTL
  #C[,24+i] <- B$ratio
}

View(B)
TTL

Three <- data.frame(
  one_Y = one$`Young Man`,
  two_Y = two$`Young Man`,
  three_Y = three$`Young Man`,
  four_Y = four$`Young Man`,
  five_Y = five$`Young Man`,
  six_Y = six$`Young Man`,
  seven_Y = seven$`Young Man`,
  eight_Y = eight$`Young Man`,
  nine_Y = nine$`Young Man`,
  ten_Y = ten$`Young Man`,
  ele_Y = ele$`Young Man`,
  twl_Y = twl$`Young Man`,
  one_TTL = one$TTL,
  two_TTL = two$TTL,
  three_TTL = three$TTL,
  four_TTL = four$TTL,
  five_TTL = five$TTL,
  six_TTL = six$TTL,
  seven_TTL = seven$TTL,
  eight_TTL = eight$TTL,
  nine_TTL = nine$TTL,
  ten_TTL = ten$TTL,
  ele_TTL = ele$TTL,
  twl_TTL = twl$TTL)
View(Three)

North <- Three[c(4,7,22,25,28,58,61),]
North$"縣市" <- c("新北","台北","宜蘭","桃園","新竹縣","基隆市","新竹市")
View(North)

write.csv(North, file = "North.csv")
