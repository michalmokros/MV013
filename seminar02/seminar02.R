setwd("~/Documents/school/MV013/seminar02")
skull <- read.table('skulls.txt', header=T)
str(skull)
dim(skull)
head(skull,10)
tail(skull)

missing <- is.na(skull) #are there null values
sum(missing) > 0
skull2 <- na.omit(skull)  

length(unique(skull2$id)) == nrow(skull2) #duplicities
sum(duplicated(skull2$id)) == 0 #other way

table(skull2$sex)
women <- skull2[skull2$sex == 'f', ]
men.125 <- skull2[skull2$sex == 'm' & skull2$skull.H > 125, ]
dim(men.125)
nrow(men.125)

skull3 <- skull2[skull2$skull.H >= 130 & skull2$skull.H < 140, ]
nrow(skull3)
table(skull3$sex)

newborns <- read.csv('newborns.csv')
str(newborns)
dim(newborns)
table(newborns$edu.M)
table(newborns$edu.M, useNA = 'ifany')
table(newborns$edu.M, useNA = 'always')
newborns$edu.M2 <- factor(newborns$edu.M, labels=c('elementary', 'highschool', 'university'))
table(newborns$edu.M2, useNA = 'ifany')
sum(is.na(newborns))
newborns2 <- na.omit(newborns)
sum(duplicated(newborns2$id))
newborns3 <- newborns2[!duplicated(newborns2$id), ]
dim(newborns3)
table(newborns3$sex.C, newborns3$edu.M2, useNA = 'always')

height <- read.table('height.txt', header = T)
dim(height)
str(height)
levels(height$gender)
n_rows <- nrow(height)
t_height <- table(height$gender, useNA = 'ifany')
sum(t_height[-c(5, 10)])/n_rows * 100
gender1 <- height$gender
gender1[gender1 == ' female'] <- 'F'
gender1[gender1 == ' Female'] <- 'F'
gender1[gender1 == 'female'] <- 'F'
gender1[gender1 == 'Female'] <- 'F'
gender1[gender1 == 'female '] <- 'F'
gender1[gender1 == 'Female '] <- 'F'
gender1[gender1 == ' male' | gender1 == ' Male' | gender1 == 'male' | gender1 == 'Male' | gender1 == 'male ' | gender1 == 'Male '] <- 'M'
gender1[gender1 == 'na' | gender1 == 'NA'] <- NA
table(gender1, useNA = 'ifany')
gender2 <- height$gender
gender2 <- factor(gender2, labels=c('F', 'F', rep('M', 2), rep('F', 5), rep('M', 5), rep(NA, 2)))
table(gender2)
levels(gender2)
gender3 <- factor(gender2)
levels(gender3)
table(gender3)
table(gender3, useNA = 'ifany')
gender4 <- factor(gender1)
levels(gender4)
