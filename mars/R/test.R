# Ex 1.
dir <- ifelse(.Platform$OS.type=="unix",
              "/Users/santiago/Library/CloudStorage/GoogleDrive-santi9608@gmail.com/My Drive/School/SFU/Upper Division/STAT360",
              "C:/Users/guerr/Google Drive/School/SFU/Upper Division/STAT360")
load(paste0(dir, "/Project/marstestdata.rda"))
load(paste0(dir, "/Exercises/ProjectTestfiles/testthat/testmars.RData"))
load(paste0(dir, "/Exercises/ProjectTestfiles/testthat/testpredict.RData"))
predict.mars(testmars)
predict.mars(testmars, marstestdata)
all.equal(predict.mars(testmars), testpredict)

# Ex 2.
d <- mars(Limit ~ ., data = ISLR::Credit)
summary(d)
anova(d)
predict(d)

# Ex 3.
d <- mars(Sales ~ ., data = ISLR::Carseats)
print(d)
plot(d)
