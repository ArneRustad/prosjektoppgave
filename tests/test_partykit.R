library("partykit")
## Loading required package: grid
## Loading required package: libcoin
## Loading required package: mvtnorm
## Loading required package: rpart

### regression
airq <- subset(airquality, !is.na(Ozone))
airct <- ctree(Ozone ~ ., data = airq,
               control = ctree_control(maxsurrogate = 3))
airct

plot(airct)
