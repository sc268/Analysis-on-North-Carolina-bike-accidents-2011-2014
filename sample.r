install.packages("leaflet")
install.packages("shiny")
install.packages("gbm")
install.packages("party")
require(devtools)
install_github('xgboost','tqchen',subdir='R-package')
library(ggplot2)
library(shiny)
library(leaflet)
library(magrittr)

setwd("~/Dropbox/codes/2015_insight")
getwd()

dat <- read.csv("~/Dropbox/codes/2015_insight/north_carolina_bicycle_crash_data_heatmap_.csv", sep=";")
head(dat)
attach(dat)
colnames(dat)

    #how many data set without location?
        sum(dat$Location=="0.0, 0.0")
        dim(dat)[1]
        dat_new = dat[dat$Location!="0.0, 0.0"]
      
    #ordered features
      Drvr_EstSp<-ordered(Drvr_EstSp, levels = c("0-5 mph", "11-15 mph", "16-20 mph","21-25 mph", "26-30 mph",
                                                 "31-35 mph", "36-40 mph", "41-45 mph", "46-50 mph", "51-55 mph",
                                                 "56-60 mph" ,"61-65 mph" ,"65-70 mph", "81-85 mph", ">100 mph"))
      Speed_Limi <-ordered(Speed_Limi, levels = c("5 - 15 MPH", "20 - 25  MPH",  "30 - 35  MPH", "40 - 45  MPH",  
                                                  "50 - 55  MPH",  "60 - 75 MPH" ))
    #change to numeric
      levels(dat$Speed_Limi) <- c(45,25,35,45,15,55,75)
      dat$Speed_Limi <- as.numeric(levels(dat$Speed_Limi))[dat$Speed_Limi]
      
      levels(dat$Drvr_EstSp) <-c(47.5, 100, 5,15,20,25,30,35,40,45,50,55,60,10,65,70,85)
      dat$Drvr_EstSp <- as.numeric(levels(dat$Drvr_EstSp))[dat$Drvr_EstSp]

    #create latitude and longitude
      dat_length = dim(dat)[1]
      locx <- vector(mode="numeric", length=dat_length)
      locy <- vector(mode="numeric", length=dat_length)
      for( i in 1:dat_length){
        tmp = as.character(dat$Location[i])
        tmp = strsplit(tmp, ",")[[1]]
        locy[i] = as.numeric(tmp[1])
        locx[i] = as.numeric(tmp[2])
      }
      dat$locx = locx
      dat$locy = locy
      dat <- dat[, !(colnames(dat) %in% c("Location"))]      

      #sampling data
      dat_sub <- subset(dat, locx != 0.0 & locy!=0.0 & 
                          City %in% c('Durham', 'Raleigh', 'Chapel Hill'))
      dim(dat_sub)

########## Data visualization #############
      accidentMap <- leaflet() %>% 
      addTiles() %>% 
      setView( -78.8785,  36.002743, zoom = 13) %>% 
      addMarkers(data = dat_sub, lng = ~ locx, lat = ~ locy, popup = dat_sub$Bike_Injur)
      accidentMap
####
      map2 <- leaflet() %>% 
        addTiles() %>% 
        setView(-78.8785, 36.002743,  zoom = 13) %>% 
        addMarkers( -78.8785,36.002743,  popup = 'Bay Area')
      map2
      write.csv(dat, file = "nc_bike_accidents.csv")


#EDA
      pairs(~Light_Cond+ Locality + Bike_Age + Bike_Injur+ Bike_Pos,data=dat, main="Scatterplot Matrix")
      ggplot(dat, aes(x=Drvr_EstSp, y=Bike_Injur)) +
        geom_point(shape=1)   

#correlation
      library(corrplot)
      colnames(dat)
      columns = c(  "Bike_Age"  , "Bike_Alc_D" ,"CrashAlcoh" , "Drvr_Age" ,"Drvr_Alc_D" ,"Drvr_EstSp","ExcsSpdInd" ,"Light_Cond", "Speed_Limi", "Weather"   )
      dat2 = dat[columns]
      corrplot(dat2)

## modeling 

########### project # of accidents ########### 
      #aggregate/ counts 
      library(plyr)
      colnames(dat)
      subcols <- c("Bike_Sex", "Bike_Injur", "Weather", "Bike_Pos", "Speed_Limi")
      #subcols <- c("Weather")
      #head(count(dat, subcols))
      dat_aggre <- count(dat, subcols)
      head(dat_aggre)
      mean(dat_aggre$freq); sd(dat_aggre$freq) #10.9 and 37
      res <- glm(freq ~ Bike_Sex + Bike_Injur + Weather + Bike_Pos+Speed_Limi, family="quasipoisson", data = dat_aggre)
      summary(res)  
## 

########## classify injury type ##############
      dat_sub <- dat[!complete.cases(dat),]
      library(party)
      res2 <- ctree(Bike_Injur ~ Light_Cond + Weather + Bike_Pos  +  Bike_Alc_D + Speed_Limi,
                     data = dat_sub)
      plot(res2)
###

      levels(Bike_Injur)
      library(rpart)
      res2 <- rpart(Bike_Injur ~ Weather + Bike_Pos + Bike_Sex +  Bike_Alc_D + Speed_Limi,
                    data = dat,
                    control = rpart.control(minsplit = 10))
      print(res2)
      plot(res2)


      #sample
      train_rows <- sample(nrow(dat), round(nrow(dat) * 0.9))
      traindf <- dat[train_rows, ]
      testdf <- dat[-train_rows, ]

      #regression 
      library(gbm)
      #response_column <- which(colnames(traindf) == "Bike_Injur")
      trainy <-traindf$Bike_Injur
      gbm_formula <- as.formula(paste0("Bike_Injur ~ ", paste(colnames(traindf[, columns]), 
                                                       collapse = " + ")))
      gbm_model <- gbm(gbm_formula, traindf, distribution = "multinomial", 
                       n.trees = 50, bag.fraction = 0.75, cv.folds = 5, interaction.depth = 3)



#xgboost
devtools::install_github('dmlc/xgboost', subdir='R-package')
install.packages("xgboost")
require(xgboost)
