

library(daewr)
library(rsm)
library(ggplot2)

wavelengths <-  c(510,520,530,550,560,570,610,620,630,700)
data <- training_data_THY[,c(1:10,13)]
names(data)[1:10] = paste0("wv",wavelengths)

mdl <- lm(pH ~ FO(wv510,wv520,wv530,wv550,wv560,wv570,wv610,wv620,wv630,wv700)+TWI(wv510,wv520,wv530,wv550,wv560,wv570,wv610,wv620,wv630,wv700), data=data)

summary(mdl)



wvnames <- paste0("wv",wavelengths)

test <- data.frame(mapply(as.numeric,testing_data_THY[,wvnames]))
test <- cbind(test,testing_data_THY[,c("dye","pH")])

testdata <- subtract_background(test,"universal")
testdata <- normalize(testdata)

wvcols <- grepl("wv",names(testdata))

x <- predict.lm(mdl,testdata[,wvcols])
preds <- data.frame(pH = testdata$pH, linear_model <- x, gp_model <- gp_prediction$mean_pH)

plt1 <- ggplot(data=preds)+
  geom_point(aes(x=pH,y=linear_model))+
  geom_abline(slope=1,intercept = 0,color="red")+
  theme_classic()

plt1

plt2 <- ggplot(data=preds)+
  geom_point(aes(x=pH,y=gp_model))+
  geom_abline(slope=1,intercept=0,color="red")+
  theme_classic()
plt2 

cowplot::plot_grid(plt1,plt2)







f <- "/Users/BDavid/Dropbox-UniversityofMichigan/Benjamin David/JensenLab/dye_files/BioTek Reads/2025_07_07_pH_standards.xlsx"

k = c(25,70,115,160)
test <- data.frame()
for (i in 1:4){


  t <- openxlsx::read.xlsx(f, rows = k[i]:(k[i]+41), cols = 3:99, colNames=TRUE)
  if (i==1){
    test = t
  }else{
    test <- cbind(test,t)
  }

}


wv <- openxlsx::read.xlsx(f, rows = k[1]:(k[1]+41), cols = 2, colNames=TRUE)

wvnames <- paste0("wv",wv[,1])

testdata <- data.frame(t(test))[1:192,]
names(testdata) = wvnames
pH_vals <- c(4.42,4.78,5.03,5.49,5.92,6.47,7.00,7.5,7.96,8.35,8.75,9.12)

pH <- rep(pH_vals, each=2,times=8)
dye1 <- c("blank","universal")
dye2 <- c("phenol","bromphenol")

dyes <- rep(c(rep(dye1,times=12),rep(dye2,times=12)),times=4)

testdata$pH <- pH
testdata$dye <- dyes
