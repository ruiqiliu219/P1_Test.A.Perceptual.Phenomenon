stroop <- read.csv("stroopdata.csv")

##mean, median and mode##
mean <- apply(stroop,2,mean)
median <- apply(stroop,2,median)

Mode <- function(x) {#function to calculate mode
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
}

mode <- apply(stroop,2,Mode)
centralTendency <- rbind(mean,median,mode)
centralTendency

##Variance and SD##
var <- apply(stroop,2,var)
var

##Distribution of Sample##
#Plot histogram
h1 <- hist(stroop$Congruent, breaks = 10,plot = F)
h2 <- hist(stroop$Incongruent, breaks =14, plot = F)
plot(h1, col=rgb(0,0,1,1/4),xlim =c(5,40),
     xlab = "Time", main = "Histogram of Stroop Data") 
plot(h2, col=rgb(1,0,0,1/4),xlim = c(5,40),add =T)
legend("topright", c("Congruent","Incongruent"), fill= c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)))


                         
#Hypothesis Testing (dependent t-test,alpha = 95%, one-sided)
t.test(stroop$Congruent,stroop$Incongruent,paired = T,conf.level = 0.95)
