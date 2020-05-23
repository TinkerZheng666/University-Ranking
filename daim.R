library(readxl)
a <- read_excel("F:/作业/还未完成的项目汇总整理/550画图/arwu.xlsx")
b <- read_excel("F:/作业/还未完成的项目汇总整理/550画图/QS2019.xlsx")
c <- read_excel("F:/作业/还未完成的项目汇总整理/550画图/USnews.xlsx")
View(a)
a <- a[,-3]
View(b)
View(c)
names(c)[3] <- "School"
###############################################three chart join
data_arwu <- a[,-c(4:5)]
data_QS <- b[,-3]
data_news <- c[,c(1,3,9)]
#rename
names(data_arwu)[1] <- "arwu_worldranking"
names(data_arwu)[3] <- "arwu_USranking"
View(data_arwu)

names(data_QS)[1] <- "QS_worldranking"
names(data_QS)[3] <- "QS_USranking"
View(data_QS)

names(data_news)[3] <- "news_worldranking"
names(data_news)[1] <- "news_USranking"
View(data_news)

#join
library(dplyr)
data1<-data_arwu%>% left_join(data_news, by="School")
View(data1)

data2<-data_arwu%>% left_join(data_QS, by="School")
View(data2)

data3<-data1%>% left_join(data2, by="School")
View(data3)
data <- na.omit(data3)
View(data)

###############################picture1 一张表作图
#chart news
attach(data_news)
us <- data.frame(news_USranking,School)
world <- data.frame(news_worldranking,School)
names(us)[1] <- "ranking"
names(world)[1] <- "ranking"
us$type <- "news_USranking"
world$type <- "news_worldranking"
View(us)
View(world)
news <- rbind(us,world)
View(news)
library(ggplot2)
ggplot(news,aes(x=ranking,fill=type))+geom_density(alpha=0.4)+ggtitle("NEWS ranking US & world")+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()+ theme(axis.title=element_text(face="bold.italic", 
                                                                                                                                    size="12", color="brown"), legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
#chart QS
attach(data_QS)
us <- data.frame(QS_USranking,School)
world <- data.frame(QS_worldranking,School)
names(us)[1] <- "ranking"
names(world)[1] <- "ranking"
us$type <- "QS_USranking"
world$type <- "QS_worldranking"
View(us)
View(world)
str(world)
QS<- rbind(us,world)
View(QS)
QS$ranking <- as.numeric(QS$ranking)
detach(data_QS)          
ggplot(QS,aes(x=ranking,fill=type))+geom_density(alpha=0.4)+ggtitle("QS ranking US & world")+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()+ theme(axis.title=element_text(face="bold.italic", size="12", color="brown"), legend.position="top")+theme(plot.title = element_text(hjust = 0.5))

##################################arwu
data_arwu_1 <- data_arwu[c(47:117),]

us_interval <- table(data_arwu_1$arwu_worldranking)
world_interval <- table(data_arwu_1$arwu_USranking)

us_interval <- as.data.frame(us_interval)
world_interval <- as.data.frame(world_interval)
View(us_interval)
View(world_interval)

names(us_interval)[1] <- "us_interval_ranking"
names(world_interval)[1] <- "world_interval_ranking"

u <- ggplot(us_interval,aes(x=us_interval_ranking,y=Freq,fill=us_interval_ranking))+ geom_bar(stat="identity")+ ggtitle("US interval ranking")+theme(plot.title = element_text(hjust = 0.5))
w <- ggplot(world_interval,aes(x=world_interval_ranking,y=Freq,fill=world_interval_ranking))+ geom_bar(stat="identity")+ ggtitle("world interval ranking")+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(u,w,ncol=2)
##################################################continuous variable
data_arwu_2 <- data_arwu[-c(47:117),]
View(data_arwu_2)
attach(data_arwu_2)
us <- data.frame(arwu_USranking,School)
world <- data.frame(arwu_worldranking,School)
names(us)[1] <- "ranking"
names(world)[1] <- "ranking"
us$type <- "arwu_USranking"
world$type <- "arwu_worldranking"
View(us)
View(world)
arwu <- rbind(us,world)
View(arwu)
library(ggplot2)
arwu$ranking <- as.numeric(arwu$ranking)
ggplot(arwu,aes(x=ranking,fill=type))+geom_density(alpha=0.4)+ggtitle("arwu ranking US & world")+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()+ theme(axis.title=element_text(face="bold.italic", 
                                                                                                                                                                                          size="12", color="brown"), legend.position="top")+theme(plot.title = element_text(hjust = 0.5))


############################################三个表合并后画图，区间数据求均值
View(data3)
View(data)
library(stringr)
data3$arwu_worldranking.y
data3$arwu_USranking.y
attach(data3)
interval <- data.frame(arwu_USranking.y,arwu_worldranking.y)
View(interval)

#############提取区间数据
interval$ulow <- substr(interval$arwu_USranking.y,1,2)
interval$uup <- substr(interval$arwu_USranking.y,4,5)

interval$wlow <- substr(interval$arwu_worldranking.y,1,3)
interval$wup <- substr(interval$arwu_worldranking.y,5,7)

interval$ulow <- as.numeric(interval$ulow)
interval$uup <- as.numeric(interval$uup)
interval$wlow <- as.numeric(interval$wlow)
interval$wup <- as.numeric(interval$wup)
interval[is.na(interval)] <- 0

interval_1 <- interval[c(1:46),]
interval_1$times <- 1
interval <- rbind(interval_1,interval_2)
View(interval)
interval_2 <- interval[-c(1:46),]
str(interval)

interval$ave_us <- (interval$ulow+interval$uup)/interval$times
interval$ave_world <- (interval$wlow+interval$wup)/interval$times
###合并到原来的数据集
mydata <- cbind(data3,interval)
View(mydata)
mydata <- mydata[,-c(1,3,6,7,10,11)]
write.csv(mydata,"F:/作业/还未完成的项目汇总整理/550画图/mydata.csv")
mydata <- read.csv("F:/作业/还未完成的项目汇总整理/550画图/mydata.csv")
View(mydata)
#miss value
install.packages("mice")
library(mice)
a <- md.pattern(mydata)
View(a)
library(VIM)
aggr(mydata,prop=T,numbers=T)
####################################如何统计每行缺失值的个数
attach(mydata)
na <- data.frame(news_USranking,news_worldranking,QS_worldranking,QS_USranking)
View(na)
na[!is.na(na)] <- 0
na[is.na(na)] <- 1
na$na_times_us <- na$news_USranking+na$QS_USranking
na$na_times_world <- na$news_worldranking+na$QS_worldranking

detach(mydata)
mydata <- cbind(na,mydata)
View(mydata)
mydata[is.na(mydata)] <- 0
###################################三个表榜单的均值
str(mydata)
mydata <- mydata[,-c(1:5)]
mydata <- mydata[,-c(3:8)]

mydata$US <- (mydata$ave_us+mydata$QS_USranking+mydata$news_USranking)/(3-mydata$na_times_us)
mydata$world <- (mydata$ave_world+mydata$news_worldranking.1+mydata$QS_worldranking.1)/(3-mydata$na_times_world)

attach(mydata)
us <- data.frame(US,School)
world <- data.frame(world,School)
names(us)[1] <- "ranking"
names(world)[1] <- "ranking"
us$type <- "USranking"
world$type <- "worldranking"
View(us)
View(world)
total <- rbind(us,world)
View(total)

str(total)
total$ranking <- as.numeric(total$ranking)
ggplot(total,aes(x=ranking,fill=type))+geom_density(alpha=0.4)+ggtitle("Total ranking US & world")+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()+ theme(axis.title=element_text(face="bold.italic", 
                                                                                           size="12", color="brown"), legend.position="top")+theme(plot.title = element_text(hjust = 0.5))


####################其它图形
View(data3)

ggplot(c,aes(x=c$Ranking,y=c$`Enroll rate`))+geom_point()+ggtitle("The relatinship between US Ranking and Enrollrate")+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()+ theme(axis.title=element_text(face="bold.italic", 
                                                                                                                                                                                        size="12", color="brown"), legend.position="top")+theme(plot.title = element_text(hjust = 0.5))

ggplot(c,aes(x=c$`World ranking`,y=c$`Enroll rate`))+geom_point()+ggtitle("The relatinship between world Ranking and Enrollrate")+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()+ theme(axis.title=element_text(face="bold.italic", 
                                                                                                                                                                                                                        size="12", color="brown"), legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
#character
c$STA <- substr(c$SAT均分,6,9)
c$ACT <- substr(c$ACT均分,4,5)

c$STA <- as.numeric(c$STA)
c$ACT <- as.numeric(c$ACT)

ggplot(c,aes(x=c$Ranking,y=c$STA))+geom_point()+ggtitle("The relatinship between US Ranking and STA")+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()+ theme(axis.title=element_text(face="bold.italic",size="12", color="brown"), legend.position="top")+theme(plot.title = element_text(hjust = 0.5))

ggplot(c,aes(x=c$`World ranking`,y=c$STA))+geom_point()+ggtitle("The relatinship between world Ranking and STA")+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()+ theme(axis.title=element_text(face="bold.italic",size="12", color="brown"), legend.position="top")+theme(plot.title = element_text(hjust = 0.5))



ggplot(c,aes(x=c$Ranking,y=c$ACT))+geom_point()+ggtitle("The relatinship between US Ranking and ACT")+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()+ theme(axis.title=element_text(face="bold.italic",size="12", color="brown"), legend.position="top")+theme(plot.title = element_text(hjust = 0.5))

ggplot(c,aes(x=c$`World ranking`,y=c$ACT))+geom_point()+ggtitle("The relatinship between world Ranking and ACT")+theme(plot.title = element_text(hjust = 0.5))+ theme_bw()+ theme(axis.title=element_text(face="bold.italic",size="12", color="brown"), legend.position="top")+theme(plot.title = element_text(hjust = 0.5))

############################找出差距最大的值
mydata$diff <- mydata$US-mydata$world
sort(mydata$diff)
attach(mydata)
diffm<- data.frame(School,diff)
diffm <-diffm[,-2] 
View(diffm)
head(diffm)
diff <- diffm[order(diffm[,2],decreasing=F),]
View(diff)
write.csv(diff,"F:/作业/还未完成的项目汇总整理/550画图/diff.csv")

#############回归
a <- read_excel("F:/作业/还未完成的项目汇总整理/550画图/arwu.xlsx")
b <- read_excel("F:/作业/还未完成的项目汇总整理/550画图/QS2019.xlsx")
c <- read_excel("F:/作业/还未完成的项目汇总整理/550画图/USnews.xlsx")
View(a)
str(a)
a$`World Ranking` <- as.numeric(a$`World Ranking`)
a$`US Ranking` <- as.numeric(a$`US Ranking`)
fit_arwu <- lm(a$`World Ranking`~a$`US Ranking`)
summary(fit_arwu)

View(b)
str(b)
b$`World Ranking` <- as.numeric(b$`World Ranking`)
b$`US Ranking` <- as.numeric(b$`US Ranking`)
fit_QS <- lm(b$`World Ranking`~b$`US Ranking`)
summary(fit_QS)

View(c)
c$STA <- substr(c$SAT均分,6,9)
c$ACT <- substr(c$ACT均分,4,5)

c$STA <- as.numeric(c$STA)
c$ACT <- as.numeric(c$ACT)
str(c)
fit_news_world <- lm(c$`World ranking`~c$ACT+c$STA+c$`Enroll rate`)
summary(fit_news_world)

fit_news_US <- lm(c$Ranking~c$ACT+c$STA+c$`Enroll rate`)
summary(fit_news_US)



fit_news <- lm(c$`World ranking`~c$Ranking)
summary(fit_news)


