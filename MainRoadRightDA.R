#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180627, By MaoYan
# 
# 1.分析妈湾主线右线的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)


# 数据切分----
df.mainright <- subset(x = df.dsdata,
                       dsScen == "YX" | dsScen == "YXwithTraffic")  # 右线数据

df.mainrightWOT <- subset(x = df.mainright,
                          dsScen == "YX")  # 右线无交通流
df.mainrightWT <- subset(x = df.mainright,
                         dsScen == "YXwithTraffic")  # 右线有交通流

df.mainrightWOTsedan <- subset(x = df.mainrightWOT,
                               dsVehicleType == "Sedan")  # 右线轿车，无交通流
df.mainrightWOTtruck <- subset(x = df.mainrightWOT,
                               dsVehicleType == "Truck")  # 右线货车，无交通流

df.mainrightWTsedan <- subset(x = df.mainrightWT,
                              dsVehicleType == "Sedan")  # 右线轿车，有交通流
df.mainrightWTtruck <- subset(x = df.mainrightWT,
                              dsVehicleType == "Truck")  # 右线货车，有交通流


# 1 右线行驶速度----
# 1.1 右线，行驶速度分析----
# 1.1.1 轿车----
plot.MRWOTsedanspeed <- ggplot(data = df.mainrightWOTsedan,
                               aes(x = disFromRoadStart, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1)

plot.MRWOTsedanspeed


unique(df.dsdata$dsScen)














