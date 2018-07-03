#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180703, By MaoYan
# 
# 1.分析妈湾主线左线的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)


# 数据切分----
df.mainleft <- subset(x = df.dsdata, dsScen == "ZX")  # 左线数据

df.mainleftsedan <- subset(x = df.mainleft,
                           dsVehicleType == "Sedan")  # 左线轿车
df.mainlefttruck <- subset(x = df.mainleft,
                           dsVehicleType == "Truck")  # 左线货车


# 1 左线，行驶速度分析----
# 1.1 左线，行驶速度----
plot.MLsedanspeed <- ggplot(data = df.mainleftsedan,
                            aes(x = disFromRoadStart, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120)) +
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 120, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 120, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))




















