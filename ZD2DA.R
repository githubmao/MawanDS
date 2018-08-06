#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180803, By MaoYan
# 
# 1.分析妈湾出口匝道匝道2的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)
library(magrittr)
library(plyr)


# 数据切分----
df.zd2 <- subset(x = df.dsdata, dsScen == "ZD2")  # 匝道2数据

df.zd2$disTravelled <- ifelse(test = df.zd2$roadName == "ZD_B",
                              df.zd2$disFromRoadStart - 2500,
                              df.zd2$disFromRoadStart + 2621)  # 桩号标定

df.zd2sedan <- subset(x = df.zd2, dsVehicleType == "Sedan")  # 匝道2，轿车
df.zd2truck <- subset(x = df.zd2, dsVehicleType == "Truck")  # 匝道2，货车


# 1 匝道2，行驶速度分析----
# 1.1 轿车，行驶速度----
plot.zd2sedanspeed <- ggplot(data = df.zd2sedan,
                             aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size =1) +
  scale_x_continuous(name = "桩号", limits = c(500, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 500, xmax = 3410,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 3410, y = 100, label = "S2匝道洞口") +
  annotate(geom = "segment", x = 2428, xend = 2478, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2400, y = 100, label = "渐变段") +
  annotate(geom = "segment", x = 2478, xend = 2621, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2600, y = 100, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd2sedanspeed


# 1.2 货车，行驶速度----
plot.zd2truckspeed <- ggplot(data = df.zd2truck,
                             aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size =1) +
  scale_x_continuous(name = "桩号", limits = c(500, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 500, xmax = 3410,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 3410, y = 100, label = "S2匝道洞口") +
  annotate(geom = "segment", x = 2428, xend = 2478, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2400, y = 100, label = "渐变段") +
  annotate(geom = "segment", x = 2478, xend = 2621, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2600, y = 100, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd2truckspeed


# 2 匝道2，轨迹分析----
# 2.1 轿车，轨迹----
df.zd2sedantraj <- CalcBatchTraj(data = df.zd2sedan,
                                 kDriverID = c("S0101", "S0201", "S0301",
                                               "S0401", "S0501", "S0601",
                                               "S0701", "S0801", "S0901",
                                               "S1001", "S1201", "S1301",
                                               "S1501", "S1601", "S1701",
                                               "S1901", "S2001"),
                                 is.main2ramp = TRUE,
                                 is.disdecrease = FALSE)

plot.zd2sedantraj <- ggplot(data = df.zd2sedantraj,
                            aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1)

plot.zd2sedantraj


# 2.2 货车，轨迹----
unique(df.zd2sedan$driverID)

























