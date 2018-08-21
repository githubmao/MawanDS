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
# 2.1 轿车，轨迹1----
df.zd2sedantraj <- CalcBatchTraj(data = df.zd2sedan,
                                 kDriverID = c("S0101", "S0201", "S0301",
                                               "S0401", "S0501", "S0601",
                                               "S0701", "S0801", "S0901",
                                               "S1001", "S1201", "S1301",
                                               "S1501", "S1601", "S1701",
                                               "S1901", "S2001"),
                                 is.main2ramp = TRUE,
                                 is.disdecrease = FALSE)

plot.zd2sedantraj1 <- ggplot(data = df.zd2sedantraj,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2sedantraj1


# 2.2 轿车，轨迹2----
plot.zd2sedantraj2 <- ggplot(data = df.zd2sedantraj,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 2000, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 2000, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(2000, 2000), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 2000, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2450, 2450), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2550, 2550), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(2000, 3600),
                     breaks = c(2428, 2478, 2621, 3410),
                     labels = c("", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2sedantraj2


# 2.3 货车，轨迹1----
df.zd2trucktraj <- CalcBatchTraj(data = df.zd2truck,
                                 kDriverID = c("T0101", "T0201", "T0301",
                                               "T0401", "T0501", "T0601",
                                               "T0701"),
                                 is.main2ramp = TRUE,
                                 is.disdecrease = FALSE)

plot.zd2trucktraj1 <- ggplot(data = df.zd2trucktraj,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2trucktraj1


# 2.4 货车，轨迹2----
plot.zd2trucktraj2 <- ggplot(data = df.zd2trucktraj,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 2000, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 2000, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(2000, 2000), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 2000, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2450, 2450), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2550, 2550), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(2000, 3600),
                     breaks = c(2428, 2478, 2621, 3410),
                     labels = c("", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2trucktraj2


# 3 匝道2，车道跨越点分析----
# 3.1 轿车，车道跨越点分析----
df.zd2sedanLCpoints <- CalcBatchLCPoint(data = df.zd2sedantraj,
                                        kDriverID = c("S0101", "S0201",
                                                      "S0301", "S0401",
                                                      "S0501", "S0601",
                                                      "S0701", "S0801",
                                                      "S0901", "S1001",
                                                      "S1201", "S1301",
                                                      "S1501", "S1601",
                                                      "S1701", "S1901",
                                                      "S2001"),
                                        kLatDis = 11.45,
                                        is.main2ramp = TRUE,
                                        is.disdecrease = FALSE)
df.zd2sedanLCpoints <- df.zd2sedanLCpoints[-18,]  # 特殊处理
median(df.zd2sedanLCpoints$disTravelled)
plot.zd2sedanLCpoints <- ggplot(data = df.zd2sedanLCpoints,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zd2sedanLCpoints$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zd2sedanLCpoints$disTravelled) + 120,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zd2sedanLCpoints$disTravelled),
           xend = median(df.zd2sedanLCpoints$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zd2sedanLCpoints$disTravelled), xend = 2428,
           y = 20, yend = 20, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2420, y = 18,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点24m")) +
  annotate(geom = "segment",
           x = median(df.zd2sedanLCpoints$disTravelled), xend = 2478,
           y = 20, yend = 20, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2490, y = 22,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段终点27m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 2300, xend = 2800, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 2300, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(2300, 2300), xend = c(2800, 2800),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 2800, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 2800, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 2800, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 2800, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 2800),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 2300, xmax = 2800,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
#  annotate(geom = "text", x = 3480, y = 1.5,
#           fontface="bold", size = 4,
#           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2450, 2450), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2550, 2550), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(2300, 2800),
                     breaks = c(2428, 2478, 2621),
                     labels = c("", "",
                                "S2K0+144.039")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2sedanLCpoints


# 3.2 货车，车道跨越点L3->L2----
df.zd2truckLCpoints1 <- CalcBatchLCPoint(data = df.zd2trucktraj,
                                         kDriverID = c("T0101", "T0201",
                                                       "T0301", "T0401",
                                                       "T0501", "T0601",
                                                       "T0701"),
                                         kLatDis = 3.82,
                                         is.main2ramp = TRUE,
                                         is.disdecrease = FALSE)

plot.zd2truckLCpoints1 <- ggplot(data = df.zd2truckLCpoints1,
                                 aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zd2truckLCpoints1$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zd2truckLCpoints1$disTravelled) + 400,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值 LK4+609")) +
  annotate(geom = "segment",
           x = median(df.zd2truckLCpoints1$disTravelled),
           xend = median(df.zd2truckLCpoints1$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
#  annotate(geom = "segment",
#           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
#           fontface = "bold", size = 4,
#           label = c("渐变段", "50.04m")) +
#  annotate(geom = "segment",
#           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
#           fontface = "bold", size = 4,
#           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2truckLCpoints1


# 3.3 货车，车道跨越点L2->L1----
df.zd2truckLCpoints2 <- CalcBatchLCPoint(data = df.zd2trucktraj,
                                         kDriverID = c("T0101", "T0201",
                                                       "T0301", "T0401",
                                                       "T0501", "T0601",
                                                       "T0701"),
                                         kLatDis = 7.64,
                                         is.main2ramp = TRUE,
                                         is.disdecrease = FALSE)

plot.zd2truckLCpoints2 <- ggplot(data = df.zd2truckLCpoints2,
                                 aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zd2truckLCpoints2$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zd2truckLCpoints2$disTravelled) + 400,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值 LK2+379")) +
  annotate(geom = "segment",
           x = median(df.zd2truckLCpoints2$disTravelled),
           xend = median(df.zd2truckLCpoints2$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zd2truckLCpoints2$disTravelled), xend = 2428,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2428, y = 2,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点148m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
#  annotate(geom = "segment",
#           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
#           fontface = "bold", size = 4,
#           label = c("渐变段", "50.04m")) +
#  annotate(geom = "segment",
#           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
#           fontface = "bold", size = 4,
#           label = c("减速段", "142.65m")) +
# 坐标轴及图例等修改
scale_x_continuous(name = NULL, limits = c(0, 3600),
                   breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                   labels = c("LK4+160", "LK3+160", "", "",
                              "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2truckLCpoints2


# 3.4 货车，车道跨越点L1->Ramp----
df.zd2truckLCpoints3 <- CalcBatchLCPoint(data = df.zd2trucktraj,
                                         kDriverID = c("T0101", "T0201",
                                                       "T0301", "T0401",
                                                       "T0501", "T0601",
                                                       "T0701"),
                                         kLatDis = 11.45,
                                         is.main2ramp = TRUE,
                                         is.disdecrease = FALSE)

plot.zd2truckLCpoints3 <- ggplot(data = df.zd2truckLCpoints3,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zd2truckLCpoints3$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zd2truckLCpoints3$disTravelled) + 120,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zd2truckLCpoints3$disTravelled),
           xend = median(df.zd2truckLCpoints3$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zd2truckLCpoints3$disTravelled), xend = 2428,
           y = 20, yend = 20, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2420, y = 18,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点50m")) +
  annotate(geom = "segment",
           x = median(df.zd2truckLCpoints3$disTravelled), xend = 2478,
           y = 20, yend = 20, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2490, y = 22,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段终点10m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 2300, xend = 2800, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 2300, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(2300, 2300), xend = c(2800, 2800),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 2800, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 2800, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 2800, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 2800, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 2800),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 2300, xmax = 2800,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
#  annotate(geom = "text", x = 3480, y = 1.5,
#           fontface="bold", size = 4,
#           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2450, 2450), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2550, 2550), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(2300, 2800),
                     breaks = c(2428, 2478, 2621),
                     labels = c("", "",
                                "S2K0+144.039")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2truckLCpoints3


# 4 匝道2，加速度分析----
# 4.1 轿车，加速度----
plot.zd2sedanacc <- ggplot(data = df.zd2sedan,
                           aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 3600),
                     breaks = c(0, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 3410, y = 5, label = "S2匝道洞口") +
  annotate(geom = "segment", x = 2428, xend = 2478, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2400, y = 5, label = "渐变段") +
  annotate(geom = "segment", x = 2478, xend = 2621, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2600, y = 5, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd2sedanacc


# 4.2 货车，加速度----
plot.zd2truckacc <- ggplot(data = df.zd2truck,
                           aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 3600),
                     breaks = c(0, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 3410, y = 5, label = "S2匝道洞口") +
  annotate(geom = "segment", x = 2428, xend = 2478, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2400, y = 5, label = "渐变段") +
  annotate(geom = "segment", x = 2478, xend = 2621, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2600, y = 5, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd2truckacc


# 5 匝道2，制动踏板位移分析----
# 5.1 轿车，制动踏板位移----
plot.zd2sedanbrakepedal <- ggplot(data = df.zd2sedan,
                                  aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 3600),
                     breaks = c(0, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 3410, y = 1, label = "S2匝道洞口") +
  annotate(geom = "segment", x = 2428, xend = 2478, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2400, y = 1, label = "渐变段") +
  annotate(geom = "segment", x = 2478, xend = 2621, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2600, y = 1, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd2sedanbrakepedal


# 5.2 货车，制动踏板位移----
plot.zd2truckbrakepedal <- ggplot(data = df.zd2truck,
                                  aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 3600),
                     breaks = c(0, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 3410, y = 1, label = "S2匝道洞口") +
  annotate(geom = "segment", x = 2428, xend = 2478, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2400, y = 1, label = "渐变段") +
  annotate(geom = "segment", x = 2478, xend = 2621, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2600, y = 1, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd2truckbrakepedal


# 6 匝道2，油门踏板位移分析----
# 6.1 轿车，油门踏板----
plot.zd2sedangaspedal <- ggplot(data = df.zd2sedan,
                                aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 3600),
                     breaks = c(0, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 3410, y = 1, label = "S2匝道洞口") +
  annotate(geom = "segment", x = 2428, xend = 2478, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2400, y = 1, label = "渐变段") +
  annotate(geom = "segment", x = 2478, xend = 2621, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2600, y = 1, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd2sedangaspedal


# 6.2 货车，油门踏板----
plot.zd2truckgaspedal <- ggplot(data = df.zd2truck,
                                aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 3600),
                     breaks = c(0, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 3410, y = 1, label = "S2匝道洞口") +
  annotate(geom = "segment", x = 2428, xend = 2478, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2400, y = 1, label = "渐变段") +
  annotate(geom = "segment", x = 2478, xend = 2621, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2600, y = 1, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd2truckgaspedal


# 7 关键位置速度分布分析----
# 7.1 轿车，关键位置速度分布----
df.zd2sedanspotspeed <- CalcBatchSpotSpeed(data = df.zd2sedan,
                                           kDriverID = c("S0101", "S0201",
                                                         "S0301", "S0401",
                                                         "S0501", "S0601",
                                                         "S0701", "S0801",
                                                         "S0901", "S1001",
                                                         "S1201", "S1301",
                                                         "S1501", "S1601",
                                                         "S1701", "S1901",
                                                         "S2001"),
                                           kDis = c(2428, 2478, 2621),
                                           is.disdecrease = FALSE,
                                           kTag = c("transSTRT",
                                                    "transEnd/decSTRT",
                                                    "decEnd"),
                                           kDisType = "Dis")  # 特征位置速度

df.zd2sedanspotspeed <- subset(df.zd2sedanLCpoints,
                               select = c("rowNo", "disTravelled",
                                          "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint")) %>%
  rbind(df.zd2sedanspotspeed)

# 特征点速度统计量计算
plotdf.zd2sedanspotspeed <- ddply(df.zd2sedanspotspeed,
                                  .(disTag),
                                  summarize,
                                  meanSpeed = mean(speedKMH),
                                  sdSpeed = sd(speedKMH))

plot.zd2sedanspotspeed <- ggplot(data = plotdf.zd2sedanspotspeed,
                                 aes(x = disTag, y = meanSpeed)) +
  geom_bar(stat = "identity", width = 0.15) +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                width = 0.1, size =1) +
  scale_x_discrete(limits = c("transSTRT", "LCPoint",
                              "transEnd/decSTRT", "decEnd"),
                   labels = c("渐变段起点", "换道位置",
                              "渐变段终点\n(减速段起点)", "减速段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zd2sedanspotspeed


# 7.2 货车，关键位置速度分布----
df.zd2truckspotspeed <- CalcBatchSpotSpeed(data = df.zd2truck,
                                           kDriverID = c("T0101", "T0201",
                                                         "T0301", "T0401",
                                                         "T0501", "T0601",
                                                         "T0701"),
                                           kDis = c(2428, 2478, 2621),
                                           is.disdecrease = FALSE,
                                           kTag = c("transSTRT",
                                                    "transEnd/decSTRT",
                                                    "decEnd"),
                                           kDisType = "Dis")  # 特征位置速度

# 货车车道跨越点1，L3->L2速度
df.zd2truckspotspeed <- subset(df.zd2truckLCpoints1,
                               select = c("rowNo", "disTravelled",
                                          "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint1")) %>%
  rbind(df.zd2truckspotspeed)

# 货车车道跨越点2，L2->L1速度
df.zd2truckspotspeed <- subset(df.zd2truckLCpoints2,
                               select = c("rowNo", "disTravelled",
                                          "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint2")) %>%
  rbind(df.zd2truckspotspeed)

# 货车车道跨越点3，L1->Ramp速度
df.zd2truckspotspeed <- subset(df.zd2truckLCpoints3,
                               select = c("rowNo", "disTravelled",
                                          "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint3")) %>%
  rbind(df.zd2truckspotspeed)

# 特征点速度统计量计算
plotdf.zd2truckspotspeed <- ddply(df.zd2truckspotspeed,
                                  .(disTag),
                                  summarize,
                                  meanSpeed = mean(speedKMH),
                                  sdSpeed = sd(speedKMH))

plot.zd2truckspotspeed <- ggplot(data = plotdf.zd2truckspotspeed,
                                 aes(x = disTag, y = meanSpeed)) +
  geom_bar(stat = "identity", width = 0.15) +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                width = 0.1, size =1) +
  scale_x_discrete(limits = c("LCPoint1", "LCPoint2", "transSTRT",
                              "LCPoint3", "transEnd/decSTRT", "decEnd"),
                   labels = c("主线内侧车道->\n主线中间车道\n换道位置",
                              "主线中间车道->\n主线外侧车道\n换道位置",
                              "渐变段起点",
                              "主线外侧车道->匝道\n换道位置",
                              "渐变段终点\n(减速段起点)",
                              "减速段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zd2truckspotspeed


# 8 开始制动位置分布----
# 8.1 轿车，开始制动位置分布----
df.zd2sedanappbrake <- CalcBatchAppBrake(data = df.zd2sedantraj,
                                         kDriverID = c("S0101", "S0201",
                                                       "S0301", "S0401",
                                                       "S0501", "S0601",
                                                       "S0701", "S0801",
                                                       "S0901", "S1001",
                                                       "S1201", "S1301",
                                                       "S1501", "S1601",
                                                       "S1701", "S1901",
                                                       "S2001"),
                                         is.disdecrease = FALSE,
                                         kPedalMod = 0.01,
                                         kCalcBrakeType = "BrakePedal")

plot.zd2sedanappbrake <- ggplot(data = df.zd2sedanappbrake,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2sedanappbrake


# 8.2 货车，开始制动位置分布----
df.zd2truckappbrake <- CalcBatchAppBrake(data = df.zd2trucktraj,
                                         kDriverID = c("T0101", "T0201",
                                                       "T0301", "T0401",
                                                       "T0501", "T0601",
                                                       "T0701"),
                                         is.disdecrease = FALSE,
                                         kPedalMod = 0.01,
                                         kCalcBrakeType = "BrakePedal")

plot.zd2truckappbrake <- ggplot(data = df.zd2truckappbrake,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2truckappbrake


# 9 开始加速位置分布----
# 9.1 轿车，开始加速位置分布----
df.zd2sedanappgas <- CalcBatchAppGas(data = df.zd2sedantraj,
                                     kDriverID = c("S0101", "S0201",
                                                   "S0301", "S0401",
                                                   "S0501", "S0601",
                                                   "S0701", "S0801",
                                                   "S0901", "S1001",
                                                   "S1201", "S1301",
                                                   "S1501", "S1601",
                                                   "S1701", "S1901",
                                                   "S2001"),
                                     is.disdecrease = FALSE,
                                     kPedalMod = 0.01)

plot.zd2sedanappgas <- ggplot(data = df.zd2sedanappgas,
                              aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2sedanappgas


# 9.2 货车，开始加速位置分布----
df.zd2truckappgas <- CalcBatchAppGas(data = df.zd2trucktraj,
                                     kDriverID = c("T0101", "T0201",
                                                   "T0301", "T0401",
                                                   "T0501", "T0601",
                                                   "T0701"),
                                     is.disdecrease = FALSE,
                                     kPedalMod = 0.01)

plot.zd2truckappgas <- ggplot(data = df.zd2truckappgas,
                              aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2truckappgas


# 10 大减速度分布分析----
# 10.1 轿车，大减速度分布----
# 10.1.1 轿车，大减速度，个体比较----
df.zd2sedandecoutlier1 <- CalcBatchAccOutliers(data = df.zd2sedantraj,
                                               kDriverID = c("S0101", "S0201",
                                                             "S0301", "S0401",
                                                             "S0501", "S0601",
                                                             "S0701", "S0801",
                                                             "S0901", "S1001",
                                                             "S1201", "S1301",
                                                             "S1501", "S1601",
                                                             "S1701", "S1901",
                                                             "S2001"),
                                               kQuantile = 0.05,
                                               kAccLimit = NA,
                                               kOutliersType = "Dec",
                                               is.indv = TRUE)

plot.zd2sedandecoutlier1 <- ggplot(data = df.zd2sedandecoutlier1,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2sedandecoutlier1


# 10.1.2 轿车，大减速度，群体比较----
df.zd2sedandecoutlier2 <- CalcBatchAccOutliers(data = df.zd2sedantraj,
                                               kDriverID = c("S0101", "S0201",
                                                             "S0301", "S0401",
                                                             "S0501", "S0601",
                                                             "S0701", "S0801",
                                                             "S0901", "S1001",
                                                             "S1201", "S1301",
                                                             "S1501", "S1601",
                                                             "S1701", "S1901",
                                                             "S2001"),
                                               kQuantile = NA,
                                               kAccLimit = -3.0,
                                               kOutliersType = "Dec",
                                               is.indv = FALSE)

plot.zd2sedandecoutlier2 <- ggplot(data = df.zd2sedandecoutlier2,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2sedandecoutlier2


# 10.2 货车，大减速度----
# 10.2.1 货车，大减速度，个体比较----
df.zd2truckdecoutlier1 <- CalcBatchAccOutliers(data = df.zd2trucktraj,
                                               kDriverID = c("T0101", "T0201",
                                                             "T0301", "T0401",
                                                             "T0501", "T0601",
                                                             "T0701"),
                                               kQuantile = 0.05,
                                               kAccLimit = NA,
                                               kOutliersType = "Dec",
                                               is.indv = TRUE)

plot.zd2truckdecoutlier1 <- ggplot(data = df.zd2truckdecoutlier1,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2truckdecoutlier1


# 10.2.2 货车，大减速度，群体比较----
df.zd2truckdecoutlier2 <- CalcBatchAccOutliers(data = df.zd2trucktraj,
                                               kDriverID = c("T0101", "T0201",
                                                             "T0301", "T0401",
                                                             "T0501", "T0601",
                                                             "T0701"),
                                               kQuantile = NA,
                                               kAccLimit = -3.0,
                                               kOutliersType = "Dec",
                                               is.indv = FALSE)

plot.zd2truckdecoutlier2 <- ggplot(data = df.zd2truckdecoutlier2,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3600, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2428, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3600, 3600),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2526, xend = 3600, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2621, xend = 3600, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2621, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2621, xmax = 3600, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2526, xend = 2621, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2549, 2573, 2597), xend = c(2621, 2621, 2621),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2428, xend = 2526, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2621, xend = 3600, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3410,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 3480, y = 1.5,
           fontface="bold", size = 4,
           label = "S2匝道洞口") +
  annotate(geom = "segment",
           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3600),
                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
                     labels = c("LK4+160", "LK3+160", "", "",
                                "S2K0+144.039", "S2K0+956.173")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd2truckdecoutlier2




