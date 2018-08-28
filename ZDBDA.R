#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180824, By MaoYan
# 
# 1.分析妈湾出口匝道匝道B的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)
library(magrittr)
library(plyr)


# 数据切分----
df.zdb <- subset(x = df.dsdata,
                 dsScen == "ZDB" | dsScen == "ZDBwithTraffic")  # 匝道B数据

# 标定匝道B数据
df.zdb$disTravelled <- df.zdb$disFromRoadStart  # 桩号标定


df.zdbWOT <- subset(x = df.zdb, dsScen == "ZDB")  # 匝道B，无交通流
df.zdbWT <- subset(x = df.zdb, dsScen == "ZDBwithTraffic")  # 匝道B，有交通流

df.zdbWOTsedan <- subset(x = df.zdbWOT,
                         dsVehicleType == "Sedan")  # 匝道B轿车，无交通流
df.zdbWOTtruck <- subset(x = df.zdbWOT,
                         dsVehicleType == "Truck")  # 匝道B货车，无交通流

df.zdbWTsedan <- subset(x = df.zdbWT,
                        dsVehicleType == "Sedan")  # 匝道B轿车，有交通流
df.zdbWTtruck <- subset(x = df.zdbWT,
                        dsVehicleType == "Truck")  # 匝道B货车，有交通流


# 1 匝道B，行驶速度分析----
# 1.1 轿车，行驶速度----
# 1.1.1 轿车，行驶速度，无交通流----
plot.zdbWOTsedanspeed <- ggplot(data = df.zdbWOTsedan,
                                aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 100, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 100, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWOTsedanspeed


# 1.1.2 轿车，行驶速度，有交通流----
plot.zdbWTsedanspeed <- ggplot(data = df.zdbWTsedan,
                               aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 100, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 100, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWTsedanspeed


# 1.2 货车，行驶速度----
plot.zdbWOTtruckspeed <- ggplot(data = df.zdbWOTtruck,
                                aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 100, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 100, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWOTtruckspeed


# 2 匝道B，轨迹分析----
# 2.1 轿车，轨迹----
# 2.1.1 轿车，轨迹，无交通流----
df.zdbWOTsedantraj <- transform(df.zdbWOTsedan,
                                drivingTraj = disToLeftBorder)

plot.zdbWOTsedantraj <- ggplot(data = df.zdbWOTsedantraj,
                               aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTsedantraj


# 2.1.2 轿车，轨迹，有交通流----
df.zdbWTsedantraj <- transform(df.zdbWTsedan,
                               drivingTraj = disToLeftBorder)

plot.zdbWTsedantraj <- ggplot(data = df.zdbWTsedantraj,
                              aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWTsedantraj


# 2.2 货车，轨迹----
df.zdbWOTtrucktraj <- transform(df.zdbWOTtruck,
                                drivingTraj = disToLeftBorder)

plot.zdbWOTtrucktraj <- ggplot(data = df.zdbWOTtrucktraj,
                               aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTtrucktraj


# 3 匝道B，车道跨越点分析----
# 3.1 轿车，车道跨越点----
# 3.1.1 轿车，车道跨越点，无交通流----
# 3.1.1.1 轿车，车道跨越点1，无交通流----
df.zdbWOTsedanLCpoint1 <- CalcBatchLCRtBrdr(data = df.zdbWOTsedantraj,
                                            kDriverID = c("S0101", "S0201",
                                                          "S0301", "S0401",
                                                          "S0501", "S0601",
                                                          "S0701", "S0801",
                                                          "S0901", "S1001",
                                                          "S1101", "S1201",
                                                          "S1301", "S1401",
                                                          "S1501", "S1601",
                                                          "S1701", "S1801",
                                                          "S1901", "S2001"),
                                            kRtBrdr = 7.65,
                                            is.disdecrease = FALSE)

plot.zdbWOTsedanLCpoint1 <- ggplot(data = df.zdbWOTsedanLCpoint1,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zdbWOTsedanLCpoint1$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zdbWOTsedanLCpoint1$disTravelled) + 170,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zdbWOTsedanLCpoint1$disTravelled),
           xend = median(df.zdbWOTsedanLCpoint1$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zdbWOTsedanLCpoint1$disTravelled), xend = 786,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(750, 750), y = c(2, 5),
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段终点", "78m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTsedanLCpoint1


# 3.1.1.2 轿车，车道跨越点2，无交通流----
df.zdbWOTsedanLCpoint2 <- CalcBatchLCRtBrdr(data = df.zdbWOTsedantraj,
                                            kDriverID = c("S0101", "S0201",
                                                          "S0301", "S0401",
                                                          "S0501", "S0601",
                                                          "S0701", "S0801",
                                                          "S0901", "S1001",
                                                          "S1101", "S1201",
                                                          "S1301", "S1401",
                                                          "S1501", "S1601",
                                                          "S1701", "S1801",
                                                          "S1901", "S2001"),
                                            kRtBrdr = 3.85,
                                            is.disdecrease = FALSE)

plot.zdbWOTsedanLCpoint2 <- ggplot(data = df.zdbWOTsedanLCpoint2,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zdbWOTsedanLCpoint2$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zdbWOTsedanLCpoint2$disTravelled) + 170,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zdbWOTsedanLCpoint2$disTravelled),
           xend = median(df.zdbWOTsedanLCpoint2$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zdbWOTsedanLCpoint2$disTravelled), xend = 706,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 730, y = 2,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点52m")) +
  annotate(geom = "segment",
           x = median(df.zdbWOTsedanLCpoint2$disTravelled), xend = 786,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 770, y = 5,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段终点28m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTsedanLCpoint2


# 3.1.2 轿车，车道跨越点，有交通流----
# 3.1.2.1 轿车，车道跨越点1，有交通流----
df.zdbWTsedanLCpoint1 <- CalcBatchLCRtBrdr(data = df.zdbWTsedantraj,
                                           kDriverID = c("S0101", "S0201",
                                                         "S0301", "S0401",
                                                         "S0501", "S0601",
                                                         "S0701", "S0801",
                                                         "S0901", "S1001",
                                                         "S1101", "S1201",
                                                         "S1301", "S1401",
                                                         "S1501", "S1601",
                                                         "S1701", "S1801",
                                                         "S1901", "S2001"),
                                           kRtBrdr = 7.65,
                                           is.disdecrease = FALSE)

plot.zdbWTsedanLCpoint1 <- ggplot(data = df.zdbWTsedanLCpoint1,
                                  aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zdbWTsedanLCpoint1$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zdbWTsedanLCpoint1$disTravelled) + 170,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zdbWTsedanLCpoint1$disTravelled),
           xend = median(df.zdbWTsedanLCpoint1$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zdbWTsedanLCpoint1$disTravelled), xend = 706,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 730, y = 2,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点8m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWTsedanLCpoint1


# 3.1.2.2 轿车，车道跨越点2，有交通流----
df.zdbWTsedanLCpoint2 <- CalcBatchLCRtBrdr(data = df.zdbWTsedantraj,
                                           kDriverID = c("S0101", "S0201",
                                                         "S0301", "S0401",
                                                         "S0501", "S0601",
                                                         "S0701", "S0801",
                                                         "S0901", "S1001",
                                                         "S1101", "S1201",
                                                         "S1301", "S1401",
                                                         "S1501", "S1601",
                                                         "S1701", "S1801",
                                                         "S1901", "S2001"),
                                           kRtBrdr = 3.85,
                                           is.disdecrease = FALSE)

# 过滤干扰点
df.zdbWTsedanLCpoint2 <- subset(df.zdbWTsedanLCpoint2, disTravelled > 486)

plot.zdbWTsedanLCpoint2 <- ggplot(data = df.zdbWTsedanLCpoint2,
                                  aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zdbWTsedanLCpoint2$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zdbWTsedanLCpoint2$disTravelled) + 170,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zdbWTsedanLCpoint2$disTravelled),
           xend = median(df.zdbWTsedanLCpoint2$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zdbWTsedanLCpoint2$disTravelled), xend = 706,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 730, y = 2,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点35m")) +
  annotate(geom = "segment",
           x = median(df.zdbWTsedanLCpoint2$disTravelled), xend = 786,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 770, y = 5,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段终点45m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWTsedanLCpoint2


# 3.2 货车，车道跨域点----
df.zdbWOTtruckLCpoint <- CalcBatchLCRtBrdr(data = df.zdbWOTtrucktraj,
                                            kDriverID = c("T0101", "T0201",
                                                          "T0301", "T0401",
                                                          "T0501", "T0701"),
                                            kRtBrdr = 7.65,
                                            is.disdecrease = FALSE)

plot.zdbWOTtruckLCpoint <- ggplot(data = df.zdbWOTtruckLCpoint,
                                  aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zdbWOTtruckLCpoint$disTravelled) + 170,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zdbWOTtruckLCpoint$disTravelled),
           xend = median(df.zdbWOTtruckLCpoint$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zdbWOTtruckLCpoint$disTravelled), xend = 706,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 730, y = 2,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点29m")) +
  annotate(geom = "segment",
           x = median(df.zdbWOTtruckLCpoint$disTravelled), xend = 786,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 770, y = 5,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段终点51m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTtruckLCpoint


# 4 匝道B，加速度分析----
# 4.1 轿车，加速度----
# 4.1.1 轿车，加速度，无交通流----
plot.zdbWOTsedanacc <- ggplot(data = df.zdbWOTsedan,
                              aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 5, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWOTsedanacc


# 4.1.2 轿车，加速度，有交通流----
plot.zdbWTsedanacc <- ggplot(data = df.zdbWTsedan,
                             aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 5, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWTsedanacc


# 4.2 货车，加速度----
plot.zdbWOTtruckacc <- ggplot(data = df.zdbWOTtruck,
                              aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 5, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWOTtruckacc


# 5 匝道B，制动踏板位移分析----
# 5.1 轿车，制动踏板位移----
# 5.1.1 轿车，制动踏板位移，无交通流----
plot.zdbWOTsedanbrakepedal <- ggplot(data = df.zdbWOTsedan,
                                     aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWOTsedanbrakepedal


# 5.1.2 轿车，制动踏板位移，有交通流----
plot.zdbWTsedanbrakepedal <- ggplot(data = df.zdbWTsedan,
                                    aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWTsedanbrakepedal


# 5.2 货车，制动踏板位移----
plot.zdbWOTtruckbrakepedal <- ggplot(data = df.zdbWOTtruck,
                                     aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWOTtruckbrakepedal


# 6 匝道B，油门踏板位移分析----
# 6.1 轿车，油门踏板位移----
# 6.1.1 轿车，油门踏板位移，无交通流----
plot.zdbWOTsedangaspedal <- ggplot(data = df.zdbWOTsedan,
                                   aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWOTsedangaspedal


# 6.1.2 轿车，油门踏板位移，有交通流----
plot.zdbWTsedangaspedal <- ggplot(data = df.zdbWTsedan,
                                  aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWTsedangaspedal


# 6.2 货车，油门踏板位移----
plot.zdbWOTtruckgaspedal <- ggplot(data = df.zdbWOTtruck,
                                   aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1, label = "匝道B洞口") +
  annotate(geom = "segment", x = 486, xend = 706, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 596, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 706, xend = 786, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 746, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdbWOTtruckgaspedal


# 7 匝道B，关键位置速度分布分析----
# 7.1 轿车，关键位置速度分布----
# 7.1.1 轿车，关键位置速度分布，无交通流----
df.zdbWOTsedanspotspeed <- CalcBatchSpotSpeed(data = df.zdbWOTsedan,
                                              kDriverID = c("S0101", "S0201",
                                                            "S0301", "S0401",
                                                            "S0501", "S0601",
                                                            "S0701", "S0801",
                                                            "S0901", "S1001",
                                                            "S1101", "S1201",
                                                            "S1301", "S1401",
                                                            "S1501", "S1601",
                                                            "S1701", "S1801",
                                                            "S1901", "S2001"),
                                              kDis = c(486, 706, 786),
                                              is.disdecrease = FALSE,
                                              kTag = c("accSTRT",
                                                       "accEnd/transSTRT",
                                                       "transEnd"),
                                              kDisType = "Dis")

# 轿车，无交通流，车道跨越点1，速度
df.zdbWOTsedanspotspeed <- subset(df.zdbWOTsedanLCpoint1,
                                  select = c("rowNo", "disTravelled",
                                             "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint1")) %>%
  rbind(df.zdbWOTsedanspotspeed)

# 轿车，无交通流，车道跨越点2，速度
df.zdbWOTsedanspotspeed <- subset(df.zdbWOTsedanLCpoint2,
                                  select = c("rowNo", "disTravelled",
                                             "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint2")) %>%
  rbind(df.zdbWOTsedanspotspeed)

df.zdbWOTsedanspotspeed <- transform(df.zdbWOTsedanspotspeed,
                                     scenType = "WOT")  # 增加对比场景标签

# 特征点速度统计量计算
plotdf.zdbWOTsedanspotspeed <- ddply(df.zdbWOTsedanspotspeed,
                                     .(disTag),
                                     summarize,
                                     meanSpeed = mean(speedKMH),
                                     sdSpeed = sd(speedKMH))

plot.zdbWOTsedanspotspeed <- ggplot(data = plotdf.zdbWOTsedanspotspeed,
                                    aes(x = disTag, y = meanSpeed)) +
  geom_bar(stat = "identity", width = 0.15) +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                width = 0.1, size =1) +
  scale_x_discrete(limits = c("accSTRT", "accEnd/transSTRT",
                              "LCPoint1", "LCPoint2", "transEnd"),
                   labels = c("加速段起点", "加速段终点\n(渐变段起点)",
                              "匝道->中间车道\n换道位置",
                              "中间车道->\n外侧车道\n换道位置",
                              "渐变段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zdbWOTsedanspotspeed


# 7.1.2 轿车，关键位置速度分布，有交通流----
df.zdbWTsedanspotspeed <- CalcBatchSpotSpeed(data = df.zdbWTsedan,
                                             kDriverID = c("S0101", "S0201",
                                                           "S0301", "S0401",
                                                           "S0501", "S0601",
                                                           "S0701", "S0801",
                                                           "S0901", "S1001",
                                                           "S1101", "S1201",
                                                           "S1301", "S1401",
                                                           "S1501", "S1601",
                                                           "S1701", "S1801",
                                                           "S1901", "S2001"),
                                             kDis = c(486, 706, 786),
                                             is.disdecrease = FALSE,
                                             kTag = c("accSTRT",
                                                      "accEnd/transSTRT",
                                                      "transEnd"),
                                             kDisType = "Dis")

# 轿车，有交通流，车道跨越点1，速度
df.zdbWTsedanspotspeed <- subset(df.zdbWTsedanLCpoint1,
                                 select = c("rowNo", "disTravelled",
                                            "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint1")) %>%
  rbind(df.zdbWTsedanspotspeed)

# 轿车，有交通流，车道跨越点2，速度
df.zdbWTsedanspotspeed <- subset(df.zdbWTsedanLCpoint2,
                                 select = c("rowNo", "disTravelled",
                                            "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint2")) %>%
  rbind(df.zdbWTsedanspotspeed)

df.zdbWTsedanspotspeed <- transform(df.zdbWTsedanspotspeed,
                                    scenType = "WT")  # 增加对比场景标签

# 特征点速度统计量计算
plotdf.zdbWTsedanspotspeed <- ddply(df.zdbWTsedanspotspeed,
                                    .(disTag),
                                    summarize,
                                    meanSpeed = mean(speedKMH),
                                    sdSpeed = sd(speedKMH))

plot.zdbWTsedanspotspeed <- ggplot(data = plotdf.zdbWTsedanspotspeed,
                                   aes(x = disTag, y = meanSpeed)) +
  geom_bar(stat = "identity", width = 0.15) +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                width = 0.1, size =1) +
  scale_x_discrete(limits = c("accSTRT", "accEnd/transSTRT",
                              "LCPoint1", "LCPoint2", "transEnd"),
                   labels = c("加速段起点", "加速段终点\n(渐变段起点)",
                              "匝道->中间车道\n换道位置",
                              "中间车道->\n外侧车道\n换道位置",
                              "渐变段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zdbWTsedanspotspeed


# 7.1.3 轿车，关键位置速度，无交通流 vs 有交通流----
df.zdbsedanspotspeed <- rbind(df.zdbWOTsedanspotspeed,
                              df.zdbWTsedanspotspeed)

plotdf.zdbsedanspotspeed <- ddply(df.zdbsedanspotspeed,
                                  .(disTag, scenType),
                                  summarize,
                                  meanSpeed = mean(speedKMH),
                                  sdSpeed = sd(speedKMH))

plot.zdbsedanspotspeed <- ggplot(data = plotdf.zdbsedanspotspeed,
                                 aes(x = disTag, y = meanSpeed,
                                     fill = scenType)) +
  geom_bar(stat = "identity", position = position_dodge(),
           width = 0.3, colour = "black") +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                position = position_dodge(0.3), width = 0.2) +
  scale_x_discrete(limits = c("accSTRT", "accEnd/transSTRT",
                              "LCPoint1", "LCPoint2", "transEnd"),
                   labels = c("加速段起点", "加速段终点\n(渐变段起点)",
                              "匝道->中间车道\n换道位置",
                              "中间车道->\n外侧车道\n换道位置",
                              "渐变段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  scale_fill_discrete(breaks=c("WOT", "WT"),
                      labels=c("w/o Traffic", "w/ Traffic")) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(face = "bold", size = 10),
        legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zdbsedanspotspeed


# 7.2 货车，关键位置速度分布----
df.zdbWOTtruckspotspeed <- CalcBatchSpotSpeed(data = df.zdbWOTtruck,
                                              kDriverID = c("T0101", "T0201",
                                                            "T0301", "T0401",
                                                            "T0501", "T0701"),
                                              kDis = c(486, 706, 786),
                                              is.disdecrease = FALSE,
                                              kTag = c("accSTRT",
                                                       "accEnd/transSTRT",
                                                       "transEnd"),
                                              kDisType = "Dis")  # 特征位置速度

# 货车车道跨越点，速度
df.zdbWOTtruckspotspeed <- subset(df.zdbWOTtruckLCpoint,
                                  select = c("rowNo", "disTravelled",
                                             "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint")) %>%
  rbind(df.zdbWOTtruckspotspeed)

df.zdbWOTtruckspotspeed <- transform(df.zdbWOTtruckspotspeed,
                                     scenType = "WOT")  # 增加对比场景标签

# 特征点速度统计量计算
plotdf.zdbWOTtruckspotspeed <- ddply(df.zdbWOTtruckspotspeed,
                                     .(disTag),
                                     summarize,
                                     meanSpeed = mean(speedKMH),
                                     sdSpeed = sd(speedKMH))

plot.zdbWOTtruckspotspeed <- ggplot(data = plotdf.zdbWOTtruckspotspeed,
                                    aes(x = disTag, y = meanSpeed)) +
  geom_bar(stat = "identity", width = 0.15) +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                width = 0.1, size =1) +
  scale_x_discrete(limits = c("accSTRT", "accEnd/transSTRT",
                              "LCPoint", "transEnd"),
                   labels = c("加速段起点", "加速段终点\n(渐变段起点)",
                              "换道位置", "渐变段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zdbWOTtruckspotspeed


# 8 匝道B，开始制动位置分布分析----
# 8.1 轿车，开始制动位置分布----
# 8.1.1 轿车，开始制动位置分布，无交通流----
df.zdbWOTsedanappbrake <- CalcBatchAppBrake(data = df.zdbWOTsedantraj,
                                            kDriverID = c("S0101", "S0201",
                                                          "S0301", "S0401",
                                                          "S0501", "S0601",
                                                          "S0701", "S0801",
                                                          "S0901", "S1001",
                                                          "S1101", "S1201",
                                                          "S1301", "S1401",
                                                          "S1501", "S1601",
                                                          "S1701", "S1801",
                                                          "S1901", "S2001"),
                                            is.disdecrease = FALSE,
                                            kPedalMod = 0.01,
                                            kCalcBrakeType = "BrakePedal")

plot.zdbWOTsedanappbrake <- ggplot(data = df.zdbWOTsedanappbrake,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTsedanappbrake


# 8.1.2 轿车，开始制动位置分布，有交通流----
df.zdbWTsedanappbrake <- CalcBatchAppBrake(data = df.zdbWTsedantraj,
                                           kDriverID = c("S0101", "S0201",
                                                         "S0301", "S0401",
                                                         "S0501", "S0601",
                                                         "S0701", "S0801",
                                                         "S0901", "S1001",
                                                         "S1101", "S1201",
                                                         "S1301", "S1401",
                                                         "S1501", "S1601",
                                                         "S1701", "S1801",
                                                         "S1901", "S2001"),
                                           is.disdecrease = FALSE,
                                           kPedalMod = 0.01,
                                           kCalcBrakeType = "BrakePedal")

plot.zdbWTsedanappbrake <- ggplot(data = df.zdbWTsedanappbrake,
                                  aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWTsedanappbrake


# 8.2 货车，开始制动位置分布----
df.zdbWOTtruckappbrake <- CalcBatchAppBrake(data = df.zdbWOTtrucktraj,
                                            kDriverID = c("T0101", "T0201",
                                                          "T0301", "T0401",
                                                          "T0501", "T0701"),
                                            is.disdecrease = FALSE,
                                            kPedalMod = 0.01,
                                            kCalcBrakeType = "BrakePedal")

plot.zdbWOTtruckappbrake <- ggplot(data = df.zdbWOTtruckappbrake,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTtruckappbrake


# 9 匝道B，开始加速位置分布分析----
# 9.1 轿车，开始加速位置分布----
# 9.1.1 轿车，开始加速位置分布，无交通流----
df.zdbWOTsedanappgas <- CalcBatchAppGas(data = df.zdbWOTsedantraj,
                                        kDriverID = c("S0101", "S0201",
                                                      "S0301", "S0401",
                                                      "S0501", "S0601",
                                                      "S0701", "S0801",
                                                      "S0901", "S1001",
                                                      "S1101", "S1201",
                                                      "S1301", "S1401",
                                                      "S1501", "S1601",
                                                      "S1701", "S1801",
                                                      "S1901", "S2001"),
                                        is.disdecrease = FALSE,
                                        kPedalMod = 0.01)

plot.zdbWOTsedanappgas <- ggplot(data = df.zdbWOTsedanappgas,
                                 aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTsedanappgas


# 9.1.2 轿车，开始加速位置分布，有交通流----
df.zdbWTsedanappgas <- CalcBatchAppGas(data = df.zdbWTsedantraj,
                                       kDriverID = c("S0101", "S0201",
                                                     "S0301", "S0401",
                                                     "S0501", "S0601",
                                                     "S0701", "S0801",
                                                     "S0901", "S1001",
                                                     "S1101", "S1201",
                                                     "S1301", "S1401",
                                                     "S1501", "S1601",
                                                     "S1701", "S1801",
                                                     "S1901", "S2001"),
                                       is.disdecrease = FALSE,
                                       kPedalMod = 0.01)

plot.zdbWTsedanappgas <- ggplot(data = df.zdbWTsedanappgas,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWTsedanappgas


# 9.2 货车，开始加速位置分布----
df.zdbWOTtruckappgas <- CalcBatchAppGas(data = df.zdbWOTtrucktraj,
                                        kDriverID = c("T0101", "T0201",
                                                      "T0301", "T0401",
                                                      "T0501", "T0701"),
                                        is.disdecrease = FALSE,
                                        kPedalMod = 0.01)

plot.zdbWOTtruckappgas <- ggplot(data = df.zdbWOTtruckappgas,
                                 aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTtruckappgas


# 10 大减速度分布分析----
# 10.1 轿车，大减速度分布----
# 10.1.1 轿车，大减速度，无交通流----
# 10.1.1.1 轿车，大减速度，无交通流，个体比较----
df.zdbWOTsedandecoutlier1 <- CalcBatchAccOutliers(data = df.zdbWOTsedantraj,
                                                  c("S0101", "S0201",
                                                    "S0301", "S0401",
                                                    "S0501", "S0601",
                                                    "S0701", "S0801",
                                                    "S0901", "S1001",
                                                    "S1101", "S1201",
                                                    "S1301", "S1401",
                                                    "S1501", "S1601",
                                                    "S1701", "S1801",
                                                    "S1901", "S2001"),
                                                  kQuantile = 0.05,
                                                  kAccLimit = NA,
                                                  kOutliersType = "Dec",
                                                  is.indv = TRUE)

plot.zdbWOTsedandecoutlier1 <- ggplot(data = df.zdbWOTsedandecoutlier1,
                                      aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTsedandecoutlier1


# 10.1.1.2 轿车，大减速度，无交通流，群体比较----
df.zdbWOTsedandecoutlier2 <- CalcBatchAccOutliers(data = df.zdbWOTsedantraj,
                                                  c("S0101", "S0201",
                                                    "S0301", "S0401",
                                                    "S0501", "S0601",
                                                    "S0701", "S0801",
                                                    "S0901", "S1001",
                                                    "S1101", "S1201",
                                                    "S1301", "S1401",
                                                    "S1501", "S1601",
                                                    "S1701", "S1801",
                                                    "S1901", "S2001"),
                                                  kQuantile = NA,
                                                  kAccLimit = -3.0, 
                                                  kOutliersType = "Dec",
                                                  is.indv = FALSE)

plot.zdbWOTsedandecoutlier2 <- ggplot(data = df.zdbWOTsedandecoutlier2,
                                      aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTsedandecoutlier2


# 10.1.2 轿车，大减速度，有交通流----
# 10.1.2.1 轿车，大减速度，有交通流，个体比较----
df.zdbWTsedandecoutlier1 <- CalcBatchAccOutliers(data = df.zdbWTsedantraj,
                                                 c("S0101", "S0201",
                                                   "S0301", "S0401",
                                                   "S0501", "S0601",
                                                   "S0701", "S0801",
                                                   "S0901", "S1001",
                                                   "S1101", "S1201",
                                                   "S1301", "S1401",
                                                   "S1501", "S1601",
                                                   "S1701", "S1801",
                                                   "S1901", "S2001"),
                                                 kQuantile = 0.05,
                                                 kAccLimit = NA,
                                                 kOutliersType = "Dec",
                                                 is.indv = TRUE)

plot.zdbWTsedandecoutlier1 <- ggplot(data = df.zdbWTsedandecoutlier1,
                                     aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWTsedandecoutlier1


# 10.1.2.2 轿车，大减速度，有交通流，群体比较----
df.zdbWTsedandecoutlier2 <- CalcBatchAccOutliers(data = df.zdbWTsedantraj,
                                                 c("S0101", "S0201",
                                                   "S0301", "S0401",
                                                   "S0501", "S0601",
                                                   "S0701", "S0801",
                                                   "S0901", "S1001",
                                                   "S1101", "S1201",
                                                   "S1301", "S1401",
                                                   "S1501", "S1601",
                                                   "S1701", "S1801",
                                                   "S1901", "S2001"),
                                                 kQuantile = NA,
                                                 kAccLimit = -3.0, 
                                                 kOutliersType = "Dec",
                                                 is.indv = FALSE)

plot.zdbWTsedandecoutlier2 <- ggplot(data = df.zdbWTsedandecoutlier2,
                                     aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWTsedandecoutlier2


# 10.2 货车，大减速度分布----
# 10.2.1 货车，大减速度，个体比较----
df.zdbWOTtruckdecoutlier1 <- CalcBatchAccOutliers(data = df.zdbWOTtrucktraj,
                                                  c("T0101", "T0201",
                                                    "T0301", "T0401",
                                                    "T0501", "T0701"),
                                                  kQuantile = 0.05,
                                                  kAccLimit = NA,
                                                  kOutliersType = "Dec",
                                                  is.indv = TRUE)

plot.zdbWOTtruckdecoutlier1 <- ggplot(data = df.zdbWOTtruckdecoutlier1,
                                      aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTtruckdecoutlier1


# 10.2.2 货车，大减速度，群体比较----
df.zdbWOTtruckdecoutlier2 <- CalcBatchAccOutliers(data = df.zdbWOTtrucktraj,
                                                  c("T0101", "T0201",
                                                    "T0301", "T0401",
                                                    "T0501", "T0701"),
                                                  kQuantile = NA,
                                                  kAccLimit = -3.0, 
                                                  kOutliersType = "Dec",
                                                  is.indv = FALSE)

plot.zdbWOTtruckdecoutlier2 <- ggplot(data = df.zdbWOTtruckdecoutlier2,
                                      aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 786, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(786, 786), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 0, xend = 486, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 486, xend = 666, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 666, xend = 786, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 0, xend = 486, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 0, xend = 486, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 486, xend = 486, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 486, xend = 666, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 486, xend = 666, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 486, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(0, 0, 486, 486, 666, 666, 666),
           xend = c(486, 486, 666, 666, 710, 786, 786),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(486, 486, 486),
           xend = c(531, 576, 621),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 249, xmax = 1400,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 249, y = 1.5,
           fontface="bold", size = 4,
           label = "匝道B洞口") +
  annotate(geom = "segment",
           x = 486, xend = 706, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(620, 620), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 706, xend = 786, y = 19.6, yend = 19.6, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(746, 746), y = c(18, 21),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 486, 706, 786),
                     labels = c("BK0+000", "BK0+486", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdbWOTtruckdecoutlier2





