#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180705, By MaoYan
# 
# 1.分析妈湾入口匝道匝道1的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)
library(magrittr)
library(plyr)


# 数据切分----
df.zd1 <- subset(x = df.dsdata,
                 dsScen == "ZD1" | dsScen == "ZD1withTraffic")  # 匝道1数据

# 标定匝道1数据
df.zd1$disTravelled <- ifelse(test = df.zd1$roadName == "ZD_1/3",
                              df.zd1$disFromRoadStart,
                              df.zd1$disFromRoadStart + 582 - 2975)  # 桩号标定

df.zd1WOT <- subset(x = df.zd1, dsScen == "ZD1")  # 匝道1，无交通流
df.zd1WT <- subset(x = df.zd1, dsScen == "ZD1withTraffic")  # 匝道1，有交通流

df.zd1WOTsedan <- subset(x = df.zd1WOT,
                         dsVehicleType == "Sedan")  # 匝道1轿车，无交通流
df.zd1WOTtruck <- subset(x = df.zd1WOT,
                         dsVehicleType == "Truck")  # 匝道1货车，无交通流

df.zd1WTsedan <- subset(x = df.zd1WT,
                        dsVehicleType == "Sedan")  # 匝道1轿车，有交通流
df.zd1WTtruck <- subset(x = df.zd1WT,
                        dsVehicleType == "Truck")  # 匝道1货车，有交通流


# 1 匝道1，行驶速度分析----
# 1.1 轿车，行驶速度----
# 1.1.1 轿车，行驶速度，无交通流----
plot.zd1WOTsedanspeed <- ggplot(data = df.zd1WOTsedan,
                                aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 120, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 120, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 120, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTsedanspeed


# 1.1.2 轿车，行驶速度，有交通流----
plot.zd1WTsedanspeed <- ggplot(data = df.zd1WTsedan,
                               aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 120, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 120, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 120, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WTsedanspeed

# 1.2 货车，行驶速度----
plot.zd1WOTtruckspeed <- ggplot(data = df.zd1WOTtruck,
                                aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 120, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 120, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 120, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTtruckspeed


# 2 匝道1，轨迹分析----
# 2.1 轿车，轨迹----
# 2.1.1 轿车，轨迹，无交通流----
df.zd1WOTsedantraj <- CalcBatchTraj(data = df.zd1WOTsedan,
                                    kDriverID = c("S0101", "S0201", "S0301",
                                                  "S0401", "S0501", "S0601",
                                                  "S0701", "S0801", "S0901",
                                                  "S1002", "S1101", "S1201",
                                                  "S1301", "S1401", "S1501",
                                                  "S1601", "S1701", "S1801",
                                                  "S2002"),
                                    is.main2ramp = FALSE,
                                    is.disdecrease = FALSE)

plot.zd1WOTsedantraj <- ggplot(data = df.zd1WOTsedantraj,
                               aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
    # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 854, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 727, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 582, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 582, xend = 582, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 582, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 582, xend = 727, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(582, 582, 582), xend = c(618, 654, 690),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 727, xend = 854, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 582, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 582, xend = 727, y = 23.35, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 727, xend = 807, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 807, xend = 854, y = 18.65, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 582, 727), xend = c(582, 727, 807),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1.5,
           fontface="bold", size = 4,
           label = "S1匝道隧道起点") +
  annotate(geom = "segment",
           x = 582, xend = 802, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(692, 692), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 802, xend = 852, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(827, 827), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "50m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd1WOTsedantraj


# 2.1.2 轿车，轨迹，有交通流----
df.zd1WTsedantraj <- CalcBatchTraj(data = df.zd1WTsedan,
                                   kDriverID = c("S0101", "S0301",
                                                 "S0401", "S0501", "S0601",
                                                 "S0701", "S0801",
                                                 "S1001", "S1101", "S1201",
                                                 "S1301", "S1401", "S1501",
                                                 "S1601", "S1701", "S1801",
                                                 "S1901", "S2001"),
                                   is.main2ramp = FALSE,
                                   is.disdecrease = FALSE)

plot.zd1WTsedantraj <- ggplot(data = df.zd1WTsedantraj,
                              aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 854, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 727, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 582, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 582, xend = 582, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 582, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 582, xend = 727, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(582, 582, 582), xend = c(618, 654, 690),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 727, xend = 854, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 582, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 582, xend = 727, y = 23.35, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 727, xend = 807, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 807, xend = 854, y = 18.65, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 582, 727), xend = c(582, 727, 807),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1.5,
           fontface="bold", size = 4,
           label = "S1匝道隧道起点") +
  annotate(geom = "segment",
           x = 582, xend = 802, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(692, 692), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 802, xend = 852, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(827, 827), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "50m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd1WTsedantraj


# 2.2 货车，轨迹----
df.zd1WOTtrucktraj <- CalcBatchTraj(data = df.zd1WOTtruck,
                                    kDriverID = c("T0101", "T0201", "T0301",
                                                  "T0401", "T0501", "T0601",
                                                  "T0701"),
                                    is.main2ramp = FALSE,
                                    is.disdecrease = FALSE)

plot.zd1WOTtrucktraj <- ggplot(data = df.zd1WOTtrucktraj,
                               aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 854, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 727, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 582, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 582, xend = 582, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 582, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 582, xend = 727, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(582, 582, 582), xend = c(618, 654, 690),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 727, xend = 854, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 582, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 582, xend = 727, y = 23.35, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 727, xend = 807, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 807, xend = 854, y = 18.65, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 582, 727), xend = c(582, 727, 807),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1.5,
           fontface="bold", size = 4,
           label = "S1匝道隧道起点") +
  annotate(geom = "segment",
           x = 582, xend = 802, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(692, 692), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 802, xend = 852, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(827, 827), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "50m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd1WOTtrucktraj


# 3 匝道1，车道跨越点分析----
# 3.1 轿车，车道跨越点----
# 3.1.1 轿车，车道跨越点，无交通流----
df.zd1WOTsedanLCpoints <- CalcBatchLCPoint(data = df.zd1WOTsedantraj,
                                           kDriverID = c("S0101", "S0201",
                                                         "S0301", "S0401",
                                                         "S0501", "S0601",
                                                         "S0701", "S0801",
                                                         "S0901", "S1002",
                                                         "S1101", "S1201",
                                                         "S1301", "S1401",
                                                         "S1501", "S1601",
                                                         "S1701", "S1801",
                                                         "S2002"),
                                           kLatDis = 11.45,
                                           is.main2ramp = FALSE,
                                           is.disdecrease = FALSE)

plot.zd1WOTsedanLCpoints <- ggplot(data = df.zd1WOTsedanLCpoints,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zd1WOTsedanLCpoints$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zd1WOTsedanLCpoints$disTravelled) + 170,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zd1WOTsedanLCpoints$disTravelled),
           xend = median(df.zd1WOTsedanLCpoints$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zd1WOTsedanLCpoints$disTravelled), xend = 852,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(827, 827), y = c(2, 5),
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段终点", "47m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 854, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 727, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 582, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 582, xend = 582, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 582, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 582, xend = 727, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(582, 582, 582), xend = c(618, 654, 690),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 727, xend = 854, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 582, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 582, xend = 727, y = 23.35, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 727, xend = 807, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 807, xend = 854, y = 18.65, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 582, 727), xend = c(582, 727, 807),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1.5,
           fontface="bold", size = 4,
           label = "S1匝道隧道起点") +
  annotate(geom = "segment",
           x = 582, xend = 802, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(692, 692), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 802, xend = 852, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(827, 827), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "50m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd1WOTsedanLCpoints


# 3.1.2 轿车，车道跨越点，有交通流----
df.zd1WTsedanLCpoints <- CalcBatchLCPoint(data = df.zd1WTsedantraj,
                                          kDriverID = c("S0101", "S0301",
                                                        "S0401", "S0501",
                                                        "S0601", "S0701",
                                                        "S0801", "S1001",
                                                        "S1101", "S1201",
                                                        "S1301", "S1401",
                                                        "S1501", "S1601",
                                                        "S1701", "S1801",
                                                        "S1901", "S2001"),
                                          kLatDis = 11.45,
                                          is.main2ramp = FALSE,
                                          is.disdecrease = FALSE)

plot.zd1WTsedanLCpoints <- ggplot(data = df.zd1WTsedanLCpoints,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zd1WTsedanLCpoints$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zd1WTsedanLCpoints$disTravelled) + 170,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zd1WTsedanLCpoints$disTravelled),
           xend = median(df.zd1WTsedanLCpoints$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zd1WTsedanLCpoints$disTravelled), xend = 802,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 750, y = 2,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点21m")) +
  annotate(geom = "segment",
           x = median(df.zd1WTsedanLCpoints$disTravelled), xend = 852,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 870, y = 5,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段终点71m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 854, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 727, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 582, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 582, xend = 582, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 582, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 582, xend = 727, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(582, 582, 582), xend = c(618, 654, 690),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 727, xend = 854, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 582, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 582, xend = 727, y = 23.35, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 727, xend = 807, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 807, xend = 854, y = 18.65, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 582, 727), xend = c(582, 727, 807),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1.5,
           fontface="bold", size = 4,
           label = "S1匝道隧道起点") +
  annotate(geom = "segment",
           x = 582, xend = 802, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(692, 692), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 802, xend = 852, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(827, 827), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "50m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd1WTsedanLCpoints


# 3.2 货车，车道跨越点----
df.zd1WOTtruckLCpoints <- CalcBatchLCPoint(data = df.zd1WOTtrucktraj,
                                           kDriverID = c("T0101", "T0201",
                                                         "T0301", "T0401",
                                                         "T0501", "T0601",
                                                         "T0701"),
                                           kLatDis = 11.45,
                                           is.main2ramp = FALSE,
                                           is.disdecrease = FALSE)

plot.zd1WOTtruckLCpoints <- ggplot(data = df.zd1WOTtruckLCpoints,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zd1WOTtruckLCpoints$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zd1WOTtruckLCpoints$disTravelled) + 170,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zd1WOTtruckLCpoints$disTravelled),
           xend = median(df.zd1WOTtruckLCpoints$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zd1WOTtruckLCpoints$disTravelled), xend = 802,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 750, y = 2,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点8m")) +
  annotate(geom = "segment",
           x = median(df.zd1WOTtruckLCpoints$disTravelled), xend = 852,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 870, y = 5,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段终点58m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 854, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 727, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 582, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 582, xend = 582, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 582, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 582, xend = 727, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(582, 582, 582), xend = c(618, 654, 690),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 727, xend = 854, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 582, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 582, xend = 727, y = 23.35, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 727, xend = 807, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 807, xend = 854, y = 18.65, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 582, 727), xend = c(582, 727, 807),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1.5,
           fontface="bold", size = 4,
           label = "S1匝道隧道起点") +
  annotate(geom = "segment",
           x = 582, xend = 802, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(692, 692), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 802, xend = 852, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(827, 827), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "50m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd1WOTtruckLCpoints


# 4 匝道1，加速度分析----
# 4.1 轿车，加速度----
# 4.1.1 轿车，加速度，无交通流----
plot.zd1WOTsedanacc <- ggplot(data = df.zd1WOTsedan,
                              aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 5, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTsedanacc


# 4.1.2 轿车，加速度，有交通流----
plot.zd1WTsedanacc <- ggplot(data = df.zd1WTsedan,
                             aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 5, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WTsedanacc


# 4.2 货车，加速度----
plot.zd1WOTtrucksacc <- ggplot(data = df.zd1WOTtruck,
                               aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 5, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTtrucksacc


# 5 匝道1，制动踏板位移分析----
# 5.1 轿车，制动踏板位移----
# 5.1.1 轿车，制动踏板位移，无交通流----
plot.zd1WOTsedanbrakepedal <- ggplot(data = df.zd1WOTsedan,
                                     aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTsedanbrakepedal


# 5.1.2 轿车，制动踏板位移，有交通流----
plot.zd1WTsedanbrakepedal <- ggplot(data = df.zd1WTsedan,
                                    aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WTsedanbrakepedal


# 5.2 货车，制动踏板位移----
plot.zd1WOTtruckbrakepedal <- ggplot(data = df.zd1WOTtruck,
                                      aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTtruckbrakepedal


# 6 匝道1，油门踏板位移分析----
# 6.1 轿车，油门踏板位移----
# 6.1.1 轿车，油门踏板位移，无交通流----
plot.zd1WOTsedangaspedal <- ggplot(data = df.zd1WOTsedan,
                                   aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "油门踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTsedangaspedal


# 6.1.2 轿车，油门踏板位移，有交通流----
plot.zd1WTsedangaspedal <- ggplot(data = df.zd1WTsedan,
                                  aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "油门踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WTsedangaspedal


# 6.2 货车，油门踏板位移----
plot.zd1WOTtruckgaspedal <- ggplot(data = df.zd1WOTtruck,
                                   aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "油门踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTtruckgaspedal


# 7 关键位置速度分布分析----
# 7.1 轿车，关键位置速度分布----
# 7.1.1 轿车，关键位置速度，无交通流----
df.zd1WOTsedanspotspeed <- CalcBatchSpotSpeed(data = df.zd1WOTsedan,
                                              kDriverID = c("S0101", "S0201",
                                                            "S0301", "S0401",
                                                            "S0501", "S0601",
                                                            "S0701", "S0801",
                                                            "S0901", "S1002",
                                                            "S1101", "S1201",
                                                            "S1301", "S1401",
                                                            "S1501", "S1601",
                                                            "S1701", "S1801",
                                                            "S2002"),
                                              kDis = c(582, 802, 852),
                                              is.disdecrease = FALSE,
                                              kTag = c("accSTRT",
                                                       "accEnd/transSTRT",
                                                       "transEnd"),
                                              kDisType = "Dis")

df.zd1WOTsedanspotspeed <- subset(df.zd1WOTsedanLCpoints,
                                  select = c("rowNo", "disTravelled",
                                             "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint")) %>%
  rbind(df.zd1WOTsedanspotspeed)  # 计算换道位置速度

df.zd1WOTsedanspotspeed <- transform(df.zd1WOTsedanspotspeed,
                                     scenType = "WOT")  # 增加对比场景标签

# 特征点速度统计量计算
plotdf.zd1WOTsedanspotspeed <- ddply(df.zd1WOTsedanspotspeed,
                                     .(disTag),
                                     summarize,
                                     meanSpeed = mean(speedKMH),
                                     sdSpeed = sd(speedKMH))

plot.zd1WOTsedanspotspeed <- ggplot(data = plotdf.zd1WOTsedanspotspeed,
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

plot.zd1WOTsedanspotspeed


# 7.1.2 轿车，关键位置速度，有交通流----
df.zd1WTsedanspotspeed <- CalcBatchSpotSpeed(data = df.zd1WTsedan,
                                             kDriverID = c("S0101", "S0301",
                                                           "S0401", "S0501",
                                                           "S0601", "S0701",
                                                           "S0801", "S1001",
                                                           "S1101", "S1201",
                                                           "S1301", "S1401",
                                                           "S1501", "S1601",
                                                           "S1701", "S1801",
                                                           "S1901", "S2001"),
                                             kDis = c(582, 802, 852),
                                             is.disdecrease = FALSE,
                                             kTag = c("accSTRT",
                                                      "accEnd/transSTRT",
                                                      "transEnd"),
                                             kDisType = "Dis")

df.zd1WTsedanspotspeed <- subset(df.zd1WTsedanLCpoints,
                                 select = c("rowNo", "disTravelled",
                                            "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint")) %>%
  rbind(df.zd1WTsedanspotspeed)  # 计算换道位置速度

df.zd1WTsedanspotspeed <- transform(df.zd1WTsedanspotspeed,
                                    scenType = "WT")  # 增加对比场景标签

# 特征点速度统计量计算
plotdf.zd1WTsedanspotspeed <- ddply(df.zd1WTsedanspotspeed,
                                    .(disTag),
                                    summarize,
                                    meanSpeed = mean(speedKMH),
                                    sdSpeed = sd(speedKMH))

plot.zd1WTsedanspotspeed <- ggplot(data = plotdf.zd1WTsedanspotspeed,
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

plot.zd1WTsedanspotspeed


# 7.1.3 轿车，关键位置速度，无交通流 vs 有交通流----
# 特征点速度统计量计算，WOT vs WT
df.zd1sedanspotspeed <- rbind(df.zd1WOTsedanspotspeed,
                                df.zd1WTsedanspotspeed)

plotdf.zd1sedanspotspeed <- ddply(df.zd1sedanspotspeed,
                                  .(disTag, scenType),
                                  summarize,
                                  meanSpeed = mean(speedKMH),
                                  sdSpeed = sd(speedKMH))

plot.zd1sedanspotspeed <- ggplot(data = plotdf.zd1sedanspotspeed,
                                 aes(x = disTag, y = meanSpeed,
                                     fill = scenType)) +
  geom_bar(stat = "identity", position = position_dodge(),
           width = 0.3, colour = "black") +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                position = position_dodge(0.3), width = 0.2) +
  scale_x_discrete(limits = c("accSTRT", "accEnd/transSTRT",
                              "LCPoint", "transEnd"),
                   labels = c("加速段起点", "加速段终点\n(渐变段起点)",
                              "换道位置", "渐变段终点")) +
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

plot.zd1sedanspotspeed


# 7.2 货车，关键位置速度分布----
df.zd1WOTtruckspotspeed <- CalcBatchSpotSpeed(data = df.zd1WOTtruck,
                                              kDriverID = c("T0101", "T0201",
                                                            "T0301", "T0401",
                                                            "T0501", "T0601",
                                                            "T0701"),
                                              kDis = c(582, 802, 852),
                                              is.disdecrease = FALSE,
                                              kTag = c("accSTRT",
                                                       "accEnd/transSTRT",
                                                       "transEnd"),
                                              kDisType = "Dis")  # 特征位置速度

df.zd1WOTtruckspotspeed <- subset(df.zd1WOTtruckLCpoints,
                                  select = c("rowNo", "disTravelled",
                                             "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint")) %>%
  rbind(df.zd1WOTtruckspotspeed)  # 计算换道位置速度

df.zd1WOTtruckspotspeed <- transform(df.zd1WOTtruckspotspeed,
                                     scenType = "WOT")  # 增加对比场景标签

# 特征点速度统计量计算
plotdf.zd1WOTtruckspotspeed <- ddply(df.zd1WOTtruckspotspeed,
                                     .(disTag),
                                     summarize,
                                     meanSpeed = mean(speedKMH),
                                     sdSpeed = sd(speedKMH))

plot.zd1WOTtruckspotspeed <- ggplot(data = plotdf.zd1WOTtruckspotspeed,
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

plot.zd1WOTtruckspotspeed


# 8 开始制动位置分布----
# 8.1 轿车，开始制动位置分布----
# 8.1.1 轿车，开始制动位置分布，无交通流----
# 8.1.2 轿车，开始制动位置分布，有交通流----
# 8.2 货车，开始制动位置分布----



# 9 开始制动位置分布----
# 9.1 轿车，开始制动位置分布----
# 9.1.1 轿车，开始制动位置分布，无交通流----
# 9.1.2 轿车，开始制动位置分布，有交通流----
# 9.2 货车，开始制动位置分布----



# 10 大加速度分布分析----
# 10.1 轿车，大加速度分布----
# 10.1.1 轿车，大加速度，无交通流----
# 10.1.2 轿车，大加速度，有交通流----
# 10.2 货车，大加速度----



# 11 大减速度分布分析----
# 11.1 轿车，大减速度分布----
# 11.1.1 轿车，大减速度，无交通流----
# 11.1.1.1 轿车，大减速度，无交通流，个体比较----
df.zd1wotsedandecoutlier1 <- CalcBatchAccOutliers(data = df.zd1WOTsedantraj,
                                                  kDriverID = c("S0101", "S0201",
                                                                "S0301", "S0401",
                                                                "S0501", "S0601",
                                                                "S0701", "S0801",
                                                                "S0901", "S1002",
                                                                "S1101", "S1201",
                                                                "S1301", "S1401",
                                                                "S1501", "S1601",
                                                                "S1701", "S1801",
                                                                "S2002"),
                                                  kQuantile = 0.05,
                                                  kAccLimit = NA, 
                                                  kOutliersType = "Dec",
                                                  is.indv = TRUE)

plot.zd1wotsedandecoutlier1 <- ggplot(data = df.zd1wotsedandecoutlier1,
                                      aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 854, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 727, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 582, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 582, xend = 582, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 582, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 582, xend = 727, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(582, 582, 582), xend = c(618, 654, 690),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 727, xend = 854, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 582, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 582, xend = 727, y = 23.35, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 727, xend = 807, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 807, xend = 854, y = 18.65, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 582, 727), xend = c(582, 727, 807),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1.5,
           fontface="bold", size = 4,
           label = "S1匝道隧道起点") +
  annotate(geom = "segment",
           x = 582, xend = 802, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(692, 692), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 802, xend = 852, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(827, 827), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "50m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd1wotsedandecoutlier1


# 11.1.1.2 轿车，大减速度，无交通流，群体比较----
df.zd1wotsedandecoutlier2 <- CalcBatchAccOutliers(data = df.zd1WOTsedantraj,
                                                  kDriverID = c("S0101", "S0201",
                                                                "S0301", "S0401",
                                                                "S0501", "S0601",
                                                                "S0701", "S0801",
                                                                "S0901", "S1002",
                                                                "S1101", "S1201",
                                                                "S1301", "S1401",
                                                                "S1501", "S1601",
                                                                "S1701", "S1801",
                                                                "S2002"),
                                                  kQuantile = NA,
                                                  kAccLimit = -3.0, 
                                                  kOutliersType = "Dec",
                                                  is.indv = FALSE)

plot.zd1wotsedandecoutlier2 <- ggplot(data = df.zd1wotsedandecoutlier2,
                                      aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1400, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 854, xend = 1400, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1400, 1400),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 727, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 582, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 582, xend = 582, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 582, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 582, xend = 727, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(582, 582, 582), xend = c(618, 654, 690),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 727, xend = 854, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 582, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 582, xend = 727, y = 23.35, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 727, xend = 807, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 807, xend = 854, y = 18.65, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 582, 727), xend = c(582, 727, 807),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1.5,
           fontface="bold", size = 4,
           label = "S1匝道隧道起点") +
  annotate(geom = "segment",
           x = 582, xend = 802, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(692, 692), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 802, xend = 852, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(827, 827), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "50m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd1wotsedandecoutlier2


# 11.1.2 轿车，大减速度，有交通流----
# 11.1.2.1 轿车，大减速度，有交通流，个体比较----
# 11.1.2.2 轿车，大减速度，有交通流，群体比较----


# 11.2 货车，大减速度----
# 11.2.1 货车，大减速度，有交通流，个体比较----
# 11.2.2 货车，大减速度，有交通流，群体比较----




























