#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180823, By MaoYan
# 
# 1.分析妈湾出口匝道匝道A的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)
library(magrittr)
library(plyr)


# 数据切分----
df.zda <- subset(x = df.dsdata, dsScen == "ZDA")  # 匝道A数据

df.zda$disTravelled <- ifelse(test = df.zda$roadName == "MainRD_Right",
                              df.zda$disFromRoadStart - 4000,
                              df.zda$disFromRoadStart + 3644.64)

df.zdasedan <- subset(x = df.zda, dsVehicleType == "Sedan")  # 匝道A，轿车
df.zdatruck <- subset(x = df.zda, dsVehicleType == "Truck")  # 匝道A，货车


# 1 匝道A，行驶速度分析----
# 1.1 轿车，行驶速度----
df.zdasedan <- subset(df.zdasedan, driverID != "S1301")  # 提出严重超速样本

plot.zdasedanspeed <- ggplot(data = df.zdasedan,
                             aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(500, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 500, xmax = 3902,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 100, label = "A匝道洞口") +
  annotate(geom = "segment", x = 3445, xend = 3525, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3485, y = 100, label = "渐变段") +
  annotate(geom = "segment", x = 3525, xend = 3645, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3585, y = 100, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdasedanspeed


# 1.2 货车，行驶速度----
plot.zdatruckspeed <- ggplot(data = df.zdatruck,
                             aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(500, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 500, xmax = 3902,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 100, label = "A匝道洞口") +
  annotate(geom = "segment", x = 3445, xend = 3525, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3485, y = 100, label = "渐变段") +
  annotate(geom = "segment", x = 3525, xend = 3645, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3585, y = 100, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdatruckspeed


# 2 匝道A，轨迹分析----
# 2.1 轿车，轨迹1----
df.zdasedantraj <- CalcBatchTraj(data = df.zdasedan,
                                 kDriverID = c("S0101", "S0201", "S0301",
                                               "S0401", "S0501", "S0601",
                                               "S0701", "S0801", "S0901",
                                               "S1001", "S1101", "S1201",
                                               "S1401", "S1501", "S1601",
                                               "S1701", "S1801", "S1901",
                                               "S2001"),
                                 is.main2ramp = TRUE,
                                 is.disdecrease = FALSE)

plot.zdasedantraj1 <- ggplot(data = df.zdasedantraj,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdasedantraj1


# 2.2 轿车，轨迹2----
plot.zdasedantraj2 <- ggplot(data = df.zdasedantraj,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 3000, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 3000, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(3000, 3000), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 3000, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(3000, 4100),
                     breaks = c(3445, 3525, 3645, 4100),
                     labels = c("", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdasedantraj2


# 2.3 货车，轨迹1----
df.zdatrucktraj <- CalcBatchTraj(data = df.zdatruck,
                                 kDriverID = c("T0101", "T0201", "T0301",
                                               "T0401", "T0501", "T0701"),
                                 is.main2ramp = TRUE,
                                 is.disdecrease = FALSE)

plot.zdatrucktraj1 <- ggplot(data = df.zdatrucktraj,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdatrucktraj1


# 2.4 货车，轨迹2----
plot.zdatrucktraj2 <- ggplot(data = df.zdatrucktraj,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 3000, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 3000, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(3000, 3000), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 3000, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(3000, 4100),
                     breaks = c(3445, 3525, 3645, 4100),
                     labels = c("", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdatrucktraj2


# 3 匝道A，车道跨越点分析----
# 3.1 轿车，车道跨越点----
# 3.2 货车，车道跨越点L3->L2----
df.zdatruckLCpoint1 <- CalcBatchLCPoint(data = df.zdatrucktraj,
                                        kDriverID = c("T0101", "T0201",
                                                      "T0301", "T0401",
                                                      "T0501", "T0701"),
                                        kLatDis = 3.82,
                                        is.main2ramp = TRUE,
                                        is.disdecrease = FALSE)

plot.zdatruckLCpoints1 <- ggplot(data = df.zdatruckLCpoint1,
                                 aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zdatruckLCpoint1$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zdatruckLCpoint1$disTravelled) + 400,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值 RK3+104")) +
  annotate(geom = "segment",
           x = median(df.zdatruckLCpoint1$disTravelled),
           xend = median(df.zdatruckLCpoint1$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
#  annotate(geom = "segment",
#           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
#           fontface = "bold", size = 4,
#           label = c("渐变段", "80m")) +
#  annotate(geom = "segment",
#           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
#           fontface = "bold", size = 4,
#           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdatruckLCpoints1


# 3.3 货车，车道跨越点L2->L1 or Ramp----
df.zdatruckLCpoint2 <- CalcBatchLCPoint(data = df.zdatrucktraj,
                                        kDriverID = c("T0101", "T0201",
                                                      "T0301", "T0401",
                                                      "T0501", "T0701"),
                                        kLatDis = 7.64,
                                        is.main2ramp = TRUE,
                                        is.disdecrease = FALSE)
median(df.zdatruckLCpoint2$disTravelled)
plot.zdatruckLCpoints2 <- ggplot(data = df.zdatruckLCpoint2,
                                 aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zdatruckLCpoint2$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zdatruckLCpoint2$disTravelled) + 480,
           y = 29.5 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值 RK6+255")) +
  annotate(geom = "segment",
           x = median(df.zdatruckLCpoint2$disTravelled),
           xend = median(df.zdatruckLCpoint2$disTravelled) + 80,
           y = 29.5, yend = 29.5 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zdatruckLCpoint2$disTravelled), xend = 3445,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3335, y = 2,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点220m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
#  annotate(geom = "segment",
#           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
#           fontface = "bold", size = 4,
#           label = c("渐变段", "80m")) +
#  annotate(geom = "segment",
#           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
#           fontface = "bold", size = 4,
#           label = c("减速段", "120m")) +
# 坐标轴及图例等修改
scale_x_continuous(name = NULL, limits = c(0, 4100),
                   breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                   labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                              "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdatruckLCpoints2


# 3.4 货车，车道跨越点L2->L1 or Ramp----
plot.zdatruckLCpoints3 <- ggplot(data = df.zdatruckLCpoint2,
                                 aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zdatruckLCpoint2$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zdatruckLCpoint2$disTravelled) + 200,
           y = 29.5 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值 RK6+255")) +
  annotate(geom = "segment",
           x = median(df.zdatruckLCpoint2$disTravelled),
           xend = median(df.zdatruckLCpoint2$disTravelled) + 80,
           y = 29.5, yend = 29.5 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zdatruckLCpoint2$disTravelled), xend = 3445,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3335, y = 2,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点220m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 3000, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 3000, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(3000, 3000), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 3000, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(3000, 4100),
                     breaks = c(3445, 3525, 3645, 4100),
                     labels = c("", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdatruckLCpoints3


# 4 匝道A，加速度分析----
# 4.1 轿车，加速度----
plot.zdasedanacc <- ggplot(data = df.zdasedan,
                           aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(500, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 500, xmax = 3902,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 5, label = "A匝道洞口") +
  annotate(geom = "segment", x = 3445, xend = 3525, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3485, y = 5, label = "渐变段") +
  annotate(geom = "segment", x = 3525, xend = 3645, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3585, y = 5, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdasedanacc


# 4.2 货车，加速度----
plot.zdatruckacc <- ggplot(data = df.zdatruck,
                           aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(500, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 500, xmax = 3902,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 5, label = "A匝道洞口") +
  annotate(geom = "segment", x = 3445, xend = 3525, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3485, y = 5, label = "渐变段") +
  annotate(geom = "segment", x = 3525, xend = 3645, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3585, y = 5, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdatruckacc


# 5 匝道A，制动踏板位移分析----
# 5.1 轿车，制动踏板位移----
plot.zdasedanbrakepedal <- ggplot(data = df.zdasedan,
                                  aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(500, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_continuous(name = "制动踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 500, xmax = 3902,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1, label = "A匝道洞口") +
  annotate(geom = "segment", x = 3445, xend = 3525, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3485, y = 1, label = "渐变段") +
  annotate(geom = "segment", x = 3525, xend = 3645, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3585, y = 1, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdasedanbrakepedal


# 5.2 货车，制动踏板位移----
plot.zdatruckbrakepedal <- ggplot(data = df.zdatruck,
                                  aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(500, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_continuous(name = "制动踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 500, xmax = 3902,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1, label = "A匝道洞口") +
  annotate(geom = "segment", x = 3445, xend = 3525, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3485, y = 1, label = "渐变段") +
  annotate(geom = "segment", x = 3525, xend = 3645, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3585, y = 1, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdatruckbrakepedal


# 6 匝道A，油门踏板位移分析----
# 6.1 轿车，油门踏板位移----
plot.zdasedangaspedal <- ggplot(data = df.zdasedan,
                                aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(500, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_continuous(name = "油门踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 500, xmax = 3902,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1, label = "A匝道洞口") +
  annotate(geom = "segment", x = 3445, xend = 3525, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3485, y = 1, label = "渐变段") +
  annotate(geom = "segment", x = 3525, xend = 3645, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3585, y = 1, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdasedangaspedal


# 6.2 货车，油门踏板位移----
plot.zdatruckgaspedal <- ggplot(data = df.zdatruck,
                                aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(500, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_continuous(name = "油门踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 500, xmax = 3902,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1, label = "A匝道洞口") +
  annotate(geom = "segment", x = 3445, xend = 3525, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3485, y = 1, label = "渐变段") +
  annotate(geom = "segment", x = 3525, xend = 3645, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3585, y = 1, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdatruckgaspedal


# 7 匝道A，关键位置速度分布分析----
# 7.1 轿车，关键位置速度分布----
df.zdasedanspotspeed <- CalcBatchSpotSpeed(data = df.zdasedan,
                                           kDriverID = c("S0101", "S0201",
                                                         "S0301", "S0401",
                                                         "S0501", "S0601",
                                                         "S0701", "S0801",
                                                         "S0901", "S1001",
                                                         "S1101", "S1201",
                                                         "S1401", "S1501",
                                                         "S1601", "S1701",
                                                         "S1801", "S1901",
                                                         "S2001"),
                                           kDis = c(3445, 3525, 3645),
                                           is.disdecrease = FALSE,
                                           kTag = c("transSTRT",
                                                    "transEnd/decSTRT",
                                                    "decEnd"),
                                           kDisType = "Dis")  # 特征位置速度

# 特征点速度统计量计算
plotdf.zdasedanspotspeed <- ddply(df.zdasedanspotspeed,
                                  .(disTag),
                                  summarize,
                                  meanSpeed = mean(speedKMH),
                                  sdSpeed = sd(speedKMH))

plot.zdasedanspotspeed <- ggplot(data = plotdf.zdasedanspotspeed,
                                 aes(x = disTag, y = meanSpeed)) +
  geom_bar(stat = "identity", width = 0.15) +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                width = 0.1, size =1) +
  scale_x_discrete(limits = c("transSTRT",
                              "transEnd/decSTRT", "decEnd"),
                   labels = c("渐变段起点",
                              "渐变段终点\n(减速段起点)", "减速段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zdasedanspotspeed


# 7.2 货车，关键位置速度分布----
df.zdatruckspotspeed <- CalcBatchSpotSpeed(data = df.zdatruck,
                                           kDriverID = c("T0101", "T0201",
                                                         "T0301", "T0401",
                                                         "T0501", "T0701"),
                                           kDis = c(3445, 3525, 3645),
                                           is.disdecrease = FALSE,
                                           kTag = c("transSTRT",
                                                    "transEnd/decSTRT",
                                                    "decEnd"),
                                           kDisType = "Dis")  # 特征位置速度

# 货车车道跨越点1，L3->L2速度
df.zdatruckspotspeed <- subset(df.zdatruckLCpoint1,
                               select = c("rowNo", "disTravelled",
                                          "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint1")) %>%
  rbind(df.zdatruckspotspeed)

# 货车车道跨越点2，L2->L1 or Ramp速度
df.zdatruckspotspeed <- subset(df.zdatruckLCpoint2,
                               select = c("rowNo", "disTravelled",
                                          "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint2")) %>%
  rbind(df.zdatruckspotspeed)

# 特征点速度统计量计算
plotdf.zdatruckspotspeed <- ddply(df.zdatruckspotspeed,
                                  .(disTag),
                                  summarize,
                                  meanSpeed = mean(speedKMH),
                                  sdSpeed = sd(speedKMH))

plot.zdatruckspotspeed <- ggplot(data = plotdf.zdatruckspotspeed,
                                 aes(x = disTag, y = meanSpeed)) +
  geom_bar(stat = "identity", width = 0.15) +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                width = 0.1, size =1) +
  scale_x_discrete(limits = c("LCPoint1", "LCPoint2", "transSTRT",
                              "transEnd/decSTRT", "decEnd"),
                   labels = c("主线内侧车道->\n主线中间车道\n换道位置",
                              "主线中间车道->\n主线外侧车道\n换道位置",
                              "渐变段起点",
                              "渐变段终点\n(减速段起点)",
                              "减速段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zdatruckspotspeed


# 8 匝道A，开始制动位置分布分析----
# 8.1 轿车，开始制动位置分布----
df.zdasedanappbrake <- CalcBatchAppBrake(data = df.zdasedantraj,
                                         kDriverID = c("S0101", "S0201",
                                                       "S0301", "S0401",
                                                       "S0501", "S0601",
                                                       "S0701", "S0801",
                                                       "S0901", "S1001",
                                                       "S1101", "S1201",
                                                       "S1401", "S1501",
                                                       "S1601", "S1701",
                                                       "S1801", "S1901",
                                                       "S2001"),
                                         is.disdecrease = FALSE,
                                         kPedalMod = 0.01,
                                         kCalcBrakeType = "BrakePedal")

plot.zdasedanappbrake <- ggplot(data = df.zdasedanappbrake,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdasedanappbrake


# 8.2 货车，开始制动位置分布----
df.zdatruckappbrake <- CalcBatchAppBrake(data = df.zdatrucktraj,
                                         kDriverID = c("T0101", "T0201",
                                                       "T0301", "T0401",
                                                       "T0501", "T0701"),
                                         is.disdecrease = FALSE,
                                         kPedalMod = 0.01,
                                         kCalcBrakeType = "BrakePedal")

plot.zdatruckappbrake <- ggplot(data = df.zdatruckappbrake,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdatruckappbrake


# 9 匝道A，开始加速位置分布分析----
# 9.1 轿车，开始加速位置分布----
df.zdasedanappgas <- CalcBatchAppGas(data = df.zdasedantraj,
                                     kDriverID = c("S0101", "S0201", "S0301",
                                                   "S0401", "S0501", "S0601",
                                                   "S0701", "S0801", "S0901",
                                                   "S1001", "S1101", "S1201",
                                                   "S1401", "S1501", "S1601",
                                                   "S1701", "S1801", "S1901",
                                                   "S2001"),
                                     is.disdecrease = FALSE,
                                     kPedalMod = 0.01)

plot.zdasedanappgas <- ggplot(data = df.zdasedanappgas,
                              aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())
  
plot.zdasedanappgas


# 9.2 货车，开始加速位置分布----
df.zdatruckappgas <- CalcBatchAppGas(data = df.zdatrucktraj,
                                     kDriverID = c("T0101", "T0201", "T0301",
                                                   "T0401", "T0501", "T0701"),
                                     is.disdecrease = FALSE,
                                     kPedalMod = 0.01)

plot.zdatruckappgas <- ggplot(data = df.zdatruckappgas,
                              aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdatruckappgas


# 10 匝道A，大减速度分布分析----
# 10.1 轿车，大减速度分布----
# 10.1.1 轿车，大减速度，个体比较----
df.zdasedandecoutlier1 <- CalcBatchAccOutliers(data = df.zdasedantraj,
                                               kDriverID = c("S0101", "S0201",
                                                             "S0301", "S0401",
                                                             "S0501", "S0601",
                                                             "S0701", "S0801",
                                                             "S0901", "S1001",
                                                             "S1101", "S1201",
                                                             "S1401", "S1501",
                                                             "S1601", "S1701",
                                                             "S1801", "S1901",
                                                             "S2001"),
                                               kQuantile = 0.05,
                                               kAccLimit = NA,
                                               kOutliersType = "Dec",
                                               is.indv = TRUE)

plot.zdasedandecoutlier1 <- ggplot(data = df.zdasedandecoutlier1,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdasedandecoutlier1


# 10.1.2 轿车，大减速度，群体比较----
df.zdasedandecoutlier2 <- CalcBatchAccOutliers(data = df.zdasedantraj,
                                               kDriverID = c("S0101", "S0201",
                                                             "S0301", "S0401",
                                                             "S0501", "S0601",
                                                             "S0701", "S0801",
                                                             "S0901", "S1001",
                                                             "S1101", "S1201",
                                                             "S1401", "S1501",
                                                             "S1601", "S1701",
                                                             "S1801", "S1901",
                                                             "S2001"),
                                               kQuantile = NA,
                                               kAccLimit = -3.0,
                                               kOutliersType = "Dec",
                                               is.indv = FALSE)

plot.zdasedandecoutlier2 <- ggplot(data = df.zdasedandecoutlier2,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdasedandecoutlier2


# 10.2 货车，大减速度分布----
# 10.2.1 货车，大减速度，个体比较----
df.zdatruckdecoutlier1 <- CalcBatchAccOutliers(data = df.zdatrucktraj,
                                               kDriverID = c("T0101", "T0201",
                                                             "T0301", "T0401",
                                                             "T0501", "T0701"),
                                               kQuantile = 0.05,
                                               kAccLimit = NA,
                                               kOutliersType = "Dec",
                                               is.indv = TRUE)

plot.zdatruckdecoutlier1 <- ggplot(data = df.zdatruckdecoutlier1,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdatruckdecoutlier1


# 10.2.2 货车，大减速度，群体比较----
df.zdatruckdecoutlier2 <- CalcBatchAccOutliers(data = df.zdatrucktraj,
                                               kDriverID = c("T0101", "T0201",
                                                             "T0301", "T0401",
                                                             "T0501", "T0701"),
                                               kQuantile = NA,
                                               kAccLimit = -3.0,
                                               kOutliersType = "Dec",
                                               is.indv = FALSE)

plot.zdatruckdecoutlier2 <- ggplot(data = df.zdatruckdecoutlier2,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 3525),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 7.64, yend = 7.64,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 22.1, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 7.64, ymax = 22.1,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 7.64, yend = 22.1,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(7.64, 7.64, 7.64), yend = c(18.44, 14.84, 11.24),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 29.5, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 3520, xend = 3645, y = 15, yend = 29.5,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 15,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(3525, 3645), xend = c(3645, 4100),
           y = c(11.32, 25.7), yend = c(25.7, 25.7),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 3902,
           ymin = 0, ymax = 29.5, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 1.5,
           fontface="bold", size = 4,
           label = "A匝道洞口") +
  annotate(geom = "segment",
           x = 3445, xend = 3525, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3485, 3485), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  annotate(geom = "segment",
           x = 3525, xend = 3645, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(3585, 3585), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdatruckdecoutlier2








