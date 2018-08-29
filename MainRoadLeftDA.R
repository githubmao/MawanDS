#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180703, By MaoYan
# ver2.0, date: 20180829, By MaoYan
# 
# 1.分析妈湾主线左线的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)
library(magrittr)
library(plyr)


# 数据切分----
df.mainleft <- subset(x = df.dsdata, dsScen == "ZX")  # 左线数据
df.mainleft <- df.mainleft[df.mainleft$roadName != "MainRD_Right",] 

# 标定左线桩号
df.mainleft$disTravelled <- ifelse(df.mainleft$roadName == "ZD_B",
                                   df.mainleft$disFromRoadStart + 1374.6 - 486,
                                   df.mainleft$disFromRoadStart)

df.mainleftsedan <- subset(x = df.mainleft,
                           dsVehicleType == "Sedan")  # 左线轿车
df.mainlefttruck <- subset(x = df.mainleft,
                           dsVehicleType == "Truck")  # 左线货车


# 1 左线，行驶速度分析----
# 1.1 轿车，行驶速度----
plot.MLsedanspeed <- ggplot(data = df.mainleftsedan,
                            aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 120, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 120, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLsedanspeed


# 1.2 货车，行驶速度----
plot.MLtruckspeed <- ggplot(data = df.mainlefttruck,
                            aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 120, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 120, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLtruckspeed


# 2 左线，加速度分析----
# 2.1 轿车，加速度----
plot.MLsedanacc <- ggplot(data = df.mainleftsedan,
                          aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 5, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLsedanacc


# 2.2 货车，加速度----
plot.MLtruckacc <- ggplot(data = df.mainlefttruck,
                          aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 5, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLtruckacc


# 3 左线，制动踏板位移分析----
# 3.1 轿车，制动踏板位移----
plot.MLsedanbrakepedal <- ggplot(data = df.mainleftsedan,
                                 aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLsedanbrakepedal


# 3.2 货车，制动踏板位移----
plot.MLtruckbrakepedal <- ggplot(data = df.mainlefttruck,
                                 aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLtruckbrakepedal


# 4 左线，油门踏板位移分析----
# 4.1 轿车，油门踏板位移----
plot.MLsedangaspedal <- ggplot(data = df.mainleftsedan,
                               aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLsedangaspedal


# 4.2 货车，油门踏板位移----
plot.MLtruckgaspedal <- ggplot(data = df.mainlefttruck,
                               aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLtruckgaspedal


# 5 左线，轨迹分析----
# 5.1 轿车，全线，轨迹----
df.MLsedantraj <- CalcBatchTraj(data = df.mainleftsedan,
                                kDriverID = c("S0101", "S0201", "S0301",
                                              "S0401", "S0501", "S0601",
                                              "S0701", "S0801", "S0901",
                                              "S1001", "S1101", "S1201",
                                              "S1301", "S1401", "S1501",
                                              "S1601", "S1701", "S1801",
                                              "S1901", "S2001"),
                                is.main2ramp = FALSE,
                                is.disdecrease = FALSE)

plot.MLsedantraj <- ggplot(data = df.MLsedantraj,
                           aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = c(50, 50), xend = c(1375, 8050),
           y = c(11.9, 0), yend = c(11.9, 0),
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 1675, xend = 8050, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(1675, 1675), xend = c(8050, 8050),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 50, xend = 1375, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 1375, xend = 1555, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 1555, xend = 1675, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 50, xend = 1375, y = 7.2, yend = 7.2,
         size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 50, xend = 1375, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 1375, xend = 1375, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 1375, xend = 1555, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 1375, xend = 1555, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 50, xmax = 1375, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(50, 50, 1375, 1375, 1555, 1555, 1555),
           xend = c(1375, 1375, 1555, 1555, 1599, 1675, 1675),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(1375, 1375, 1375),
           xend = c(1420, 1465, 1510),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MLsedantraj


# 5.2 货车，全线，轨迹----
df.MLtrucktraj <- CalcBatchTraj(data = df.mainlefttruck,
                                kDriverID = c("T0101", "T0201", "T0301",
                                              "T0401", "T0501", "T0601",
                                              "T0701"),
                                is.main2ramp = FALSE,
                                is.disdecrease = FALSE)

plot.MLtrucktraj <- ggplot(data = df.MLtrucktraj,
                           aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = c(50, 50), xend = c(1375, 8050),
           y = c(11.9, 0), yend = c(11.9, 0),
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 1675, xend = 8050, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(1675, 1675), xend = c(8050, 8050),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 50, xend = 1375, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 1375, xend = 1555, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 1555, xend = 1675, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 50, xend = 1375, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 50, xend = 1375, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 1375, xend = 1375, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 1375, xend = 1555, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 1375, xend = 1555, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 50, xmax = 1375, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(50, 50, 1375, 1375, 1555, 1555, 1555),
           xend = c(1375, 1375, 1555, 1555, 1599, 1675, 1675),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(1375, 1375, 1375),
           xend = c(1420, 1465, 1510),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MLtrucktraj


# 6 左线，车道跨越点分析----
# 6.1 轿车，车道跨越点----
# 6.2 货车，车道跨越点----



# 7 左线，开始制动位置分布分析----
# 7.1 轿车，开始制动位置分布----
df.MLsedanappbrake <- CalcBatchAppBrake(data = df.MLsedantraj,
                                        kDriverID = c("S0101", "S0201", "S0301",
                                                      "S0401", "S0501", "S0601",
                                                      "S0701", "S0801", "S0901",
                                                      "S1001", "S1101", "S1201",
                                                      "S1301", "S1401", "S1501",
                                                      "S1601", "S1701", "S1801",
                                                      "S1901", "S2001"),
                                        is.disdecrease = FALSE,
                                        kPedalMod = 0.01,
                                        kCalcBrakeType = "BrakePedal")

plot.MLsedanappbrake <- ggplot(data = df.MLsedanappbrake,
                               aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = c(50, 50), xend = c(1375, 8050),
           y = c(11.9, 0), yend = c(11.9, 0),
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 1675, xend = 8050, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(1675, 1675), xend = c(8050, 8050),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 50, xend = 1375, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 1375, xend = 1555, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 1555, xend = 1675, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 50, xend = 1375, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 50, xend = 1375, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 1375, xend = 1375, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 1375, xend = 1555, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 1375, xend = 1555, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 50, xmax = 1375, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(50, 50, 1375, 1375, 1555, 1555, 1555),
           xend = c(1375, 1375, 1555, 1555, 1599, 1675, 1675),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(1375, 1375, 1375),
           xend = c(1420, 1465, 1510),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MLsedanappbrake


# 7.2 货车，开始制动位置分布----
df.MLtruckappbrake <- CalcBatchAppBrake(data = df.MLtrucktraj,
                                        kDriverID = c("T0101", "T0201", "T0301",
                                                      "T0401", "T0501", "T0601",
                                                      "T0701"),
                                        is.disdecrease = FALSE,
                                        kPedalMod = 0.01,
                                        kCalcBrakeType = "BrakePedal")

plot.MLtruckappbrake <- ggplot(data = df.MLtruckappbrake,
                               aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = c(50, 50), xend = c(1375, 8050),
           y = c(11.9, 0), yend = c(11.9, 0),
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 1675, xend = 8050, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(1675, 1675), xend = c(8050, 8050),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 50, xend = 1375, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 1375, xend = 1555, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 1555, xend = 1675, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 50, xend = 1375, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 50, xend = 1375, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 1375, xend = 1375, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 1375, xend = 1555, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 1375, xend = 1555, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 50, xmax = 1375, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(50, 50, 1375, 1375, 1555, 1555, 1555),
           xend = c(1375, 1375, 1555, 1555, 1599, 1675, 1675),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(1375, 1375, 1375),
           xend = c(1420, 1465, 1510),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MLtruckappbrake


# 8 左线，开始加速位置分布分析----
# 8.1 轿车，开始加速位置分布----
df.MLsedanappgas <- CalcBatchAppGas(data = df.MLsedantraj,
                                    kDriverID = c("S0101", "S0201", "S0301",
                                                  "S0401", "S0501", "S0601",
                                                  "S0701", "S0801", "S0901",
                                                  "S1001", "S1101", "S1201",
                                                  "S1301", "S1401", "S1501",
                                                  "S1601", "S1701", "S1801",
                                                  "S1901", "S2001"),
                                    is.disdecrease = FALSE,
                                    kPedalMod = 0.01)

plot.MLsedanappgas <- ggplot(data = df.MLsedanappgas,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = c(50, 50), xend = c(1375, 8050),
           y = c(11.9, 0), yend = c(11.9, 0),
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 1675, xend = 8050, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(1675, 1675), xend = c(8050, 8050),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 50, xend = 1375, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 1375, xend = 1555, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 1555, xend = 1675, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 50, xend = 1375, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 50, xend = 1375, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 1375, xend = 1375, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 1375, xend = 1555, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 1375, xend = 1555, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 50, xmax = 1375, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(50, 50, 1375, 1375, 1555, 1555, 1555),
           xend = c(1375, 1375, 1555, 1555, 1599, 1675, 1675),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(1375, 1375, 1375),
           xend = c(1420, 1465, 1510),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MLsedanappgas


# 8.2 货车，开始加速位置分布----
df.MLtruckappgas <- CalcBatchAppGas(data = df.MLtrucktraj,
                                    kDriverID = c("T0101", "T0201", "T0301",
                                                  "T0401", "T0501", "T0601",
                                                  "T0701"),
                                    is.disdecrease = FALSE,
                                    kPedalMod = 0.01)

plot.MLtruckappgas <- ggplot(data = df.MLtruckappgas,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = c(50, 50), xend = c(1375, 8050),
           y = c(11.9, 0), yend = c(11.9, 0),
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 1675, xend = 8050, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(1675, 1675), xend = c(8050, 8050),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 50, xend = 1375, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 1375, xend = 1555, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 1555, xend = 1675, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 50, xend = 1375, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 50, xend = 1375, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 1375, xend = 1375, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 1375, xend = 1555, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 1375, xend = 1555, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 50, xmax = 1375, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(50, 50, 1375, 1375, 1555, 1555, 1555),
           xend = c(1375, 1375, 1555, 1555, 1599, 1675, 1675),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(1375, 1375, 1375),
           xend = c(1420, 1465, 1510),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MLtruckappgas


# 9 左线，大减速度分布----
# 9.1 轿车，大减速度分布----
# 9.1.1 轿车，大减速度，个体比较----
df.MLsedandecoutlier1 <- CalcBatchAccOutliers(data = df.MLsedantraj,
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
                                              kQuantile = 0.05,
                                              kAccLimit = NA,
                                              kOutliersType = "Dec",
                                              is.indv = TRUE)

plot.MLsedandecoutlier1 <- ggplot(data = df.MLsedandecoutlier1,
                                  aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = c(50, 50), xend = c(1375, 8050),
           y = c(11.9, 0), yend = c(11.9, 0),
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 1675, xend = 8050, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(1675, 1675), xend = c(8050, 8050),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 50, xend = 1375, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 1375, xend = 1555, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 1555, xend = 1675, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 50, xend = 1375, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 50, xend = 1375, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 1375, xend = 1375, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 1375, xend = 1555, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 1375, xend = 1555, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 50, xmax = 1375, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(50, 50, 1375, 1375, 1555, 1555, 1555),
           xend = c(1375, 1375, 1555, 1555, 1599, 1675, 1675),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(1375, 1375, 1375),
           xend = c(1420, 1465, 1510),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MLsedandecoutlier1


# 9.1.2 轿车，大减速度，群体比较----
df.MLsedandecoutlier2 <- CalcBatchAccOutliers(data = df.MLsedantraj,
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
                                              kQuantile = NA,
                                              kAccLimit = -3.0, 
                                              kOutliersType = "Dec",
                                              is.indv = FALSE)

plot.MLsedandecoutlier2 <- ggplot(data = df.MLsedandecoutlier2,
                                  aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = c(50, 50), xend = c(1375, 8050),
           y = c(11.9, 0), yend = c(11.9, 0),
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 1675, xend = 8050, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(1675, 1675), xend = c(8050, 8050),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 50, xend = 1375, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 1375, xend = 1555, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 1555, xend = 1675, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 50, xend = 1375, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 50, xend = 1375, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 1375, xend = 1375, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 1375, xend = 1555, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 1375, xend = 1555, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 50, xmax = 1375, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(50, 50, 1375, 1375, 1555, 1555, 1555),
           xend = c(1375, 1375, 1555, 1555, 1599, 1675, 1675),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(1375, 1375, 1375),
           xend = c(1420, 1465, 1510),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MLsedandecoutlier2


# 9.2 货车，大减速度分布----
# 9.2.1 货车，大减速度，个体比较----
df.MLtruckdecoutlier1 <- CalcBatchAccOutliers(data = df.MLtrucktraj,
                                              kDriverID = c("T0101", "T0201",
                                                            "T0301", "T0401",
                                                            "T0501", "T0601",
                                                            "T0701"),
                                              kQuantile = 0.05,
                                              kAccLimit = NA,
                                              kOutliersType = "Dec",
                                              is.indv = TRUE)

plot.MLtruckdecoutlier1 <- ggplot(data = df.MLtruckdecoutlier1,
                                  aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = c(50, 50), xend = c(1375, 8050),
           y = c(11.9, 0), yend = c(11.9, 0),
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 1675, xend = 8050, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(1675, 1675), xend = c(8050, 8050),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 50, xend = 1375, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 1375, xend = 1555, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 1555, xend = 1675, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 50, xend = 1375, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 50, xend = 1375, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 1375, xend = 1375, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 1375, xend = 1555, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 1375, xend = 1555, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 50, xmax = 1375, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(50, 50, 1375, 1375, 1555, 1555, 1555),
           xend = c(1375, 1375, 1555, 1555, 1599, 1675, 1675),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(1375, 1375, 1375),
           xend = c(1420, 1465, 1510),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MLtruckdecoutlier1


# 9.2.2 货车，大减速度，群体比较----
df.MLtruckdecoutlier2 <- CalcBatchAccOutliers(data = df.MLtrucktraj,
                                              kDriverID = c("T0101", "T0201",
                                                            "T0301", "T0401",
                                                            "T0501", "T0601",
                                                            "T0701"),
                                              kQuantile = NA,
                                              kAccLimit = -3.0, 
                                              kOutliersType = "Dec",
                                              is.indv = FALSE)

plot.MLtruckdecoutlier2 <- ggplot(data = df.MLtruckdecoutlier2,
                                  aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = c(50, 50), xend = c(1375, 8050),
           y = c(11.9, 0), yend = c(11.9, 0),
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 1675, xend = 8050, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(1675, 1675), xend = c(8050, 8050),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3
  annotate(geom = "segment", x = 50, xend = 1375, y = 19.6, yend = 19.6,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线4
  annotate(geom = "segment", x = 1375, xend = 1555, y = 19.6, yend = 15.5,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道边缘线5
  annotate(geom = "segment", x = 1555, xend = 1675, y = 15.5, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线1
  annotate(geom = "segment", x = 50, xend = 1375, y = 7.2, yend = 7.2,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道&匝道分割线2
  annotate(geom = "segment", x = 50, xend = 1375, y = 11.9, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 1375, xend = 1375, y = 7.2, yend = 11.9,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线1
  annotate(geom = "segment", x = 1375, xend = 1555, y = 7.2, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 渠化线2
  annotate(geom = "segment", x = 1375, xend = 1555, y = 11.9, yend = 7.74,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 50, xmax = 1375, ymin = 7.2, ymax = 11.9,
           colour = "black") +
  # 其他车道分隔线
  annotate(geom = "segment",
           x = c(50, 50, 1375, 1375, 1555, 1555, 1555),
           xend = c(1375, 1375, 1555, 1555, 1599, 1675, 1675),
           y = c(3.6, 15.75, 3.6, 15.75, 3.87, 11.61, 7.74),
           yend = c(3.6, 15.75, 3.87, 11.61, 3.87, 7.64, 3.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(1375, 1375, 1375),
           xend = c(1420, 1465, 1510),
           y = c(8.37, 9.54, 10.71),
           yend = c(7.3, 7.4, 7.6),
           size = 1, colour = "black", linetype = "solid") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 19.6, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4",
                                "LK3", "LK2", "LK1", "LK0")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MLtruckdecoutlier2















