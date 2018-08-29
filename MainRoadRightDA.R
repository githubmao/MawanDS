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
library(magrittr)
library(plyr)


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


# 1 右线，行驶速度----
# 1.1 轿车，行驶速度----
plot.MRWOTsedanspeed <- ggplot(data = df.mainrightWOTsedan,
                               aes(x = disFromRoadStart, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 120, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 120, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRWOTsedanspeed


# 1.2 货车，行驶速度----
plot.MRWOTtruckspeed <- ggplot(data = df.mainrightWOTtruck,
                               aes(x = disFromRoadStart, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 120, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 120, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRWOTtruckspeed


# 2 右线，加速度分析----
# 2.1 轿车，加速度----
plot.MRWOTsedanacc <- ggplot(data = df.mainrightWOTsedan,
                             aes(x = disFromRoadStart, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 5, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRWOTsedanacc


# 2.2 货车，加速度----
plot.MRWOTtruckacc <- ggplot(data = df.mainrightWOTtruck,
                             aes(x = disFromRoadStart, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 5, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRWOTtruckacc


# 3 右线，制动踏板位移分析----
# 3.1 轿车，制动踏板位移----
plot.MRWOTsedanbrakepedal <- ggplot(data = df.mainrightWOTsedan,
                                    aes(x = disFromRoadStart, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRWOTsedanbrakepedal


# 3.2 货车，制动踏板位移----
plot.MRWOTtruckbrakepedal <- ggplot(data = df.mainrightWOTtruck,
                                    aes(x = disFromRoadStart, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRWOTtruckbrakepedal


# 4 右线，油门踏板位移分析----
# 4.1 轿车，油门踏板位移----
plot.MRWOTsedangaspedal <- ggplot(data = df.mainrightWOTsedan,
                                  aes(x = disFromRoadStart, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRWOTsedangaspedal


# 4.2 货车，油门踏板位移----
plot.MRWOTtruckgaspedal <- ggplot(data = df.mainrightWOTtruck,
                                  aes(x = disFromRoadStart, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRWOTtruckgaspedal


# 5 右线，轨迹分析----
# 5.1 轿车，全线，轨迹----
df.MRWOTsedantraj <- transform(df.mainrightWOTsedan,
                               drivingTraj = disToLeftBorder)

plot.MRWOTsedantraj <- ggplot(data = df.MRWOTsedantraj,
                              aes(x = disFromRoadStart, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 969.9, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(969.9, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(969.9, 969.9),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1.5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 9, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWOTsedantraj


# 5.2 轿车，大铲湾出口，轨迹----
# 5.2.1 轿车，大铲湾出口，轨迹，无交通流----
plot.MRexitWOTsedantraj <- ggplot(data = df.MRWOTsedantraj,
                                  aes(x = disFromRoadStart, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRexitWOTsedantraj


# 5.2.2 轿车，大铲湾出口，轨迹，有交通流----
df.MRWTsedantraj <- transform(df.mainrightWTsedan,
                              drivingTraj = disToLeftBorder)

plot.MRexitWTsedantraj <- ggplot(data = df.MRWTsedantraj,
                                 aes(x = disFromRoadStart, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRexitWTsedantraj


# 5.3 货车，全线，轨迹----
df.MRWOTtrucktraj <- transform(df.mainrightWOTtruck,
                               drivingTraj = disToLeftBorder)

plot.MRWOTtrucktraj <- ggplot(data = df.MRWOTtrucktraj,
                              aes(x = disFromRoadStart, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 969.9, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(969.9, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(969.9, 969.9),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1.5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 9, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWOTtrucktraj


# 6 右线，大铲湾出口，车道跨越点分析----
# 6.1 轿车，大铲湾出口，车道跨越点，无交通流----
df.MRWOTsedantraj$disTravelled <- df.MRWOTsedantraj$disFromRoadStart

df.MRWOTsedanLCpoint <- CalcBatchLCPoint(data = df.MRWOTsedantraj,
                                         kDriverID =  c("S0101", "S0201",
                                                        "S0301", "S0401",
                                                        "S0501", "S0601",
                                                        "S0701", "S0801",
                                                        "S0901", "S1001",
                                                        "S1101", "S1201",
                                                        "S1301", "S1401",
                                                        "S1501", "S1601",
                                                        "S1701", "S1801",
                                                        "S1901", "S2001"),
                                         kLatDis = 7.64,
                                         is.main2ramp = FALSE,
                                         is.disdecrease = FALSE)

plot.MRWOTsedanLCpoint <- ggplot(data = df.MRWOTsedanLCpoint,
                                 aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.MRWOTsedanLCpoint$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.MRWOTsedanLCpoint$disTravelled) + 450,
           y = 4 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值 RK6+228")) +
  annotate(geom = "segment",
           x = median(df.MRWOTsedanLCpoint$disTravelled),
           xend = median(df.MRWOTsedanLCpoint$disTravelled) + 80,
           y = 4, yend = 4 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())
  
plot.MRWOTsedanLCpoint


# 6.2 轿车，大铲湾出口，车道跨越点，有交通流----
df.MRWTsedantraj$disTravelled <- df.MRWTsedantraj$disFromRoadStart

df.MRWTsedanLCpoint <- CalcBatchLCPoint(data = df.MRWTsedantraj,
                                        kDriverID =  c("S0101", "S0201",
                                                       "S0301", "S0401",
                                                       "S0501", "S0601",
                                                       "S0701", "S0801",
                                                       "S0901", "S1001",
                                                       "S1101", "S1201",
                                                       "S1301", "S1401",
                                                       "S1501", "S1601",
                                                       "S1701", "S1801",
                                                       "S1901", "S2001"),
                                        kLatDis = 7.64,
                                        is.main2ramp = FALSE,
                                        is.disdecrease = FALSE)

plot.MRWTsedanLCpoint <- ggplot(data = df.MRWTsedanLCpoint,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.MRWTsedanLCpoint$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.MRWTsedanLCpoint$disTravelled) + 450,
           y = 4 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值 RK6+277")) +
  annotate(geom = "segment",
           x = median(df.MRWTsedanLCpoint$disTravelled),
           xend = median(df.MRWTsedanLCpoint$disTravelled) + 80,
           y = 4, yend = 4 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWTsedanLCpoint


# 7 右线，开始制动位置分布分析----
# 7.1 轿车，开始制动位置分布----
df.MRWOTsedanappbrake <- CalcBatchAppBrake(data = df.MRWOTsedantraj,
                                           kDriverID =  c("S0101", "S0201",
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

plot.MRWOTsedanappbrake <- ggplot(data = df.MRWOTsedanappbrake,
                                  aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 969.9, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(969.9, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(969.9, 969.9),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1.5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 9, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWOTsedanappbrake


# 7.2 轿车，大铲湾出口，开始制动位置分布----
# 7.2.1 轿车，大铲湾出口，开始制动位置分布，无交通流----
plot.MRexitWOTsedanappbrake <- ggplot(data = df.MRWOTsedanappbrake,
                                      aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRexitWOTsedanappbrake


# 7.2.2 轿车，大铲湾出口，开始制动位置分布，有交通流----
df.MRWTsedanappbrake <- CalcBatchAppBrake(data = df.MRWTsedantraj,
                                          kDriverID =  c("S0101", "S0201",
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

plot.MRexitWTsedanappbrake <- ggplot(data = df.MRWTsedanappbrake,
                                     aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRexitWTsedanappbrake


# 7.3 货车，开始制动位置分布----
df.MRWOTtrucktraj$disTravelled <- df.MRWOTtrucktraj$disFromRoadStart

df.MRWOTtruckappbrake <- CalcBatchAppBrake(data = df.MRWOTtrucktraj,
                                           kDriverID = c("T0101", "T0201",
                                                         "T0301", "T0401",
                                                         "T0501", "T0601",
                                                         "T0701"),
                                           is.disdecrease = FALSE,
                                           kPedalMod = 0.01,
                                           kCalcBrakeType = "BrakePedal")

plot.MRWOTtruckappbrake <- ggplot(data = df.MRWOTtruckappbrake,
                                  aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 969.9, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(969.9, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(969.9, 969.9),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1.5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 9, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWOTtruckappbrake


# 8 右线，开始加速位置分布分析----
# 8.1 轿车，开始加速位置分布----
df.MRWOTsedanappgas <- CalcBatchAppGas(data = df.MRWOTsedantraj,
                                       kDriverID =  c("S0101", "S0201",
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

plot.MRWOTsedanappgas <- ggplot(data = df.MRWOTsedanappgas,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 969.9, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(969.9, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(969.9, 969.9),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1.5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 9, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWOTsedanappgas


# 8.2 轿车，大铲湾出口，开始加速位置分布----
# 8.2.1 轿车，大铲湾出口，开始加速位置分布，无交通流----
plot.MRexitWOTsedanappgas <- ggplot(data = df.MRWOTsedanappgas,
                                    aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRexitWOTsedanappgas


# 8.2.2 轿车，大铲湾出口，开始加速位置分布，有交通流----
df.MRWTsedanappgas <- CalcBatchAppGas(data = df.MRWTsedantraj,
                                      kDriverID =  c("S0101", "S0201",
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

plot.MRexitWTsedanappgas <- ggplot(data = df.MRWTsedanappgas,
                                   aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRexitWTsedanappgas


# 8.3 货车，开始加速位置分布----
df.MRWOTtruckappgas <- CalcBatchAppGas(data = df.MRWOTtrucktraj,
                                       kDriverID = c("T0101", "T0201",
                                                     "T0301", "T0401",
                                                     "T0501", "T0601",
                                                     "T0701"),
                                       is.disdecrease = FALSE,
                                       kPedalMod = 0.01)

plot.MRWOTtruckappgas <- ggplot(data = df.MRWOTtruckappgas,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 969.9, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(969.9, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(969.9, 969.9),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1.5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 9, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWOTtruckappgas


# 9 右线，大减速度分布----
# 9.1 轿车，大减速度分布----
# 9.1.1 轿车，大减速度，个体比较----
df.MRWOTsedandecoutlier1 <- CalcBatchAccOutliers(data = df.MRWOTsedantraj,
                                                 kDriverID =  c("S0101", "S0201",
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

plot.MRWOTsedandecoutlier1 <- ggplot(data = df.MRWOTsedandecoutlier1,
                                     aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 969.9, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(969.9, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(969.9, 969.9),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1.5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 9, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWOTsedandecoutlier1


# 9.1.2 轿车，大减速度，群体比较----
df.MRWOTsedandecoutlier2 <- CalcBatchAccOutliers(data = df.MRWOTsedantraj,
                                                 kDriverID =  c("S0101", "S0201",
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

plot.MRWOTsedandecoutlier2 <- ggplot(data = df.MRWOTsedandecoutlier2,
                                     aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 969.9, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(969.9, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(969.9, 969.9),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1.5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 9, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWOTsedandecoutlier2


# 9.2 轿车，大铲湾出口，大减速度分布----
# 9.2.1 轿车，大铲湾出口，大减速度分布，无交通流----
# 9.2.1.1 轿车，大铲湾出口，大减速度，无交通流，个体比较----
plot.MRexitWOTsedandecoutlier1 <- ggplot(data = df.MRWOTsedandecoutlier1,
                                         aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRexitWOTsedandecoutlier1


# 9.2.1.2 轿车，大铲湾出口，大减速度，无交通流，群体比较----
plot.MRexitWOTsedandecoutlier2 <- ggplot(data = df.MRWOTsedandecoutlier2,
                                         aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRexitWOTsedandecoutlier2


# 9.2.2 轿车，大铲湾出口，大减速度分布，有交通流----
# 9.2.2.1 轿车，大铲湾出口，大减速度，有交通流，个体比较----
df.MRWTsedandecoutlier1 <- CalcBatchAccOutliers(data = df.MRWTsedantraj,
                                                kDriverID =  c("S0101", "S0201",
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

plot.MRexitWTsedandecoutlier1 <- ggplot(data = df.MRWTsedandecoutlier1,
                                        aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRexitWTsedandecoutlier1


# 9.2.2.2 轿车，大铲湾出口，大减速度，有交通流，群体比较----
df.MRWTsedandecoutlier2 <- CalcBatchAccOutliers(data = df.MRWTsedantraj,
                                                kDriverID =  c("S0101", "S0201",
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

plot.MRexitWTsedandecoutlier2 <- ggplot(data = df.MRWTsedandecoutlier2,
                                        aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 6800, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(6800, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(6800, 6800),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道位置
  annotate(geom = "rect", xmin = 6800, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 8490, y = 1, label = "主线隧道洞口") +
  annotate(geom = "segment",
           x = 7445, xend = 7525, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7485, y = 9, size = 3,
           label = c("渐变段80m")) +
  annotate(geom = "segment",
           x = 7525, xend = 7645, y = 10, yend = 10, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 7585, y = 10.5, size = 3,
           label = c("减速段120m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(6800, 9010),
                     breaks = c(seq(6959.9, 9010, by = 1000),
                                7445, 7525, 7645),
                     labels = c("RK6", "RK7", "RK8", "", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRexitWTsedandecoutlier2


# 9.3 货车，大减速度分布----
# 9.3.1 货车，大减速度，个体比较----
df.MRWOTtruckdecoutlier1 <- CalcBatchAccOutliers(data = df.MRWOTtrucktraj,
                                                 kDriverID =  c("T0101", "T0201",
                                                                "T0301", "T0401",
                                                                "T0501", "T0601",
                                                                "T0701"),
                                                 kQuantile = 0.05,
                                                 kAccLimit = NA,
                                                 kOutliersType = "Dec",
                                                 is.indv = TRUE)

plot.MRWOTtruckdecoutlier1 <- ggplot(data = df.MRWOTtruckdecoutlier1,
                                     aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 969.9, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(969.9, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(969.9, 969.9),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1.5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 9, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWOTtruckdecoutlier1


# 9.3.2 货车，大减速度，群体比较----
df.MRWOTtruckdecoutlier2 <- CalcBatchAccOutliers(data = df.MRWOTtrucktraj,
                                                 kDriverID =  c("T0101", "T0201",
                                                                "T0301", "T0401",
                                                                "T0501", "T0601",
                                                                "T0701"),
                                                 kQuantile = NA,
                                                 kAccLimit = -3.0,
                                                 kOutliersType = "Dec",
                                                 is.indv = FALSE)

plot.MRWOTtruckdecoutlier2 <- ggplot(data = df.MRWOTtruckdecoutlier2,
                                     aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 969.9, xend = 9010, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment",
           x = c(969.9, 7645),
           xend = c(7665, 9010),
           y = c(11.45, 7.64),
           yend = c(11.45, 7.64),
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道分隔线
  annotate(geom = "segment",
           x = c(969.9, 969.9),
           xend = c(9010, 7645),
           y = c(3.82, 7.64),
           yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  
  # 隧道位置
  annotate(geom = "rect", xmin = 2650, xmax = 8490,
           ymin = 0, ymax = 11.45, alpha = 0.2) +
  annotate(geom = "text", x = 2650, y = 1.5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 8490, y = 9, label = "主线隧道洞口") +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(969.9, 9010),
                     breaks = seq(959.9, 9010, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4",
                                "RK5", "RK6", "RK7", "RK8")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.MRWOTtruckdecoutlier2



