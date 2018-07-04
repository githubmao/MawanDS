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

