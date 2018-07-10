#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180626, By MaoYan
# 
# 1.妈湾（施工图阶段/初设图纸）驾驶模拟数据分析所用函数汇总
#------------------------------------------------------------------------------#


# UC-winRoad V12数据重命名----
RenameSimDataV12 <- function(data){
  # 重命名UC-winRoad V12版本输出数据变量名。
  #
  # 输入：UC-winRoad V12版本输出数据。
  # 输出：重命名后的数据框。
  
  kSimDataNewName <- c("simTime",         # Time
                       "logTime",         # TimeStamp
                       "type",            # Type
                       "carModel",        # Model
                       "logID",           # ID
                       "customID",        # customID
                       "logDescription",  # description
                       "positionX",       # position X
                       "positionY",       # position Y
                       "positionZ",       # position Z
                       "yawAngle",        # yawAngle
                       "pitchAngle",      # pitchAngle
                       "rollAngle",       # rollAngle
                       "directionX",      # direction X
                       "directionY",      # direction Y
                       "directionZ",      # direction Z
                       "bodyPitchAngle",  # bodyPitchAngle
                       "bodyRollAngle",   # bodyRollAngle
                       "RPM",             # RPM
                       "gearNumber",      # gearNumber
                       "speedXMS",        # speedVectInMetresPerSecond X
                       "speedYMS",        # speedVectInMetresPerSecond Y
                       "speedZMS",        # speedVectInMetresPerSecond Z
                       "speedKMH",        # speedInKmPerHour
                       "speedMS",         # speedInMetresPerSecond
                       "accXMS2",         # localAccelInMetresPerSecond2 X
                       "accYMS2",         # localAccelInMetresPerSecond2 Y
                       "accZMS2",         # localAccelInMetresPerSecond2 Z
                       "bodyRotSpeedYawRS",   # bodyRotSpeedInRadsPerSecond Yaw
                       "bodyRotSpeedPitchRS", # bodyRotSpeedInRadsPerSecond Pitch
                       "bodyRotSpeedRollRS",  # bodyRotSpeedInRadsPerSecond Roll
                       "bodyRotAccYawRS2",    # bodyRotAccelInRadsPerSecond Yaw
                       "bodyRotAccPitchRS2",  # bodyRotAccelInRadsPerSecond Pitch
                       "bodyRotAccRollRS2",   # bodyRotAccelInRadsPerSecond Roll
                       "rotSpeedYawRS",       # rotSpeedInRadsPerSecond Yaw
                       "rotSpeedPitchRS",     # rotSpeedInRadsPerSecond Pitch
                       "rotSpeedRollRS",      # rotSpeedInRadsPerSecond Roll
                       "rotAccYawRS2",        # rotAccelInRadsPerSecond Yaw
                       "rotAccPitchRS2",      # rotAccelInRadsPerSecond Pitch
                       "rotAccRollRS2",       # rotAccelInRadsPerSecond Roll
                       "disTravelled",        # distanceTravelled
                       "steeringValue",       # steering
                       "appSteering",         # appliedSteering
                       "steeringVelocity",    # steeringVelocity
                       "turningCurvature",    # turningCurvature
                       "gasPedal",            # throttle
                       "pedalTorque",         # pedalTorque
                       "appGasPedal",         # appliedThrottle
                       "brakePedal",          # brake
                       "appBrake",            # appliedBrake
                       "lightState",          # lightState
                       "automaticControl",    # automaticControl
                       "dragForce",           # dragForce
                       "carMass",             # mass
                       "carWheelBase",        # wheelBase
                       "centerOfGravityHeight",    # centerOfGravityHeight
                       "centerOfGravityPosition",  # centerOfGravityPosition
                       "rollAxisHeight",           # rollAxisHeight
                       "trailerState",             # trailer
                       "trailerAngle",             # trailerAngle
                       "trailerPitchAngle",        # trailerPitchAngle
                       "trailerWheelbase",         # trailerWheelbase
                       "isInIntersection",         # inIntersection
                       "roadName",                 # road
                       "disFromRoadStart",         # distanceAlongRoad
                       "latestRoad",               # latestRoad
                       "disFromLatestRoadStart",   # distanceAnlongLatestRoad
                       "disToLeftBorder",          # distanceToLeftBorder
                       "disToRightBorder",         # distanceToRightBorder
                       "carriagewayWidth",         # carriagewayWidth
                       "roadOffset",               # offsetFromRoadCenter
                       "laneOffset",               # offsetFromLaneCenter
                       "logitudinalSlope",         # roadLongitudinalSlop
                       "lateralSlope",             # roadLateralSlop
                       "laneNumber",               # laneNumber
                       "laneWidth",                # laneWidth
                       "laneDirectionX",           # laneDirection X
                       "laneDirectionY",           # laneDirection Y
                       "laneDirectionZ",           # laneDirection Z
                       "laneCurvature",            # laneCurvature
                       "isDrivingForward",         # drivingForwards
                       "speedLimit",               # speedLimit
                       "isSpeedOver",              # speedOver
                       "leftLaneOverlap",          # leftLaneOverLap
                       "rightLaneOverlap",         # rightLaneOverLap
                       "collisionWithUser",        # collicionWithUser
                       "pedestrianNumber",         # pedestriansNumber
                       "roadSurface",              # surface
                       "averageFlux")              # averageFlux
  
  names(data) <- kSimDataNewName
  return(data)
}


# 计算行车轨迹----
CalcDrivingTraj <- function(data,
                            kMainRoadName = NA,
                            kRampRoadName = NA,
                            kDeltaDisBorder = NA){
  # 依据 dsToLeftBorder 计算模拟车行车轨迹。
  #
  # 输入：
  # data：重命名后的数据框，含 disToLeftBorder, roadName等数据。
  # is.main2ramp：主线进入匝道为 TRUE，匝道进入主线为 FALSE，默认为TRUE。
  # kMainRoadName：主线道路名称，默认为NA。
  # kRampRoadName：匝道道路名称，默认为NA。
  # kDeltaDisBorder：主线与匝道Traj计算差，默认为NA。
  #
  # 输出：含行车轨迹变量 drivingTraj 的数据框。

  if (is.na(kDeltaDisBorder)) {
    stop("Please input the 'kDeltaDisBorder'.")  # 没有输入kDeltaDisBorder
  } else if (is.na(kMainRoadName) | is.na(kRampRoadName)) {
    stop("Please input the 'road name'.")  # 没有输入RoadName
  } else {
    
    data$DrivingTraj <- ifelse(test = data$roadName == kMainRoadName,
                               data$disToLeftBorder,
                               data$disToLeftBorder + kDeltaDisBorder)
  }
  return(data)  # 返回新数据框
}


# 计算CalcDrivingTraj中的 kDeltaDisBorder 参数----
CalcDeltaDisBorder <- function(data,
                               is.main2ramp = TRUE,
                               is.disdecrease = FALSE){
  # 计算CalcDrivingTraj中的 kDeltaDisBorder 参数。
  #
  # 输入：
  # data：重命名后的数据框，含 disToLeftBorder, roadName等数据。
  # is.main2ramp：主线进入匝道为 TRUE，匝道进入主线为 FALSE，默认为TRUE。
  # is.disdecrease：data中的disTraveled变量是否为递减趋势，默认为FALSE。
  #
  # 输出：kDeltaDisBorder计算值。
  
  data <- data[order(data$disTravelled,
                     decreasing = is.disdecrease),]  # data按桩号排序
  
  kRoadName <- unique(data$roadName)  # 数据所含道路名称
  
  tmp.subdata1 <- subset(x = data, subset = data$roadName == kRoadName[1])
  tmp.subdata2 <- subset(x = data, subset = data$roadName == kRoadName[2])
  
  if (length(kRoadName) != 2) {
    stop("The number of 'road name' is not equal to 2.")  # 异常情况
  } else {
    
    kData1Length <- length(tmp.subdata1$disToLeftBorder)  # tmp.subdata1行数
    
    # 主线进入匝道
    kDeltaDisBorder <- ifelse(test = is.main2ramp,
                              tmp.subdata1[kData1Length,]$disToLeftBorder -
                                tmp.subdata2[1,]$disToLeftBorder,
                              tmp.subdata2[1,]$disToLeftBorder -
                                tmp.subdata1[kData1Length,]$disToLeftBorder)
  }
  return(kDeltaDisBorder)
}



# 计算行车轨迹----
CalcDrivingTraj <- function(data,
                            is.main2ramp = NA,
                            is.disdecrease = NA){
  # 依据 dsToLeftBorder 计算模拟车行车轨迹。
  #
  # 输入：
  # data：重命名后的数据框，含 disToLeftBorder, roadName等数据。
  # kMainRoadName：主线道路名称，默认为NA。
  # kRampRoadName：匝道道路名称，默认为NA。
  # is.main2ramp：主线进入匝道为 TRUE，匝道进入主线为 FALSE，默认为NA。
  # is.disdecrease：data中的disTraveled变量是否为递减趋势，默认为NA。
  #
  # 输出：含行车轨迹变量 drivingTraj 的数据框。
  
  if (is.na(is.main2ramp)) {
    stop("Please input the 'is.main2ramp'.")  # 没有输入is.main2ramp
  } else if (is.na(is.disdecrease)) {
    stop("Please input the 'is.disdecrease'.")  # 没有输入is.disdecrease
  } else{
    
    data <- data[order(data$disTravelled,
                       decreasing = is.disdecrease),]  # data按桩号排列
    
    kRoadName <- unique(data$roadName)  # 数据所含道路名称
    
    if (length(kRoadName) != 2) {
      stop("The number of 'road name' is not equal to 2.")  # 异常情况
    } else {
      
      tmp.subdata1 <- subset(x = data, subset = data$roadName == kRoadName[1])
      tmp.subdata2 <- subset(x = data, subset = data$roadName == kRoadName[2])
      
      kData1Length <- length(tmp.subdata1$disToLeftBorder)  # tmp.subdata1行数
      
      # 计算主线与匝道Traj计算差
      kDeltaDisBorder <- abs(tmp.subdata1[kData1Length,]$disToLeftBorder -
                               tmp.subdata2[1,]$disToLeftBorder)
      
      # 计算行车轨迹drivingTraj
      tmp.subdata1$drivingTraj <- ifelse(test = is.main2ramp,
                                         tmp.subdata1$disToLeftBorder,
                                         tmp.subdata1$disToLeftBorder +
                                           kDeltaDisBorder)
      
      tmp.subdata2$drivingTraj <- ifelse(test = is.main2ramp,
                                         tmp.subdata2$disToLeftBorder +
                                           kDeltaDisBorder,
                                         tmp.subdata2$disToLeftBorder)
      
      return(rbind(tmp.subdata1, tmp.subdata2))
    }
  }
}


data <- df.zd1WOTsedan[df.zd1WOTsedan$driverID == "S0101",]
data <- data[order(data$disTravelled,
                   decreasing = FALSE),]  # data按桩号排列

kRoadName <- unique(data$roadName)  # 数据所含道路名称

tmp.subdata1 <- subset(x = data, subset = data$roadName == kRoadName[1])
tmp.subdata2 <- subset(x = data, subset = data$roadName == kRoadName[2])

kData1Length <- length(tmp.subdata1$disToLeftBorder)  # tmp.subdata1行数

# 计算主线与匝道Traj计算差
kDeltaDisBorder <- abs(tmp.subdata1[kData1Length,]$disToLeftBorder -
                         tmp.subdata2[1,]$disToLeftBorder)

# 计算行车轨迹drivingTraj
tmp.subdata1$drivingTraj <- tmp.subdata1$disToLeftBorder + kDeltaDisBorder
  
  
  ifelse(test = FALSE,
                                   tmp.subdata1$disToLeftBorder,
                                   tmp.subdata1$disToLeftBorder +
                                     kDeltaDisBorder)

tmp.subdata2$drivingTraj <- ifelse(test = FALSE,
                                   tmp.subdata2$disToLeftBorder +
                                     kDeltaDisBorder,
                                   tmp.subdata2$disToLeftBorder)

tmp.subdata1$drivingTraj[1]
tmp.subdata1$drivingTraj[300]
tmp.subdata1$drivingTraj[500]

tmp.subdata1$disToLeftBorder[1]
tmp.subdata1$disToLeftBorder[300]
tmp.subdata1$disToLeftBorder[500]

tmp.subdata2$drivingTraj[1]
tmp.subdata2$drivingTraj[300]
tmp.subdata2$drivingTraj[500]

tmp.subdata2$disToLeftBorder[1]
tmp.subdata2$disToLeftBorder[300]
tmp.subdata2$disToLeftBorder[500]









df.test <- CalcDrivingTraj(df.zd1WOTsedan[df.zd1WOTsedan$driverID == "S0101",],
                           is.main2ramp = FALSE, is.disdecrease = FALSE)

ggplot(data = df.test, aes(x = disTravelled, y = drivingTraj)) +
  geom_line()


View(df.test$drivingTraj)

df.zd1$disTravelled

{
  
  data$DrivingTraj <- ifelse(test = data$roadName == kMainRoadName,
                             data$disToLeftBorder,
                             data$disToLeftBorder + kDeltaDisBorder)
}
return(data)  # 返回新数据框
unique(df.zd1WOTsedan$driverID)















CalcTraj <- function(data,
                     is.Main2Ramp = TRUE,
                     is.DisDecrease = FALSE,
                     kMainRoad = NA,
                     kRampRoad = NA){
  
  tmp.deltaborder <- CalcDeltaDisBorder(data,
                                        is.main2ramp = is.Main2Ramp,
                                        is.disdecrease = is.DisDecrease)
  df.result <- CalcDrivingTraj(data,
                               kMainRoadName = kMainRoad,
                               kRampRoadName = kRampRoad,
                               kDeltaDisBorder = tmp.deltaborder)
  
  return(df.result)
}

unique(df.zd1WOTsedan$roadName)
df.test <- CalcTraj(data = df.zd1WOTsedan,
                    is.Main2Ramp = FALSE,
                    is.DisDecrease = FALSE,
                    kMainRoad = "MainRD_Right",
                    kRampRoad = "ZD_1/3")





length(c(1,2,3,4)) == length(unique(c(1,2,3,4)))






















