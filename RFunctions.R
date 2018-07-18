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
                            is.main2ramp = NA,
                            is.disdecrease = NA){
  # 依据 dsToLeftBorder 计算模拟车行车轨迹。
  #
  # 输入：
  # data：重命名后的数据框，含 disToLeftBorder, roadName等数据。
  # is.main2ramp：主线进入匝道为 TRUE，匝道进入主线为 FALSE，默认为NA。
  # is.disdecrease：data中的disTraveled变量是否为递减趋势，默认为NA。
  #
  # 输出：含行车轨迹变量 drivingTraj 的数据框。
  
  if (is.na(is.main2ramp)) {
    stop("Please input the 'is.main2ramp'.")  # 没有输入is.main2ramp
  } else if (is.na(is.disdecrease)) {
    stop("Please input the 'is.disdecrease'.")  # 没有输入is.disdecrease
  } else if (is.logical(is.main2ramp) & is.logical(is.disdecrease)) {
    
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
      if (is.main2ramp) {
        tmp.subdata1$drivingTraj <- tmp.subdata1$disToLeftBorder  # 主线轨迹
        tmp.subdata2$drivingTraj <- tmp.subdata2$disToLeftBorder +
          kDeltaDisBorder  # 匝道轨迹
      } else {
        tmp.subdata1$drivingTraj <- tmp.subdata1$disToLeftBorder +
          kDeltaDisBorder  # 匝道轨迹
        tmp.subdata2$drivingTraj <- tmp.subdata2$disToLeftBorder  # 主线轨迹
      }
      
      return(rbind(tmp.subdata1, tmp.subdata2))
    }
  } else {
    stop("Please check the input 'data', 'is.main2ramp' and 'is.disdecrease'.\
  The 'data' should be a Rioh 8dof driving simulator data.\
  The 'is.main2ramp' should be a logical variable.\
  The 'is.disdecrease' should be a logical variable.")
  }
}


# 批量计算行车轨迹----
CalcTraj <- function(data,
                     kDriverID = NA,
                     is.main2ramp = NA,
                     is.disdecrease = NA){
  # 依据CalcDrivingTraj函数，批量计算行车轨迹。
  #
  # 输入：
  # data：重命名后的数据框，含 disToLeftBorder, roadName等数据。
  # kDriverID：需要计算的驾驶人ID向量集，默认为NA。
  # is.main2ramp：CalcDrivingTraj函数中的is.main2ramp参数值，默认为NA。
  # is.disdecrease：CalcDrivingTraj函数中的is.disdecrease参数值，默认为NA。
  #
  # 输出：含行车轨迹变量 drivingTraj 的数据框。
  
  if (is.character(kDriverID)) {
    
    df.traj <- data.frame()  # 输出数据康
    
    for (kIDidx in 1:length(kDriverID)) {  # 依次计算各个驾驶人的DrivingTraj
      tmpdf.calctraj <- CalcDrivingTraj(data[data$driverID == kDriverID[kIDidx],],
                                        is.main2ramp = is.main2ramp,
                                        is.disdecrease = is.disdecrease)
      
      df.traj <- rbind(df.traj, tmpdf.calctraj)
    }
    
    return(df.traj)
    
  } else {
    stop("Please check the input 'kDriverID'.\
  The 'kDriverID' should be a character vector variable.")
  }
}


# 计算车道跨越点----
CalcLCPoint <- function(data,
                        kLatDis = NA,
                        is.main2ramp = NA,
                        is.disdecrease = NA){
  # 计算车道跨越点的数据行行号，车道跨越判定原则：依据DisToLeftBorder输出值，即
  # drivingTraj值，设定阈值，模拟车从主线进入匝道时，车道跨越位置为前一时刻该输
  # 出值小于阈值、且下一时刻该输出值大于或等于阈值的位置点（匝道进入主线反之）。
  #
  # 输出：
  # data：重命名且经过CalcDrivingTraj函数计算的数据框。
  # kLatDis：车道跨越阈值，默认为NA。
  # is.main2ramp：CalcDrivingTraj函数中的is.main2ramp参数值，默认为NA。
  # is.disdecrease：CalcDrivingTraj函数中的is.disdecrease参数值，默认为NA。
  #
  # 输出：车道跨越点的位置行号，有多个满足判定条件的位置点（即反复换道时），输出
  #       为一组数值向量。
  
  if (is.na(kLatDis)) {
    stop("Please input the 'kLatDis'.")  # 没有输出kLatDis
  } else if (is.na(is.main2ramp)) {
    stop("Please input the 'is.main2ramp'.")  # 没有输出is.main2ramp
  } else if (is.na(is.disdecrease)) {
    stop("Please input the 'is.disdecrease'.")  # 没有输入is.disdecrease
  } else if (is.numeric(kLatDis) & is.logical(is.main2ramp) &
             is.logical(is.disdecrease)) {
    
    data <- data[order(data$disTravelled,
                       decreasing = is.disdecrease),]  # 按桩号排列数据
    
    # 生成前一时刻的drivingTraj
    tmp.data <- transform(data,
                          drivingTrajB4 = c(drivingTraj[1],
                                            drivingTraj[-length(drivingTraj)]))
    
    # 计算车道跨越点数据行号
    if (is.main2ramp) {
      kLCRowNo <- which(tmp.data$drivingTraj > kLatDis &
                          tmp.data$drivingTrajB4 <= kLatDis)
    } else {
      kLCRowNo <- which(tmp.data$drivingTraj < kLatDis &
                          tmp.data$drivingTrajB4 >= kLatDis)
    }
    
    return(kLCRowNo)
  } else {
    stop("Please check the input 'data', 'kLatDis', 'is.main2ramp'\
  and 'is.disdecrease'.\
  The 'data' should be a Rioh 8dof driving simulator data.\
  The 'kLatDis' should be a numeric variable.\
  The 'is.main2ramp' should be a logical variable.\
  The 'is.disdecrease' should be a logical variable.")
  }
}


# 批量计算车道跨越点及其他特征点的车速----
CalcDisSpeed <- function(data,
                         kDriverID = NA,
                         kDis = NA){
  
  
}












