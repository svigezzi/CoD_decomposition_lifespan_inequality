##############################################
# Aim: Functions to be used
# Date: 17/02/2023
# Project: Cause-of-death determinants of lifespan inequality
# Author: Serena Vigezzi
##############################################

# Closing lifetable at age x

lt95 <- function(data, age=95){
  
  if(data$age[length(data$age)]>=age){
    agef <- ifelse(data$lx[data$age==age]>0, 
                   age, 
                   data$age[which(data$lx>0)][length(which(data$lx>0))])
  } else{
    agef <- data$age[which(data$lx>0)][length(which(data$lx>0))]
  }
  
  lx.new <- sum(data$lx[data$age==agef])
  
  dx.new <- sum(data$dx[data$age>=agef])
  
  ax.new <- ifelse(lx.new==data$lx[data$age==agef],
                   data$ax[data$age==agef],
                   sum(data$ax[data$age>=agef & data$lx!=0]))
  
  qx.new <- 1
  
  Lx.new <- sum(data$lx_2[data$age>=agef])
  
  Tx.new <- sum(data$tx[data$age>=agef])
  
  ex.new <- Tx.new/lx.new
  
  mx.new <- ifelse(Lx.new==0,
                   0,
                   dx.new/Lx.new)
  
  n <- c(diff(data$age[data$age<=agef]),ax.new)
  mx <- c(data$mx[data$age<agef], mx.new)
  qx <- c(data$qx[data$age<agef], qx.new)
  px <- c(1-qx)
  ax <- c(data$ax[data$age<agef], ax.new)
  dx <- c(data$dx[data$age<agef], dx.new)
  lx <- c(data$lx[data$age<agef], lx.new)
  Lx <- c(data$lx_2[data$age<agef], Lx.new)
  Tx <- c(data$tx[data$age<agef], Tx.new)
  ex <- c(data$ex[data$age<agef], ex.new)
  
  age <- c(data$age[data$age<agef],agef)
  Year <- data$Year[data$age<=agef]
  
  lifetable <- data.table(Year=Year,age=age,
                          n=n,mx=round(mx,5),qx=round(qx,5),
                          px=round(px,5),ax=round(ax,2),
                          dx=round(dx,0),lx=round(lx,0),Lx=round(Lx,0),
                          Tx=round(Tx,0),ex=round(ex,2))
  
  return(lifetable)
}

#- Multi decrement life table function (from JAA, slightly changed)

# Cut vector at some item
cut <- function(x, cut){
  x[1:cut]
}

MLT <- function(id, causes, data){
  print(id)
  
  #Select all the variables needed for the LT just by
  #using country and year
  country<- data[[id]]$country
  year   <- data[[id]]$year
  sex    <- data[[id]]$sex
  mx     <- data[[id]]$mx
  qx <- data[[id]]$qx
  ax <- data[[id]]$ax
  n     <- c(diff(data[[id]]$age),as.numeric(last(ax)))
  dx <- data[[id]]$dx
  lx  <- data[[id]]$lx
  ex  <- data[[id]]$ex
  Lx  <- data[[id]]$lx_2
  Tx  <- data[[id]]$tx
  
  age    <- data[[id]]$age
  
  # Create list with cause-specific deaths
  d_i <- list()
  for(i in causes){
    d_i[i] <- data[[id]] %>% select(paste0("d_",i))
  }
  d_tot  <- data[[id]][, "d_1"]
  
  data[[id]][, "d_1"] - data[[id]][, "d_4"]
  
  l <- length(d_tot$d_1)
  for(i in l:1){
    sum <- sum(d_tot$d_1[i:l])
    if(sum!=0){
      here <- i
      break
    }
  }
  
  d_tot <- cut(d_tot$d_1,cut=here)
  d_i <- lapply(d_i,cut,cut=here)
  qx <- cut(qx,here)
  lx <- cut(lx,here)
  n <- cut(n,here)
  ax <- cut(ax,here)
  Lx <- cut(Lx,here)
  age <- cut(age,here)
  
  # Calculating the decrements: qx_i
  qx_i <- list()
  for(i in causes){
    qx_i[[i]] <- (d_i[[i]]/d_tot*as.numeric(qx))
    qx_i[[i]][d_tot==0] <- 0
  }
  
  # Calculating the decrements: dx_i
  dx_i <- list()
  for(i in causes){
    dx_i[[i]] <- qx_i[[i]]*as.numeric(lx)
  }
  
  # Readjusting to lifetable radix
  tot_i <- sum(unlist(lapply(dx_i[-1], sum, na.rm=T)))
  adj_i <- 100000/tot_i
  
  tot <- sum(dx_i[[1]], na.rm=T)
  adj <- 100000/tot
  
  dx_i <- append(list(dx_i[[1]]*adj),lapply(dx_i[-1], function(x) x*adj_i))
  
  # Lifetable elements
  qx_i <- list()
  for(i in causes){
    qx_i[[i]] <- dx_i[[i]]/as.numeric(lx)
  }
  qx_i[[1]][length(qx_i[[1]])] <- 1
  
  lx_i <- list()
  for(i in causes){
    lx_i[[i]] <- rev(cumsum(rev(dx_i[[i]][!is.na(dx_i[[i]])])))
    lx_i[[i]][is.na(dx_i[[i]])] <- 0
  }
  
  px_i <- list()
  for(i in causes){
    px_i[[i]] <- 1 - qx_i[[i]]
  }
  px_i[[1]][length(px_i[[1]])] <- 0
  
  Lx_i <- list()
  for(i in causes){
    Lx_i[[i]] <- lead(lx_i[[i]])*as.numeric(n) + dx_i[[i]]*as.numeric(ax)
    Lx_i[[i]][length(n)] <- lx_i[[i]][length(n)] * as.numeric(ax[length(n)])
  }
  
  mx_i <- list()
  for(i in causes){
    mx_i[[i]] <- dx_i[[i]]/as.numeric(Lx)
  }
  
  Tx_i <- list()
  for(i in causes){
    Tx_i[[i]] <- rev(cumsum(rev(Lx_i[[i]])))
  }
  
  ex_i <- list()
  for(i in causes){
    ex_i[[i]] <- Tx_i[[i]]/lx_i[[i]]
    ex_i[[i]][lx_i[[i]]==0] <- 0
  }
  
  # Constructing the data frame that will be shown in the output
  MDLT <- data.table()
  for(i in causes){
    x <- do.call(rbind, Map(data.frame, 
                            mx_i=mx_i[[i]], qx_i=qx_i[[i]], px_i=px_i[[i]], dx_i=dx_i[[i]],
                            lx_i=lx_i[[i]], Lx_i=Lx_i[[i]], Tx_i=Tx_i[[i]], ex_i=ex_i[[i]]))
    
    x <- cbind(country=substr(id,2,5),year=substr(id,6,9),sex=substr(id,1,1),
               age=as.numeric(age),cause=i,ax=as.numeric(ax),x)
    MDLT <- rbind(MDLT,x)
  }
  
  return(MDLT)
}

# Function for IPM difference decomposition

counterfactual <- function(id,data,causes,all,measure){
  
  # id: name of list item to extract (using lapply)
  # data: dataset to use, in form of list
  # causes: names to assign to causes in the output
  # all: name of category for all-cause deaths
  # measure: either v for variance or cv2 for coefficient of variation squared
  
  # Extract list item
  data <- data[[id]]
  
  # Separate all-cause contributions
  data_all <- data %>% filter(cause==all)
  data <- data %>% filter(cause!=all)
  
  # Separate first and second year
  data1 <- data[1:(dim(data)[1]/2),]
  pars1 <- c(data1$P[1:length(causes)],data1$M[1:length(causes)],
             data1$I[1:length(causes)],data1$C[1:length(causes)])
  
  data2 <- data[(dim(data)[1]/2 + 1):dim(data)[1],]
  pars2 <- c(data2$P[1:length(causes)],data2$M[1:length(causes)],
             data2$I[1:length(causes)],data2$C[1:length(causes)])
  
  # Warnings
  if(anyNA(pars1)){
    warning("pars1 has NAs. They will be omitted from calculations")
  }
  
  if(anyNA(pars2)){
    warning("pars2 has NAs. They will be omitted from calculations")
  }
  
  if(!(measure%in%c("v","cv2"))){
    stop("Only `v` for variance and `cv2` for coefficient of variation squared supported")
  }
  
  
  # Identify each parameter for eahc year
  l <- length(pars1)/4
  
  P1 <- pars1[1:l]
  M1 <- pars1[(l+1):(2*l)]
  I1 <- pars1[(2*l+1):(3*l)]
  cont1 <- pars1[(3*l+1):(4*l)]
  
  P2 <- pars2[1:l]
  M2 <- pars2[(l+1):(2*l)]
  I2 <- pars2[(2*l+1):(3*l)]
  cont2 <- pars2[(3*l+1):(4*l)]
  
  # Estimate counterfactual contributions by path
  if(measure=="v"){
    C1 <- P2*((M1-sum(P2*M1))^2+I2)
    C2 <- P1*((M2-sum(P1*M2))^2+I2)
    C3 <- P2*((M2-sum(P2*M2))^2+I1)
    C4 <- P1*((M2-sum(P1*M2))^2+I1)
    C5 <- P2*((M1-sum(P2*M1))^2+I1)
    C6 <- P1*((M1-sum(P1*M1))^2+I2)
  }
  
  if(measure=="cv2"){
    C1 <- P2*((M1-sum(P2*M1))^2+I2)/sum(P2*M1)^2
    C2 <- P1*((M2-sum(P1*M2))^2+I2)/sum(P1*M2)^2
    C3 <- P2*((M2-sum(P2*M2))^2+I1)/sum(P2*M2)^2
    C4 <- P1*((M2-sum(P1*M2))^2+I1)/sum(P1*M2)^2
    C5 <- P2*((M1-sum(P2*M1))^2+I1)/sum(P2*M1)^2
    C6 <- P1*((M1-sum(P1*M1))^2+I2)/sum(P1*M1)^2
  }
  
  # Estimate total counterfactual contributions
  contr_P <- (2*(cont2-C1)+2*(C4-cont1)+(C2-C6)+(C3-C5))/6
  contr_M <- (2*(cont2-C3)+2*(C6-cont1)+(C1-C5)+(C2-C4))/6
  contr_I <- (2*(cont2-C2)+2*(C5-cont1)+(C3-C4)+(C1-C6))/6
  
  table <- as.data.table(cbind(cause=causes,P=contr_P,M=contr_M,I=contr_I,
                               pop_name=unique(data$pop_name), sex=unique(data$sex)))
  
  return(table)
}

# Compare all-cause and cause specific variance/cv2 as a proportion of all-cause variance/cv2
check.var.per <- function(data, id){
  data <- data[[id]]
  var_i <- sum(data[-1]$C, na.rm=T)
  var <- data[1]$C
  diff <- (var-var_i)/var
  # data <- data.table(var_all = var,
  #                    var_sum = var_i,
  #                    diff = diff)
  return(diff)
}

# Compare all-cause and cause specific variance/cv2 as an absolute difference
check.var.abs <- function(data, id){
  data <- data[[id]]
  var_i <- sum(data[-1]$C, na.rm=T)
  var <- data[1]$C
  diff <- (var-var_i)
  # data <- data.table(var_all = var,
  #                    var_sum = var_i,
  #                    diff = diff)
  return(diff)
}
