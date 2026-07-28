
# Descomposicion - Colmex 2026
# Funciones

# Funcion para utilizar con el paquete DemoDecomp
# Esperanza de vida saludable
healthy_ex <- function(mx_prev, age, sex = NULL, ax = NULL) {
  m <- length(age)
  
  mx <- mx_prev[1:m]
  prev <- mx_prev[(m+1):(2*m)]
  
  n <- c(diff(age), NA)
  if(is.null(ax)){
    ax <- rep(0,m)
    if(age[1]!=0 | age[2]!=1){
      ax <- n/2
      ax[m] <- 1 / mx[m]
    }else{    
      if(sex=="F"){
        if(mx[1]>=0.107){
          ax[1] <- 0.350
        }else{
          ax[1] <- 0.053 + 2.800*mx[1]
        }
      }
      if(sex=="M"){
        if(mx[1]>=0.107){
          ax[1] <- 0.330
        }else{
          ax[1] <- 0.045 + 2.684*mx[1]
        }
      }
      ax[-1] <- n[-1]/2
      ax[m] <- 1 / mx[m]
    }
  }
  
  qx <- (n * mx)/(1 + (n - ax) * mx)
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  
  px <- 1 - qx
  lx <- cumprod(c(1, px))
  lx <- lx[1:length(age)]
  dx <- -diff(lx)
  dx[length(age)] <- lx[length(age)] 
  lxpn <- c(lx[-1],0)
  Lxpn <- n * lxpn + dx * ax
  Lx <- c(Lxpn[-length(Lxpn)], lx[length(lx)]/mx[length(mx)])
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx

  Lx_healthy = Lx*(1-prev)
  Tx_healthy <- rev(cumsum(rev(Lx_healthy)))
  ex_healthy = Tx_healthy/lx

  e0_healthy <- ex_healthy[1]
  
  return(e0_healthy)
}
