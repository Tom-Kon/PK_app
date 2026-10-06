source("Helper functions.R")

ImmFunction <- function(input) {
  generalList <- generalParams(input)
  list2env(generalList, envir = environment())
  
  release_imm_list <- list()
  GI_imm_list <- list()
  B_imm_list <- list()
  

  if (length(starts_imm) > 0) {
    
    for (j in seq_along(starts_imm)) {
      
      start_idx <- which(t >= starts_imm[j])[1]
      
      if (is.na(start_idx)) next
      
      # Time points before the dose are simply zero
      t_before <- t[seq_len(start_idx - 1)]
      
      # Integrate from dose start onwards
      t_after <- t[start_idx:length(t)]
      diff(tail(t_after, 20))
      
      
      # Initial conditions at dose time
      state <- c(
        M_solid = imm_dose,
        M_GI    = 0,
        C_B     = 0
      )

      # Coupled dissolution / GI / blood model
      model <- function(time, state, parms) {
        
        with(as.list(c(state, parms)), {
          M_solid <- max(M_solid, 0)
          M_GI    <- max(M_GI, 0)
          C_B     <- max(C_B, 0)
          
          if (M_solid < 1e-20) M_solid <- 0
          if (M_GI    < 1e-20) M_GI    <- 0
          if (C_B     < 1e-20) C_B    <- 0
          
          if (
            M_solid < 1e-20 &&
            M_GI    < 1e-20 &&
            C_B     < 1e-22
          ) {
            
            return(list(c(
              0,
              0,
              0
            )))
          }
          
          C_GI <- max(M_GI, 0) / V
          
          driving_force <- max(Cs - C_GI, 0)
        
          
          if (M_solid <= 1e-12 | !is.finite(M_solid)) {
            dissolution_rate <- 0
          } else {
            dissolution_rate <-
              4 * pi * D / h *
              (3 / (4 * rho * pi))^(2/3) *
              M_solid^(2/3) *
              driving_force
          }
          
          absorption_rate <- k_gi * M_GI
          
          dM_solid <- -dissolution_rate
          
          dM_GI <- dissolution_rate - absorption_rate
          
          dC_B <- F * absorption_rate / Vd - ke * C_B
        
          
          list(
            c(
              dM_solid,
              dM_GI,
              dC_B
            )
          )
          

        })
      }
      
      parms <- c(
        Cs = Cs,
        V = V,
        k_gi = k_gi,
        F = F,
        Vd = Vd,
        ke = ke
      )
      
      # Adaptive integration
      out <- ode(
        y = state,
        times = t_after,
        func = model,
        parms = parms,
        method = "lsoda",
        rtol = 1e-4,
        atol = c(
          M_solid = 1e-8,
          M_GI = 1e-8,
          C_B = 1e-10
        )
      )
      
      out <- as.data.frame(out)
      
      
      # Full-length vectors
      M_solid <- numeric(length(t))
      M_GI <- numeric(length(t))
      C_B <- numeric(length(t))
      
      M_solid[start_idx:length(t)] <- out$M_solid
      M_GI[start_idx:length(t)] <- out$M_GI
      C_B[start_idx:length(t)] <- out$C_B
      
      # Amount dissolved during each output interval
      r <- numeric(length(t))
      
      if (length(t) > 1) {
        r[(start_idx + 1):length(t)] <-
          pmax(
            M_solid[start_idx:(length(t) - 1)] -
              M_solid[(start_idx + 1):length(t)],
            0
          )
      }
      
      release_imm_list[[j]] <- r
      
      GI_imm_list[[j]] <- data.frame(
        x = t,
        y = M_GI / V * 1000000,
        group = paste0("Immediate dose ", j)
      )
      
      B_imm_list[[j]] <- data.frame(
        x = t,
        y = C_B * 1000000,
        group = paste0("Immediate dose ", j)
      )
    }
  }
  
  immResults <- list(
    GI_imm_list = GI_imm_list,
    B_imm_list = B_imm_list,
    release_imm_list = release_imm_list,
    t = t
  )
  

  
  return(immResults)
}
