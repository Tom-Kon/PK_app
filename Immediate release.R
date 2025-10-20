source("Helper functions.R")

ImmFunction <- function(input) {
  generalList <- generalParams(input)
  list2env(generalList, envir = environment())
  
  
  # Immediate = z-factor dissolution
  release_imm_list <- list()
  if (length(starts_imm) > 0) {
    for (j in seq_along(starts_imm)) {
      r <- numeric(length(t))
      M0 <- imm_dose
      M  <- M0
      start_idx <- which(t >= starts_imm[j])[1]
      if (!is.na(start_idx)) {
        for (i in seq(start_idx + 1, length(t))) {
          dti <- t[i] - t[i-1]
          M23 <- if (M > 0) M^(2/3) else 0
          dMdt <- - z*M23*(Cs-M/V)
          dM <- dMdt * dti
          newM <- M + dM
          if (newM < 0) newM <- 0
          released <- max(M - newM, 0)
          r[i] <- released
          M <- newM
          if (M <= 0) break
        }
      }
      release_imm_list[[j]] <- r
    }
  }
  
  # ---- Calculate per-dose GI and Blood contributions for immediate ----
  GI_imm_list <- list(); B_imm_list <- list()
  for (j in seq_along(release_imm_list)) {
    # r is the mass released (dM_diss) during each time interval dti
    dM_diss <- release_imm_list[[j]] 
    
    M_GI <- numeric(length(t)) # M_GI is the MASS (mg) in the GI fluid
    C_B <- numeric(length(t))  # C_B is the CONCENTRATION (mg/L) in the Blood
    
    for (i in 2:length(t)) {
      dti <- t[i] - t[i-1]
      
      # 1. GI Compartment (Mass Balance)
      # Rate In (from dissolution): dM_diss[i] is the mass change over dti
      R_diss_rate <- dM_diss[i] / dti 
      
      # Rate Out (Absorption to Blood): R_abs = k_gi * M_GI
      R_abs_rate <- k_gi * M_GI[i-1] 
      
      # dM_GI/dt = R_diss_rate - R_abs_rate
      M_GI[i] <- M_GI[i-1] + (R_diss_rate - R_abs_rate) * dti
      M_GI[i] <- max(M_GI[i], 0) 
      
      # 2. Blood Compartment (Concentration Balance)
      # Rate In (Absorption): R_abs_C = F * R_abs_rate / Vd
      # R_abs_C = F * (k_gi * M_GI) / Vd
      R_abs_C <- F * k_gi * M_GI[i-1] / Vd
      
      # Rate Out (Elimination): R_elim_C = ke * C_B
      R_elim_C <- ke * C_B[i-1]
      
      # dC_B/dt = R_abs_C - R_elim_C
      C_B[i] <- C_B[i-1] + (R_abs_C - R_elim_C) * dti
      C_B[i] <- max(C_B[i], 0)
    }
    
    # Store results
    GI_imm_list[[j]] <- data.frame(x = t, y = M_GI / V*1000000, group = paste0("Immediate dose ", j)) # Convert M_GI back to C_GI for plotting and from kg/L to g/mL
    B_imm_list[[j]]  <- data.frame(x = t, y = C_B*1000000, group = paste0("Immediate dose ", j)) # from kg/L to g/mL
  }
  
 immResults <- list(GI_imm_list = GI_imm_list, B_imm_list = B_imm_list, t = t, z=z)
 

 return(immResults)
 
}