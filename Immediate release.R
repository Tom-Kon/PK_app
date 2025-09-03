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
          dMdt <- - z * (M0)^(1/3) * M23 * (Cs - (M0 - M) / V)
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
    rel <- release_imm_list[[j]]
    GI_j <- numeric(length(t))
    B_j  <- numeric(length(t))
    for (i in 2:length(t)) {
      dti <- t[i] - t[i-1]
      GI_j[i] <- GI_j[i-1] + rel[i] - k_gi * GI_j[i-1] * dti
      blood_in <- F * k_gi * GI_j[i-1] * dti
      B_j[i] <- B_j[i-1] + blood_in - ke * B_j[i-1] * dti
    }
    GI_imm_list[[j]] <- data.frame(x = t, y = GI_j, group = paste0("Immediate dose ", j))
    B_imm_list[[j]]  <- data.frame(x = t, y = B_j,  group = paste0("Immediate dose ", j))
  }
  
 immResults <- list(GI_imm_list = GI_imm_list, B_imm_list = B_imm_list, t = t, z=z)
 

 return(immResults)
 
}