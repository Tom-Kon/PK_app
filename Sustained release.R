source("Helper functions.R")

SustFunction <- function(input) {
  generalList <- generalParams(input)
  list2env(generalList, envir = environment())
  
  
  # Sustained
  GI_sus_list <- list()
  B_sus_list <- list()
  if (length(starts_sus) > 0) {
    ka <- k_gi
    kS <- kS_input
    DS <- DS_input
    
    fS <- function(x) {
      ka * x * (kS + ka) - kS * exp(-x * (kS + ka)) -
        ((DS * (kS + ka)^2) - (Cs * kS^2)) / (Cs * kS)
    }
    t1S <- tryCatch(uniroot(fS, interval = c(1e-9, 1e6))$root,
                    error = function(e) NA)
    if (is.na(t1S) || !is.finite(t1S)) t1S <- 0.1
    
    # calculate bS and ZS
    ZS <- (kS * Cs - kS*Cs*exp(-(kS+ka)*t1S))/ (kS + ka)
    
    # piecewise GI per-dose function (returns GI concentration contribution for a single dose)
    GI_sustained_single <- function(tau) {
      y <- numeric(length(tau))
      idx1 <- tau >= 0 & tau <= t1S
      if (any(idx1)) {
        y[idx1] <- (kS * Cs - kS*Cs*exp(-(kS+ka)*tau[idx1]))/ (kS + ka)
      }
      idx2 <- tau > t1S
      if (any(idx2)) {
        y[idx2] <- ZS * exp(-ka * (tau[idx2] - t1S))
      }
      y[tau < 0] <- 0
      y
    }
    
    # For each sustained dose, calculate GI contribution directly and then blood via integration
    for (j in seq_along(starts_sus)) {
      t0 <- starts_sus[j]
      GI_j_vec <- GI_sustained_single(t - t0)
      # blood for this dose via integration (ke and F)
      B_j_vec <- numeric(length(t))
      for (i in 2:length(t)) {
        dti <- t[i] - t[i-1]
        B_j_vec[i] <- B_j_vec[i-1] * exp(-ke * dti) + F * GI_j_vec[i-1] * dti
      }
      GI_sus_list[[j]] <- data.frame(x = t, y = GI_j_vec, group = paste0("Sustained dose ", j))
      B_sus_list[[j]]  <- data.frame(x = t, y = B_j_vec,  group = paste0("Sustained dose ", j))
    }
  }
  
 susResults <- list(GI_sus_list = GI_sus_list, B_sus_list = B_sus_list, t = t, z=z)
 return(susResults)

}
