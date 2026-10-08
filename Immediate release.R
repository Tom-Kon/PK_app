source("Helper functions.R")

ImmFunction <- function(input) {
  generalList <- generalParams(input)
  list2env(generalList, envir = environment())
  
  release_imm_list <- list()
  GI_imm_list <- list()
  B_imm_list <- list()
  
  n_doses <- length(starts_imm)
  
  if (n_doses > 0) {
    
    # ============================================================
    # LOADING DOSE
    # ============================================================
    
    if (isTRUE(input$use_loading_dose)) {
      loading_dose <- imm_dose * input$loading_factor
    } else {
      loading_dose <- imm_dose
    }
    
    
    # ============================================================
    # INITIAL STATE
    #
    # Separate solid/GI/blood compartment for every dose
    # ============================================================
    
    state <- c(
      rep(0, n_doses),  # M_solid
      rep(0, n_doses),  # M_GI
      rep(0, n_doses)   # C_B
    )
    
    names(state) <- c(
      paste0("M_solid_", seq_len(n_doses)),
      paste0("M_GI_", seq_len(n_doses)),
      paste0("C_B_", seq_len(n_doses))
    )
    
    
    # ============================================================
    # PARAMETERS
    # ============================================================
    
    parms <- c(
      Cs = Cs,
      V = V,
      k_gi = k_gi,
      F = F,
      Vd = Vd,
      ke = ke
    )
    
    
    # ============================================================
    # COUPLED DISSOLUTION / GI / BLOOD MODEL
    # ============================================================
    
    model <- function(time, state, parms) {
      
      with(as.list(c(state, parms)), {
        
        # --------------------------------------------------------
        # Extract the individual dose compartments
        # --------------------------------------------------------
        
        M_solid <- state[paste0(
          "M_solid_", seq_len(n_doses)
        )]
        
        M_GI <- state[paste0(
          "M_GI_", seq_len(n_doses)
        )]
        
        C_B <- state[paste0(
          "C_B_", seq_len(n_doses)
        )]
        
        
        # --------------------------------------------------------
        # Numerical protection against negative values
        # --------------------------------------------------------
        
        M_solid <- pmax(M_solid, 0)
        M_GI    <- pmax(M_GI, 0)
        C_B     <- pmax(C_B, 0)
        
        
        # --------------------------------------------------------
        # Numerical protection against tiny residual values
        # --------------------------------------------------------
        
        if (any(M_solid < 1e-20)) {
          M_solid[M_solid < 1e-20] <- 0
        }
        
        if (any(M_GI < 1e-20)) {
          M_GI[M_GI < 1e-20] <- 0
        }
        
        if (any(C_B < 1e-20)) {
          C_B[C_B < 1e-20] <- 0
        }
        
        
        # --------------------------------------------------------
        # If everything is effectively zero, stop the dynamics
        # --------------------------------------------------------
        
        if (
          all(M_solid < 1e-20) &&
          all(M_GI < 1e-20) &&
          all(C_B < 1e-22)
        ) {
          
          return(list(c(
            rep(0, n_doses),
            rep(0, n_doses),
            rep(0, n_doses)
          )))
        }
        
        
        # --------------------------------------------------------
        # TOTAL GI concentration
        #
        # All doses share the same GI environment.
        # Therefore dissolution of every dose depends on the
        # total amount currently dissolved in the GI tract.
        # --------------------------------------------------------
        
        M_GI_total <- sum(M_GI)
        
        C_GI <- max(M_GI_total, 0) / V
        
        driving_force <- max(Cs - C_GI, 0)
        
        
        # --------------------------------------------------------
        # Dissolution rate for EACH dose
        # --------------------------------------------------------
        
        dissolution_rate <- numeric(n_doses)
        
        for (j in seq_len(n_doses)) {
          
          if (
            time >= starts_imm[j] + t_transit
          ) {
            
            dissolution_rate[j] <- 0
            
          } else if (
            M_solid[j] <= 1e-12 |
            !is.finite(M_solid[j])
          ) {
            
            dissolution_rate[j] <- 0
            
          } else {
            
            dissolution_rate[j] <-
              4 * pi * D / h *
              (3 / (4 * rho * pi))^(2/3) *
              M_solid[j]^(2/3) *
              driving_force
          }
        }
        
        
        # --------------------------------------------------------
        # Absorption rate for EACH dose
        # --------------------------------------------------------
        
        absorption_rate <- k_gi * M_GI
        
        
        # --------------------------------------------------------
        # Derivatives
        # --------------------------------------------------------
        
        dM_solid <- -dissolution_rate
        
        dM_GI <- dissolution_rate - absorption_rate
        
        dC_B <- F * absorption_rate / Vd - ke * C_B
        
        
        # --------------------------------------------------------
        # Return derivatives
        # --------------------------------------------------------
        
        list(
          c(
            dM_solid,
            dM_GI,
            dC_B
          )
        )
      })
    }
    
    
    # ============================================================
    # OUTPUT STORAGE
    # ============================================================
    
    state_output <- matrix(
      0,
      nrow = length(t),
      ncol = length(state)
    )
    
    colnames(state_output) <- names(state)
    
    
    # Current state carried from one dosing interval to the next
    current_state <- state
    
    
    # ============================================================
    # FIND DOSE INDICES
    # ============================================================
    
    dose_indices <- sapply(
      starts_imm,
      function(x) which(t >= x)[1]
    )
    
    
    # ============================================================
    # INTEGRATE THROUGH ALL DOSES
    # ============================================================
    
    for (j in seq_along(dose_indices)) {
      
      start_idx <- dose_indices[j]
      
      if (is.na(start_idx)) next
      
      
      # ----------------------------------------------------------
      # Determine end of this integration interval
      # ----------------------------------------------------------
      
      if (j < length(dose_indices)) {
        end_idx <- dose_indices[j + 1]
      } else {
        end_idx <- length(t)
      }
      
      
      # ----------------------------------------------------------
      # Add dose to its own solid compartment
      #
      # First dose can be a loading dose.
      # Subsequent doses are maintenance doses.
      # ----------------------------------------------------------
      
      if (j == 1 && isTRUE(input$use_loading_dose)) {
        dose <- loading_dose
      } else {
        dose <- imm_dose
      }
      
      current_state[paste0("M_solid_", j)] <-
        current_state[paste0("M_solid_", j)] + dose
      
      
      # ----------------------------------------------------------
      # Time points for this interval
      # ----------------------------------------------------------
      
      t_after <- t[start_idx:end_idx]
      
      
      # ----------------------------------------------------------
      # Integrate if there are enough time points
      # ----------------------------------------------------------
      
      if (length(t_after) >= 2) {
        
        out <- ode(
          y = current_state,
          times = t_after,
          func = model,
          parms = parms,
          method = "lsoda",
          rtol = 1e-4,
          atol = c(
            rep(1e-8, n_doses),   # M_solid
            rep(1e-8, n_doses),   # M_GI
            rep(1e-10, n_doses)   # C_B
          )
        )
        
        out <- as.data.frame(out)
        
        
        # --------------------------------------------------------
        # Store this interval
        # --------------------------------------------------------
        
        state_output[start_idx:end_idx, ] <- as.matrix(
          out[, -1, drop = FALSE]
        )
        
        
        # --------------------------------------------------------
        # Final state becomes initial state for next dose
        # --------------------------------------------------------
        
        current_state <- as.numeric(
          out[nrow(out), -1]
        )
        
        names(current_state) <- names(state)
        
      } else {
        
        state_output[start_idx, ] <- current_state
      }
    }
    
    
    # ============================================================
    # CREATE INDIVIDUAL DOSE OUTPUTS
    # ============================================================
    
    for (j in seq_len(n_doses)) {
      
      M_solid <- state_output[
        ,
        paste0("M_solid_", j)
      ]
      
      M_GI <- state_output[
        ,
        paste0("M_GI_", j)
      ]
      
      C_B <- state_output[
        ,
        paste0("C_B_", j)
      ]
      
      
      # ----------------------------------------------------------
      # Amount dissolved during each output interval
      # ----------------------------------------------------------
      
      r <- numeric(length(t))
      
      if (length(t) > 1) {
        
        r[2:length(t)] <-
          pmax(
            M_solid[1:(length(t) - 1)] -
              M_solid[2:length(t)],
            0
          )
      }
      
      
      release_imm_list[[j]] <- r
      
      
      # ----------------------------------------------------------
      # GI concentration
      # ----------------------------------------------------------
      
      GI_imm_list[[j]] <- data.frame(
        x = t,
        y = M_GI / V * 1000000,
        group = paste0("Immediate dose ", j)
      )
      
      
      # ----------------------------------------------------------
      # Blood concentration
      # ----------------------------------------------------------
      
      B_imm_list[[j]] <- data.frame(
        x = t,
        y = C_B * 1000000,
        group = paste0("Immediate dose ", j)
      )
    }
  }
  
  
  # ============================================================
  # RETURN RESULTS
  # ============================================================
  
  immResults <- list(
    GI_imm_list = GI_imm_list,
    B_imm_list = B_imm_list,
    release_imm_list = release_imm_list,
    t = t
  )
  
  
  return(immResults)
}