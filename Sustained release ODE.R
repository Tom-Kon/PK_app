library(deSolve)
source("Helper functions.R")

SustFunctionODE <- function(input) {
  generalList <- generalParams(input)
  list2env(generalList, envir = environment())
  
  # Parameters
  params <- c(
    kS_input = kS_input,   # release rate
    k_gi = k_gi,   # absorption rate
    ke   = ke,   # elimination rate
    Cs = Cs,  # formulation concentration
    V = V,      # GI volume
    Vd  = Vd,      # plasma volume
    DS_input = DS_input         # threshold cumulative input
  )
  
  # ODE system with cumulative input tracker
  pk_model_cumulative <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      # state variables
      A_GI   <- state["A_GI"]
      A_plasma <- state["A_plasma"]
      cum_input <- state["cum_input"]  # cumulative drug released into GI
      
      # concentrations
      C_GI <- A_GI / V
      C_plasma <- A_plasma / Vd
      
      # release term (only active if cum_input < D)
      release <- if (cum_input < DS_input) kS_input * (Cs - C_GI) else 0
      
      # ODEs
      dA_GI <- release - k_gi * (C_GI - C_plasma)
      dA_plasma <- F*k_gi * (C_GI - C_plasma) - ke * C_plasma * Vd
      dcum_input <- if (cum_input < DS_input) release else 0
      
      list(c(dA_GI, dA_plasma, dcum_input))
    })
  }
  
  # Initial states
  state <- c(A_GI = 0, A_plasma = 0, cum_input = 0)
  
  # Time sequence
  times <- t
  
  # Solve
  out <- ode(y = state, times = times, func = pk_model_cumulative, parms = params)
  out_df <- as.data.frame(out)
  
  # Compute concentrations
  out_df$C_GI <- cbind(x = t, y = out_df$A_GI / params["V"], group = paste0("Sustained dose ", 1))
  out_df$C_plasma <- cbind(x = t, y = out_df$A_plasma / params["Vd"], group = paste0("Sustained dose ", 1))
  
  susResultsODE <- list(GI_sus_list = out_df$C_GI, B_sus_list = out_df$C_plasma, t = t)
  return(susResultsODE)

}
