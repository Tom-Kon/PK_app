# small helper for distinct colors
palette_hcl <- function(n, h = c(15, 375), c = 100, l = 60) {
  if (n <= 0) return(character(0))
  grDevices::hcl(h = seq(h[1], h[2], length.out = n + 1)[1:n], c = c, l = l)
}

generalParams <- function(input){
  # Read parameters
  P_eff <- input$P_eff 
  A_GI <- input$A_GI
  Vd   <- input$Vd*input$weight
  Cl   <- input$Cl
  weight   <- input$weight
  
  ke   <- Cl/(Vd*weight)
  F    <- input$F
  V   <- input$V_GI
  t_transit <- input$t_transit
  
  k_gi <- P_eff*A_GI/V

  sim_sus <- isTRUE(input$simulateSustained)
  sus_delay <- input$sus_delay
  # sus_num   <- input$sus_num
  sus_num <- 1
  sus_interval <- 1
  partK <- input$partK
  thickness <- input$thickness/10000
  area <- input$area
  DS_input <- input$sust_dose
  DiffSust <- input$D_Sus
  

  sim_imm <- isTRUE(input$simulateImmediate)
  imm_delay <- input$imm_delay
  imm_dose  <- input$imm_dose      
  imm_num   <- input$imm_num
  imm_interval <- input$imm_interval
  
  # Immediate formulation params (z-factor)
  D   <- input$D_Imm
  h   <- input$h
  rho <- input$rho
  r0  <- input$r0
  Cs  <- input$Cs
  
  # z-factor calculation
  z <- 4 * pi*D/h*(3/(4*rho*pi))^(2/3)
  
  #k for sustained release calculation
  kS_input <- DiffSust*partK*area/thickness/V
    
  
  starts_sus <- if (sim_sus && sus_num > 0) sus_delay + (0:(sus_num - 1)) * sus_interval + 1e-6 else numeric(0)
  starts_imm <- if (sim_imm && imm_num > 0) imm_delay + (0:(imm_num - 1)) * imm_interval + 1e-6 else numeric(0)
  
  
  # Build time grid and include exact dose times
  last_start <- if (length(c(starts_imm, starts_sus)) > 0) max(c(starts_imm, starts_sus)) else 0
  tail_guess <- max(5 / max(ke, 1e-6), 5 / max(k_gi, 1e-6), 5 / max(kS_input, 1e-6))  # short tail (alter this to extend the view, if you use a number higher than 3 = longer view, lower than = 3 shorter view)
  t_end_guess <- last_start + tail_guess
  N <- 100000 #(Higher N means more data points, smoother graph but more demanding of the computer)
  t <- seq(0, t_end_guess, length.out = N)
  t <- sort(unique(c(t, starts_imm, starts_sus)))
  dt <- c(diff(t)[1], diff(t))
  
  
  generalList <- list(Vd = Vd, k_gi = k_gi, ke = ke, F = F, t_transit = t_transit, sim_sus = sim_sus, sus_delay = sus_delay, sus_num = sus_num, 
                      sus_interval = sus_interval, kS_input = kS_input, DS_input = DS_input, sim_imm = sim_imm, imm_delay = imm_delay, 
                      imm_dose = imm_dose, imm_num = imm_num, imm_interval = imm_interval, D = D, h = h, rho = rho, r0 = r0, 
                      Cs = Cs, V = V, z = z, starts_sus = starts_sus, starts_imm = starts_imm, last_start = last_start, tail_guess = tail_guess, 
                      t_end_guess = t_end_guess, N = N, t = t, dt = dt)

  return(generalList)
  
}