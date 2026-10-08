# small helper for distinct colors
palette_hcl <- function(n, h = c(15, 375), c = 100, l = 60) {
  if (n <= 0) return(character(0))
  grDevices::hcl(h = seq(h[1], h[2], length.out = n + 1)[1:n], c = c, l = l)
}

# IMPORTANT NOTE: EVERYTHING IS CONVERTED TO SECONDS, LITER, KILOGRAM, AND M.  

generalParams <- function(input){
  fixed <- input$ClVdt0.5Fix
  
  # Read parameters
  P_eff <- input$P_eff 
  A_GI <- input$A_GI/10000
  weight <- input$weight
  Cs  <- input$Cs/1000
  
  if (fixed == "t0.5Fix") {
    
    Vd <- input$Vd * input$weight
    Cl <- input$Cl / 60 * input$weight / 1000
    t0.5 <- log(2) * Vd / Cl
    
    Vd_UI <- Vd / input$weight
    Cl_UI <- Cl * 1000 * 60 / input$weight
    t0.5_UI <- t0.5 / 3600
    
  } else if (fixed == "VdFix") {
    
    Cl <- input$Cl / 60 * input$weight / 1000
    t0.5 <- input$t0.5 * 3600
    Vd <- t0.5 * Cl / log(2)
    
    Vd_UI <- Vd / input$weight
    Cl_UI <- Cl * 1000 * 60 / input$weight
    t0.5_UI <- t0.5 / 3600
    
  } else {
    
    t0.5 <- input$t0.5 * 3600
    Vd <- input$Vd * input$weight
    Cl <- log(2) * Vd / t0.5
    
    Vd_UI <- Vd / input$weight
    Cl_UI <- Cl * 1000 * 60 / input$weight
    t0.5_UI <- t0.5 / 3600
  }
  
  ke   <- Cl/Vd
  F    <- input$F
  V   <- input$V_GI/1000
  t_transit <- input$t_transit*3600
  
  k_gi <- P_eff*A_GI/V

  sim_sus <- isTRUE(input$simulateSustained)
  sus_delay <- input$sus_delay*3600
  # sus_num   <- input$sus_num
  sus_num <- 1
  sus_interval <- 1
  partK <- input$partK
  thickness <- input$thickness/1000000
  area <- input$area/10000
  DS_input <- input$sust_dose/1000000
  DiffSust <- input$D_Sus/10000
  

  sim_imm <- isTRUE(input$simulateImmediate)
  imm_delay <- input$imm_delay*3600
  imm_dose  <- input$imm_dose/1000000
  imm_num   <- input$imm_num
  imm_interval <- input$imm_interval*3600
  
  # Immediate formulation params (z-factor)
  D   <- input$D_Imm
  h   <- input$h/1000000
  rho <- input$rho #No need for conversion because g/mL = kg/L
  r0  <- input$r0/100
  
  #k for sustained release calculation
  kS_input <- DiffSust*partK*area/thickness/V
    
  
  starts_sus <- if (sim_sus && sus_num > 0) sus_delay + (0:(sus_num - 1)) * sus_interval + 1e-6 else numeric(0)
  starts_imm <- if (sim_imm && imm_num > 0) imm_delay + (0:(imm_num - 1)) * imm_interval + 1e-6 else numeric(0)
  
  
  # Build time grid and include exact dose times
  last_start <- if (length(c(starts_imm, starts_sus)) > 0) max(c(starts_imm, starts_sus)) else 0
  tail_guess <- max(5 / max(ke, 1e-6), 5 / max(k_gi, 1e-6), 5 / max(kS_input, 1e-6))  # short tail (alter this to extend the view, if you use a number higher than 3 = longer view, lower than = 3 shorter view)
  t_end_guess <- last_start + tail_guess
  N <- 200000 #(Higher N means more data points, smoother graph but more demanding of the computer)
  t <- seq(0, t_end_guess, length.out = N)
  t <- sort(unique(c(t, starts_imm, starts_sus)))
  dt <- c(diff(t)[1], diff(t))
  
  
  generalList <- list(Vd = Vd, Vd_UI = Vd_UI, Cl_UI = Cl_UI, t0.5_UI = t0.5_UI, k_gi = k_gi, ke = ke, F = F, t_transit = t_transit, sim_sus = sim_sus, sus_delay = sus_delay, sus_num = sus_num, 
                      sus_interval = sus_interval, kS_input = kS_input, DS_input = DS_input, sim_imm = sim_imm, imm_delay = imm_delay, 
                      imm_dose = imm_dose, imm_num = imm_num, imm_interval = imm_interval, D = D, h = h, rho = rho, r0 = r0, 
                      Cs = Cs, V = V, starts_sus = starts_sus, starts_imm = starts_imm, last_start = last_start, tail_guess = tail_guess, 
                      t_end_guess = t_end_guess, N = N, t = t, dt = dt)

  return(generalList)
  
}

outputGenerator <- function(input, finalList) {
  generalList <- generalParams(input)
  list2env(generalList, envir = environment())
  list2env(finalList, envir = environment())
  textResult <- list()

  if(input$simulateSustained) {

    if(any(plateau)) {
      textSus <- paste0(
        "The total AUC of the sustained release formulation was found to be ", signif(AUCTotalSus, 3), " µg*h/mL. A plateau was detected at  "
        , signif(cPlat, 3), " µg/mL, which was reached at ", signif(tPlat, 3), " h."
      )
    } else {
      textSus <- paste0(
        "The total AUC of the sustained release formulation was found to be ", signif(AUCTotalSus, 3), " µg*h/mL. The maximum concentration 
      for the sustained release formulation was ", signif(CmaxSus, 3), " µg/mL, which was reached at ", signif(tmaxSus, 3), " h."
      )
    }
    
    textResult$textSus <- textSus
  }
  
  
  if(input$simulateImmediate) {
  
    if(input$imm_num == 1) {
    textImm <- paste0(
      "The total AUC of the immediate release formulation was found to be ", signif(AUCtotalImm, 3), " µg*h/mL. A single dosage form was administered,
      resulting in a maximum concentration of ", signif(CmaxImm, 3), " µg/mL at ", signif(tmaxImm, 3), " h."
    )
      
    } else if (input$imm_num != 1 & input$imm_num*input$imm_interval > 6*input$t0.5) {
      textImm <- paste0(
        "The total AUC of the immediate release formulation was found to be ", signif(AUCtotalImm, 3), " µg*h/mL. Multiple dosage forms were administered,
      resulting in an equilibrium that was reached at ", signif(tReached, 3), " h. The maxima of this equilibrium lie at ", signif(CmaxImmEq, 3), " µg/mL, 
      while the the minima lie at ", signif(CminImmEq, 3), " µg/mL and the average lies at ", signif(CAvImmEq, 3), " µg/mL. 
      The AUC of each dosage form when equilibrium is reached is ", signif(AUCDoseImm, 3), " µg*h/mL."
      )
      
    } else {
      textImm <- paste0(
        "The total AUC of the immediate release formulation was found to be ", signif(AUCtotalImm, 3), " µg*h/mL. Multiple dosage forms were administered, but no equilibrium was reached. You can reach
        equilibrium by decreasing half life, by decreasing dosage interval, or by increasing the number of administered dosage forms. The result is a 
        maximum concentration of ", signif(CmaxImm, 3), " µg/mL at ", signif(tmaxImm, 3), " h, but these parameters are rather meaningless due to multiple administrations without equilibrium."
      )
    }
    
    textResult$textImm <- textImm
  }
  
  return(textResult)

}















