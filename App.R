if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  tryCatch({
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }, error = function(e) {
    setwd(normalizePath("."))
  })
} else {
  setwd(normalizePath("."))
}
source("Libraries and notes.R")
source("Helper functions.R")
source("UI.R")
source("General parameters.R")
source("Immediate release.R")
source("Sustained release.R")

ui <- fluidPage(
  UIFunc()
)

server <- function(input, output, session) {
  
  simulate_model <- reactive({
    # Read parameters
    k_gi <- req(input$k_gi)        # used as 'ka' in closed-form sustained formula
    ke   <- req(input$ke)
    F    <- req(input$F)
    c_par <- if (!is.null(input$c)) input$c else 2   # 'c' used by sustained closed-form
    
    sim_imm <- isTRUE(input$simulateImmediate)
    imm_delay <- input$imm_delay
    imm_dose  <- input$imm_dose      # M0 for IMMEDIATE
    imm_num   <- input$imm_num
    imm_interval <- input$imm_interval
    
    sim_sus <- isTRUE(input$simulateSustained)
    sus_delay <- input$sus_delay
    sus_num   <- input$sus_num
    sus_interval <- input$sus_interval
    kS_input <- if (!is.null(input$kSustained)) input$kSustained else 2
    DS_input <- if (!is.null(input$DSustained)) input$DSustained else 3
    
    # Immediate formulation params (z-factor)
    D   <- input$D
    h   <- input$h
    rho <- input$rho
    r0  <- input$r0
    Cs  <- input$Cs
    V   <- input$V
    
    # z-factor calculation
    z <- 3 * D / (h * rho * r0)
    
    # Dose start times (+ 1e-6 added otherwise first dose wouldn't show)
    starts_imm <- if (sim_imm && imm_num > 0) imm_delay + (0:(imm_num - 1)) * imm_interval + 1e-6 else numeric(0)
    starts_sus <- if (sim_sus && sus_num > 0) sus_delay + (0:(sus_num - 1)) * sus_interval + 1e-6 else numeric(0)
    
    # Build time grid and include exact dose times
    last_start <- if (length(c(starts_imm, starts_sus)) > 0) max(c(starts_imm, starts_sus)) else 0
    tail_guess <- max(3 / max(ke, 1e-6), 3 / max(k_gi, 1e-6))  # short tail (alter this to extend the view, if you use a number higher than 3 = longer view, lower than = 3 shorter view)
    t_end_guess <- last_start + tail_guess
    N <- 8000 #(Higher N means more data points, smoother graph but more demanding of the computer)
    t <- seq(0, t_end_guess, length.out = N)
    t <- sort(unique(c(t, starts_imm, starts_sus)))
    dt <- c(diff(t)[1], diff(t))
    
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
    
    # Sustained
    GI_sus_list <- list()
    B_sus_list <- list()
    if (length(starts_sus) > 0) {
      ka <- k_gi
      kS <- kS_input
      DS <- DS_input
      
      fS <- function(x) {
        ka * x * (kS + ka) - kS * exp(-x * (kS + ka)) -
          ((DS * (kS + ka)^2) - (c_par * kS^2)) / (c_par * kS)
      }
      t1S <- tryCatch(uniroot(fS, interval = c(1e-9, 1e6))$root,
                      error = function(e) NA)
      if (is.na(t1S) || !is.finite(t1S)) t1S <- 0.1
      
      # calculate bS and ZS
      ZS <- (kS * c_par - kS*c_par*exp(-(kS+ka)*t1S))/ (kS + ka)
      
      # piecewise GI per-dose function (returns GI concentration contribution for a single dose)
      GI_sustained_single <- function(tau) {
        y <- numeric(length(tau))
        idx1 <- tau >= 0 & tau <= t1S
        if (any(idx1)) {
          y[idx1] <- (kS * c_par - kS*c_par*exp(-(kS+ka)*tau[idx1]))/ (kS + ka)
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
    
    # totals (sum of all per-dose contributions)
    GI_total_vec <- numeric(length(t))
    # combine lists safely: elements of lists are data.frames with $y
    for (lst in c(GI_imm_list, GI_sus_list)) {
      if (length(lst) > 0) {
        # lst is a data.frame element here; sum its y
        GI_total_vec <- GI_total_vec + lst$y
      }
    }
    Blood_total_vec <- numeric(length(t))
    for (lst in c(B_imm_list, B_sus_list)) {
      if (length(lst) > 0) {
        Blood_total_vec <- Blood_total_vec + lst$y
      }
    }
    
    # per-type totals
    GI_imm_total <- Reduce(`+`, lapply(GI_imm_list, `[[`, "y"), init = numeric(length(t)))
    GI_sus_total <- Reduce(`+`, lapply(GI_sus_list, `[[`, "y"), init = numeric(length(t)))
    B_imm_total  <- Reduce(`+`, lapply(B_imm_list,  `[[`, "y"), init = numeric(length(t)))
    B_sus_total  <- Reduce(`+`, lapply(B_sus_list,  `[[`, "y"), init = numeric(length(t)))
    
    # dynamic trimming
    thr <- 1e-3
    act_idx <- which((GI_total_vec > thr) | (Blood_total_vec > thr))
    if (length(act_idx) == 0) {
      keep_idx <- seq_len(min(50, length(t)))
    } else {
      last_idx <- max(act_idx)
      last_time <- t[last_idx]
      buffer <- max(0.15, 0.02 * last_time)
      keep_idx <- which(t <= (last_time + buffer))
      if (length(keep_idx) == 0) keep_idx <- seq_len(min(50, length(t)))
    }
    
    t_trim <- t[keep_idx]
    GI_total_df    <- data.frame(x = t_trim, y = GI_total_vec[keep_idx], group = "GI total")
    Blood_total_df <- data.frame(x = t_trim, y = Blood_total_vec[keep_idx], group = "Blood total")
    
    GI_imm_list_trim <- lapply(GI_imm_list, function(df) df[df$x <= max(t_trim), , drop = FALSE])
    GI_sus_list_trim <- lapply(GI_sus_list, function(df) df[df$x <= max(t_trim), , drop = FALSE])
    B_imm_list_trim  <- lapply(B_imm_list,  function(df) df[df$x <= max(t_trim), , drop = FALSE])
    B_sus_list_trim  <- lapply(B_sus_list,  function(df) df[df$x <= max(t_trim), , drop = FALSE])
    
    GI_imm_total_df <- data.frame(x = t_trim, y = GI_imm_total[keep_idx], group = "Immediate total")
    GI_sus_total_df <- data.frame(x = t_trim, y = GI_sus_total[keep_idx], group = "Sustained total")
    B_imm_total_df  <- data.frame(x = t_trim, y = B_imm_total[keep_idx], group = "Immediate total")
    B_sus_total_df  <- data.frame(x = t_trim, y = B_sus_total[keep_idx], group = "Sustained total")
    
    list(
      t = t_trim,
      GI_total = GI_total_df,
      Blood_total = Blood_total_df,
      GI_imm_list = GI_imm_list_trim,
      GI_sus_list = GI_sus_list_trim,
      B_imm_list = B_imm_list_trim,
      B_sus_list = B_sus_list_trim,
      GI_imm_total = GI_imm_total_df,
      GI_sus_total = GI_sus_total_df,
      B_imm_total = B_imm_total_df,
      B_sus_total = B_sus_total_df,
      z = z,
      last_time = max(t_trim)
    )
  })
  
  # --- GI plot ---
  output$plotGI <- renderPlotly({
    req(input$showGI)
    sim <- simulate_model()
    separate <- isTRUE(input$separateColors)
    show_imm <- input$GIview %in% c("immediate", "both")
    show_sus <- input$GIview %in% c("sustained", "both")
    
    p <- plot_ly()
    # color palettes
    nI <- length(sim$GI_imm_list); colsI <- if (nI>0) palette_hcl(nI, h = c(200,500)) else character(0)
    nS <- length(sim$GI_sus_list); colsS <- if (nS>0) palette_hcl(nS, h = c(0,140)) else character(0)
    
    if (separate) {
      if (show_sus && nS>0) {
        for (j in seq_len(nS)) {
          df <- sim$GI_sus_list[[j]]
          p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines",
                         name = paste0("Sustained ", j), legendgroup = "Sustained",
                         line = list(color = colsS[j]), fill = "none")
        }
      }
      if (show_imm && nI>0) {
        for (j in seq_len(nI)) {
          df <- sim$GI_imm_list[[j]]
          p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines",
                         name = paste0("Immediate ", j), legendgroup = "Immediate",
                         line = list(color = colsI[j]), fill = "none")
        }
      }
    } else {
      #color graph immediate
      if (input$GIview == "immediate") {
        if (nI > 0) {
          p <- add_trace(p, x = sim$GI_imm_total$x, y = sim$GI_imm_total$y, type = "scatter", mode = "lines",
                         name = "Immediate (total)", fill = "none", line = list(color = "darkorange"))
        } else {
          p <- add_trace(p, x = sim$GI_total$x, y = sim$GI_total$y, type = "scatter", mode = "lines",
                         name = "GI (total)", fill = "none")
        }
        #color graph sustained
      } else if (input$GIview == "sustained") {
        if (nS > 0) {
          p <- add_trace(p, x = sim$GI_sus_total$x, y = sim$GI_sus_total$y, type = "scatter", mode = "lines",
                         name = "Sustained (total)", fill = "none", line = list(color = "steelblue"))
        } else {
          p <- add_trace(p, x = sim$GI_total$x, y = sim$GI_total$y, type = "scatter", mode = "lines",
                         name = "GI (total)", fill = "none")
        }
        # both
      } else {
        p <- add_trace(p, x = sim$GI_sus_total$x, y = sim$GI_sus_total$y, type = "scatter", mode = "lines",
                       name = "Sustained (total)", fill = "none", line = list(color = "steelblue"))
        p <- add_trace(p, x = sim$GI_imm_total$x, y = sim$GI_imm_total$y, type = "scatter", mode = "lines",
                       name = "Immediate (total)", fill = "none", line = list(color = "darkorange"))
      }
    }
    
    x_rng <- c(0, sim$last_time)
    p %>% layout(title = "GI Concentration",
                 xaxis = list(title = "Time (h)", range = x_rng),
                 yaxis = list(title = "Conc. (GI)"))
  })
  
  # --- Blood plot ---
  output$plotBlood <- renderPlotly({
    req(input$showBlood)
    sim <- simulate_model()
    separate <- isTRUE(input$separateColors)
    
    p <- plot_ly()
    nI <- length(sim$B_imm_list); colsI <- if (nI>0) palette_hcl(nI, h = c(200,500)) else character(0)
    nS <- length(sim$B_sus_list); colsS <- if (nS>0) palette_hcl(nS, h = c(0,140)) else character(0)
    
    if (separate) {
      if (length(sim$B_sus_list) > 0) {
        for (j in seq_along(sim$B_sus_list)) {
          df <- sim$B_sus_list[[j]]
          p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines",
                         name = paste0("Sustained ", j, " (blood)"),
                         legendgroup = "Sustained", line = list(color = colsS[j]), fill = "none")
        }
      }
      if (length(sim$B_imm_list) > 0) {
        for (j in seq_along(sim$B_imm_list)) {
          df <- sim$B_imm_list[[j]]
          p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines",
                         name = paste0("Immediate ", j, " (blood)"),
                         legendgroup = "Immediate", line = list(color = colsI[j]), fill = "none")
        }
      }
    } else {
      # Color graph combined (blood)
      if (input$bloodMode == "combined") {
        p <- add_trace(p, x = sim$Blood_total$x, y = sim$Blood_total$y, type = "scatter", mode = "lines",
                       name = "Blood (combined)", fill = "none", line = list(color = "red"))
      } else {
        # Color graph seperate (blood)
        p <- add_trace(p, x = sim$B_sus_total$x, y = sim$B_sus_total$y, type = "scatter", mode = "lines",
                       name = "Blood (sustained)", legendgroup = "Sustained", fill = "none", line = list(color = "steelblue"))
        p <- add_trace(p, x = sim$B_imm_total$x, y = sim$B_imm_total$y, type = "scatter", mode = "lines",
                       name = "Blood (immediate)", legendgroup = "Immediate", fill = "none", line = list(color = "darkorange"))
      }
    }
    
    x_rng <- c(0, sim$last_time)
    p %>% layout(title = "Blood Concentration",
                 xaxis = list(title = "Time (h)", range = x_rng),
                 yaxis = list(title = "Conc. (Blood)"))
  })
}

shinyApp(ui = ui, server = server)
