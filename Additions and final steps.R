finalSteps <- function(B_imm_list, GI_imm_list, B_sus_list, GI_sus_list, t, z) {
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
  
  
  finalList <- list(
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
  
  return(finalList)
}