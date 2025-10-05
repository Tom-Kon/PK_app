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
  
  thrGI <- 0.01*max(max(GI_total_vec), max(Blood_total_vec))
  act_idxGI <- which(GI_total_vec > thrGI)
  if (length(act_idxGI) == 0) {
    keep_idxGI <- seq_len(min(50, length(t)))
  } else {
    last_idxGI <- max(act_idxGI)
    last_timeGI <- t[last_idxGI]
    bufferGI <- max(0.15, 0.02 * last_timeGI)
    keep_idxGI <- which(t <= (last_timeGI + bufferGI))
    if (length(keep_idxGI) == 0) keep_idxGI <- seq_len(min(50, length(t)))
  }
  
  
  thrBlood <- 0.01*max(Blood_total_vec)
  act_idxBlood <- which(Blood_total_vec > thrBlood)
  if (length(act_idxBlood) == 0) {
    keep_idxBlood <- seq_len(min(50, length(t)))
  } else {
    last_idxBlood <- max(act_idxBlood)
    last_timeBlood <- t[last_idxBlood]
    bufferBlood <- max(0.15, 0.02 * last_timeBlood)
    keep_idxBlood <- which(t <= (last_timeBlood + bufferBlood))
    if (length(keep_idxBlood) == 0) keep_idxBlood <- seq_len(min(50, length(t)))
  }
  
  t_trimGI <- t[keep_idxGI]
  t_trimBlood <- t[keep_idxBlood]
  
  GI_total_df    <- data.frame(x = t_trimGI, y = GI_total_vec[keep_idxGI], group = "GI total")
  Blood_total_df <- data.frame(x = t_trimBlood, y = Blood_total_vec[keep_idxBlood], group = "Blood total")
  
  GI_imm_list_trim <- lapply(GI_imm_list, function(df) df[df$x <= max(t_trimGI), , drop = FALSE])
  GI_sus_list_trim <- lapply(GI_sus_list, function(df) df[df$x <= max(t_trimGI), , drop = FALSE])
  B_imm_list_trim  <- lapply(B_imm_list,  function(df) df[df$x <= max(t_trimBlood), , drop = FALSE])
  B_sus_list_trim  <- lapply(B_sus_list,  function(df) df[df$x <= max(t_trimBlood), , drop = FALSE])
  
  GI_imm_total_df <- data.frame(x = t_trimGI, y = GI_imm_total[keep_idxGI], group = "Immediate total")
  GI_sus_total_df <- data.frame(x = t_trimGI, y = GI_sus_total[keep_idxGI], group = "Sustained total")
  B_imm_total_df  <- data.frame(x = t_trimBlood, y = B_imm_total[keep_idxBlood], group = "Immediate total")
  B_sus_total_df  <- data.frame(x = t_trimBlood, y = B_sus_total[keep_idxBlood], group = "Sustained total")
  
  
  finalList <- list(
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
    last_timeGI = max(t_trimGI),
    last_timeBlood = max(t_trimBlood)
  )
  
  return(finalList)
}