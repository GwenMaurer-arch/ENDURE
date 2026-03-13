pkgs <- c(
  "readxl","dplyr","tidyr","stringr","ggplot2","ggridges",
  "car","rstatix","openxlsx","tibble","purrr"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

file_path <- "data pigs.xlsx"
out_xlsx  <- "isotope_statistics_results.xlsx"
plot_dir  <- "plots_isotopes"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------- Import (skip grouped header row) ----------------
raw <- read_excel(file_path, skip = 1)

dat <- raw %>%
  rename(
    id = 1,
    site = 2,
    century = 3,
    dN = 4,
    dC = 5,
    dS = 6,
    bN = 7,
    bC = 8
  ) %>%
  mutate(
    site = as.factor(site),
    century = str_replace_all(as.character(century), "\\s+", ""),
    century = str_remove(century, "th$"),
    century = suppressWarnings(as.integer(century)),
    across(c(dN, dC, dS, bN, bC), ~ suppressWarnings(as.numeric(.x)))
  )

iso_cols <- c("dN","dC","dS","bN","bC")

# ---------------- Long format ----------------
long <- dat %>%
  pivot_longer(all_of(iso_cols), names_to = "isotope", values_to = "value") %>%
  filter(!is.na(value))

# ---------------- Plot function ----------------
save_plots <- function(df_long, group_var, prefix) {
  for (iso in unique(df_long$isotope)) {
    x <- df_long %>% filter(isotope == iso, !is.na(.data[[group_var]]))
    if (nrow(x) == 0) next
    
    p_ridge <- ggplot(x, aes(x = value, y = .data[[group_var]], fill = .data[[group_var]])) +
      ggridges::geom_density_ridges(alpha = 0.6, show.legend = FALSE) +
      labs(title = paste0(prefix, " – ", iso, " (ridge)"), x = iso, y = group_var) +
      theme_classic()
    
    p_box <- ggplot(x, aes(x = .data[[group_var]], y = value)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.15, height = 0, alpha = 0.85) +
      labs(title = paste0(prefix, " – ", iso, " (box + points)"), x = group_var, y = iso) +
      theme_classic()
    
    p_qq <- ggplot(x, aes(sample = value)) +
      stat_qq() +
      stat_qq_line() +
      facet_wrap(vars(.data[[group_var]]), scales = "free") +
      labs(title = paste0(prefix, " – ", iso, " (Q–Q by ", group_var, ")")) +
      theme_classic()
    
    ggsave(file.path(plot_dir, paste0(prefix, "_", group_var, "_", iso, "_ridge.png")),
           p_ridge, width = 9, height = 5, dpi = 300)
    ggsave(file.path(plot_dir, paste0(prefix, "_", group_var, "_", iso, "_box.png")),
           p_box, width = 9, height = 5, dpi = 300)
    ggsave(file.path(plot_dir, paste0(prefix, "_", group_var, "_", iso, "_qq.png")),
           p_qq, width = 10, height = 6, dpi = 300)
  }
}

# ---------------- Diagnostics helpers ----------------
shapiro_by_group <- function(df_long, group_var) {
  df_long %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(isotope, .data[[group_var]]) %>%
    summarise(
      n = n(),
      W = if (n() >= 3 && n() <= 5000) unname(shapiro.test(value)$statistic) else NA_real_,
      p = if (n() >= 3 && n() <= 5000) shapiro.test(value)$p.value else NA_real_,
      .groups = "drop"
    ) %>%
    rename(group = all_of(group_var)) %>%
    mutate(group = as.character(group))
}

levene_by_group <- function(df_long, group_var) {
  df_long %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(isotope) %>%
    group_modify(~{
      x <- .x %>% mutate(grp = as.factor(.data[[group_var]]))
      if (length(unique(x$grp)) < 2) {
        return(tibble(statistic = NA_real_, p = NA_real_))
      }
      lv <- tryCatch(
        car::leveneTest(value ~ grp, data = x, center = median),
        error = function(e) NULL
      )
      if (is.null(lv)) {
        tibble(statistic = NA_real_, p = NA_real_)
      } else {
        tibble(
          statistic = unname(lv[1, "F value"]),
          p = unname(lv[1, "Pr(>F)"])
        )
      }
    }) %>%
    ungroup()
}

# ---------------- Adaptive test function ----------------
run_adaptive_test <- function(df, group_var, alpha = 0.05) {
  df <- df %>%
    filter(!is.na(value), !is.na(.data[[group_var]])) %>%
    mutate(group = as.factor(.data[[group_var]]))
  
  group_counts <- df %>%
    count(group, name = "n")
  
  n_groups <- n_distinct(df$group)
  
  if (n_groups < 2) {
    return(list(
      shapiro = tibble(group = NA_character_, n = NA_integer_, W = NA_real_, p = NA_real_),
      levene = tibble(statistic = NA_real_, p = NA_real_),
      decision = tibble(
        method = NA_character_,
        normality_ok = NA,
        variance_ok = NA,
        reason = "Fewer than 2 groups",
        omnibus_statistic = NA_real_,
        omnibus_p = NA_real_
      ),
      posthoc = tibble()
    ))
  }
  
  # Shapiro by group
  shapiro_res <- df %>%
    group_by(group) %>%
    summarise(
      n = n(),
      W = if (n() >= 3 && n() <= 5000) unname(shapiro.test(value)$statistic) else NA_real_,
      p = if (n() >= 3 && n() <= 5000) shapiro.test(value)$p.value else NA_real_,
      .groups = "drop"
    )
  
  valid_shapiro <- shapiro_res %>% filter(!is.na(p))
  
  # Programmable rule for "normality met":
  # all groups with valid Shapiro tests must have p > alpha
  normality_ok <- if (nrow(valid_shapiro) == 0) FALSE else all(valid_shapiro$p > alpha)
  
  # Levene
  lev <- tryCatch(
    car::leveneTest(value ~ group, data = df, center = median),
    error = function(e) NULL
  )
  
  levene_res <- if (is.null(lev)) {
    tibble(statistic = NA_real_, p = NA_real_)
  } else {
    tibble(
      statistic = unname(lev[1, "F value"]),
      p = unname(lev[1, "Pr(>F)"])
    )
  }
  
  variance_ok <- !is.na(levene_res$p[1]) && levene_res$p[1] > alpha
  
  # Choose omnibus + posthoc
  if (normality_ok && variance_ok) {
    fit <- aov(value ~ group, data = df)
    fit_sum <- summary(fit)[[1]]
    
    decision <- tibble(
      method = "ANOVA",
      normality_ok = TRUE,
      variance_ok = TRUE,
      reason = "Normality met; homogeneity of variance met",
      omnibus_statistic = unname(fit_sum[1, "F value"]),
      omnibus_p = unname(fit_sum[1, "Pr(>F)"])
    )
    
    posthoc <- TukeyHSD(fit)$group %>%
      as.data.frame() %>%
      tibble::rownames_to_column("comparison") %>%
      as_tibble() %>%
      rename(
        estimate = diff,
        conf.low = lwr,
        conf.high = upr,
        p.adj = `p adj`
      ) %>%
      mutate(posthoc_method = "Tukey HSD")
    
  } else if (normality_ok && !variance_ok) {
    welch <- oneway.test(value ~ group, data = df, var.equal = FALSE)
    
    decision <- tibble(
      method = "Welch ANOVA",
      normality_ok = TRUE,
      variance_ok = FALSE,
      reason = "Normality met; homogeneity of variance violated",
      omnibus_statistic = unname(welch$statistic),
      omnibus_p = unname(welch$p.value)
    )
    
    posthoc <- rstatix::games_howell_test(df, value ~ group) %>%
      as_tibble() %>%
      mutate(
        comparison = paste(group1, group2, sep = " - "),
        posthoc_method = "Games-Howell"
      )
    
  } else {
    kw <- rstatix::kruskal_test(df, value ~ group)
    
    decision <- tibble(
      method = "Kruskal-Wallis",
      normality_ok = FALSE,
      variance_ok = variance_ok,
      reason = "Normality not met",
      omnibus_statistic = kw$statistic,
      omnibus_p = kw$p
    )
    
    posthoc <- rstatix::pairwise_wilcox_test(
      df,
      value ~ group,
      p.adjust.method = "bonferroni"
    ) %>%
      as_tibble() %>%
      mutate(
        comparison = paste(group1, group2, sep = " - "),
        posthoc_method = "Pairwise Mann-Whitney U"
      )
  }
  
  list(
    shapiro = shapiro_res,
    levene = levene_res,
    decision = decision,
    posthoc = posthoc,
    counts = group_counts
  )
}

# ---------------- Run all isotopes for one grouping variable ----------------
run_for_all_isotopes <- function(df_long, group_var, alpha = 0.05) {
  isotopes <- unique(df_long$isotope)
  
  res_list <- lapply(isotopes, function(iso) {
    x <- df_long %>% filter(isotope == iso)
    res <- run_adaptive_test(x, group_var, alpha = alpha)
    
    list(
      shapiro = res$shapiro %>% mutate(isotope = iso, .before = 1),
      levene = res$levene %>% mutate(isotope = iso, .before = 1),
      decision = res$decision %>% mutate(isotope = iso, .before = 1),
      posthoc = if (nrow(res$posthoc) > 0) res$posthoc %>% mutate(isotope = iso, .before = 1) else tibble(),
      counts = res$counts %>% mutate(isotope = iso, .before = 1)
    )
  })
  
  bind_or_empty <- function(lst, name) {
    out <- lapply(res_list, `[[`, name)
    out <- out[lengths(out) > 0]
    if (length(out) == 0) tibble() else bind_rows(out)
  }
  
  list(
    shapiro = bind_or_empty(res_list, "shapiro"),
    levene = bind_or_empty(res_list, "levene"),
    decision = bind_or_empty(res_list, "decision"),
    posthoc = bind_or_empty(res_list, "posthoc"),
    counts = bind_or_empty(res_list, "counts")
  )
}

# ---------------- Save plots ----------------
save_plots(long, "site", "Sites")

cent <- long %>%
  filter(!is.na(century)) %>%
  mutate(century = as.factor(century))

save_plots(cent, "century", "Centuries")

# ---------------- Run adaptive stats ----------------
site_results <- run_for_all_isotopes(long, "site", alpha = 0.05)
cent_results <- run_for_all_isotopes(cent, "century", alpha = 0.05)

# ---------------- Export Excel ----------------
wb <- createWorkbook()

add_sheet <- function(name, df) {
  addWorksheet(wb, name)
  writeData(wb, sheet = name, x = df)
}

add_sheet("Sites_GroupCounts", site_results$counts)
add_sheet("Sites_Shapiro", site_results$shapiro)
add_sheet("Sites_Levene", site_results$levene)
add_sheet("Sites_Decision", site_results$decision)
add_sheet("Sites_Posthoc", site_results$posthoc)

add_sheet("Cent_GroupCounts", cent_results$counts)
add_sheet("Cent_Shapiro", cent_results$shapiro)
add_sheet("Cent_Levene", cent_results$levene)
add_sheet("Cent_Decision", cent_results$decision)
add_sheet("Cent_Posthoc", cent_results$posthoc)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

cat(
  "\nDONE\n",
  "Excel saved to: ", out_xlsx, "\n",
  "Plots saved to folder: ", plot_dir, "\n",
  sep = ""
)