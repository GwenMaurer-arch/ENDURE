

# ---- 1. Packages ----
install_if_missing <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(to_install) > 0) install.packages(to_install)
}

install_if_missing(c("readxl", "dplyr", "purrr", "readr"))

library(readxl)
library(dplyr)
library(purrr)
library(readr)

# Read Excel
df <- read_excel("stats pigs.xlsx", sheet = 1)


# Adjust only if your imported names differ slightly
df <- df %>%
  rename(
    ID = `IDzoo`,
    Site = `Site`,
    Century = `Century`,
    Phase = `Phase`,
    Age = `Age`,
    Age_group = `Age group`,
    d15N_dentine = `δ¹⁵N dentine`,
    d13C_dentine = `δ¹³C dentine`,
    d34S_dentine = `δ³⁴S dentine`,
    d15N_bone = `δ¹⁵N bone`,
    d13C_bone = `δ¹³C bone`,
    d15N_shift_sheet = `δ¹⁵N shift`,
    d13C_shift_sheet = `δ¹³C shift`
  )

# ---- 4. Clean and prepare data ----
df <- df %>%
  mutate(
    Age_group = ifelse(Age_group == "Audlt", "Adult", Age_group),
    Age_group = factor(
      Age_group,
      levels = c("Juvenile", "Subadult", "Adult", "Old"),
      ordered = TRUE
    ),
    age_num = as.numeric(Age_group)
  )

# Calculate shifts directly from bone - dentine
df <- df %>%
  mutate(
    d15N_shift = d15N_bone - d15N_dentine,
    d13C_shift = d13C_bone - d13C_dentine,
    d15N_abs_shift = abs(d15N_shift),
    d13C_abs_shift = abs(d13C_shift)
  )

# Keep only paired observations
df_paired <- df %>%
  filter(
    !is.na(d15N_dentine),
    !is.na(d13C_dentine),
    !is.na(d15N_bone),
    !is.na(d13C_bone)
  ) %>%
  filter(!(d15N_bone == 0 & d13C_bone == 0))

# ---- 5. Helper functions ----
format_equation <- function(model, digits = 3) {
  coefs <- coef(model)
  intercept <- round(coefs[1], digits)
  slope <- round(coefs[2], digits)
  
  if (is.na(intercept) || is.na(slope)) return(NA_character_)
  
  if (slope >= 0) {
    paste0("y = ", intercept, " + ", slope, "x")
  } else {
    paste0("y = ", intercept, " - ", abs(slope), "x")
  }
}

pearson_summary <- function(data, x, y, shift_var) {
  sub <- data %>%
    select(all_of(c(x, y, shift_var))) %>%
    filter(complete.cases(.))
  
  if (nrow(sub) < 3) {
    return(data.frame(
      n = nrow(sub),
      r = NA_real_,
      slope = NA_character_,
      r2 = NA_real_,
      p_value = NA_real_,
      mean_shift = NA_real_
    ))
  }
  
  ct <- cor.test(sub[[x]], sub[[y]], method = "pearson")
  model <- lm(sub[[y]] ~ sub[[x]], data = sub)
  
  data.frame(
    n = nrow(sub),
    r = unname(ct$estimate),
    slope = format_equation(model, digits = 3),
    r2 = summary(model)$r.squared,
    p_value = ct$p.value,
    mean_shift = mean(sub[[shift_var]], na.rm = TRUE)
  )
}

spearman_summary <- function(data, age_var, shift_var) {
  sub <- data %>%
    select(all_of(c(age_var, shift_var))) %>%
    filter(complete.cases(.))
  
  if (nrow(sub) < 3 || length(unique(sub[[age_var]])) < 2) {
    return(data.frame(
      n = nrow(sub),
      rho = NA_real_,
      p_value = NA_real_
    ))
  }
  
  ct <- suppressWarnings(
    cor.test(sub[[age_var]], sub[[shift_var]], method = "spearman", exact = FALSE)
  )
  
  data.frame(
    n = nrow(sub),
    rho = unname(ct$estimate),
    p_value = ct$p.value
  )
}

# ---- 6. Pearson results by site ----
sites <- sort(unique(df_paired$Site))

pearson_results <- map_dfr(sites, function(site_name) {
  site_data <- df_paired %>% filter(Site == site_name)
  
  d15N_res <- pearson_summary(site_data, "d15N_dentine", "d15N_bone", "d15N_shift")
  names(d15N_res) <- paste0("d15N_", names(d15N_res))
  
  d13C_res <- pearson_summary(site_data, "d13C_dentine", "d13C_bone", "d13C_shift")
  names(d13C_res) <- paste0("d13C_", names(d13C_res))
  
  bind_cols(data.frame(Site = site_name), d15N_res, d13C_res)
})

# ---- 7. Spearman results by site ----
spearman_results <- map_dfr(sites, function(site_name) {
  site_data <- df_paired %>% filter(Site == site_name)
  
  d15N_signed <- spearman_summary(site_data, "age_num", "d15N_shift")
  names(d15N_signed) <- c("d15N_signed_n", "d15N_signed_rho", "d15N_signed_p")
  
  d15N_abs <- spearman_summary(site_data, "age_num", "d15N_abs_shift")
  names(d15N_abs) <- c("d15N_abs_n", "d15N_abs_rho", "d15N_abs_p")
  
  d13C_signed <- spearman_summary(site_data, "age_num", "d13C_shift")
  names(d13C_signed) <- c("d13C_signed_n", "d13C_signed_rho", "d13C_signed_p")
  
  d13C_abs <- spearman_summary(site_data, "age_num", "d13C_abs_shift")
  names(d13C_abs) <- c("d13C_abs_n", "d13C_abs_rho", "d13C_abs_p")
  
  bind_cols(
    data.frame(Site = site_name),
    d15N_signed,
    d15N_abs,
    d13C_signed,
    d13C_abs
  )
})

# ---- 8. Compact tables ----
pearson_table <- pearson_results %>%
  transmute(
    Site,
    d15N_r = round(d15N_r, 2),
    d15N_slope = d15N_slope,
    d15N_r2 = round(d15N_r2, 2),
    d15N_p = round(d15N_p_value, 3),
    d15N_mean_shift = round(d15N_mean_shift, 2),
    d13C_r = round(d13C_r, 2),
    d13C_slope = d13C_slope,
    d13C_r2 = round(d13C_r2, 2),
    d13C_p = round(d13C_p_value, 3),
    d13C_mean_shift = round(d13C_mean_shift, 2)
  )

spearman_table <- spearman_results %>%
  transmute(
    Site,
    d15N_signed_rho = round(d15N_signed_rho, 2),
    d15N_signed_p = round(d15N_signed_p, 3),
    d15N_abs_rho = round(d15N_abs_rho, 2),
    d15N_abs_p = round(d15N_abs_p, 3),
    d13C_signed_rho = round(d13C_signed_rho, 2),
    d13C_signed_p = round(d13C_signed_p, 3),
    d13C_abs_rho = round(d13C_abs_rho, 2),
    d13C_abs_p = round(d13C_abs_p, 3)
  )

# ---- 9. Print ----
cat("\n============================\n")
cat("PEARSON RESULTS BY SITE\n")
cat("============================\n")
print(pearson_table, row.names = FALSE)

cat("\n============================\n")
cat("SPEARMAN RESULTS BY SITE\n")
cat("============================\n")
print(spearman_table, row.names = FALSE)

# ---- 10. Export ----
write_csv(pearson_results, "pearson_results_full.csv")
write_csv(spearman_results, "spearman_results_full.csv")