library(dplyr)
library(readr)
library(haven)
library(stringr)
library(fixest)
library(ggplot2)

base <- "Crowdout and Competition"

welfare <- read_csv(file.path(base, "Data/derived/welfare_data.csv"), show_col_types = FALSE)
key <- welfare %>%
  filter(post_2013 == 1) %>%
  mutate(mun_psgc = as.character(mun_psgc), prov_psgc = as.character(prov_psgc)) %>%
  select(lk4, mun_psgc, prov_psgc, treat, pi, land, bgytot, pop07nso) %>%
  group_by(lk4) %>%
  summarise(across(everything(), ~ dplyr::first(na.omit(.x))), .groups = "drop")

ad <- read_csv(file.path(base, "Data/derived/analysis_data.csv"), show_col_types = FALSE)
mun_disc <- ad %>%
  mutate(
    mun_psgc = as.character(mun_psgc),
    own_source_share = if_else(total_current_operating_income > 0, total_local_sources / total_current_operating_income, NA_real_),
    tax_pc = if_else(pop07nso > 0, total_tax_revenue / pop07nso, NA_real_),
    cash_pc = if_else(pop07nso > 0, fund_cash_balance_end / pop07nso, NA_real_)
  ) %>%
  filter(year < 2013) %>%
  group_by(mun_psgc) %>%
  summarise(own_source_share = mean(own_source_share, na.rm = TRUE), tax_pc = mean(tax_pc, na.rm = TRUE), cash_pc = mean(cash_pc, na.rm = TRUE), .groups = "drop") %>%
  mutate(discretion_index = rowMeans(cbind(scale(own_source_share), scale(tax_pc), scale(cash_pc)), na.rm = TRUE)) %>%
  select(mun_psgc, discretion_index)

z_from_control <- function(x, treat) {
  mu <- mean(x[treat == 0], na.rm = TRUE)
  sdv <- sd(x[treat == 0], na.rm = TRUE)
  if (is.na(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
  (x - mu) / sdv
}

make_domain <- function(df, vars, treat_var = "treat") {
  out <- df
  for (v in vars) out[[paste0("z_", v)]] <- z_from_control(out[[v]], out[[treat_var]])
  zvars <- paste0("z_", vars)
  out$domain_index <- rowMeans(out[, zvars], na.rm = TRUE)
  out$domain_index[!is.finite(out$domain_index)] <- NA_real_
  out
}

enddir <- file.path(base, "Data/KALAHI/Endline Public Use Data Package/Datasets")

infra <- read_dta(file.path(enddir, "3rdRnd_Hypothesis_1a_AnalysisVarsA.dta")) %>%
  as_tibble() %>%
  select(lk4, matches("^acs1_m"), matches("^acs3"), matches("^acs4")) %>%
  mutate(dist_pub_services = -rowSums(across(matches("^acs1_m")), na.rm = TRUE),
         travel_time = -rowSums(across(matches("^acs3")), na.rm = TRUE),
         travel_cost = -rowSums(across(matches("^acs4")), na.rm = TRUE)) %>%
  select(lk4, dist_pub_services, travel_time, travel_cost)

water <- read_dta(file.path(enddir, "3rdRnd_Hypothesis_1h_AnalysisVarsA.dta")) %>%
  as_tibble() %>%
  mutate(across(starts_with("ws"), ~ ifelse(.x == -95, NA, as.numeric(.x)))) %>%
  mutate(amnt_paid = -(ws12_r + ws12_d),
         time_spent = -(ws8_r + ws8_d),
         num_trips = -(ws9_dt + ws9_rt)) %>%
  select(lk4, amnt_paid, time_spent, num_trips)

labor <- read_dta(file.path(enddir, "3rdRnd_LT2_Labor_HH-AnalysisVarsB.dta")) %>%
  as_tibble() %>%
  mutate(across(starts_with("lm"), ~ ifelse(.x == -95, NA, as.numeric(.x)))) %>%
  transmute(lk4, income = as.numeric(lm9))

spending <- read_dta(file.path(enddir, "3rdRnd_LT1_AnalysisVarsB.dta")) %>%
  as_tibble() %>%
  mutate(across(everything(), ~ ifelse(.x == -95, NA, .x))) %>%
  mutate(hh_spending = rowSums(across(matches("1$")), na.rm = TRUE),
         hh_produce = rowSums(across(matches("2$")), na.rm = TRUE)) %>%
  select(lk4, hh_spending, hh_produce)

edu <- read_dta(file.path(enddir, "3rdRnd_Hypothesis_1e_AnalysisVarsB.dta")) %>%
  as_tibble() %>%
  mutate(num_teachers = rowSums(across(matches("teacher")), na.rm = TRUE),
         num_schools = rowSums(across(any_of(c("edf_public_elem", "edf_public_sec"))), na.rm = TRUE),
         enrollment = rowSums(across(matches("enrolled")), na.rm = TRUE)) %>%
  select(lk4, num_teachers, num_schools, enrollment)

hardship <- read_dta(file.path(enddir, "3rdRnd_H8_Hardship_HH_AnalysisVarsB.dta")) %>%
  as_tibble() %>%
  mutate(across(starts_with("de"), ~ suppressWarnings(ifelse(.x == -95, NA, as.numeric(.x))))) %>%
  mutate(assistance = rowSums(across(any_of(c("de6_death_finass_a", "de6_ill_finass_a", "de6_job_finass_a", "de6_calam_finass_a", "de6_noharv_finass_a", "de6_van_finass_a"))), na.rm = TRUE),
         asked_lgu = if_else(coalesce(de4_death_helpLguOff,0)==1 | coalesce(de4_illness_helpLguOff,0)==1 |
                               coalesce(de4_jobless_helpLguOff,0)==1 | coalesce(de4_calamity_helpLguOff,0)==1 |
                               coalesce(de4_noharvest_helpLguOff,0)==1 | coalesce(de4_vandal_helpLguOff,0)==1, 1, 0)) %>%
  select(lk4, assistance, asked_lgu)

prep_domain <- function(df, vars, domain_name) {
  x <- df %>%
    left_join(key, by = "lk4") %>%
    left_join(mun_disc, by = "mun_psgc") %>%
    mutate(log_pop = log(pmax(pop07nso, 1))) %>%
    filter(!is.na(treat), !is.na(discretion_index), !is.na(prov_psgc), !is.na(mun_psgc))
  x <- make_domain(x, vars, treat_var = "treat")
  x %>% mutate(domain = domain_name)
}

infra_d <- prep_domain(infra, c("dist_pub_services", "travel_time", "travel_cost"), "Infrastructure")
water_d <- prep_domain(water, c("amnt_paid", "time_spent", "num_trips"), "Water Access")
labor_d <- prep_domain(labor, c("income"), "Labor Income")
spend_d <- prep_domain(spending, c("hh_spending", "hh_produce"), "Household Spending")
edu_d <- prep_domain(edu, c("num_teachers", "num_schools", "enrollment"), "Education")
hard_d <- prep_domain(hardship, c("assistance", "asked_lgu"), "Hardship Support")

domain_list <- list(
  Infrastructure = infra_d,
  `Water Access` = water_d,
  `Labor Income` = labor_d,
  `Household Spending` = spend_d,
  Education = edu_d,
  `Hardship Support` = hard_d
)

all_domain_scores <- bind_rows(lapply(names(domain_list), function(nm) {
  domain_list[[nm]] %>% select(lk4, domain_index) %>% mutate(domain = nm)
}))

overall <- all_domain_scores %>%
  group_by(lk4) %>%
  summarise(overall_welfare = mean(domain_index, na.rm = TRUE), .groups = "drop") %>%
  left_join(key, by = "lk4") %>%
  left_join(mun_disc, by = "mun_psgc") %>%
  mutate(log_pop = log(pmax(pop07nso, 1)), domain = "Overall Welfare", domain_index = overall_welfare)

estimate_domain <- function(df) {
  d <- df %>%
    transmute(y = domain_index, treat, discretion_index, pi, land, bgytot, log_pop, prov_psgc, mun_psgc) %>%
    filter(is.finite(y), is.finite(treat), is.finite(discretion_index), is.finite(pi), is.finite(land), is.finite(bgytot), is.finite(log_pop), !is.na(prov_psgc), !is.na(mun_psgc))

  if (nrow(d) < 150 || sd(d$y) == 0 || length(unique(d$treat)) < 2) return(tibble())
  m <- tryCatch(feols(y ~ treat * discretion_index + pi + land + bgytot + log_pop | prov_psgc, data = d, cluster = ~mun_psgc), error = function(e) NULL)
  if (is.null(m)) return(tibble())
  ct <- coeftable(m)
  if (!("treat:discretion_index" %in% rownames(ct))) return(tibble())
  tibble(
    n = nobs(m),
    municipalities = n_distinct(d$mun_psgc),
    provinces = n_distinct(d$prov_psgc),
    estimate = ct["treat:discretion_index", "Estimate"],
    se = ct["treat:discretion_index", "Std. Error"],
    p = ct["treat:discretion_index", "Pr(>|t|)"],
    y_mean = mean(d$y),
    y_sd = sd(d$y)
  )
}

results <- bind_rows(lapply(names(domain_list), function(nm) {
  estimate_domain(domain_list[[nm]]) %>% mutate(domain = nm)
}))
results <- bind_rows(results, estimate_domain(overall) %>% mutate(domain = "Overall Welfare")) %>%
  mutate(ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se)

csv_out <- file.path(base, "Data/derived/discretion_domain_moderation_results.csv")
plot_out <- file.path(base, "Data/derived/discretion_domain_moderation_plot.png")
write_csv(results, csv_out)

plot_df <- results %>% mutate(domain = factor(domain, levels = rev(domain)))
g <- ggplot(plot_df, aes(x = estimate, y = domain)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray55") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, color = "gray35") +
  geom_point(size = 2.4, color = "#0F6E8C") +
  labs(x = "Coefficient on Treat x Discretion", y = NULL, title = "Budget Discretion Moderation Across Welfare Domains", subtitle = "Endline non-DiD models with 95% CIs") +
  theme_minimal(base_size = 12)

ggsave(plot_out, g, width = 9, height = 5.5, dpi = 300)

print(results %>% arrange(p))
cat("\nWrote:", csv_out, "\n")
cat("Wrote:", plot_out, "\n")
