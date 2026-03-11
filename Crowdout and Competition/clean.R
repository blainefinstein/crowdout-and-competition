library(tidyverse)
library(readxl)
library(haven)
library(sf)
library(fuzzyjoin)

set.seed(252463)

out_dir <- "Data/derived"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

canonical_mun <- function(x) {
  y <- x |>
    str_trim() |>
    str_to_title()

  case_when(
    y == "Villa Viciosa" ~ "Villaviciosa",
    y == "Catarman (Capital)" ~ "Catarman",
    y == "General Macarthur" ~ "General MacArthur",
    y == "Macarthur" ~ "MacArthur",
    y == "Brooke'S Point" ~ "Brooke's Point",
    y == "El Nido (Bacuit)" ~ "El Nido",
    y == "Romblon (Capital)" ~ "Romblon",
    y == "Lau-An" ~ "Lau An",
    y == "Tobias Fornier (Dao)" ~ "Tobias Fornier",
    y == "Ipil (Capital)" ~ "Ipil",
    y == "R. T. Lim" ~ "Roseller Lim",
    y == "Licuan-Baay" ~ "Baay Licuan",
    y == "Licuan-Baay (Licuan)" ~ "Baay Licuan",
    y == "Asuncion (Saug)" ~ "Asuncion",
    y == "Lambayong (Mariano Marcos)" ~ "Lambayang",
    y == "Santa Monica (Sapao)" ~ "Santa Monica",
    y == "San Francisco (Anao-Aon)" ~ "San Francisco",
    y == "Anini-Y" ~ "Anini Y",
    y == "Roseller T. Lim" ~ "Roseller Lim",
    y == "Lavesares" ~ "Lavezares",
    str_detect(y, "Sofronio") ~ "Sofronio Espanola",
    TRUE ~ y
  )
}

std_key <- function(x) {
  x |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9]", "")
}

canonical_region <- function(x) {
  x |>
    str_trim() |>
    str_to_upper() |>
    str_replace_all("^REGION\\s+", "") |>
    str_replace_all("\\s+REGION$", "") |>
    str_replace_all("MIMAROPA", "IV-B") |>
    str_replace_all("CARAGA", "XIII")
}

canonical_province <- function(x) {
  y <- x |>
    str_trim() |>
    str_to_upper() |>
    str_replace_all("\\s+", " ")
  case_when(
    y == "COMPOSTELA VALLEY" ~ "DAVAO DE ORO",
    TRUE ~ y
  )
}

process_quarterly <- function(file_path, sheet_name) {
  raw <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)

  names_clean <- raw[1:10, ] |>
    as.data.frame() |>
    summarise_all(~ last(na.omit(.))) |>
    unlist() |>
    str_to_lower() |>
    str_replace_all(" ", "_") |>
    str_replace_all("/", "_") |>
    str_replace_all("[^a-z_]", "") |>
    str_replace_all("_+", "_")

  raw[-(1:10), ] |>
    setNames(names_clean) |>
    filter(lgu_type == "Municipality") |>
    select(-lgu_type) |>
    rename(mun = lgu_name) |>
    mutate(across(4:last_col(), as.numeric)) |>
    mutate(
      quarter = str_match(sheet_name, "Q[1-4]20[0-1][0-9]")[, 1],
      year = as.numeric(str_match(sheet_name, "20[0-1][0-9]")[, 1]),
      kalahi_spending = rowSums(
        across(
          c(
            education_culture_sports_manpower_development,
            health_nutrition_population_control,
            housing_and_community_development,
            social_services_and_social_welfare,
            general_public_services
          )
        ),
        na.rm = TRUE
      ),
      total_spending = rowSums(
        across(
          c(
            total_current_operating_expenditures,
            total_nonoperating_expenditures,
            total_nonincome_receipts
          )
        ),
        na.rm = TRUE
      ),
      region = region |>
        canonical_region(),
      province = canonical_province(province),
      mun = canonical_mun(str_replace(mun, " \\(.*$", ""))
    )
}

process_single_file <- function(file_path) {
  path_parts <- str_split(file_path, "/", simplify = TRUE)
  region <- ifelse(length(path_parts) >= 2, path_parts[length(path_parts) - 2], NA_character_)
  region <- region |>
    str_replace_all("REGION ", "") |>
    str_replace_all("C.A.R.", "CAR") |>
    str_replace_all("4A", "IV-A") |>
    str_replace_all("4B", "IV-B")
  if (str_detect(region, "^[0-9]+$")) {
    region <- as.character(as.roman(as.integer(region)))
  }

  sheets <- excel_sheets(file_path)
  sheet_name <- if ("MAYOR" %in% sheets) "MAYOR" else if ("Mayor" %in% sheets) "Mayor" else NA_character_
  if (is.na(sheet_name)) return(NULL)

  mayor_data <- read_excel(file_path, sheet = sheet_name)
  metadata_cols <- c(
    "Region", "Province", "Municipality/City", "Barangay", "Precinct Code",
    "Actual Voters Voted", "Contest Name"
  )
  vote_cols <- setdiff(colnames(mayor_data), metadata_cols)
  if (length(vote_cols) == 0) return(NULL)

  vote_sum <- mayor_data |>
    select(all_of(vote_cols)) |>
    mutate(across(everything(), ~ as.numeric(gsub(",", "", .)))) |>
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "candidate", values_to = "votes") |>
    mutate(vote_share = votes / sum(votes, na.rm = TRUE)) |>
    arrange(desc(vote_share))

  if (nrow(vote_sum) == 0) return(NULL)
  winner_row <- vote_sum |> slice(1)
  runner_up_row <- if (nrow(vote_sum) > 1) vote_sum |> slice(2) else tibble(candidate = NA_character_, votes = 0, vote_share = 0)

  tibble(
    region = region,
    mun = canonical_mun(str_to_title(tools::file_path_sans_ext(basename(file_path)))),
    winner = winner_row$candidate,
    uncontested = ifelse(is.na(runner_up_row$candidate), 1, 0),
    vote_share = winner_row$vote_share,
    vote_diff = winner_row$vote_share - runner_up_row$vote_share
  )
}

process_yearly <- function(file_path) {
  raw <- read_excel(file_path)
  names_clean <- colnames(raw) |>
    str_to_lower() |>
    str_replace_all(" ", "_") |>
    str_replace_all("/", "_") |>
    str_replace_all("[^a-z_]", "") |>
    str_replace_all("_+", "_") |>
    str_replace_all("social_security_social_services_welfare", "social_services_and_social_welfare")

  out <- setNames(raw, names_clean) |>
    filter(lgu_type == "Municipality") |>
    select(-lgu_type) |>
    rename(mun = lgu_name) |>
    mutate(across(4:last_col(), as.numeric)) |>
    mutate(
      year = as.numeric(str_match(file_path, "20[0-2][0-9]")[, 1]),
      region = region |>
        str_replace_all("Region ", "") |>
        str_replace_all(" Region", "") |>
        str_replace_all("MIMAROPA", "IV-B"),
      mun = canonical_mun(mun),
      kalahi_spending = rowSums(
        across(
          c(
            education_culture_sports_manpower_development,
            health_nutrition_population_control,
            housing_and_community_development,
            social_services_and_social_welfare
          )
        ),
        na.rm = TRUE
      ),
      total_spending = ifelse(
        year < 2009,
        total_expenditures,
        rowSums(across(c(total, total_current_operating_expenditures, total_nonoperating_expenditures)), na.rm = TRUE)
      )
    )

  if (str_detect(file_path, "200[1-8]")) {
    numeric_cols <- names(out)[sapply(out, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, "year")
    out <- out |>
      mutate(across(all_of(numeric_cols), ~ .x * 1e6))
  }

  out
}

standardize <- function(data, vars) {
  for (v in vars) {
    mu <- mean(data[[v]], na.rm = TRUE)
    sigma <- sd(data[[v]], na.rm = TRUE)
    data[[v]] <- ifelse(is.na(sigma) || sigma == 0, 0, (data[[v]] - mu) / sigma)
  }
  data
}

winsorize_vec <- function(x, probs = c(0.01, 0.99)) {
  if (all(is.na(x))) {
    return(x)
  }

  bounds <- quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  pmin(pmax(x, bounds[1]), bounds[2])
}

zscore_vec <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)

  if (is.na(sigma) || sigma == 0) {
    return(if_else(is.na(x), NA_real_, 0))
  }

  (x - mu) / sigma
}

mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

weighted_mean_or_na <- function(x, w) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  weighted.mean(x, w, na.rm = TRUE)
}

matching_raw <- read_csv("Data/KALAHI/PHL-KC Matching Data for Sample/matching data for sample.csv", show_col_types = FALSE)
interim_noncompliers <- read_dta("Data/KALAHI/Endline Public Use Data Package/Datasets/Interim Details on Full Sample 198 Sample v5.dta") |>
  transmute(pairnum, treatment, non_complier) |>
  distinct()
psgc_geo <- read_csv("Data/philippines_barangays.csv", show_col_types = FALSE) |>
  transmute(
    mun_psgc = city_mun_code,
    province_psgc = canonical_province(prov_name)
  ) |>
  distinct(mun_psgc, province_psgc)

matching <- matching_raw |>
  mutate(
    mun_psgc = str_pad(mun_psgc, width = 6, side = "left", pad = "0"),
    mun = canonical_mun(str_replace(mun, " \\(.*$", "")) |>
      str_replace("R\\. T\\. Lim", "Roseller Lim")
  ) |>
  rename(region = reg_psgc, treat = treatment) |>
  mutate(region = canonical_region(region)) |>
  left_join(interim_noncompliers, by = c("pairnum", "treat" = "treatment")) |>
  left_join(psgc_geo, by = "mun_psgc") |>
  mutate(
    province_psgc = case_when(
      mun_psgc == "175306" & is.na(province_psgc) ~ "PALAWAN",
      TRUE ~ province_psgc
    )
  ) |>
  select(island, region, mun, treat, pi, bgytot, pop07nso, land, mun_psgc, province_psgc, pairnum)

qtr_files <- crossing(year = 2009:2015, q = 1:4) |>
  mutate(
    file = paste0("Data/Quarterly Spending/SRE-QTR-", year, ".xlsx"),
    sheet = paste0("Q", q, year)
  ) |>
  filter(file.exists(file))

quarterly <- pmap_dfr(qtr_files, ~ process_quarterly(..3, ..4))

elections_mayor <- read_csv("Data/municipal_elections.csv", show_col_types = FALSE) |>
  filter(position == "Mayor", !is.na(year)) |>
  transmute(
    election_year = as.numeric(year),
    region = canonical_region(region),
    province = canonical_province(province),
    mun = canonical_mun(municity),
    candidate = candidate,
    candidate_key = candidate_key,
    first_place_flag = first_place_flag,
    vote_share = vote_share,
    vote_diff = coalesce(margin_share_top1_top2, pmax(0, 2 * vote_share - 1)),
    votes = votes,
    candidate_last_word = str_to_upper(str_match(candidate, "([A-Za-z'`.-]+)\\s*,")[, 2]),
    candidate_first_word = str_to_upper(str_match(candidate, ",\\s*([A-Za-z'`.-]+)")[, 2])
  )

elections_panel <- elections_mayor |>
  filter(first_place_flag == 1, !is.na(vote_diff)) |>
  arrange(region, province, mun, election_year, desc(votes), desc(vote_share), candidate) |>
  group_by(region, province, mun, election_year) |>
  slice(1) |>
  ungroup() |>
  transmute(
    election_year,
    region,
    province,
    mun,
    winner = candidate,
    winner_candidate_key = candidate_key,
    winner_last_word = candidate_last_word,
    winner_first_word = candidate_first_word,
    vote_share,
    vote_diff,
    region_key = std_key(region),
    province_key = std_key(province),
    mun_key = std_key(mun)
  ) |>
  group_by(region_key, province_key, mun_key) |>
  arrange(election_year, .by_group = TRUE) |>
  mutate(next_election_year = lead(election_year)) |>
  left_join(
    elections_mayor |>
      transmute(
        region_key = std_key(region),
        province_key = std_key(province),
        mun_key = std_key(mun),
        next_election_year = election_year,
        next_candidate_key = candidate_key,
        next_last_word = candidate_last_word,
        next_first_word = candidate_first_word
    ),
    by = c("region_key", "province_key", "mun_key", "next_election_year"),
    relationship = "one-to-many"
  ) |>
  group_by(
    region_key, province_key, mun_key, election_year,
    region, province, mun, winner, winner_candidate_key,
    winner_last_word, winner_first_word, vote_share, vote_diff
  ) |>
  summarise(
    dynasty_next_mayor = as.integer(any(
      !is.na(next_last_word) &
        !is.na(winner_last_word) &
        next_last_word == winner_last_word &
        !is.na(next_first_word) &
        !is.na(winner_first_word) &
        next_first_word != winner_first_word
    )),
    .groups = "drop"
  ) |>
  select(
    election_year, region, province, mun, winner, vote_share, vote_diff,
    dynasty_next_mayor, region_key, province_key, mun_key
  )

historic_vote_diff <- elections_panel |>
  filter(election_year %in% c(2007, 2010)) |>
  select(region, province, mun, election_year, vote_diff) |>
  distinct(region, province, mun, election_year, .keep_all = TRUE) |>
  pivot_wider(
    names_from = election_year,
    values_from = vote_diff,
    names_prefix = "vote_diff_"
  ) |>
  select(region, province, mun, vote_diff_2007, vote_diff_2010)

yolanda_seed <- c(
  "Gumaca", "Lopez", "General Nakar", "Tagkawayan", "Mauban", "San Antonio", "Real",
  "Alabat", "General Luna", "Padre Burgos", "Quezon", "Agdangan", "Montevista", "Pantukan"
)
yolanda <- unique(c(yolanda_seed, matching$mun[matching$region %in% c("IV-B", "V", "VI", "VII", "VIII", "XIII")]))

analysis_data <- matching |>
  select(-island) |>
  mutate(province_psgc = canonical_province(province_psgc)) |>
  ungroup() |>
  left_join(
    quarterly |>
      mutate(province = canonical_province(province)),
    by = c("region", "mun", "province_psgc" = "province"),
    relationship = "one-to-many"
  ) |>
  rename(province = province_psgc) |>
  filter(!is.na(quarter), !is.na(treat)) |>
  mutate(
    panel_year = as.numeric(substr(quarter, 3, 6)),
    lgu_id = str_c(region, province, mun, sep = "__"),
    region_key = std_key(region),
    province_key = std_key(province),
    mun_key = std_key(mun)
  ) |>
  filter(panel_year >= 2013, panel_year <= 2015) |>
  left_join(
    elections_panel |>
      arrange(election_year) |>
      select(region_key, province_key, mun_key, election_year, winner, vote_share, vote_diff, dynasty_next_mayor),
    by = join_by(region_key, province_key, mun_key, panel_year >= election_year),
    multiple = "last",
    relationship = "many-to-one"
  ) |>
  mutate(
    kalahi_spending_percap = kalahi_spending / pop07nso,
    total_spending_percap = total_spending / pop07nso,
    election_year_recent = election_year
  ) |>
  mutate(
    eightieth_margin = ifelse(vote_diff > .8, 1, 0),
    seventyfifth_margin = ifelse(vote_diff > .75, 1, 0),
    seventieth_margin = ifelse(vote_diff > .7, 1, 0),
    sixtyfifth_margin = ifelse(vote_diff > .65, 1, 0),
    sixtieth_margin = ifelse(vote_diff > .6, 1, 0),
    fiftyfifth_margin = ifelse(vote_diff > .55, 1, 0),
    fiftieth_margin = ifelse(vote_diff > .5, 1, 0),
    fortyfifth_margin = ifelse(vote_diff > .45, 1, 0),
    fortieth_margin = ifelse(vote_diff < .40, 1, 0),
    thirtyfifth_margin = ifelse(vote_diff < .35, 1, 0),
    thirtieth_margin = ifelse(vote_diff < .30, 1, 0),
    twentyfifth_margin = ifelse(vote_diff < .25, 1, 0),
    twentieth_margin = ifelse(vote_diff < .2, 1, 0),
    fifteenth_margin = ifelse(vote_diff < .15, 1, 0),
    tenth_margin = ifelse(vote_diff < .1, 1, 0),
    fifth_margin = ifelse(vote_diff < .05, 1, 0),
    marginal_margin = ifelse(vote_diff < .025, 1, 0),
    yolanda = ifelse(
      mun %in% yolanda &
        (as.numeric(substr(quarter, 3, 6)) > 2013 | quarter == "Q42013"),
      1, 0
    ),
    post = ifelse(as.numeric(substr(quarter, 3, 6)) >= 2013, 1, 0),
    post_treat = treat * post,
    dynasty_next_mayor = replace_na(dynasty_next_mayor, 0),
    # Approximate Kalahi funding intensity: $10k per barangay per annual cycle, 3 cycles typical
    program_intensity_usd = treat * bgytot * 10000 * 3,
    program_intensity_usd_percap = program_intensity_usd / pop07nso
  )

quarterly_geo <- quarterly |>
  mutate(
    region_key = std_key(region),
    mun_key = std_key(mun),
    province_key = std_key(canonical_province(province))
  ) |>
  distinct(region_key, mun_key, province_key) |>
  mutate(in_quarterly = 1L)

election_geo <- elections_panel |>
  distinct(region_key, mun_key, province_key) |>
  mutate(in_elections = 1L)

treated_audit <- analysis_data |>
  filter(treat == 1) |>
  distinct(region, mun, province, region_key, province_key, mun_key) |>
  left_join(quarterly_geo, by = c("region_key", "mun_key", "province_key")) |>
  left_join(election_geo, by = c("region_key", "mun_key", "province_key")) |>
  mutate(
    n_quarterly_geo_keys = ifelse(is.na(in_quarterly), 0L, 1L),
    n_election_geo_keys = ifelse(is.na(in_elections), 0L, 1L),
    one_to_one_quarterly = n_quarterly_geo_keys == 1,
    one_to_one_elections = n_election_geo_keys == 1,
    one_to_one_both = one_to_one_quarterly & one_to_one_elections
  ) |>
  select(-in_quarterly, -in_elections) |>
  arrange(region, mun)

phl_munis <- st_read("Data/Municities/MuniCities.shp", quiet = TRUE) |>
  rename(mun = NAME_2) |>
  mutate(mun = canonical_mun(mun)) |>
  group_by(mun) |>
  filter(n() == 1) |>
  ungroup()

muni_treat <- analysis_data |>
  distinct(mun, treat) |>
  inner_join(phl_munis |> select(mun, geometry), by = "mun") |>
  st_as_sf()

adj_matrix <- st_touches(muni_treat, muni_treat)
bordering_control <- sapply(adj_matrix, function(neighbors) any(muni_treat$treat[neighbors] == 1))
border_df <- muni_treat |>
  st_drop_geometry() |>
  mutate(border_treat = ifelse(treat == 0 & bordering_control, 1, 0)) |>
  select(mun, border_treat)

analysis_data <- analysis_data |>
  left_join(border_df, by = "mun") |>
  mutate(border_treat = replace_na(border_treat, 0))

term_limited <- c(
  "La Paz", "Langiden", "Luba", "Malibcong", "San Juan", "Kapalong", "Lopez", "Quezon", "Real",
  "Bansud", "Balabac", "Culion", "Banton", "Calatrava", "Romblon", "Paracale", "Cabusao", "Goa",
  "Ragay", "Tigaon", "Tinambac", "Gigmoto", "San Miguel", "Cataingan", "Prieto Diaz",
  "New Washington", "Tangalan", "Bugasong", "Arteche", "Dolores", "General MacArthur",
  "Quinapondan", "Alangalang", "Babatngon", "Barugo", "Burauen", "Jaro", "Lavezares",
  "Dumalinao", "Tambulig", "Ipil", "Naga", "Talusan"
)

analysis_data <- analysis_data |>
  mutate(term_limited = ifelse(mun %in% term_limited, 1, 0))

dynasties <- read_excel("Data/ASoG-POLITICAL-DYNASTIES-DATASET-V2016.xlsx", sheet = "Data") |>
  rename(mun = Municipality.City, first_name = `First Name`, last_name = `Last Name`) |>
  mutate(
    mun = canonical_mun(mun),
    first_name = str_to_upper(str_trim(first_name)),
    last_name = str_to_upper(str_trim(last_name))
  ) |>
  filter(Year == 2013) |>
  select(mun, first_name, last_name, fat) |>
  distinct(mun, first_name, last_name, .keep_all = TRUE)

analysis_data <- analysis_data |>
  mutate(
    last_name = str_to_upper(str_match(winner, "^([^,]+),")[, 2]),
    first_name = str_to_upper(str_match(winner, ",\\s(.*?)\\s\\(")[, 2])
  ) |>
  left_join(dynasties, by = c("mun", "first_name", "last_name")) |>
  select(-first_name, -last_name)

historic_comp <- read_csv("Data/historic_competition.csv", show_col_types = FALSE) |>
  mutate(
    region = canonical_region(region),
    province = canonical_province(province),
    mun = canonical_mun(mun),
    z_mayor_votes_2010 = -as.numeric(scale(mayor_votes_2010 / pop07nso)),
    z_council_votes_mean_2010 = -as.numeric(scale(council_votes_mean_2010)),
    z_council_votes_max_2010 = -as.numeric(scale(council_votes_max_2010)),
    z_council_hhi_2010 = as.numeric(scale(1 - council_hhi_2010)),
    z_mcr_2010 = -as.numeric(scale(mayor_council_ratio_2010)),
    z_mayor_votes_2007 = -as.numeric(scale(mayor_votes_2007 / pop07nso)),
    z_council_votes_mean_2007 = -as.numeric(scale(council_votes_mean_2007)),
    z_council_votes_max_2007 = -as.numeric(scale(council_votes_max_2007)),
    z_council_hhi_2007 = as.numeric(scale(1 - council_hhi_2007)),
    z_mcr_2007 = -as.numeric(scale(mayor_council_ratio_2007)),
    comp_2010 = rowMeans(cbind(z_mayor_votes_2010, z_council_votes_mean_2010, z_council_votes_max_2010, z_council_hhi_2010, z_mcr_2010), na.rm = TRUE),
    comp_2007 = rowMeans(cbind(z_mayor_votes_2007, z_council_votes_mean_2007, z_council_votes_max_2007, z_council_hhi_2007, z_mcr_2007), na.rm = TRUE)
  ) |>
  select(region, province, mun, comp_2010, comp_2007) |>
  distinct(region, province, mun, .keep_all = TRUE)

analysis_data <- analysis_data |>
  left_join(historic_comp, by = c("region", "province", "mun")) |>
  left_join(historic_vote_diff, by = c("region", "province", "mun"))

dup_panel <- analysis_data |>
  count(region, province, mun, quarter) |>
  filter(n > 1)

if (nrow(dup_panel) > 0) {
  stop("analysis_data has duplicate region-province-municipality-quarter keys after cleaning.")
}

pc <- read_excel("Data/peace_corps.xlsx", sheet = 2) |>
  mutate(
    pc_EndDate = case_when(
      is.na(pc_EndDate) ~ as.Date(NA),
      suppressWarnings(!is.na(as.numeric(pc_EndDate))) ~ as.Date(as.numeric(pc_EndDate), origin = "1899-12-30"),
      TRUE ~ as.Date(lubridate::ymd_hms(pc_EndDate, quiet = TRUE))
    ),
    start_year = as.numeric(str_extract(pc_StartDate, "\\d{4}")),
    end_year = as.numeric(str_extract(pc_EndDate, "\\d{4}"))
  ) |>
  filter(start_year <= 2013, end_year >= 2015) |>
  distinct(municity)

analysis_data <- analysis_data |>
  mutate(pc = ifelse(mun %in% pc$municity, 1, 0))

# Competition recode for interpretability:
# higher values = more competition (smaller vote margin).
analysis_data <- analysis_data |>
  mutate(
    competition_c = (-vote_diff) - mean(-vote_diff, na.rm = TRUE),
    competition_z = if_else(
      is.na(vote_diff),
      NA_real_,
      as.numeric(scale(-vote_diff))
    )
  )

psgc_muni_lookup <- read_csv("Data/philippines_barangays.csv", show_col_types = FALSE) |>
  transmute(
    mun_psgc = city_mun_code,
    mun_from_psgc = canonical_mun(city_mun_name),
    prov_psgc_from_psgc = prov_code,
    reg_psgc_from_psgc = region_code,
    province_from_psgc = canonical_province(prov_name),
    region_from_psgc = canonical_region(region_name)
  ) |>
  distinct(mun_psgc, .keep_all = TRUE)

matching_hist <- matching |>
  select(region, mun, province_psgc, pairnum, treat, pi, bgytot, pop07nso, land) |>
  distinct(region, mun, province_psgc, .keep_all = TRUE)

historic_data <- list.files("Data/Yearly Spending", full.names = TRUE) |>
  keep(~ str_detect(.x, regex("by-lgu-sre-20[0-1][0-9]\\.xlsx$", ignore_case = TRUE))) |>
  map_df(process_yearly) |>
  mutate(province = canonical_province(province)) |>
  inner_join(
    matching_hist,
    by = c("region", "mun", "province" = "province_psgc")
  ) |>
  filter(!is.na(treat), !is.na(year)) |>
  distinct(region, mun, year, .keep_all = TRUE) |>
  mutate(
    year = as.integer(year),
    kalahi_spending_percap = kalahi_spending / pop07nso,
    total_spending_percap = total_spending / pop07nso,
    historic_date = as.Date(sprintf("%d-01-01", as.integer(year))),
    lgu_id = str_c(region, province, mun, sep = "__")
  )

write_csv(analysis_data, file.path(out_dir, "analysis_data.csv"))
write_csv(historic_data, file.path(out_dir, "historic_data.csv"))

analysis_match <- matching_raw |>
  rename(region = reg_psgc) |>
  select(-island) |>
  mutate(
    mun = canonical_mun(mun),
    yolanda = ifelse(mun %in% yolanda, 1, 0)
  ) |>
  left_join(interim_details[, -4], by = c("pairnum", "treatment")) |>
  rename(treat = treatment) |>
  select(-treatmen) |>
  left_join(
    analysis_data |>
      group_by(region, mun) |>
      summarise(
        vote_share = first(na.omit(vote_share), default = NA_real_),
        vote_diff = first(na.omit(vote_diff), default = NA_real_),
        dynasty_next_mayor = first(na.omit(dynasty_next_mayor), default = NA_integer_),
        border_treat = first(na.omit(border_treat), default = NA_real_),
        kalahi_spending_percap = first(na.omit(kalahi_spending_percap), default = NA_real_),
        total_spending_percap = first(na.omit(total_spending_percap), default = NA_real_),
        internal_revenue_allotment = first(na.omit(internal_revenue_allotment), default = NA_real_),
        total_current_operating_income = first(na.omit(total_current_operating_income), default = NA_real_),
        .groups = "drop"
      ) |>
      mutate(
        dynasty_next_mayor = replace_na(dynasty_next_mayor, 0L),
        border_treat = replace_na(border_treat, 0)
      ) |>
      mutate(percent_ira = internal_revenue_allotment / total_current_operating_income) |>
      select(region, mun, vote_share, vote_diff, dynasty_next_mayor, border_treat, kalahi_spending_percap, total_spending_percap, percent_ira),
    by = c("region", "mun")
  )

z_from_control <- function(x, treat) {
  mu <- mean(x[treat == 0], na.rm = TRUE)
  sigma <- sd(x[treat == 0], na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mu) / sigma
}

sanitize_special_missing <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  if_else(x_num %in% c(-95, -96, -97, -98, -99), NA_real_, x_num)
}

sanitize_acs4_cost <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  # In KALAHI acs4_r* labels, -97 denotes walking (zero monetary cost).
  x_num <- if_else(x_num == -97, 0, x_num)
  if_else(x_num %in% c(-95, -96, -98, -99), NA_real_, x_num)
}

build_domain_index <- function(data, vars, treat_var = "treat") {
  out <- data
  for (v in vars) {
    out[[paste0("z_", v)]] <- z_from_control(out[[v]], out[[treat_var]])
  }
  z_vars <- paste0("z_", vars)
  out$domain_index <- rowMeans(out[, z_vars, drop = FALSE], na.rm = TRUE)
  out$domain_index[!is.finite(out$domain_index)] <- NA_real_
  out
}

discretion_index_by_mun <- analysis_data |>
  mutate(
    mun_psgc = as.character(mun_psgc),
    own_source_share = if_else(
      total_current_operating_income > 0,
      total_local_sources / total_current_operating_income,
      NA_real_
    ),
    tax_pc = if_else(pop07nso > 0, total_tax_revenue / pop07nso, NA_real_),
    cash_pc = if_else(pop07nso > 0, fund_cash_balance_end / pop07nso, NA_real_)
  ) |>
  filter(year < 2013) |>
  group_by(mun_psgc) |>
  summarise(
    own_source_share = mean(own_source_share, na.rm = TRUE),
    tax_pc = mean(tax_pc, na.rm = TRUE),
    cash_pc = mean(cash_pc, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    z_own_source_share = as.numeric(scale(own_source_share)),
    z_tax_pc = as.numeric(scale(tax_pc)),
    z_cash_pc = as.numeric(scale(cash_pc)),
    discretion_index = rowMeans(
      cbind(z_own_source_share, z_tax_pc, z_cash_pc),
      na.rm = TRUE
    ),
    discretion_index = if_else(is.nan(discretion_index), NA_real_, discretion_index)
  ) |>
  select(mun_psgc, discretion_index)

sct5o6_from_wave <- function(path) {
  read_dta(path) |>
    transmute(
      lk4,
      hh_id_psgc,
      sct5o6_raw = sanitize_special_missing(sct5o6)
    ) |>
    group_by(lk4, hh_id_psgc) |>
    summarise(
      sct5o6 = if_else(
        all(is.na(sct5o6_raw)),
        NA_real_,
        first(sct5o6_raw[!is.na(sct5o6_raw)])
      ),
      .groups = "drop"
    )
}

sct5o6_endline <- sct5o6_from_wave(
  "Data/KALAHI/Endline Public Use Data Package/Datasets/H4_Encoding_AnalysisVarsA.dta"
)
sct5o6_midline <- sct5o6_from_wave(
  "Data/KALAHI/Interim Public Use Data Package (3)/Data/Additional_Propositions_INT_A.dta"
)

infra <- read_dta("Data/KALAHI/Endline Public Use Data Package/Datasets/3rdRnd_Hypothesis_1a_AnalysisVarsA.dta") |>
  left_join(sct5o6_endline, by = c("lk4", "hh_id_psgc")) |>
  left_join(analysis_match, by = "lk4") |>
  select(!matches("\\.x|\\.y")) |>
  mutate(
    dist_pub_services = -rowSums(across(matches("^acs1_m"), sanitize_special_missing), na.rm = TRUE),
    travel_time = -rowSums(across(matches("^acs3"), sanitize_special_missing), na.rm = TRUE),
    travel_cost = -rowSums(across(matches("^acs4"), sanitize_acs4_cost), na.rm = TRUE),
    post_2013 = 1
  )

midline <- read_dta("Data/KALAHI/Interim Public Use Data Package (3)/Data/Interim_H1_AnalysisVars_INT.dta") |>
  left_join(sct5o6_midline, by = c("lk4", "hh_id_psgc")) |>
  left_join(analysis_match, by = "lk4") |>
  select(!matches("\\.x|\\.y")) |>
  mutate(
    dist_pub_services = -rowSums(across(matches("acs1_m"), sanitize_special_missing), na.rm = TRUE),
    travel_time = -rowSums(across(matches("acs3"), sanitize_special_missing), na.rm = TRUE),
    travel_cost = -rowSums(across(matches("acs4"), sanitize_acs4_cost), na.rm = TRUE),
    post_2013 = 0
  )

baseline_infra <- read_dta("Data/KALAHI/Interim Public Use Data Package (3)/Data/Baseline_outcome_vars_BASE_A.dta") |>
  left_join(analysis_match |> select(lk4, treat), by = "lk4") |>
  mutate(
    baseline_mode_close_n = rowSums(
      across(matches("^acs2_"), ~ as.numeric(!is.na(sanitize_special_missing(.x)))),
      na.rm = TRUE
    ),
    # Baseline acs2 is nominal transport mode. Treat walking/bicycle as a proximity proxy.
    baseline_mode_close = rowMeans(
      across(
        matches("^acs2_"),
        ~ case_when(
          sanitize_special_missing(.x) %in% c(12, 7) ~ 1,
          !is.na(sanitize_special_missing(.x)) ~ 0,
          TRUE ~ NA_real_
        )
      ),
      na.rm = TRUE
    ),
    baseline_mode_close = if_else(baseline_mode_close_n == 0, NA_real_, baseline_mode_close),
    baseline_travel_time_n = rowSums(across(matches("^acs3_"), ~ as.numeric(!is.na(sanitize_special_missing(.x))))),
    baseline_travel_time = -rowSums(across(matches("^acs3_"), sanitize_special_missing), na.rm = TRUE),
    baseline_travel_time = if_else(baseline_travel_time_n == 0, NA_real_, baseline_travel_time),
    baseline_travel_cost_dry_n = rowSums(across(matches("^acs4_[0-9]+$"), ~ as.numeric(!is.na(sanitize_special_missing(.x))))),
    baseline_travel_cost_dry = -rowSums(across(matches("^acs4_[0-9]+$"), sanitize_special_missing), na.rm = TRUE),
    baseline_travel_cost_dry = if_else(baseline_travel_cost_dry_n == 0, NA_real_, baseline_travel_cost_dry),
    baseline_travel_cost_wet_n = rowSums(across(matches("^acs4a_[0-9]+$"), ~ as.numeric(!is.na(sanitize_special_missing(.x))))),
    baseline_travel_cost_wet = -rowSums(across(matches("^acs4a_[0-9]+$"), sanitize_special_missing), na.rm = TRUE),
    baseline_travel_cost_wet = if_else(baseline_travel_cost_wet_n == 0, NA_real_, baseline_travel_cost_wet),
    baseline_mode_close = z_from_control(baseline_mode_close, treat),
    baseline_travel_time = z_from_control(baseline_travel_time, treat),
    baseline_travel_cost_dry = z_from_control(baseline_travel_cost_dry, treat),
    baseline_travel_cost_wet = z_from_control(baseline_travel_cost_wet, treat),
    pretreat_infra_access = baseline_travel_time,
    pretreat_welfare = rowMeans(
      across(c(
        baseline_mode_close,
        baseline_travel_time,
        baseline_travel_cost_dry,
        baseline_travel_cost_wet
      )),
      na.rm = TRUE
    ),
    pretreat_welfare = if_else(is.nan(pretreat_welfare), NA_real_, pretreat_welfare)
  ) |>
  transmute(lk4, hh_id_psgc, pretreat_infra_access, pretreat_welfare)

domain_infrastructure <- read_dta("Data/KALAHI/Endline Public Use Data Package/Datasets/3rdRnd_Hypothesis_1a_AnalysisVarsA.dta") |>
  left_join(analysis_match |> select(lk4, treat), by = "lk4") |>
  mutate(
    dist_pub_services = -rowSums(across(matches("^acs1_m")), na.rm = TRUE),
    travel_time = -rowSums(across(matches("^acs3")), na.rm = TRUE)
  ) |>
  build_domain_index(c("dist_pub_services", "travel_time")) |>
  transmute(lk4, domain_infrastructure = domain_index)

domain_water_access <- read_dta("Data/KALAHI/Endline Public Use Data Package/Datasets/3rdRnd_Hypothesis_1h_AnalysisVarsA.dta") |>
  left_join(analysis_match |> select(lk4, treat), by = "lk4") |>
  mutate(
    across(starts_with("ws"), sanitize_special_missing),
    amnt_paid = -(ws12_r + ws12_d),
    time_spent = -(ws8_r + ws8_d),
    num_trips = -(ws9_dt + ws9_rt)
  ) |>
  build_domain_index(c("amnt_paid", "time_spent", "num_trips")) |>
  transmute(lk4, domain_water_access = domain_index)

domain_labor_income <- read_dta("Data/KALAHI/Endline Public Use Data Package/Datasets/3rdRnd_LT2_Labor_HH-AnalysisVarsB.dta") |>
  left_join(analysis_match |> select(lk4, treat), by = "lk4") |>
  mutate(
    income = sanitize_special_missing(lm9)
  ) |>
  build_domain_index("income") |>
  transmute(lk4, domain_labor_income = domain_index)

domain_household_spending <- read_dta("Data/KALAHI/Endline Public Use Data Package/Datasets/3rdRnd_LT1_AnalysisVarsB.dta") |>
  left_join(analysis_match |> select(lk4, treat), by = "lk4") |>
  mutate(
    across(everything(), sanitize_special_missing),
    hh_spending = rowSums(across(matches("1$")), na.rm = TRUE),
    hh_produce = rowSums(across(matches("2$")), na.rm = TRUE)
  ) |>
  build_domain_index(c("hh_spending", "hh_produce")) |>
  transmute(lk4, domain_household_spending = domain_index)

domain_education <- read_dta("Data/KALAHI/Endline Public Use Data Package/Datasets/3rdRnd_Hypothesis_1e_AnalysisVarsB.dta") |>
  left_join(analysis_match |> select(lk4, treat), by = "lk4") |>
  mutate(
    num_teachers = rowSums(across(matches("teacher")), na.rm = TRUE),
    num_schools = rowSums(across(any_of(c("edf_public_elem", "edf_public_sec"))), na.rm = TRUE),
    enrollment = rowSums(across(matches("enrolled")), na.rm = TRUE)
  ) |>
  build_domain_index(c("num_teachers", "num_schools", "enrollment")) |>
  transmute(lk4, domain_education = domain_index)

domain_hardship_support <- read_dta("Data/KALAHI/Endline Public Use Data Package/Datasets/3rdRnd_H8_Hardship_HH_AnalysisVarsB.dta") |>
  left_join(analysis_match |> select(lk4, treat), by = "lk4") |>
  mutate(
    across(starts_with("de"), sanitize_special_missing),
    assistance = rowSums(
      across(any_of(c(
        "de6_death_finass_a",
        "de6_ill_finass_a",
        "de6_job_finass_a",
        "de6_calam_finass_a",
        "de6_noharv_finass_a",
        "de6_van_finass_a"
      ))),
      na.rm = TRUE
    ),
    asked_lgu = if_else(
      coalesce(de4_death_helpLguOff, 0) == 1 |
        coalesce(de4_illness_helpLguOff, 0) == 1 |
        coalesce(de4_jobless_helpLguOff, 0) == 1 |
        coalesce(de4_calamity_helpLguOff, 0) == 1 |
        coalesce(de4_noharvest_helpLguOff, 0) == 1 |
        coalesce(de4_vandal_helpLguOff, 0) == 1,
      1,
      0
    )
  ) |>
  build_domain_index(c("assistance", "asked_lgu")) |>
  transmute(lk4, domain_hardship_support = domain_index)

domain_indices <- domain_infrastructure |>
  full_join(domain_water_access, by = "lk4") |>
  full_join(domain_labor_income, by = "lk4") |>
  full_join(domain_household_spending, by = "lk4") |>
  full_join(domain_education, by = "lk4") |>
  full_join(domain_hardship_support, by = "lk4") |>
  mutate(
    overall_welfare_index = rowMeans(
      across(starts_with("domain_")),
      na.rm = TRUE
    ),
    overall_welfare_index = if_else(is.nan(overall_welfare_index), NA_real_, overall_welfare_index)
  )

welfare_data <- bind_rows(infra, midline)
if (!("province" %in% names(welfare_data))) {
  welfare_data <- welfare_data |> mutate(province = NA_character_)
}
if (!("prov_psgc" %in% names(welfare_data))) {
  welfare_data <- welfare_data |> mutate(prov_psgc = NA_character_)
}
if (!("reg_psgc" %in% names(welfare_data))) {
  welfare_data <- welfare_data |> mutate(reg_psgc = NA_character_)
}
welfare_data <- welfare_data |>
  # Midline raw files carry mun_psgc while analysis_match also carries it;
  # after join/cleanup, geography can be dropped for midline rows. Re-attach via lk4.
  left_join(
    analysis_match |>
      select(lk4, mun_psgc, region, mun),
    by = "lk4",
    suffix = c("", "_am")
  ) |>
  mutate(
    mun_psgc = coalesce(mun_psgc, mun_psgc_am),
    region = coalesce(region, region_am),
    mun = coalesce(mun, mun_am)
  ) |>
  select(-mun_psgc_am, -region_am, -mun_am) |>
  left_join(psgc_muni_lookup, by = "mun_psgc") |>
  mutate(
    prov_psgc = coalesce(as.character(prov_psgc), as.character(prov_psgc_from_psgc)),
    reg_psgc = coalesce(as.character(reg_psgc), as.character(reg_psgc_from_psgc)),
    province = coalesce(province, province_from_psgc),
    mun = coalesce(mun, mun_from_psgc),
    region = coalesce(region, region_from_psgc)
  ) |>
  select(
    any_of(c("lk4", "region", "province", "mun", "mun_psgc", "prov_psgc", "reg_psgc")),
    everything(),
    -mun_from_psgc,
    -prov_psgc_from_psgc,
    -reg_psgc_from_psgc,
    -province_from_psgc,
    -region_from_psgc
  ) |>
  mutate(mun_psgc = as.character(mun_psgc)) |>
  left_join(baseline_infra, by = c("lk4", "hh_id_psgc")) |>
  left_join(discretion_index_by_mun, by = "mun_psgc") |>
  left_join(domain_indices, by = "lk4")

welfare_data <- welfare_data |>
  mutate(
    # Build infra_access from infrastructure outcomes common to both waves using
    # control-group standardization, matching domain-index construction logic.
    # Exclude travel-cost components.
    dist_pub_services = z_from_control(dist_pub_services, treat),
    travel_time = z_from_control(travel_time, treat)
  )

welfare_data <- welfare_data |>
  mutate(across(any_of("sct5o6"), sanitize_special_missing))

if ("sct5o6" %in% names(welfare_data)) {
  welfare_data <- welfare_data |>
    mutate(infra_access_sct5o6 = z_from_control(sct5o6, treat))
}

in_both_waves <- function(x, wave) {
  has_by_wave <- tapply(!is.na(x), wave, any)
  length(has_by_wave) >= 2 && all(has_by_wave)
}

infra_access_item_cols <- c(
  if (in_both_waves(welfare_data$dist_pub_services, welfare_data$post_2013)) "dist_pub_services",
  if (in_both_waves(welfare_data$travel_time, welfare_data$post_2013)) "travel_time",
  if ("infra_access_sct5o6" %in% names(welfare_data) && in_both_waves(welfare_data$infra_access_sct5o6, welfare_data$post_2013)) "infra_access_sct5o6"
)

welfare_data <- welfare_data |>
  mutate(
    infra_access = rowMeans(across(all_of(infra_access_item_cols)), na.rm = TRUE),
    infra_access = if_else(is.nan(infra_access), NA_real_, infra_access)
  ) |>
  select(
    -any_of("infra_access_sct5o6")
  ) |>
  mutate(
    across(
      any_of(c(
        "discretion_index",
        "domain_infrastructure",
        "domain_water_access",
        "domain_labor_income",
        "domain_household_spending",
        "domain_education",
        "domain_hardship_support",
        "overall_welfare_index"
      )),
      as.numeric
    )
  )

write_csv(welfare_data, file.path(out_dir, "welfare_data.csv"))

lfs_region_from_code <- function(code) {
  code2 <- str_pad(str_trim(code), width = 2, side = "left", pad = "0")
  case_when(
    code2 == "01" ~ "I",
    code2 == "02" ~ "II",
    code2 == "03" ~ "III",
    code2 == "04" ~ "IV-A",
    code2 == "05" ~ "V",
    code2 == "06" ~ "VI",
    code2 == "07" ~ "VII",
    code2 == "08" ~ "VIII",
    code2 == "09" ~ "IX",
    code2 == "10" ~ "X",
    code2 == "11" ~ "XI",
    code2 == "12" ~ "XII",
    code2 == "13" ~ "NCR",
    code2 == "14" ~ "CAR",
    code2 == "15" ~ "ARMM",
    code2 == "16" ~ "XIII",
    code2 == "17" ~ "IV-B",
    TRUE ~ NA_character_
  )
}

read_lfs_file <- function(path) {
  if (str_detect(path, "\\.zip$")) {
    zip_contents <- utils::unzip(path, list = TRUE)$Name
    csv_name <- zip_contents[str_detect(zip_contents, regex("\\.csv$", ignore_case = TRUE))][1]
    if (is.na(csv_name)) return(tibble())
    raw <- read_csv(unz(path, csv_name), col_types = cols(.default = col_character()), show_col_types = FALSE)
  } else {
    raw <- read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
  }

  raw |>
    mutate(source_file = basename(path))
}

coalesce_existing_chr <- function(data, cols) {
  existing <- intersect(cols, names(data))
  if (length(existing) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  out <- data[[existing[1]]]
  if (length(existing) > 1) {
    for (nm in existing[-1]) {
      out <- dplyr::coalesce(out, data[[nm]])
    }
  }
  out
}

lfs_raw <- list.files("Data/Labor Force Surveys", full.names = TRUE, recursive = TRUE) |>
  discard(dir.exists) |>
  keep(~ str_detect(.x, regex("\\.(csv|zip)$", ignore_case = TRUE))) |>
  map_df(read_lfs_file) |>
  mutate(
    mun_name_raw = coalesce_existing_chr(
      cur_data(),
      c("MUN", "MUNICITY", "MUNICIPALITY", "CITYMUN", "CITY_MUN", "PSU_NAME")
    )
  ) |>
  mutate(
    region_code = str_pad(
      coalesce_existing_chr(cur_data(), c("CREG", "REG")),
      width = 2,
      side = "left",
      pad = "0"
    ),
    psu_code = str_pad(
      str_extract(coalesce_existing_chr(cur_data(), c("PSU")), "\\d+"),
      width = 5,
      side = "left",
      pad = "0"
    ),
    mun_psgc_raw = str_sub(paste0(region_code, psu_code), 1, 6),
    region = lfs_region_from_code(region_code) |>
      canonical_region(),
    mun_name = canonical_mun(mun_name_raw),
    quarter = case_when(
      SVYMO == "01" ~ "Q1",
      SVYMO == "04" ~ "Q2",
      SVYMO == "07" ~ "Q3",
      SVYMO == "10" ~ "Q4",
      TRUE ~ NA_character_
    ),
    year = suppressWarnings(as.integer(SVYYR)),
    quarter = paste0(quarter, SVYYR),
    w = suppressWarnings(as.numeric(PWGT)),
    w = ifelse(is.na(w) | w <= 0, 1, w),
    emp = case_when(C13_WORK == "1" ~ 1, C13_WORK == "2" ~ 0, TRUE ~ NA_real_),
    gov_sector = case_when(
      C19PCLAS == "2" | J04_OCLS == "2" ~ 1,
      C19PCLAS %in% c("1", "3", "4", "5", "6") | J04_OCLS %in% c("1", "3", "4", "5", "6") ~ 0,
      TRUE ~ NA_real_
    ),
    # LFS schema shifts in 2012: C20_NATEM/C22_PHOURS are used instead of C20_NTEM/C22_PHRS
    short_term_raw = coalesce(C20_NTEM, C20_NATEM),
    short_term = case_when(short_term_raw == "2" ~ 1, short_term_raw == "1" ~ 0, TRUE ~ NA_real_),
    work_hrs_last_wk = suppressWarnings(as.numeric(coalesce(C22_PHRS, C22_PHOURS)))
  ) |>
  filter(!is.na(region), !is.na(quarter), !is.na(year), dplyr::between(year, 2013L, 2015L))

lfs_controls_region_q <- lfs_raw |>
  group_by(region, quarter) |>
  summarise(
    emp_rate = ifelse(all(is.na(emp)), NA_real_, weighted.mean(emp, w, na.rm = TRUE)),
    short_term_share = ifelse(all(is.na(short_term)), NA_real_, weighted.mean(short_term, w, na.rm = TRUE)),
    avg_work_hrs = ifelse(all(is.na(work_hrs_last_wk)), NA_real_, weighted.mean(work_hrs_last_wk, w, na.rm = TRUE)),
    lfs_n = n(),
    .groups = "drop"
  )

analysis_lfs_covars <- analysis_data |>
  transmute(
    lgu_id,
    region,
    province,
    mun,
    mun_psgc = str_pad(as.character(mun_psgc), width = 6, side = "left", pad = "0"),
    pairnum,
    quarter,
    treat,
    pi,
    bgytot,
    pop07nso,
    land,
    yolanda,
    competition_c,
    competition_z
  ) |>
  distinct()

lfs_match <- matching |>
  transmute(
    region,
    mun,
    mun_psgc = str_pad(as.character(mun_psgc), width = 6, side = "left", pad = "0")
  ) |>
  distinct()

lfs_individual <- lfs_raw |>
  left_join(
    lfs_match |>
      rename(mun_psgc_match = mun_psgc, mun_match = mun),
    by = c("region", "mun_psgc_raw" = "mun_psgc_match")
  ) |>
  left_join(
    lfs_match |>
      rename(mun_name_match = mun, mun_psgc_name = mun_psgc),
    by = c("region", "mun_name" = "mun_name_match")
  ) |>
  mutate(
    mun = coalesce(mun_match, mun_name),
    mun_psgc = coalesce(
      if_else(!is.na(mun_match), mun_psgc_raw, NA_character_),
      mun_psgc_name
    )
  ) |>
  filter(!is.na(mun), !is.na(mun_psgc)) |>
  transmute(
    region,
    mun,
    mun_psgc = str_pad(as.character(mun_psgc), width = 6, side = "left", pad = "0"),
    year,
    quarter,
    C06_SEX,
    C07_AGE,
    gov_sector,
    short_term,
    work_hrs_last_wk
  )

lfs_data <- analysis_lfs_covars |>
  right_join(
    lfs_individual,
    by = c("region", "mun_psgc", "quarter")
  ) |>
  transmute(
    lgu_id,
    region,
    province,
    mun = coalesce(mun.x, mun.y),
    mun_psgc = str_pad(as.character(mun_psgc), width = 6, side = "left", pad = "0"),
    pairnum,
    year,
    quarter,
    treat,
    pi,
    bgytot,
    pop07nso,
    land,
    yolanda,
    competition_c,
    competition_z,
    C06_SEX,
    C07_AGE,
    gov_sector,
    short_term,
    work_hrs_last_wk
  )

write_csv(lfs_data, file.path(out_dir, "lfs_data.csv"))
write_csv(lfs_controls_region_q, file.path(out_dir, "lfs_controls_region_quarter.csv"))
write_csv(treated_audit, file.path(out_dir, "treated_municipality_match_audit.csv"))

analysis_roster <- analysis_data |>
  transmute(
    lgu_id,
    region,
    province,
    mun,
    mun_psgc = str_pad(as.character(mun_psgc), width = 6, side = "left", pad = "0"),
    pairnum
  ) |>
  distinct()

fiscal_pre <- historic_data |>
  filter(year < 2012) |>
  transmute(
    lgu_id,
    year,
    discretionary_share = if_else(
      total_current_operating_expenditures > 0,
      (general_public_services + economic_services - total_debt_service_principal_cost - debt_service_interest_expense_other_charges) / total_current_operating_expenditures,
      NA_real_
    ),
    ira_dependence = if_else(
      total_local_sources + internal_revenue_allotment + total_external_sources > 0,
      1 - total_local_sources / (total_local_sources + internal_revenue_allotment + total_external_sources),
      NA_real_
    ),
    cash_accumulation = if_else(
      total_current_operating_income > 0,
      fund_cash_balance_end / total_current_operating_income,
      NA_real_
    ),
    debt_share = if_else(
      total_current_operating_expenditures > 0,
      (total_debt_service_principal_cost + debt_service_interest_expense_other_charges) / total_current_operating_expenditures,
      NA_real_
    )
  )

fiscal_pre_avg <- fiscal_pre |>
  group_by(lgu_id) |>
  summarise(
    n_pre_years_fiscal = n_distinct(year),
    discretionary_share = mean_or_na(discretionary_share),
    ira_dependence = mean_or_na(ira_dependence),
    cash_accumulation = mean_or_na(cash_accumulation),
    debt_share = mean_or_na(debt_share),
    .groups = "drop"
  )

lfs_pre_files <- list.files("Data/Labor Force Surveys", full.names = TRUE, recursive = TRUE) |>
  discard(dir.exists) |>
  keep(~ str_detect(.x, regex("\\.(csv|zip)$", ignore_case = TRUE))) |>
  keep(~ str_detect(.x, "2009|2010|2011"))

lfs_pre_raw <- map_df(lfs_pre_files, read_lfs_file) |>
  mutate(
    region_code = str_pad(
      coalesce_existing_chr(cur_data(), c("CREG", "REG")),
      width = 2,
      side = "left",
      pad = "0"
    ),
    psu_code = str_pad(
      str_extract(coalesce_existing_chr(cur_data(), c("PSU")), "\\d+"),
      width = 5,
      side = "left",
      pad = "0"
    ),
    mun_psgc = str_sub(paste0(region_code, psu_code), 1, 6),
    region = lfs_region_from_code(region_code) |>
      canonical_region(),
    year = suppressWarnings(as.integer(SVYYR)),
    quarter = case_when(
      SVYMO == "01" ~ "Q1",
      SVYMO == "04" ~ "Q2",
      SVYMO == "07" ~ "Q3",
      SVYMO == "10" ~ "Q4",
      TRUE ~ NA_character_
    ),
    w = suppressWarnings(as.numeric(coalesce_existing_chr(cur_data(), c("PWGT", "FWGT")))),
    w = ifelse(is.na(w) | w <= 0, 1, w),
    gov_sector = case_when(
      C19PCLAS == "2" | J04_OCLS == "2" ~ 1,
      C19PCLAS %in% c("1", "3", "4", "5", "6") | J04_OCLS %in% c("1", "3", "4", "5", "6") ~ 0,
      TRUE ~ NA_real_
    ),
    short_term = case_when(C20_NTEM == "2" ~ 1, C20_NTEM == "1" ~ 0, TRUE ~ NA_real_),
    work_hrs_last_wk = suppressWarnings(as.numeric(C22_PHRS))
  ) |>
  filter(!is.na(region), !is.na(mun_psgc), !is.na(year), year <= 2011)

lfs_pre_avg <- lfs_pre_raw |>
  inner_join(
    analysis_roster |>
      select(lgu_id, region, mun_psgc),
    by = c("region", "mun_psgc")
  ) |>
  group_by(lgu_id) |>
  summarise(
    n_pre_years_lfs = n_distinct(year),
    n_pre_quarters_lfs = n_distinct(str_c(year, quarter)),
    lfs_obs = n(),
    government_employment_share = weighted_mean_or_na(gov_sector, w),
    .groups = "drop"
  )

discretion_indices <- analysis_roster |>
  left_join(fiscal_pre_avg, by = "lgu_id") |>
  left_join(lfs_pre_avg, by = "lgu_id") |>
  mutate(
    discretionary_share_w = winsorize_vec(discretionary_share),
    ira_dependence_w = winsorize_vec(ira_dependence),
    cash_accumulation_w = winsorize_vec(cash_accumulation),
    government_employment_share_w = winsorize_vec(government_employment_share),
    debt_share_w = winsorize_vec(debt_share),
    discretionary_share_z = zscore_vec(discretionary_share_w),
    ira_dependence_z = zscore_vec(ira_dependence_w),
    cash_accumulation_z = zscore_vec(cash_accumulation_w),
    government_employment_share_z = zscore_vec(government_employment_share_w),
    debt_share_z = zscore_vec(debt_share_w),
    fiscal_slack_index_raw = if_else(
      !is.na(discretionary_share_z) & !is.na(ira_dependence_z) & !is.na(cash_accumulation_z),
      (discretionary_share_z + ira_dependence_z + cash_accumulation_z) / 3,
      NA_real_
    ),
    structural_commitment_index_raw = if_else(
      !is.na(government_employment_share_z) & !is.na(debt_share_z),
      (government_employment_share_z + debt_share_z) / 2,
      NA_real_
    ),
    fiscal_slack_index = zscore_vec(fiscal_slack_index_raw),
    structural_commitment_index = zscore_vec(structural_commitment_index_raw),
    fewer_than_three_pre_years_fiscal = coalesce(n_pre_years_fiscal, 0) < 3,
    fewer_than_three_pre_years_lfs = coalesce(n_pre_years_lfs, 0) < 3
  ) |>
  select(
    lgu_id,
    region,
    province,
    mun,
    mun_psgc,
    pairnum,
    fiscal_slack_index,
    structural_commitment_index,
    discretionary_share_z,
    ira_dependence_z,
    cash_accumulation_z,
    government_employment_share_z,
    debt_share_z,
    n_pre_years_fiscal,
    n_pre_years_lfs,
    n_pre_quarters_lfs,
    lfs_obs,
    fewer_than_three_pre_years_fiscal,
    fewer_than_three_pre_years_lfs
  )

discretion_summary <- discretion_indices |>
  select(
    fiscal_slack_index,
    structural_commitment_index,
    discretionary_share_z,
    ira_dependence_z,
    cash_accumulation_z,
    government_employment_share_z,
    debt_share_z
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    n_non_missing = sum(!is.na(value)),
    .groups = "drop"
  )

discretion_cor <- discretion_indices |>
  select(
    fiscal_slack_index,
    structural_commitment_index,
    discretionary_share_z,
    ira_dependence_z,
    cash_accumulation_z,
    government_employment_share_z,
    debt_share_z
  ) |>
  cor(use = "pairwise.complete.obs")

analysis_data <- analysis_data |>
  left_join(
    discretion_indices |>
      select(
        -region,
        -province,
        -mun,
        -mun_psgc,
        -pairnum
      ),
    by = "lgu_id"
  )

message("Discretion summary:")
print(discretion_summary)
message("Discretion correlations:")
print(round(discretion_cor, 3))
message("Municipalities with <3 fiscal pre-treatment years: ", sum(discretion_indices$fewer_than_three_pre_years_fiscal, na.rm = TRUE))
message("Municipalities with <3 LFS pre-treatment years: ", sum(discretion_indices$fewer_than_three_pre_years_lfs, na.rm = TRUE))
message("Non-missing fiscal_slack_index: ", sum(!is.na(discretion_indices$fiscal_slack_index)))
message("Non-missing structural_commitment_index: ", sum(!is.na(discretion_indices$structural_commitment_index)))
message("Analysis merge rate by lgu_id: ", round(mean(analysis_data$lgu_id %in% discretion_indices$lgu_id) * 100, 1), "%")

write_csv(discretion_indices, file.path(out_dir, "discretion_indices.csv"))
write_csv(analysis_data, file.path(out_dir, "analysis_data.csv"))
write_csv(historic_data, file.path(out_dir, "historic_data.csv"))

canonical_region_match <- function(x) {
  y <- x |>
    str_to_upper() |>
    str_replace_all("\\([^\\)]*\\)", " ") |>
    str_replace_all("^REGION\\s+", "") |>
    str_replace_all("\\s+REGION$", "") |>
    str_squish()

  case_when(
    str_detect(y, "MIMAROPA|IV-B") ~ "IV-B",
    str_detect(y, "CALABARZON|IV-A") ~ "IV-A",
    str_detect(y, "ILOCOS|\\bI\\b") ~ "I",
    str_detect(y, "CAGAYAN VALLEY|\\bII\\b") ~ "II",
    str_detect(y, "CENTRAL LUZON|\\bIII\\b") ~ "III",
    str_detect(y, "CARAGA|XIII") ~ "XIII",
    str_detect(y, "CORDILLERA|\\bCAR\\b") ~ "CAR",
    str_detect(y, "NATIONAL CAPITAL|\\bNCR\\b") ~ "NCR",
    str_detect(y, "BICOL|\\bV\\b") ~ "V",
    str_detect(y, "WESTERN VISAYAS|\\bVI\\b") ~ "VI",
    str_detect(y, "CENTRAL VISAYAS|\\bVII\\b") ~ "VII",
    str_detect(y, "EASTERN VISAYAS|\\bVIII\\b") ~ "VIII",
    str_detect(y, "ZAMBOANGA|\\bIX\\b") ~ "IX",
    str_detect(y, "NORTHERN MINDANAO|\\bX\\b") ~ "X",
    str_detect(y, "DAVAO|\\bXI\\b") ~ "XI",
    str_detect(y, "SOCCSKSARGEN|\\bXII\\b") ~ "XII",
    str_detect(y, "ARMM|BARMM") ~ "BARMM",
    TRUE ~ y
  )
}

canonical_geo_text <- function(x) {
  x |>
    str_to_upper() |>
    str_replace_all("&", " AND ") |>
    str_replace_all("\\([^\\)]*\\)", " ") |>
    str_replace_all(
      "\\b(CITY OF|MUNICIPALITY OF|CITY|MUNICIPALITY|PROVINCE OF|PROVINCE|MUN\\.?|PROV\\.?|DISTRICT)\\b",
      " "
    ) |>
    str_replace_all("[^A-Z0-9]", "") |>
    str_squish()
}

canonical_mun_match <- function(x) {
  x |>
    str_replace("R\\. T\\. Lim", "Roseller Lim") |>
    str_replace("Brooke?s\\s*Point", "Brooke's Point") |>
    canonical_mun() |>
    canonical_geo_text()
}

canonical_province_match <- function(x) {
  x |>
    canonical_province() |>
    canonical_geo_text()
}

procurement_cols <- c(
  "source_year", "region", "province", "city_municipality",
  "area_of_delivery", "procuring_entity", "client_agency",
  "notice_title", "item_description",
  "award_reference_no", "reference_id", "bid_reference_no", "contract_no"
)

procurement_raw <- read_csv(
  "Data/municipal_procurement copy.csv",
  show_col_types = FALSE,
  col_select = any_of(procurement_cols),
  progress = FALSE
)

ie_proc_base <- matching_raw |>
  transmute(
    mun_psgc = str_pad(mun_psgc, width = 6, side = "left", pad = "0"),
    mun_ie = mun
  ) |>
  left_join(
    read_csv("Data/philippines_barangays.csv", show_col_types = FALSE) |>
      transmute(
        mun_psgc = str_pad(city_mun_code, width = 6, side = "left", pad = "0"),
        region_psgc = region_name,
        province_psgc = prov_name,
        mun_psgc_name = city_mun_name,
        barangay = brgy_name
      ) |>
      distinct(),
    by = "mun_psgc"
  )

ie_lookup <- bind_rows(
  ie_proc_base |>
    transmute(mun_psgc, region = region_psgc, province = province_psgc, mun = mun_psgc_name),
  ie_proc_base |>
    transmute(mun_psgc, region = region_psgc, province = province_psgc, mun = mun_ie)
) |>
  filter(!is.na(mun_psgc), !is.na(region), !is.na(province), !is.na(mun)) |>
  mutate(
    region_key = canonical_region_match(region) |> canonical_geo_text(),
    province_key = canonical_province_match(province),
    mun_key = canonical_mun_match(mun)
  ) |>
  distinct(mun_psgc, region_key, province_key, mun_key)

ie_lookup_rpm <- ie_lookup |>
  group_by(region_key, province_key, mun_key) |>
  mutate(n_match = n_distinct(mun_psgc)) |>
  ungroup() |>
  filter(n_match == 1) |>
  distinct(region_key, province_key, mun_key, mun_psgc)

ie_lookup_pm <- ie_lookup |>
  group_by(province_key, mun_key) |>
  mutate(n_match = n_distinct(mun_psgc)) |>
  ungroup() |>
  filter(n_match == 1) |>
  distinct(province_key, mun_key, mun_psgc)

procurement_matched <- procurement_raw |>
  mutate(
    source_year = suppressWarnings(as.integer(source_year)),
    region_key = canonical_region_match(region) |> canonical_geo_text(),
    province_key = canonical_province_match(province),
    mun_key = canonical_mun_match(city_municipality),
    proc_row_id = row_number()
  ) |>
  filter(source_year >= 2012, source_year <= 2015) |>
  left_join(
    ie_lookup_rpm |>
      rename(mun_psgc_rpm = mun_psgc),
    by = c("region_key", "province_key", "mun_key")
  ) |>
  left_join(
    ie_lookup_pm |>
      rename(mun_psgc_pm = mun_psgc),
    by = c("province_key", "mun_key")
  ) |>
  mutate(
    mun_psgc = coalesce(mun_psgc_rpm, mun_psgc_pm),
    match_level = case_when(
      !is.na(mun_psgc_rpm) ~ "region_province_mun",
      is.na(mun_psgc_rpm) & !is.na(mun_psgc_pm) ~ "province_mun_fallback",
      TRUE ~ "unmatched"
    )
  )

barangay_dict <- ie_proc_base |>
  filter(!is.na(barangay)) |>
  transmute(mun_psgc, barangay_key = canonical_geo_text(barangay)) |>
  filter(barangay_key != "") |>
  distinct()

matched_rows <- procurement_matched |>
  filter(!is.na(mun_psgc)) |>
  mutate(
    text_blob = str_c(
      coalesce(area_of_delivery, ""), " ",
      coalesce(procuring_entity, ""), " ",
      coalesce(client_agency, ""), " ",
      coalesce(notice_title, ""), " ",
      coalesce(item_description, "")
    ),
    text_blob_key = canonical_geo_text(text_blob)
  )

matched_rows_with_hits <- matched_rows |>
  group_by(mun_psgc) |>
  group_modify(~ {
    brgys <- barangay_dict |>
      filter(mun_psgc == .y$mun_psgc) |>
      pull(barangay_key) |>
      unique()

    if (length(brgys) == 0) {
      .x |>
        mutate(has_barangay_name_any_field = FALSE)
    } else {
      pat <- str_c("\\b(", str_c(str_replace_all(brgys, "([\\W])", "\\\\\\1"), collapse = "|"), ")\\b")
      .x |>
        mutate(has_barangay_name_any_field = str_detect(text_blob_key, regex(pat)))
    }
  }) |>
  ungroup()

contract_counts <- matched_rows_with_hits |>
  mutate(
    contract_id = coalesce(award_reference_no, reference_id, bid_reference_no, contract_no, str_c("ROW_", proc_row_id)),
    contract_id = ifelse(contract_id == "", str_c("ROW_", proc_row_id), contract_id),
    abc_value = suppressWarnings(as.numeric(approved_budget_of_the_contract))
  ) |>
  group_by(mun_psgc, year = source_year, contract_id) |>
  summarise(
    has_barangay_name_any_field = as.integer(any(has_barangay_name_any_field, na.rm = TRUE)),
    abc_value = ifelse(any(!is.na(abc_value)), abc_value[which(!is.na(abc_value))[1]], NA_real_),
    .groups = "drop"
  ) |>
  group_by(mun_psgc, year) |>
  summarise(
    contracts_total = n(),
    contracts_with_barangay_name_any_field = sum(has_barangay_name_any_field, na.rm = TRUE),
    abc_total = sum(abc_value, na.rm = TRUE),
    abc_barangay = sum(ifelse(has_barangay_name_any_field == 1, abc_value, 0), na.rm = TRUE),
    .groups = "drop"
  )

ie_mun_year_frame <- ie_proc_base |>
  distinct(mun_psgc) |>
  crossing(year = 2012:2015)

contract_counts_full <- ie_mun_year_frame |>
  left_join(contract_counts, by = c("mun_psgc", "year")) |>
  mutate(
    contracts_total = replace_na(contracts_total, 0L),
    contracts_with_barangay_name_any_field = replace_na(contracts_with_barangay_name_any_field, 0L),
    abc_total = replace_na(abc_total, 0),
    abc_barangay = replace_na(abc_barangay, 0)
  )

coverage_by_mun <- ie_proc_base |>
  distinct(mun_psgc, region_psgc, province_psgc, mun_psgc_name) |>
  left_join(
    contract_counts_full |>
      group_by(mun_psgc) |>
      summarise(
        has_any_contract_2012_2015 = as.integer(any(contracts_total > 0)),
        has_any_barangay_name_any_field_2012_2015 = as.integer(any(contracts_with_barangay_name_any_field > 0)),
        .groups = "drop"
      ),
    by = "mun_psgc"
  ) |>
  mutate(
    has_any_contract_2012_2015 = replace_na(has_any_contract_2012_2015, 0L),
    has_any_barangay_name_any_field_2012_2015 = replace_na(has_any_barangay_name_any_field_2012_2015, 0L)
  ) |>
  rename(
    province = province_psgc,
    municipality = mun_psgc_name
  ) |>
  mutate(region = canonical_region_match(region_psgc)) |>
  select(-region_psgc) |>
  arrange(mun_psgc)

unmatched_ie_procurement <- coverage_by_mun |>
  filter(has_any_contract_2012_2015 == 0)

procurement_analysis_matched <- contract_counts_full |>
  left_join(
    coverage_by_mun |>
      select(
        mun_psgc,
        region,
        province,
        municipality,
        has_any_contract_2012_2015,
        has_any_barangay_name_any_field_2012_2015
      ),
    by = "mun_psgc"
  ) |>
  mutate(
    barangay_contract_share = ifelse(
      contracts_total > 0,
      contracts_with_barangay_name_any_field / contracts_total,
      0
    ),
    barangay_value_share = ifelse(abc_total > 0, abc_barangay / abc_total, 0)
  ) |>
  select(
    mun_psgc, region, province, municipality, year,
    contracts_total, contracts_with_barangay_name_any_field, barangay_contract_share,
    abc_total, abc_barangay, barangay_value_share,
    has_any_contract_2012_2015, has_any_barangay_name_any_field_2012_2015
  ) |>
  arrange(mun_psgc, year)

proc_controls_muny <- analysis_data |>
  mutate(
    mun_psgc = str_pad(mun_psgc, width = 6, side = "left", pad = "0"),
    year = as.integer(panel_year),
    q = as.integer(str_sub(quarter, 2, 2)),
    yearq = year * 10 + q,
    ira_pc = internal_revenue_allotment / pop07nso,
    oper_income_pc = total_current_operating_income / pop07nso,
    cash_balance_pc = fund_cash_balance_end / pop07nso,
    debt_service_pc = debt_service_interest_expense_other_charges / pop07nso,
    local_sources_pc = total_local_sources / pop07nso,
    external_sources_pc = total_external_sources / pop07nso
  ) |>
  arrange(mun_psgc, yearq) |>
  group_by(mun_psgc) |>
  mutate(
    l1_ira_pc = lag(ira_pc, 1),
    l1_oper_income_pc = lag(oper_income_pc, 1),
    l1_cash_balance_pc = lag(cash_balance_pc, 1),
    l1_debt_service_pc = lag(debt_service_pc, 1),
    l1_local_sources_pc = lag(local_sources_pc, 1),
    l1_external_sources_pc = lag(external_sources_pc, 1)
  ) |>
  ungroup() |>
  filter(year >= 2012, year <= 2015) |>
  group_by(mun_psgc, year) |>
  summarise(
    pairnum = first(na.omit(pairnum)),
    treat = first(na.omit(treat)),
    vote_diff = mean(vote_diff, na.rm = TRUE),
    competition_c = mean(competition_c, na.rm = TRUE),
    competition_z = mean(competition_z, na.rm = TRUE),
    yolanda = mean(yolanda, na.rm = TRUE),
    border_treat = mean(border_treat, na.rm = TRUE),
    dynasty_next_mayor = mean(dynasty_next_mayor, na.rm = TRUE),
    pop07nso = mean(pop07nso, na.rm = TRUE),
    pi = mean(pi, na.rm = TRUE),
    land = mean(land, na.rm = TRUE),
    bgytot = mean(bgytot, na.rm = TRUE),
    l1_ira_pc = mean(l1_ira_pc, na.rm = TRUE),
    l1_oper_income_pc = mean(l1_oper_income_pc, na.rm = TRUE),
    l1_cash_balance_pc = mean(l1_cash_balance_pc, na.rm = TRUE),
    l1_debt_service_pc = mean(l1_debt_service_pc, na.rm = TRUE),
    l1_local_sources_pc = mean(l1_local_sources_pc, na.rm = TRUE),
    l1_external_sources_pc = mean(l1_external_sources_pc, na.rm = TRUE),
    .groups = "drop"
  )

proc_pair_lookup <- matching |>
  transmute(
    mun_psgc = str_pad(as.character(mun_psgc), width = 6, side = "left", pad = "0"),
    pairnum
  ) |>
  distinct()

procurement_analysis_matched <- procurement_analysis_matched |>
  mutate(
    mun_psgc = str_pad(mun_psgc, width = 6, side = "left", pad = "0"),
    year = as.integer(year)
  ) |>
  left_join(proc_controls_muny, by = c("mun_psgc", "year")) |>
  left_join(proc_pair_lookup, by = "mun_psgc", suffix = c("", "_pair")) |>
  mutate(pairnum = coalesce(pairnum, pairnum_pair)) |>
  select(-pairnum_pair) |>
  mutate(
    mayor_won = if_else(
      is.na(brgy_vote_diff),
      NA_integer_,
      if_else(brgy_vote_diff > 0, 1L, 0L)
    ),
    brgy_vote_share = if_else(
      is.na(brgy_vote_diff),
      NA_real_,
      (1 + brgy_vote_diff) / 2
    )
  )

write_csv(
  procurement_analysis_matched,
  file.path(out_dir, "procurement_analysis.csv")
)

match_diag <- tibble(
  check = c(
    "missing election vote_diff",
    "missing quarterly rows",
    "missing shapefile border_treat",
    "treated not one_to_one quarterly/election",
    "treated missing vote_diff",
    "IE municipalities with no procurement match (2012-2015)"
  ),
  n = c(
    sum(is.na(analysis_data$vote_diff)),
    sum(is.na(analysis_data$kalahi_spending_percap)),
    sum(is.na(analysis_data$border_treat)),
    sum(!treated_audit$one_to_one_both),
    analysis_data |> filter(treat == 1) |> summarise(n = sum(is.na(vote_diff))) |> pull(n),
    nrow(unmatched_ie_procurement)
  )
)
