## Challenge: #TidyTuesday 2026 week 46
## Data:      Cholesterol and Mortality in US Adults (NHANES 1999-2018 + NCHS LMF)
## Author:    Steven Ponce
## Date:      2026-11-16
##
## PURPOSE: Decision record for the three falsification gates run before the
##          Evidence Scorecard. Gate B (age adjustment), Gate D (incremental
##          discrimination), Gate S (early-death exclusion staircase).
##          Its printed output -- run on Steven's machine -- is the only source
##          for any number that later reaches copy. Not a chart script.
##
## WEIGHTS: Rebuilds CDC 20-year exam weights: 1999-2002 rows use
##          2/10 x WTMEC4YR (pulled from the original DEMO / DEMO_B files by
##          seqn); 2003-2018 rows use 1/10 x WTMEC2YR (= survey_weight).
##
## OUTPUT:  Everything printed is also written to
##          tt_2026_46_decision_record_output.txt -- upload that file.

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,     # Easily Install and Load the 'Tidyverse'
    janitor,       # Simple Tools for Examining and Cleaning Dirty Data
    survival,      # Survival Analysis
    survey,        # Analysis of Complex Survey Samples
    sessioninfo,   # R Session Information
    haven,         # Import and Export 'SPSS', 'Stata' and 'SAS' Files
    glue,          # Interpreted String Literals
    here           # A Simpler Way to Find Your Files
)

options(survey.lonely.psu = "adjust", width = 200)
set.seed(46)

run_id   <- "run6"   # run 2 output is the record for Gate D (settled); keep each run's file
out_file <- glue("tt_2026_46_decision_record_output_{run_id}.txt")
RUN_GATE_D <- FALSE  # TRUE re-runs the bootstrap (slow); run 2 printed the recorded values
sink(out_file, split = TRUE)   # closed in section 8

## 2. READ IN THE DATA ----
csv_url <- paste0(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/",
    "data/2026/2026-11-17/cholesterol_mortality.csv"
)

# Read every column as character with na = character() so no literal "NA"
# code is silently turned into missing; restore NA column by column below.
cm_raw <- read_csv(csv_url, col_types = cols(.default = col_character()), na = character()) |>
    clean_names()

### |- read guards ----
stopifnot(nrow(cm_raw) == 52678L, ncol(cm_raw) == 18L)
na_cols_allowed <- c(
    "total_chol", "hdl", "ldl", "triglycerides", "non_hdl",
    "tc_hdl_ratio", "ldl_hdl_ratio", "cause_of_death"
)
na_counts <- map_int(cm_raw, \(x) sum(x == "NA"))
stopifnot(all(names(na_counts[na_counts > 0]) %in% na_cols_allowed))
stopifnot(na_counts[["cause_of_death"]] == 45035L)

num_cols <- c(
    "seqn", "age", "total_chol", "hdl", "ldl", "triglycerides", "non_hdl",
    "tc_hdl_ratio", "ldl_hdl_ratio", "died", "followup_months",
    "survey_weight", "psu", "strata"
)

cm <- cm_raw |>
    mutate(across(all_of(c(na_cols_allowed)), \(x) na_if(x, "NA"))) |>
    mutate(across(all_of(num_cols), as.numeric))

stopifnot(sum(cm$died) == 7643, !anyNA(cm$survey_weight), !anyNA(cm$followup_months))

## 3. EXAMINING THE DATA ----
### |- age top-codes (dictionary has them reversed; NCHS: 85 in 1999-2006, 80 from 2007) ----
age_max <- cm |> summarise(age_max = max(age), .by = cycle)
print(age_max)
stopifnot(
    all(age_max$age_max[age_max$cycle <= "2005-2006"] == 85),
    all(age_max$age_max[age_max$cycle >= "2007-2008"] == 80)
)

### |- strata unique across cycles ----
stopifnot(cm |> distinct(strata, cycle) |> count(strata) |> filter(n > 1) |> nrow() == 0)

## 4. TIDY DATA ----
### |- rebuild 20-year exam weights ----
read_demo <- function(year, file) {
    tmp <- tempfile(fileext = ".xpt")
    download.file(
        glue("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/{year}/DataFiles/{file}.xpt"),
        tmp, mode = "wb", quiet = TRUE
    )
    read_xpt(tmp) |>
        transmute(seqn = as.numeric(SEQN), wtmec2yr = WTMEC2YR, wtmec4yr = WTMEC4YR)
}
# If the CDC URL pattern fails: nhanesA::nhanes("DEMO", translated = FALSE) and "DEMO_B"
demo_4yr <- bind_rows(read_demo(1999, "DEMO"), read_demo(2001, "DEMO_B"))

early_cycles <- c("1999-2000", "2001-2002")

cm <- cm |>
    left_join(demo_4yr, by = "seqn") |>
    mutate(
        early = cycle %in% early_cycles,
        wt20  = if_else(early, wtmec4yr * 2 / 10, survey_weight / 10)
    )

### |- weight guards ----
stopifnot(!anyNA(cm$wt20))                                     # every 1999-2002 row matched
stopifnot(all(abs(cm$wtmec2yr[cm$early] - cm$survey_weight[cm$early]) < 1e-3))  # file = WTMEC2YR
cat("\nPopulation represented (all rows, wt20):", round(sum(cm$wt20) / 1e6, 1), "M\n")
cat("Same, naive survey_weight / 10:        ", round(sum(cm$survey_weight) / 10 / 1e6, 1), "M\n")

### |- analysis variables ----
tc_breaks  <- c(-Inf, 160, 180, 200, 220, 240, Inf)
tc_labels  <- c("<160", "160-179", "180-199", "200-219", "220-239", "240+")
tc_breaks2 <- c(-Inf, 170, 190, 210, 230, 250, Inf)            # shifted +10 (robustness)
tc_labels2 <- c("<170", "170-189", "190-209", "210-229", "230-249", "250+")

cm <- cm |>
    mutate(
        an        = !is.na(total_chol) & !is.na(hdl) & hdl <= 140,   # matches Li et al.'s HDL cut
        t         = pmax(followup_months, 0.5),
        male      = as.integer(sex == "Male"),
        age_z     = (age - 47) / 10,
        age_band  = cut(age, c(17, 30, 40, 50, 60, 70, 80, 86), right = FALSE),
        age_exit  = age + t / 12,
        age65     = as.integer(age >= 65),
        tc_grp    = fct_relevel(cut(total_chol, tc_breaks, tc_labels, right = FALSE), "180-199"),
        tc_grp2   = fct_relevel(cut(total_chol, tc_breaks2, tc_labels2, right = FALSE), "190-209"),
        cvd       = as.integer(died == 1 & cause_of_death %in% c("Heart disease", "Cerebrovascular (stroke)"))
    )

an_df <- cm |> filter(an)
stopifnot(nrow(an_df) == 52634L, sum(an_df$died) == 7632, sum(an_df$cvd) == 2399)

### |- survey design (defined on all rows, then subset = domain estimation) ----
des    <- svydesign(ids = ~psu, strata = ~strata, weights = ~wt20, nest = TRUE, data = cm)
des_an <- subset(des, an)

### |- helper: hazard ratios for chosen terms ----
# Display labels are passed explicitly -- never parsed from coefficient names
# (str_remove("^tc_grp2?") turned "tc_grp220-239" into "20-239" in run 1).
terms_main  <- c("tc_grp<160", "tc_grp220-239", "tc_grp240+")
terms_shift <- c("tc_grp2<170", "tc_grp2230-249", "tc_grp2250+")
labels_cols <- c("low", "high_mid", "high")   # same columns for both binnings

hr_row <- function(fit, label, terms = terms_main) {
    stopifnot(all(terms %in% names(coef(fit))))
    b  <- coef(fit)[terms]
    se <- sqrt(diag(vcov(fit)))[terms]
    tibble(
        spec   = as.character(label),
        term   = labels_cols,
        hr     = unname(exp(b)),
        lo     = unname(exp(b - 1.96 * se)),
        hi     = unname(exp(b + 1.96 * se)),
        n      = if (is.null(fit$n)) NA_integer_ else as.integer(fit$n),
        events = if (is.null(fit$nevent)) NA_real_ else as.numeric(fit$nevent)
    )
}
fmt_hr <- function(tab) {
    out <- tab |>
        mutate(est = sprintf("%.2f [%.2f, %.2f]", hr, lo, hi)) |>
        select(spec, n, events, term, est) |>
        pivot_wider(names_from = term, values_from = est)
    stopifnot(!any(map_lgl(out, is.list)), anyDuplicated(out$spec) == 0)   # no list-cols, one row per spec
    out
}
cat("\nColumns: low = <160 | high_mid = 220-239 | high = 240+ (ref 180-199);",
    "B9 uses <170 | 230-249 | 250+ (ref 190-209)\n")

## 5. GATE B -- does the high-cholesterol arm stay gone across specifications? ----
cat("\n\n==== GATE B ====\n")
rhs <- list(
    B0_crude          = "tc_grp",
    B1_cycle          = "tc_grp + strata(cycle)",
    B1a_sex_only      = "tc_grp + male + strata(cycle)",
    B1b_age_only      = "tc_grp + age + strata(cycle)",
    B2_age_sex        = "tc_grp + age + male + strata(cycle)",
    B3_age_quadratic  = "tc_grp + age_z + I(age_z^2) + male + strata(cycle)",
    B4_age_bands      = "tc_grp + age_band + male + strata(cycle)"
)

fit_weighted   <- \(r, nm) hr_row(svycoxph(as.formula(paste("Surv(t, died) ~", r)), design = des_an), paste(nm, "| weighted"))
fit_unweighted <- \(r, nm) hr_row(coxph(as.formula(paste("Surv(t, died) ~", r)), data = an_df), paste(nm, "| unweighted"))

gate_b <- bind_rows(
    imap(rhs, fit_weighted) |> list_rbind(),
    imap(rhs, fit_unweighted) |> list_rbind(),
    hr_row(svycoxph(Surv(age, age_exit, died) ~ tc_grp + male + strata(cycle), design = des_an),
           "B5_age_timescale | weighted"),
    hr_row(coxph(Surv(age, age_exit, died) ~ tc_grp + male + strata(cycle), data = an_df),
           "B5_age_timescale | unweighted"),
    hr_row(svycoxph(Surv(t, died) ~ tc_grp + age + male + strata(cycle), design = subset(des_an, !early)),
           "B8_excl_1999_2002 | weighted"),
    hr_row(svycoxph(Surv(t, died) ~ tc_grp2 + age + male + strata(cycle), design = des_an),
           "B9_bins_shifted | weighted", terms = terms_shift),
    hr_row(svycoxph(Surv(t, died) ~ tc_grp + age + strata(cycle), design = subset(des_an, male == 1)),
           "B10_men | weighted"),
    hr_row(svycoxph(Surv(t, died) ~ tc_grp + age + strata(cycle), design = subset(des_an, male == 0)),
           "B11_women | weighted"),
    hr_row(svycoxph(Surv(t, died) ~ tc_grp + age + male + strata(cycle), design = subset(des_an, age < 65)),
           "B12_age_lt65 | weighted"),
    hr_row(svycoxph(Surv(t, died) ~ tc_grp + age + male + strata(cycle), design = subset(des_an, age >= 65)),
           "B13_age_65plus | weighted")
)
gate_b_tab <- fmt_hr(gate_b)
stopifnot(nrow(gate_b_tab) == 2 * length(rhs) + 8)
print(gate_b_tab, n = Inf, width = Inf)

cat("\n-- Composition: why the crude comparison misleads (weighted) --\n")
comp <- svyby(~ age + age65 + died, ~tc_grp, des_an, svymean)
print(comp)

## 6. GATE D -- out-of-sample incremental discrimination (unweighted) ----
cat("\n\n==== GATE D ====\n")
measures <- c("total_chol", "hdl", "non_hdl", "tc_hdl_ratio")
d_df <- an_df |>
    mutate(across(all_of(measures), \(x) pmin(pmax(as.numeric(scale(x)), -4), 4), .names = "{.col}_z"))
folds <- sample(rep(1:5, length.out = nrow(d_df)))

oof_lp <- function(event, extra = NULL) {
    f <- as.formula(paste0("Surv(t, ", event, ") ~ age + male + factor(cycle)",
                           if (!is.null(extra)) paste0(" + ", extra, " + I(", extra, "^2)") else ""))
    lp <- numeric(nrow(d_df))
    for (k in 1:5) {
        fit <- coxph(f, data = d_df[folds != k, ])
        lp[folds == k] <- predict(fit, newdata = d_df[folds == k, ], type = "lp")
    }
    lp
}
c_index <- function(time, event, lp) {
    concordance(Surv(time, event) ~ lp, reverse = TRUE)$concordance
}

if (RUN_GATE_D) gate_d <- map_dfr(c("died", "cvd"), \(ev) {
    lp_base <- oof_lp(ev)
    c_base  <- c_index(d_df$t, d_df[[ev]], lp_base)
    map_dfr(measures, \(m) {
        lp_plus <- oof_lp(ev, paste0(m, "_z"))
        c_plus  <- c_index(d_df$t, d_df[[ev]], lp_plus)
        boots <- replicate(200, {
            i <- sample.int(nrow(d_df), replace = TRUE)
            c_index(d_df$t[i], d_df[[ev]][i], lp_plus[i]) - c_index(d_df$t[i], d_df[[ev]][i], lp_base[i])
        })
        tibble(outcome = ev, measure = m, c_base = c_base, c_plus = c_plus,
               delta = c_plus - c_base,
               delta_lo = quantile(boots, 0.025), delta_hi = quantile(boots, 0.975))
    })
})
if (RUN_GATE_D) print(gate_d |> mutate(across(where(is.numeric), \(x) round(x, 4))), n = Inf)

## 7. GATE S -- early-death exclusion staircase ----
cat("\n\n==== GATE S ====\n")
# Baseline (yrs = 0) keeps everyone; later landmarks keep only people still
# under observation AFTER the landmark (strictly >), so entry < exit on the age
# time scale and a death at exactly the landmark month is not counted after it.
landmark_fit <- function(design, yrs, label, timescale = "followup") {
    d <- if (yrs == 0) design else subset(design, followup_months > 12 * yrs)
    d <- update(d, t_lm = pmax(t - 12 * yrs, 0.5), age_lm = age + yrs, age_exit_lm = age + t / 12)
    fit <- if (timescale == "age") {
        svycoxph(Surv(age_lm, age_exit_lm, died) ~ tc_grp + male + strata(cycle), design = d)
    } else {
        svycoxph(Surv(t_lm, died) ~ tc_grp + age_lm + male + strata(cycle), design = d)
    }
    hr_row(fit, glue("{label} | landmark {yrs}y | {timescale}"))
}
fixed_cohort <- c("1999-2000", "2001-2002", "2003-2004", "2005-2006", "2007-2008", "2009-2010")
des_fixed <- subset(des_an, cycle %in% fixed_cohort)   # every cycle has >= 135 months potential follow-up

gate_s <- bind_rows(
    map_dfr(c(0, 2, 5), \(y) landmark_fit(des_an, y, "all cycles")),
    landmark_fit(des_an, 5, "all cycles", "age"),
    map_dfr(c(0, 2, 5), \(y) landmark_fit(des_fixed, y, "fixed cohort 1999-2010")),
    landmark_fit(des_fixed, 5, "fixed cohort 1999-2010", "age")
)
gate_s_tab <- fmt_hr(gate_s)

### |- both time scales must use the same people and events at 5 years ----
lm5 <- gate_s_tab |> filter(str_detect(spec, "landmark 5y"))
stopifnot(
    n_distinct(lm5$n[str_detect(lm5$spec, "^all cycles")]) == 1,
    n_distinct(lm5$events[str_detect(lm5$spec, "^all cycles")]) == 1,
    n_distinct(lm5$n[str_detect(lm5$spec, "^fixed")]) == 1,
    n_distinct(lm5$events[str_detect(lm5$spec, "^fixed")]) == 1
)
print(gate_s_tab, n = Inf, width = Inf)

## 7b. FULL SIX-BIN SERIES + AGE INTERACTION ----
# A frame includes the middle: print every bin before any shape language, and
# test whether the high-end pattern differs by age group (run 2: <65 = 1.19,
# 65+ = 0.89 -- opposite sides of 1 inside a flat all-ages average).
cat("\n\n==== FULL SERIES (weighted, all six bins; ref 180-199) ====\n")
bin_labels <- c("<160", "160-179", "200-219", "220-239", "240+")
all_terms  <- paste0("tc_grp", bin_labels)

full_row <- function(fit, label) {
    stopifnot(all(all_terms %in% names(coef(fit))))
    b  <- coef(fit)[all_terms]
    se <- sqrt(diag(vcov(fit)))[all_terms]
    tibble(
        spec = label, bin = bin_labels,
        est  = sprintf("%.2f [%.2f, %.2f]", exp(b), exp(b - 1.96 * se), exp(b + 1.96 * se))
    )
}

full_specs <- list(
    "crude (cycle-stratified)" = list(f = Surv(t, died) ~ tc_grp + strata(cycle),              d = des_an),
    "age only"                 = list(f = Surv(t, died) ~ tc_grp + age + strata(cycle),        d = des_an),
    "age + sex"                = list(f = Surv(t, died) ~ tc_grp + age + male + strata(cycle), d = des_an),
    "age + sex | under 65"     = list(f = Surv(t, died) ~ tc_grp + age + male + strata(cycle), d = subset(des_an, age < 65)),
    "age + sex | 65+"          = list(f = Surv(t, died) ~ tc_grp + age + male + strata(cycle), d = subset(des_an, age >= 65)),
    "crude | under 65"         = list(f = Surv(t, died) ~ tc_grp + strata(cycle),              d = subset(des_an, age < 65)),
    "crude | 65+"              = list(f = Surv(t, died) ~ tc_grp + strata(cycle),              d = subset(des_an, age >= 65))
)

full_tab <- imap(full_specs, \(s, nm) full_row(svycoxph(s$f, design = s$d), nm)) |>
    list_rbind() |>
    pivot_wider(names_from = bin, values_from = est) |>
    mutate(`180-199` = "1.00 (ref)", .after = `160-179`)
stopifnot(!any(map_lgl(full_tab, is.list)), nrow(full_tab) == length(full_specs))
print(full_tab, n = Inf, width = Inf)

# (run 3 also printed weighted svytotal(~died): dropped -- expanded death totals
#  are not national counts when follow-up runs 2-20 years by cycle)
cat("\n-- Unweighted deaths per bin, by age group (evidence behind each subgroup estimate) --\n")
print(an_df |> count(age65, tc_grp, wt = died, name = "deaths_unweighted") |>
          pivot_wider(names_from = tc_grp, values_from = deaths_unweighted))

cat("\n-- Does the cholesterol pattern differ by age group? (weighted Wald test on tc_grp x age65) --\n")
fit_int  <- svycoxph(Surv(t, died) ~ tc_grp * age65 + age + male + strata(cycle), design = des_an)
int_test <- regTermTest(fit_int, ~ tc_grp:age65)
print(int_test)

## 7c. TARGETED CHECKS (run 4) ----
### |- bin-specific age contrasts: ratio of HRs, 65+ vs under 65 ----
# The 5-df test says the pattern differs somewhere; these say where.
# Ratio < 1 = the bin's HR is lower at 65+ than under 65.
cat("\n\n==== BIN-SPECIFIC AGE CONTRASTS (ratio of HRs, 65+ / under 65) ====\n")
int_terms <- paste0("tc_grp", bin_labels, ":age65")
stopifnot(all(int_terms %in% names(coef(fit_int))))
df_design <- degf(des_an)
int_tab <- tibble(
    bin = bin_labels,
    b   = unname(coef(fit_int)[int_terms]),
    se  = unname(sqrt(diag(vcov(fit_int)))[int_terms])
) |>
    mutate(
        ratio = sprintf("%.2f [%.2f, %.2f]", exp(b), exp(b - 1.96 * se), exp(b + 1.96 * se)),
        p     = signif(2 * pt(-abs(b / se), df = df_design), 3)
    ) |>
    select(bin, ratio, p)
print(int_tab, width = Inf)
cat("Design df used for p:", df_design, "\n")

### |- age-cut robustness: is the split an artefact of the 65 cut? ----
# Three bands (<50, 50-69, 70+), the split used in the 2025 NHANES non-HDL paper.
cat("\n\n==== AGE-CUT ROBUSTNESS: three age bands, age + sex adjusted ====\n")
des_an <- update(des_an, age_b3 = cut(age, c(17, 50, 70, 86), right = FALSE,
                                      labels = c("18-49", "50-69", "70+")))
band_tab <- map(c("18-49", "50-69", "70+"), \(b) {
    full_row(svycoxph(Surv(t, died) ~ tc_grp + age + male + strata(cycle),
                      design = subset(des_an, age_b3 == b)), paste("age + sex |", b))
}) |>
    list_rbind() |>
    pivot_wider(names_from = bin, values_from = est) |>
    mutate(`180-199` = "1.00 (ref)", .after = `160-179`)
stopifnot(!any(map_lgl(band_tab, is.list)), nrow(band_tab) == 3)
print(band_tab, width = Inf)
print(an_df |>
          mutate(age_b3 = cut(age, c(17, 50, 70, 86), right = FALSE, labels = c("18-49", "50-69", "70+"))) |>
          count(age_b3, tc_grp, wt = died, name = "deaths") |>
          pivot_wider(names_from = tc_grp, values_from = deaths))

fit_int3 <- svycoxph(Surv(t, died) ~ tc_grp * age_b3 + age + male + strata(cycle), design = des_an)
print(regTermTest(fit_int3, ~ tc_grp:age_b3))

## 7d. OBSERVED vs EXPECTED DEATHS (concept D admission gate, run 5) ----
# Expected deaths for each person = their cumulative hazard over their OWN
# follow-up from a Cox model WITHOUT cholesterol: E_i = died_i - martingale
# residual_i. This handles unequal follow-up and censoring, and sum(E) = sum(O)
# by construction, so O/E for a bin compares it with what age, sex and cycle
# alone predict for the same people over the same follow-up time.
# Never labelled "deaths caused by cholesterol".
cat("\n\n==== 7d. OBSERVED vs EXPECTED DEATHS ====\n")

exp_unw <- function(data, rhs) {
    fit <- coxph(as.formula(paste("Surv(t, died) ~", rhs)), data = data)
    data$died - residuals(fit, type = "martingale")
}
exp_wt <- function(data, rhs) {
    fit <- coxph(as.formula(paste("Surv(t, died) ~", rhs)), data = data, weights = wt20)
    data$died - residuals(fit, type = "martingale")
}
poisson_ci <- function(o, e) {
    sprintf("%.2f [%.2f, %.2f]", o / e, qchisq(0.025, 2 * o) / 2 / e, qchisq(0.975, 2 * (o + 1)) / 2 / e)
}

an_df <- an_df |>
    mutate(
        age_b3      = cut(age, c(17, 50, 70, 86), right = FALSE, labels = c("18-49", "50-69", "70+")),
        E_crude     = exp_unw(an_df, "strata(cycle)"),               # cycle only: the "apparent" picture
        E_adj       = exp_unw(an_df, "age + male + strata(cycle)"),  # age + sex + cycle
        E_adj_w     = exp_wt(an_df,  "age + male + strata(cycle)")
    )

# Band-specific expectations: refit within each band so each band's own total O/E = 1
an_df <- an_df |>
    group_by(age_b3) |>
    group_modify(\(d, k) mutate(d, E_band = exp_unw(d, "age + male + strata(cycle)"))) |>
    ungroup() |>
    arrange(match(seqn, cm$seqn[cm$an]))                           # restore design row order
stopifnot(identical(an_df$seqn, cm$seqn[cm$an]))

### |- admission-gate checks: units ----
stopifnot(
    all(an_df$E_crude >= 0), all(an_df$E_adj >= 0), all(an_df$E_band >= 0),
    abs(sum(an_df$E_crude) - sum(an_df$died)) < 1,
    abs(sum(an_df$E_adj)   - sum(an_df$died)) < 1,
    abs(sum(an_df$wt20 * an_df$E_adj_w) - sum(an_df$wt20 * an_df$died)) / sum(an_df$wt20 * an_df$died) < 1e-6,
    all(abs(tapply(an_df$E_band, an_df$age_b3, sum) - tapply(an_df$died, an_df$age_b3, sum)) < 1)
)

### |- pooled table: crude vs age-and-sex expected ----
hr_unw <- coxph(Surv(t, died) ~ tc_grp + age + male + strata(cycle), data = an_df)
hr_vec <- c(exp(coef(hr_unw)[all_terms]), `tc_grp180-199` = 1)
names(hr_vec) <- str_remove(names(hr_vec), "^tc_grp")

oe_pooled <- an_df |>
    summarise(O = sum(died), E_crude = sum(E_crude), E_adj = sum(E_adj), .by = tc_grp) |>
    mutate(tc_grp = factor(tc_grp, levels = tc_labels)) |>
    arrange(tc_grp) |>
    mutate(
        OE_crude = poisson_ci(O, E_crude),
        OE_adj   = poisson_ci(O, E_adj),
        rel_OE   = (O / E_adj) / (O[tc_grp == "180-199"] / E_adj[tc_grp == "180-199"]),
        HR_unw   = unname(hr_vec[as.character(tc_grp)])
    )
print(oe_pooled |> mutate(across(c(E_crude, E_adj), \(x) round(x, 1)),
                          across(c(rel_OE, HR_unw), \(x) round(x, 3))), width = Inf)

unit_gap <- max(abs(log(oe_pooled$rel_OE) - log(oe_pooled$HR_unw)))
cat(sprintf("\nUnit check: max |log(rel O/E) - log(HR)| = %.3f -> %s (pass if < 0.05)\n",
            unit_gap, if (unit_gap < 0.05) "PASS" else "FAIL"))

### |- weighted O/E with design-based intervals ----
des_an <- update(des_an, E_adj_w = an_df$E_adj_w)
oe_w <- svyby(~died, ~tc_grp, des_an, svyratio, denominator = ~E_adj_w)
oe_w_tab <- tibble(
    tc_grp = as.character(oe_w$tc_grp),
    OE_w   = coef(oe_w),
    lo     = confint(oe_w)[, 1],
    hi     = confint(oe_w)[, 2]
) |>
    mutate(tc_grp = factor(tc_grp, levels = tc_labels)) |>
    arrange(tc_grp) |>
    mutate(est = sprintf("%.2f [%.2f, %.2f]", OE_w, lo, hi))
print(oe_w_tab |> select(tc_grp, weighted_OE_adj = est), width = Inf)

### |- by age band: O/E against each band's own age-and-sex expectation ----
oe_band <- an_df |>
    summarise(O = sum(died), E = sum(E_band), .by = c(age_b3, tc_grp)) |>
    mutate(tc_grp = factor(tc_grp, levels = tc_labels)) |>
    arrange(age_b3, tc_grp) |>
    mutate(OE = poisson_ci(O, E), E = round(E, 1))
print(oe_band |> select(age_b3, tc_grp, O, E, OE) |>
          pivot_wider(names_from = age_b3, values_from = c(O, E, OE)), width = Inf)

cat("\n7d GATE:", if (unit_gap < 0.05) "PASS -- D may be rendered" else "FAIL -- D does not advance", "\n")

## 9. EXPORT ESTIMATES FOR THE HERO SCRIPT (run 6) ----
# The hero chart reads this file; it never types a number. One tidy table:
#   kind = hr_weighted       5 rows x 6 bins (the matrix), survey-weighted
#   kind = hr_unweighted     ages 18-49, 6 bins (weighting-sensitivity note)
#   kind = mean_age_weighted 6 bins (context row)
cat("\n\n==== 9. EXPORT FOR HERO SCRIPT ====\n")
est_rows <- function(fit, key) {
    stopifnot(all(all_terms %in% names(coef(fit))))
    b  <- coef(fit)[all_terms]
    se <- sqrt(diag(vcov(fit)))[all_terms]
    tibble(
        row_key = key,
        bin     = c(bin_labels, "180-199"),
        value   = c(unname(exp(b)), 1),
        lo      = c(unname(exp(b - 1.96 * se)), 1),
        hi      = c(unname(exp(b + 1.96 * se)), 1)
    )
}
band_key <- c("18-49" = "adj_18_49", "50-69" = "adj_50_69", "70+" = "adj_70p")

row_specs <- list(
    crude_all = list(f = Surv(t, died) ~ tc_grp + strata(cycle),              d = des_an),
    adj_all   = list(f = Surv(t, died) ~ tc_grp + age + male + strata(cycle), d = des_an),
    adj_18_49 = list(f = Surv(t, died) ~ tc_grp + age + male + strata(cycle), d = subset(des_an, age_b3 == "18-49")),
    adj_50_69 = list(f = Surv(t, died) ~ tc_grp + age + male + strata(cycle), d = subset(des_an, age_b3 == "50-69")),
    adj_70p   = list(f = Surv(t, died) ~ tc_grp + age + male + strata(cycle), d = subset(des_an, age_b3 == "70+"))
)

deaths_cells <- bind_rows(
    an_df |> count(tc_grp, wt = died, name = "deaths") |> mutate(row_key = "crude_all"),
    an_df |> count(tc_grp, wt = died, name = "deaths") |> mutate(row_key = "adj_all"),
    an_df |> count(age_b3, tc_grp, wt = died, name = "deaths") |>
        mutate(row_key = unname(band_key[as.character(age_b3)])) |> select(-age_b3)
) |>
    mutate(bin = as.character(tc_grp)) |>
    select(row_key, bin, deaths)

export <- bind_rows(
    imap(row_specs, \(s, k) est_rows(svycoxph(s$f, design = s$d), k)) |>
        list_rbind() |> mutate(kind = "hr_weighted"),
    est_rows(coxph(Surv(t, died) ~ tc_grp + age + male + strata(cycle),
                   data = filter(an_df, age_b3 == "18-49")), "adj_18_49") |>
        mutate(kind = "hr_unweighted"),
    tibble(kind = "mean_age_weighted", row_key = "all", bin = as.character(comp$tc_grp),
           value = comp$age, lo = NA_real_, hi = NA_real_)
) |>
    left_join(deaths_cells, by = c("row_key", "bin")) |>
    relocate(kind)

### |- export guards (re-assert the decision-record values the chart will show) ----
hrw <- export |> filter(kind == "hr_weighted")
stopifnot(
    nrow(export) == 42, nrow(hrw) == 30,
    all(hrw$value[hrw$bin == "180-199"] == 1),
    sum(hrw$deaths[hrw$row_key == "adj_all"]) == 7632,
    sum(hrw$deaths[hrw$row_key %in% band_key]) == 7632,
    round(hrw$value[hrw$row_key == "crude_all" & hrw$bin == "240+"], 2) == 1.27,
    round(hrw$value[hrw$row_key == "adj_all"   & hrw$bin == "240+"], 2) == 1.00,
    round(hrw$value[hrw$row_key == "adj_18_49" & hrw$bin == "240+"], 2) == 1.42,
    hrw$deaths[hrw$row_key == "adj_18_49" & hrw$bin == "240+"] == 128
)
print(export |> mutate(across(c(value, lo, hi), \(x) round(x, 3))), n = Inf, width = Inf)

export_path <- here::here("2026/Week_46/tt_2026_46_estimates.csv")
write_csv(export, export_path, na = "")
stopifnot(nrow(read_csv(export_path, show_col_types = FALSE)) == 42)
cat("Wrote", export_path, "\n")

## 8. SESSION INFO ----
cat("\n\nDecision record complete:", format(Sys.time()), "\n")
sink()
stopifnot(sink.number() == 0)   # output file closed
stop()
sessioninfo::session_info(include_base = TRUE)