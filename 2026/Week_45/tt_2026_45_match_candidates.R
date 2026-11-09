## TidyTuesday 2026 W45 -- where tt_2026_45_matched_events.csv comes from

## Step 1 (scripted): candidate same-event pairs. For every character cited in both a prose
##   original and its adaptation, each adaptation citation is paired with the prose citation
##   whose claim shares the most words (Jaccard on content words); pairs at >= 0.12 are kept.
## Step 2 (judgment, recorded below): each candidate was read and coded on 2026-11-09 as
##   clean   -- the same event in both citations
##   partial -- one citation's feat is contained in the other's (one kill vs a report of four)
##   rejected -- different events or different beats of one encounter
## Step 3 (scripted): writes every candidate, with its decision, to the CSV the hero reads.

suppressPackageStartupMessages(library(tidyverse))

tt_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-10/"
cit <- read_csv(paste0(tt_url, "fiction_citations.csv"),
  col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE
)
stopifnot(nrow(cit) == 1813)

### |- step 1: candidates ----
stop_words <- c(
  "the", "a", "an", "and", "of", "to", "in", "his", "her", "he", "she", "him", "it", "its", "is",
  "on", "at", "with", "for", "by", "as", "from", "into", "that", "this", "their", "they", "them",
  "be", "was", "are", "who", "after", "before", "while", "one", "two", "all", "out", "up", "over"
)
content_words <- function(s) {
  w <- str_extract_all(str_to_lower(s), "[a-z']+")[[1]]
  unique(w[!w %in% stop_words & nchar(w) > 2])
}
jaccard <- function(a, b) {
  A <- content_words(a)
  B <- content_words(b)
  length(intersect(A, B)) / max(1, length(union(A, B)))
}

adaptation_type <- c(westeros = "episode", overlord = "episode", `solo-leveling` = "comic")

entry_cites <- cit |>
  filter(subject_kind == "entry", franchise_slug %in% names(adaptation_type)) |>
  mutate(adapt_type = adaptation_type[franchise_slug])

candidates <- entry_cites |>
  filter(type == adapt_type) |>
  rowwise() |>
  mutate(best = list({
    prose <- entry_cites |>
      filter(franchise_slug == .env$franchise_slug, subject_id == .env$subject_id, type == "novel")
    if (nrow(prose) == 0) {
      tibble()
    } else {
      sims <- map_dbl(prose$claim, \(p) jaccard(claim, p))
      i <- which.max(sims)
      tibble(
        similarity = sims[i], prose_locator = prose$locator[i],
        prose_confidence = prose$confidence[i], prose_claim = prose$claim[i]
      )
    }
  })) |>
  ungroup() |>
  filter(map_int(best, nrow) == 1) |>
  unnest(best) |>
  filter(similarity >= 0.12) |>
  transmute(
    franchise_slug,
    entry_id = subject_id, similarity = round(similarity, 2),
    prose_locator, prose_claim, adapt_type, adapt_locator = locator, adapt_claim = claim
  )

print(candidates |> count(franchise_slug))
stopifnot(nrow(candidates) == 37)

### |- step 2: the hand review, one row per candidate ----
## adapt_has: a fragment unique to the adaptation citation's claim
decisions <- tribble(
  ~entry_id,              ~adapt_has,                    ~match,     ~event_label,                         ~reason,
  "areo-hotah",           "Prince Doran's guard",        "rejected", NA,                                   "different scenes at the Water Gardens",
  "arthur-dayne",         "Book of Brothers",            "clean",    "Kills the Smiling Knight",           "Kingswood campaign, single combat",
  "arthur-dayne",         "In the vision",               "clean",    "Tower of Joy",                       "same fight: depicted on screen, Eddard's account in the book",
  "brienne-of-tarth",     "bear pit",                    "clean",    "The bear pit at Harrenhal",          "same scene",
  "gregor-clegane",       "burning Red Keep",            "rejected", NA,                                   "different fights",
  "jon-snow",             "Lured out",                   "clean",    "Stabbed at Castle Black",            "same scene",
  "robert-baratheon",     "Set on riding in the joust",  "clean",    "Too fat for his armour",             "same scene",
  "sandor-clegane",       "Steps between Gregor",        "clean",    "Stops Gregor at the Hand's tourney", "same scene",
  "sandor-clegane",       "shield on fire",              "clean",    "Fights Beric Dondarrion",            "same fight",
  "syrio-forel",          "Disarms and disables",        "rejected", NA,                                   "different claims",
  "ainz-ooal-gown",       "Touch Me's silver armour",    "clean",    "Touch Me's armour vs Shalltear",     "same fight",
  "albedo",               "Pursues Azuth",               "clean",    "Punches Azuth's powered suit",       "same blow",
  "aura-bella-fiora",     "block her path",              "rejected", NA,                                   "different beats of one encounter",
  "brain-unglaus",        "Bows to Cocytus",             "rejected", NA,                                   "different beats",
  "climb",                "Grasp Heart",                 "clean",    "Killed by Grasp Heart, revived",     "same death and revival",
  "narberal-gamma",       "Nabe kills Khajiit",          "clean",    "Kills Khajiit and his dragons",      "same fight",
  "nigun-grid-luin",      "Dominion Authority",          "clean",    "Summons a Dominion Authority",       "same fight",
  "olasirdarc-haylilyal", "Grasp Heart",                 "clean",    "Killed by Grasp Heart",              "same death",
  "sebas-tian",           "Malmvist",                    "partial",  "Arena kills: one shown, four told",  "one arena kill vs a report of four",
  "shalltear-bloodfallen","Brain Unglaus",               "rejected", NA,                                   "different scenes",
  "tsaindorcus-vaision",  "Grasp Heart",                 "clean",    "Grasp Heart fails on his armour",    "same spell, same target",
  "victim",               "introduces Victim",           "rejected", NA,                                   "different claims",
  "baran",                "Cup of Reincarnation",        "rejected", NA,                                   "revival vs title",
  "iron",                 "Baruka's warriors",           "rejected", NA,                                   "raid deaths vs naming",
  "iron",                 "Igris kills",                 "rejected", NA,                                   "death vs naming",
  "iron",                 "extracts the shadow",         "rejected", NA,                                   "extraction vs naming",
  "jima",                 "names the shadow",            "clean",    "Named as the boss naga's shadow",    "same naming",
  "jima",                 "tridents",                    "rejected", NA,                                   "weapons vs naming",
  "kaisel",               "extracts the shadow",         "rejected", NA,                                   "extraction vs naming",
  "kaisel",               "hospital",                    "rejected", NA,                                   "flight vs naming",
  "kaisel",               "Cut down",                    "rejected", NA,                                   "death vs naming",
  "sung-jinwoo",          "register",                    "clean",    "Rated the weakest hunter",           "same rating",
  "sung-jinwoo",          "Ant King",                    "rejected", NA,                                   "fight vs introduction",
  "the-architect",        "commandments",                "rejected", NA,                                   "rules vs statues attacking",
  "the-architect",        "slices the Architect",        "rejected", NA,                                   "different scenes",
  "thomas-andre",         "Rulers granted",              "rejected", NA,                                   "origin vs epithet",
  "yogumunt",             "Antares",                     "rejected", NA,                                   "deaths vs title"
)

coded <- candidates |>
  mutate(key = row_number()) |>
  cross_join(decisions |> mutate(dkey = row_number()), suffix = c("", "_d")) |>
  filter(entry_id == entry_id_d, str_detect(adapt_claim, fixed(adapt_has))) |>
  select(-entry_id_d)

## every candidate gets exactly one decision, and every decision is used exactly once
stopifnot(
  nrow(coded) == nrow(candidates), n_distinct(coded$key) == nrow(candidates),
  n_distinct(coded$dkey) == nrow(decisions)
)
print(count(coded, match))
stopifnot(sum(coded$match == "clean") == 16, sum(coded$match == "partial") == 1, sum(coded$match == "rejected") == 20)

### |- step 3: write the file the hero reads ----
coded |>
  transmute(
    franchise_slug, entry_id,
    event_label = replace_na(event_label, ""), match, similarity,
    prose_locator, prose_claim_start = str_sub(prose_claim, 1, 40),
    adapt_type, adapt_locator, adapt_claim_start = str_sub(adapt_claim, 1, 40),
    reviewer_note = reason
  ) |>
  write_csv(here::here("2026/Week_45/tt_2026_45_matched_events.csv"))

cat("wrote 2026/Week_45/tt_2026_45_matched_events.csv\n")
