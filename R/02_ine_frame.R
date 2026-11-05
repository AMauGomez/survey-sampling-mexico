# Sampling frame: one row per polling station (casilla) ---------------------------
#
# y = votes for the Morena–PT–PVEM coalition (the three parties alone or in any
#     of their joint combinations on the ballot)
# z = valid votes = total votes − null votes
# Parameter of interest: R = sum(y) / sum(z), the coalition's share of valid votes.

raw <- readr::read_delim("data/raw/ine_diputaciones_2021.csv", delim = "|", skip = 5,
                         col_types = readr::cols(.default = "c"),
                         locale = readr::locale(encoding = "latin1"), progress = FALSE)
num <- function(x) as.numeric(x)

coalition <- c("PVEM", "PT", "MORENA", "PVEM-PT-MORENA", "PVEM-PT", "PVEM-MORENA", "PT-MORENA")

# The five electoral circumscriptions (2021), by state code
circ_of_state <- c(
  "2" = 1, "3" = 1, "8" = 1, "10" = 1, "14" = 1, "18" = 1, "25" = 1, "26" = 1,
  "1" = 2, "5" = 2, "11" = 2, "19" = 2, "22" = 2, "24" = 2, "28" = 2, "32" = 2,
  "4" = 3, "7" = 3, "20" = 3, "23" = 3, "27" = 3, "30" = 3, "31" = 3,
  "9" = 4, "12" = 4, "17" = 4, "21" = 4, "29" = 4,
  "6" = 5, "13" = 5, "15" = 5, "16" = 5
)

frame <- tibble(
  state    = as.integer(raw$ID_ESTADO),
  district = paste(as.integer(raw$ID_ESTADO), as.integer(raw$ID_DISTRITO), sep = "-"),
  y = rowSums(sapply(coalition, function(v) num(raw[[v]]))),
  z = num(raw$TOTAL_VOTOS_CALCULADOS) - num(raw[["VOTOS NULOS"]])
) |>
  mutate(circ = unname(circ_of_state[as.character(state)]))

stopifnot(!anyNA(frame$y), !anyNA(frame$z), !anyNA(frame$circ), nrow(frame) == 163666)

R_TRUE <- sum(frame$y) / sum(frame$z)

frame_summary <- tibble(
  polling_stations = nrow(frame),
  districts = n_distinct(frame$district),
  circumscriptions = n_distinct(frame$circ),
  valid_votes = sum(frame$z),
  coalition_votes = sum(frame$y),
  coalition_share_pct = 100 * R_TRUE
)
readr::write_csv(frame_summary, "results/ine_frame_summary.csv")
readr::write_csv(count(frame, circ, name = "polling_stations") |>
                   left_join(frame |> distinct(circ, district) |> count(circ, name = "districts"), by = "circ"),
                 "results/ine_circumscriptions.csv")
cat(sprintf("INE frame: %d polling stations, %d districts; coalition share of valid votes = %.3f%%\n",
            nrow(frame), n_distinct(frame$district), 100 * R_TRUE))
