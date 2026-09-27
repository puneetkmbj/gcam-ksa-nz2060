# ------------------------------------------------------------------------------
# constants.R — scenario labels, unit conversions and GWPs
# ------------------------------------------------------------------------------

# GCAM scenario names -> labels used in the papers (order = legend/facet order)
SCENARIO_LABELS <- c(
  no_policy      = "No Policy",
  current_policy = "Current Policy",
  NDCs_continued = "NDCs Continued",
  NZ_2060        = "NZ 2060"
)
SCENARIO_LEVELS <- unname(SCENARIO_LABELS)

# Unit conversions
EJ_TO_TWH      <- 277.778        # 1 EJ = 277.778 TWh
C_TO_CO2       <- 44 / 12        # MtC -> MtCO2
TWH_TO_BTC     <- 284.345 / 1000 # TWh (thermal) -> billion tons of cooling (1 TR-h = 3.517 kWh)
# GDP: GCAM reports million 1990 USD (MER). 1.98 = US GDP deflator 1990->2020,
# 3.75 = SAR/USD peg; result in trillion 2020 SAR.
GDP_1990USD_TO_2020SAR_TRN <- 1.98 * 3.75 / 1e6

# 100-year GWPs, IPCC AR5 (WG1 Ch.8, no climate-carbon feedback).
# `convert` rescales Gg (F-gases) to Tg; carbon-mass species are converted to CO2.
GWP_AR5 <- tibble::tribble(
  ~ghg,        ~gwp,    ~convert,
  "CO2",       44/12,   1,
  "LUC",       44/12,   1,
  "CH4",       28,      1,
  "CH4_AGR",   28,      1,
  "CH4_AWB",   28,      1,
  "N2O",       265,     1,
  "N2O_AGR",   265,     1,
  "N2O_AWB",   265,     1,
  "C2F6",      11100,   0.001,
  "CF4",       6630,    0.001,
  "HFC125",    3170,    0.001,
  "HFC134a",   1300,    0.001,
  "HFC245fa",  858,     0.001,
  "HFC143a",   4800,    0.001,
  "HFC152a",   138,     0.001,
  "HFC227ea",  3350,    0.001,
  "HFC23",     12400,   0.001,
  "HFC236fa",  8060,    0.001,
  "HFC32",     677,     0.001,
  "HFC365mfc", 804,     0.001,
  "HFC43",     1650,    0.001,
  "SF6",       23500,   0.001
)

FGASES <- c("C2F6", "CF4", "HFC125", "HFC134a", "HFC245fa", "HFC143a", "HFC152a",
            "HFC227ea", "HFC23", "HFC236fa", "HFC32", "HFC365mfc", "HFC43", "SF6")

# Saudi Arabia's updated NDC (2021): reduce, avoid and remove 278 MtCO2e annually
# by 2030 relative to the baseline (here the No Policy scenario).
NDC_2030_REDUCTION <- 278
