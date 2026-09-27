# ------------------------------------------------------------------------------
# prep.R — data preparation shared by the paper scripts
#
# Each function returns a tidy tibble ready to plot. All values are for
# Saudi Arabia, 2015-2060, five-year steps. Functions that read the small
# number of inputs not held in the scenario database are grouped at the end
# (section "External inputs") and read from data/external/.
# ------------------------------------------------------------------------------

# ---- Emissions ---------------------------------------------------------------

#' Emissions of all Kyoto gases in MtCO2e (AR5 GWP100) at the finest grain
#' available: sector x subsector x gas. Combines energy/industry (incl. CO2),
#' resource production and land-use change.
ghg_detail <- function() {
  luc <- gq("LUC emissions by region") %>%
    mutate(ghg = "LUC", sector = "land-use change", subsector = "land-use change")
  sub <- gq("nonCO2 emissions by subsector")
  res <- gq("nonCO2 emissions by resource production") %>%
    mutate(sector = paste("resource production", resource), subsector = subresource)
  bind_rows(luc, sub, res) %>%
    inner_join(GWP_AR5, by = "ghg") %>%
    mutate(value = value * gwp * convert) %>%
    group_by(scenario, sector, subsector, ghg, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(Units = "MtCO2e")
}

#' Economy-wide GHG emissions (MtCO2e)
ghg_total <- function() {
  ghg_detail() %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(Units = "MtCO2e")
}

#' CO2 emissions by assigned end-use sector (MtCO2). `sector` is one of
#' Buildings, Transport, Industry, Electricity, CDR.
co2_by_assigned_sector <- function() {
  gq("CO2 emissions by assigned sector (no bio)") %>%
    mutate(sector = recode(sector,
                           buildings = "Buildings", transportation = "Transport",
                           industry = "Industry", cement = "Industry",
                           electricity = "Electricity", `CO2 removal` = "CDR")) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = sum(value) * C_TO_CO2, .groups = "drop") %>%
    mutate(Units = "MtCO2")
}

#' Building sector direct emissions split into CO2 and F-gases (MtCO2e)
BUILDING_SECTORS <- c("comm cooling", "comm heating", "comm others",
                      "resid cooling", "resid heating", "resid others")
building_ghg <- function() {
  ghg_detail() %>%
    filter(sector %in% BUILDING_SECTORS) %>%
    mutate(gas = case_when(ghg == "CO2" ~ "CO2",
                           ghg %in% FGASES ~ "F-Gases",
                           TRUE ~ "CH4 and N2O")) %>%
    group_by(scenario, gas, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(Units = "MtCO2e")
}

# ---- Electricity -------------------------------------------------------------

ELEC_TECH_MAP <- c(
  "gas (CC)" = "Gas", "gas (steam/CT)" = "Gas", "gas (CC CCS)" = "Gas_CCS",
  "refined liquids (CC)" = "Liquid Fuels", "refined liquids (steam/CT)" = "Liquid Fuels",
  "rooftop_pv" = "Other RE", "geothermal" = "Other RE",
  "wind" = "Wind", "wind_offshore" = "Wind",
  "Gen_III" = "Nuclear", "Gen_II_LWR" = "Nuclear",
  "CSP" = "CSP", "PV" = "PV",
  "wind_storage" = "Storage", "PV_storage" = "Storage", "CSP_storage" = "Storage"
)

#' Electricity generation by technology group (TWh)
elec_gen_by_tech <- function() {
  x <- gq("elec gen by gen tech")
  unmapped <- setdiff(unique(x$technology[x$value > 0]), names(ELEC_TECH_MAP))
  if (length(unmapped)) stop("Unmapped generation technologies: ", paste(unmapped, collapse = ", "))
  x %>%
    mutate(technology = factor(ELEC_TECH_MAP[technology], levels = names(PAL_ELEC_TECH))) %>%
    group_by(scenario, technology, year) %>%
    summarise(value = sum(value) * EJ_TO_TWH, .groups = "drop") %>%
    mutate(Units = "TWh")
}

elec_gen_total <- function() {
  elec_gen_by_tech() %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(Units = "TWh")
}

#' End-use electricity demand (TWh)
elec_demand <- function() {
  gq("elec consumption by demand sector") %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value) * EJ_TO_TWH, .groups = "drop") %>%
    mutate(Units = "TWh")
}

# ---- Final energy ------------------------------------------------------------

#' Total final energy (EJ)
final_energy_total <- function() {
  gq("total final energy by aggregate sector") %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(Units = "EJ")
}

#' Final energy by end-use sector with shares (%). DAC and its process heat
#' are reported together as "Direct Air Capture".
final_energy_by_sector <- function() {
  gq("total final energy by aggregate sector") %>%
    mutate(sector = recode(sector, building = "Buildings", transportation = "Transport",
                           industry = "Industry", `CO2 removal` = "Direct Air Capture",
                           `process heat dac` = "Direct Air Capture")) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    group_by(scenario, year) %>%
    mutate(share = 100 * value / sum(value)) %>%
    ungroup() %>%
    mutate(sector = factor(sector, levels = names(PAL_FE_SECTOR)), Units = "EJ")
}

#' Final energy by fuel with shares (%); energy used for DAC is shown separately
#' as "Electricity - CDR" and "Natural Gas - CDR" (DAC process heat).
final_energy_by_fuel <- function() {
  x <- gq("final energy consumption by sector and fuel") %>% filter(sector != "process heat dac")
  cdr <- x %>% filter(sector == "CO2 removal") %>%
    mutate(fuel = recode(input, electricity = "Electricity - CDR",
                         `process heat dac` = "Natural Gas - CDR"))
  rest <- x %>% filter(sector != "CO2 removal") %>%
    mutate(fuel = recode(input, biomass = "Biomass", electricity = "Electricity",
                         gas = "Natural Gas", hydrogen = "Hydrogen",
                         `refined liquids` = "Liquid Fuels"))
  bind_rows(cdr, rest) %>%
    group_by(scenario, fuel, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    group_by(scenario, year) %>%
    mutate(share = 100 * value / sum(value)) %>%
    ungroup() %>%
    mutate(fuel = factor(fuel, levels = names(PAL_FE_FUEL)), Units = "EJ")
}

# ---- Socio-economics and buildings --------------------------------------------

population <- function() {
  gq("population by region") %>%
    transmute(scenario, year, value = value / 1000, Units = "million")
}

gdp <- function() {
  gq("GDP MER by region") %>%
    transmute(scenario, year, value = value * GDP_1990USD_TO_2020SAR_TRN,
              Units = "trillion 2020 SAR")
}

floorspace <- function() {
  gq("building floorspace") %>%
    mutate(building = recode(building, resid = "Residential", comm = "Commercial")) %>%
    group_by(scenario, building, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(building = factor(building, levels = c("Residential", "Commercial")),
           Units = "billion m2")
}

building_final_energy <- function() {
  gq("building total final energy by region") %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(Units = "EJ")
}

building_final_energy_by_fuel <- function() {
  gq("building final energy by fuel") %>%
    mutate(fuel = recode(input, elect_td_bld = "Electricity",
                         `refined liquids enduse` = "LPG/Liquid Fuels",
                         `delivered biomass` = "Biomass")) %>%
    group_by(scenario, fuel, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(fuel = factor(fuel, levels = names(PAL_BLD_FUEL)), Units = "EJ")
}

# ---- Industry ----------------------------------------------------------------
# Industry-paper figures (and main-paper Fig. 7) use the published figure data in
# data/external/industry/: the NDCs Continued and NZ 2060 industry results in those
# publications come from a model run that differs slightly from ksa_nz.dat
# (No Policy and Current Policy are identical). See README, "Data provenance".

INDUSTRY_SCENARIOS <- c(cur_policy = "Current Policy", nz_2060 = "NZ 2060")

read_industry <- function(file) {
  x <- read_external(file.path("industry", file))
  if ("scenario" %in% names(x)) {
    x$scenario <- ifelse(x$scenario %in% names(INDUSTRY_SCENARIOS),
                         INDUSTRY_SCENARIOS[x$scenario], x$scenario)
    x$scenario <- factor(x$scenario, levels = SCENARIO_LEVELS)
  }
  x
}

with_share <- function(x) {
  x %>% group_by(scenario, year) %>% mutate(share = 100 * value / sum(value)) %>% ungroup()
}

#' Industrial CO2 emissions, all four scenarios (MtCO2)
industry_co2_total <- function() {
  read_industry("industry_co2_total.csv") %>%
    transmute(scenario, year, value = ind_co2_emission, Units = "MtCO2")
}

#' Industrial CO2 by industry type for 2020, 2060 Current Policy and 2060 NZ 2060
industry_co2_by_type <- function() {
  read_industry("industry_co2_by_type.csv") %>%
    transmute(case = factor(scenario_year, levels = c("2020", "2060 (Current Policy)", "2060 (NZ 2060)")),
              sector = factor(recode(sector, `Other industry` = "Other Industry"),
                              levels = names(PAL_IND_SECTOR)),
              value, Units = "MtCO2")
}

#' Change in 2060 industrial CO2 by industry type, Current Policy -> NZ 2060
industry_co2_waterfall <- function() {
  x <- industry_co2_by_type() %>% filter(case != "2020") %>%
    select(case, sector, value) %>%
    pivot_wider(names_from = case, values_from = value, values_fill = 0) %>%
    transmute(sector, change = `2060 (NZ 2060)` - `2060 (Current Policy)`)
  steps <- c("Chemical", "Cement", "Iron and Steel", "Refining", "Fertilizer",
             "Heat (DAC)", "Other Industry")
  cp <- sum(industry_co2_by_type()$value[industry_co2_by_type()$case == "2060 (Current Policy)"])
  x <- x[match(steps, x$sector), ]
  end <- cp + cumsum(x$change)
  bind_rows(
    tibble(label = "Current Policy", start = 0, end = cp, type = "total"),
    tibble(label = steps, start = c(cp, head(end, -1)), end = end, type = "change"),
    tibble(label = "NZ 2060", start = 0, end = tail(end, 1), type = "total")
  ) %>% mutate(label = factor(label, levels = label), Units = "MtCO2")
}

#' Industrial final energy by fuel, CCS-equipped use shown separately (EJ, %)
industry_final_energy_by_fuel <- function() {
  read_industry("industry_final_energy_by_fuel.csv") %>%
    mutate(fuel = recode(agg_fuel, electricity = "Electricity", gas = "Natural gas",
                         `gas CCS` = "Natural gas CCS", hydrogen = "Hydrogen",
                         `refined liquids` = "Oil", `refined liquids CCS` = "Oil CCS")) %>%
    group_by(scenario, fuel, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    with_share() %>%
    mutate(fuel = factor(fuel, levels = names(PAL_IND_FUEL)), Units = "EJ")
}

#' Share (%) of cement output by technology
cement_tech_share <- function() {
  read_industry("cement_production_by_tech.csv") %>%
    mutate(technology = recode(technology, cement = "Cement", `cement CCS` = "Cement (w/CCS)")) %>%
    group_by(scenario, technology, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    with_share() %>%
    mutate(technology = factor(technology, levels = names(PAL_CEMENT)))
}

#' Share (%) of iron and steel output by technology
steel_tech_share <- function() {
  read_industry("steel_production_by_tech.csv") %>%
    mutate(technology = recode(technology,
                               `Hydrogen-based DRI` = "EAF with DRI (Hydrogen-based)",
                               `EAF with DRI CCS`   = "EAF with DRI (w/CCS)")) %>%
    group_by(scenario, technology, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    with_share() %>%
    mutate(technology = factor(technology, levels = names(PAL_STEEL)))
}

#' Refined liquids output of the domestic refining sector (EJ)
refining_output <- function() {
  read_industry("refining_output.csv") %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(Units = "EJ")
}

#' Energy use of direct air capture by fuel, NZ 2060 (EJ)
dac_energy <- function() {
  read_industry("dac_energy_by_fuel.csv") %>%
    mutate(fuel = recode(input, elect_td_ind = "Electricity", `wholesale gas` = "Natural gas")) %>%
    group_by(scenario, fuel, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(fuel = factor(fuel, levels = names(PAL_DAC_FUEL)), Units = "EJ")
}

#' CO2 captured by direct air capture (MtCO2)
dac_sequestration <- function() {
  gq("CO2 sequestration by tech") %>%
    filter(sector == "CO2 removal") %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value) * C_TO_CO2, .groups = "drop") %>%
    mutate(Units = "MtCO2")
}

# ---- Transport ---------------------------------------------------------------

TRN_MODE_MAP <- c(
  "Car" = "Four Wheelers", "Large Car and Truck" = "Four Wheelers", "Mini Car" = "Four Wheelers",
  "Bus" = "Bus", "2W and 3W" = "Other LDVs",
  "Passenger Rail" = "Passenger Rail", "HSR" = "Passenger Rail",
  "Domestic Aviation" = "Domestic Aviation", "International Aviation" = "International Aviation",
  "Heavy truck" = "Trucks", "Medium truck" = "Trucks", "Light truck" = "Trucks",
  "Freight Rail" = "Freight Rail", "International Ship" = "International Shipping"
)
PASSENGER_MODES <- c("Four Wheelers", "Bus", "Other LDVs", "Passenger Rail",
                     "Domestic Aviation", "International Aviation")
FREIGHT_MODES   <- c("Trucks", "Freight Rail", "International Shipping")

#' Transport service demand by mode (billion pkm / billion tkm).
#' Aggregate nodes (road, LDV, 4W) and non-motorised modes are excluded.
transport_service_by_mode <- function(combine_aviation = FALSE) {
  x <- gq("transport service output by mode") %>%
    filter(mode %in% names(TRN_MODE_MAP)) %>%
    mutate(mode = TRN_MODE_MAP[mode])
  if (combine_aviation) x <- x %>% mutate(mode = sub("^(Domestic|International) Aviation$", "Aviation", mode))
  x %>%
    mutate(segment = ifelse(mode %in% FREIGHT_MODES, "Freight", "Passenger")) %>%
    group_by(scenario, segment, mode, year) %>%
    summarise(value = sum(value) / 1000, .groups = "drop") %>%
    mutate(Units = ifelse(segment == "Freight", "billion tkm", "billion pkm"))
}

#' Share (%) of new vehicle sales (new-vintage service output) by powertrain
#' @param subsectors GCAM transport subsectors (e.g. car or truck size classes)
new_sales_share <- function(subsectors) {
  gq("transport service output by tech (new)") %>%
    filter(subsector %in% subsectors) %>%
    mutate(technology = case_when(technology == "Liquids" ~ "ICE",
                                  technology == "Hybrid Liquids" ~ "ICE Hybrid",
                                  technology %in% c("BEV", "FCEV") ~ "Zero Emission")) %>%
    filter(!is.na(technology)) %>%
    group_by(scenario, technology, year) %>%
    summarise(value = sum(value) / 1000, .groups = "drop") %>%
    group_by(scenario, year) %>%
    mutate(share = 100 * value / sum(value)) %>%
    ungroup() %>%
    mutate(technology = factor(technology, levels = names(PAL_TRN_TECH)))
}
CAR_SUBSECTORS   <- c("Car", "Large Car and Truck", "Mini Car")
TRUCK_SUBSECTORS <- c("Heavy truck", "Medium truck", "Light truck")

#' Transport CO2 emissions by mode (MtCO2), aviation combined
transport_co2_by_mode <- function() {
  gq("CO2 emissions by subsector") %>%
    filter(subsector %in% names(TRN_MODE_MAP)) %>%
    mutate(mode = sub("^(Domestic|International) Aviation$", "Aviation", TRN_MODE_MAP[subsector])) %>%
    group_by(scenario, mode, year) %>%
    summarise(value = sum(value) * C_TO_CO2, .groups = "drop") %>%
    group_by(scenario, year) %>%
    mutate(share = 100 * value / sum(value)) %>%
    ungroup() %>%
    mutate(Units = "MtCO2")
}

# ---- External inputs (not held in the scenario database) ----------------------

read_external <- function(file) {
  utils::read.csv(file.path(REPO_ROOT, "data", "external", file),
                  check.names = FALSE, stringsAsFactors = FALSE)
}

#' Decomposition of the 2060 No Policy -> NZ 2060 GHG gap by measure (MtCO2e).
#' Derived from single-measure sensitivity runs that are not part of ksa_nz.dat.
ghg_decomposition <- function() read_external("ghg_reduction_decomposition_2060.csv")

#' Cooling demand (billion tons of cooling) — building service output, not in ksa_nz.dat
cooling_demand <- function() read_external("cooling_demand.csv")

#' Electricity used for space cooling (TWh), No Policy vs NZ 2060
cooling_electricity <- function() {
  read_external("cooling_electricity_consumption.csv") %>%
    mutate(scenario = factor(scenario, levels = SCENARIO_LEVELS))
}

#' Installed capacity (GW) and new-capacity investment (billion 2020 USD);
#' post-processed from capacity factors and capital costs outside the database
power_capacity_investment <- function() {
  read_external("power_capacity_investment.csv") %>%
    mutate(scenario = recode(scenario, `Current Policies` = "Current Policy"),
           technology = recode(technology, Geothermal = "Other RE"))
}
