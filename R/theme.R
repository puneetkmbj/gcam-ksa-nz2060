# ------------------------------------------------------------------------------
# theme.R — common ggplot theme and colour palettes
# ------------------------------------------------------------------------------

theme_ksa <- function(base_size = 11, legend_position = "bottom") {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey88", linewidth = 0.25),
      panel.border      = element_rect(colour = "grey30", fill = NA, linewidth = 0.4),
      strip.background  = element_rect(fill = "grey90", colour = "grey30", linewidth = 0.4),
      strip.text        = element_text(size = base_size + 1, margin = margin(3, 3, 3, 3)),
      plot.title        = element_text(size = base_size + 2, hjust = 0.5),
      axis.title.y      = element_text(margin = margin(r = 8)),
      axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.title      = element_blank(),
      legend.position   = legend_position,
      plot.margin       = margin(8, 10, 8, 8)
    )
}

YEARS_5 <- seq(2015, 2060, by = 5)
scale_x_years <- function(from = 2015) {
  scale_x_continuous(limits = c(from, 2060), breaks = seq(from, 2060, by = 5),
                     expand = expansion(mult = 0.01))
}

PAL_SCENARIO <- c(
  "No Policy"      = "#000000",
  "Current Policy" = "#708090",
  "NDCs Continued" = "#FF8C00",
  "NZ 2060"        = "#006400"
)

PAL_ELEC_TECH <- c(
  "Other RE" = "#6B8E23", "Nuclear" = "#FF00FF", "Storage" = "#8B4513",
  "Wind" = "#1E90FF", "PV" = "#FFD700", "CSP" = "#FFA500",
  "Liquid Fuels" = "#708090", "Gas" = "#FA8072", "Gas_CCS" = "#FFA07A"
)

PAL_FE_SECTOR <- c(
  "Buildings" = "#FF8C00", "Direct Air Capture" = "#00BFFF",
  "Industry" = "#B0C4DE", "Transport" = "#8A2BE2"
)

PAL_FE_FUEL <- c(
  "Natural Gas - CDR" = "#00BFFF", "Electricity - CDR" = "#87CEEB",
  "Biomass" = "#008000", "Liquid Fuels" = "#708090", "Hydrogen" = "#7B68EE",
  "Natural Gas" = "#FA8072", "Electricity" = "#FEE12B"
)

PAL_BLD_FUEL <- c("Electricity" = "#FEE12B", "LPG/Liquid Fuels" = "#708090",
                  "Biomass" = "#008000")

PAL_BLD_TYPE <- c("Residential" = "#1F6FB5", "Commercial" = "#E31A1C")

PAL_BLD_GHG <- c("CO2" = "#E8706B", "F-Gases" = "#26B5C0")

PAL_IND_SECTOR <- c(
  "Cement" = "#708090", "Chemical" = "#F7988F", "Fertilizer" = "#25A9E0",
  "Heat (DAC)" = "#DAB4C7", "Iron and Steel" = "#C0C6C7",
  "Other Industry" = "#A0237C", "Refining" = "#8B4513"
)

# ordered top-of-stack first (ggplot stacks the first level on top)
PAL_IND_FUEL <- c(
  "Electricity" = "#3C8D3C", "Hydrogen" = "#BCCFB4", "Natural gas CCS" = "#6FA35A",
  "Natural gas" = "#7EC0D6", "Oil CCS" = "#F4A6A6", "Oil" = "#2B3A24"
)

PAL_CEMENT <- c("Cement" = "#3C8D3C", "Cement (w/CCS)" = "#BCCFB4")

PAL_STEEL <- c(
  "EAF with DRI (Hydrogen-based)" = "#2B3A24", "EAF with DRI (w/CCS)" = "#E3B8D0",
  "EAF with DRI" = "#3C8D3C", "EAF with scrap" = "#BCCFB4"
)

PAL_DAC_FUEL <- c("Natural gas" = "#BCCFB4", "Electricity" = "#3C8D3C")

PAL_TRN_MODE <- c(
  "Domestic Aviation" = "#00BFFF", "International Aviation" = "#1E90FF",
  "Aviation" = "#00BFFF", "Four Wheelers" = "#A9A9A9", "Other LDVs" = "#F08080",
  "Bus" = "#3CB371", "Passenger Rail" = "#EF8E27", "Freight Rail" = "#F7988F",
  "Trucks" = "#D3D3D3", "International Shipping" = "#6495ED"
)

PAL_TRN_TECH <- c("Zero Emission" = "#0188A7", "ICE Hybrid" = "#DAB4C7", "ICE" = "#787878")
