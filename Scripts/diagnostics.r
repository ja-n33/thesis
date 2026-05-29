##############################################################################################################################################################################

############################################################################## Run Diagnostics ##############################################################################

##############################################################################################################################################################################


    pacman::p_load(dplyr, tidyr, econdatar, ggplot2, showtext, tseries, urca, kableExtra)

#### 
## Graph Theme
####


# Load a serif font (matches academic typesetting)
font_add_google("Source Serif 4", "source_serif")
font_add_google("Source Sans 3", "source_sans")
showtext_auto()

# ── Palette ────────────────────────────────────────────────────────────────────
# Muted, print-safe colours that distinguish well in greyscale too
erpt_palette <- c(
  "oilshock"    = "#27AE60",   # muted green
  "outputgap_bci" = "#C0392B", # muted red
  "usdzar_fred"      = "#2C3E50",   # near-black navy
  "ppi_manuf"   = "#8E44AD",   # muted purple
  "cpi"         = "#D35400"    # burnt orange
)

# ── Theme ──────────────────────────────────────────────────────────────────────
theme_publication <- function(base_size = 11, base_family = "source_serif") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Panel
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "grey20", linewidth = 0.4),
      panel.grid.major  = element_line(colour = "grey88", linewidth = 0.3),
      panel.grid.minor  = element_blank(),

      # Axes
      axis.line         = element_blank(),           # border handles this
      axis.ticks        = element_line(colour = "grey20", linewidth = 0.3),
      axis.ticks.length = unit(3, "pt"),
      axis.text         = element_text(size = rel(0.85), colour = "grey20",
                                       family = "source_sans"),
      axis.title        = element_text(size = rel(0.90), colour = "grey10",
                                       family = "source_sans",
                                       margin = margin(t = 4, r = 4)),

      # Legend
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA),
      legend.key.width  = unit(18, "pt"),
      legend.key.height = unit(9, "pt"),
      legend.text       = element_text(size = rel(0.80), family = "source_sans",
                                       colour = "grey20"),
      legend.title      = element_text(size = rel(0.85), family = "source_sans",
                                       face = "bold", colour = "grey10"),
      legend.position   = "bottom",
      legend.margin     = margin(t = 4),
      legend.spacing.x  = unit(6, "pt"),

      # Strip (for facets)
      strip.background  = element_rect(fill = "grey93", colour = "grey20",
                                       linewidth = 0.4),
      strip.text        = element_text(size = rel(0.85), family = "source_sans",
                                       face = "bold", colour = "grey10",
                                       margin = margin(3, 6, 3, 6)),

      # Titles
      plot.title        = element_text(size = rel(1.3), face = "bold",
                                       family = "source_serif", colour = "grey10",
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(size = rel(1), family = "source_sans",
                                       colour = "grey30",
                                       margin = margin(b = 6)),
      plot.caption      = element_text(size = rel(1), family = "source_sans",
                                       colour = "grey40", hjust = 0,
                                       margin = margin(t = 6)),
      plot.margin       = margin(12, 14, 10, 12),
      plot.background   = element_rect(fill = "white", colour = NA),

      complete = TRUE
    )
}



full <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
       mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) %>%
      filter(time_period <= as.Date("2025-06-01"), time_period >= as.Date("1990-02-01")) 

full_filtered <- full %>% filter(time_period < as.Date("2023-01-01"))

short <- full_filtered %>% filter(time_period < as.Date("2013-01-01"))


####
## Show the 3 measures of output gap to support one another



output_long <- outputgap %>%
    pivot_longer(-time_period, names_to = "type", values_to = "value") %>%
    group_by(type) %>%
    mutate(value_n = scale(value))


##NEER from SARB
neer <- readr::read_csv(here::here("data", "neer.csv"), skip = 2) %>%
    mutate(time_period = lubridate::my(Date),
    neer_sarb = log(Value) - log(lag(Value, n = 1))) %>%
    select(-Date) %>%
    filter(time_period >= as.Date("1992-01-01") & time_period <= as.Date("2026-03-01"))
View(neer)
full_corr <- readr::read_csv(here::here("data", "samples", "fullsample.csv"))  %>%
    filter(time_period >= as.Date(2013)) %>%
  select(outputgap_bci, outputgap_cl, outputgap_dc) %>%
  rename("SARB BCI" = "outputgap_bci",
        "Denton-Cholette" = "outputgap_dc",
        "Chow-Lin" = "outputgap_cl"
  ) %>%
  cor(use = "complete.obs")
print(full_corr)

full_corr_diag <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
  select(time_period, outputgap_bci, outputgap_cl, outputgap_dc) %>%
  filter(time_period <= as.Date("2025-06-01")) %>%
  mutate(period = if_else(time_period <= as.Date("2008-10-01"), 
                          "historical", "new")) %>%
  group_by(period) %>%
  summarise(
    mean_bci = mean(outputgap_bci, na.rm = TRUE) / 100,
    mean_cl  = mean(outputgap_cl,  na.rm = TRUE),
    mean_dc  = mean(outputgap_dc,  na.rm = TRUE),
    sd_bci   = sd(outputgap_bci,   na.rm = TRUE) / 100,
    sd_cl    = sd(outputgap_cl,    na.rm = TRUE),
    sd_dc    = sd(outputgap_dc,    na.rm = TRUE),
    cor_bci_dc = cor(outputgap_bci, outputgap_dc, use = "complete.obs")
  )
    
full_corr_diag <- full_corr_diag %>%
  mutate(
    Period = tools::toTitleCase(period),
    `SARB BCI` = paste0(
      formatC(mean_bci, format = "f", digits = 4),
      "\n(",
      formatC(sd_bci, format = "f", digits = 4),
      ")"
    ),
    `Chow-Lin` = paste0(
      formatC(mean_cl, format = "f", digits = 4),
      "\n(",
      formatC(sd_cl, format = "f", digits = 4),
      ")"
    ),
    `Denton-Cholette` = paste0(
      formatC(mean_dc, format = "f", digits = 4),
      "\n(",
      formatC(sd_dc, format = "f", digits = 4),
      ")"
    ),
    `BCI $\\times$ DC` = formatC(cor_bci_dc, format = "f", digits = 3)
  ) %>%
  select(Period, `SARB BCI`, `Chow-Lin`, `Denton-Cholette`, `BCI $\\times$ DC`) %>%
  kbl(
    booktabs = TRUE,
    escape   = FALSE,
    align    = c("l", "c", "c", "c", "c"),
    caption  = "Output Gap Proxy Comparison by Subsample"
  ) %>%
  kable_styling(
    latex_options     = c("hold_position"),
    bootstrap_options = c("striped", "hover"),  # HTML only
    full_width        = FALSE,
    position          = "center"
  ) %>%
  add_header_above(
    c(" " = 1, "Mean (SD)" = 3, " " = 1),
    bold   = TRUE,
    line   = TRUE,
    escape = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(
    general         = "HP-filtered output gap proxies ($\\lambda = 14{,}400$, monthly). Standard deviations in parentheses. Sample split at 2008-10. BCI $\\times$ CL denotes the Pearson correlation between the SARB BCI and Chow-Lin measures within each subsample.",
    general_title   = "\\textit{Notes:}",
    escape          = FALSE,
    threeparttable  = TRUE
  )

print(full_corr_diag)




sarb_gdp <- read_dataset(id = "QB_NATLACC", 
                        series_key = "KBP6006D.Q.R.S.LA") %>%
                        as_tibble() %>%
                        rename(gdp = "KBP6006D.Q.R.S.LA") %>%
                        mutate(lgdp = log(gdp),
           detrended_gdp = gdp - lag(gdp)) %>%
    filter(time_period >= as.Date("1992-01-01") & 
           time_period <= as.Date("2026-02-01")) %>%
           mutate(gdp_cycle = mFilter::hpfilter(lgdp, freq = 1600)$cycle) 
            
              

bci <- readxl::read_excel(here::here("data", "BCI DATA", "BCI_April_2026.xls")) %>%
    rename(time_period = "...1", 
            leading = "Leading\n indicator", 
            lagging = "Lagging\n indicator",
            coincident = "Coincident\n indicator") %>%
    filter(!is.na(time_period)) %>%
    mutate(time_period = lubridate::floor_date(lubridate::as_date(time_period), "month"),
    leading_lagged = lag(leading, n = 1), 
    coincident_lagged = lag(coincident, n = 1),
    across(c(leading, lagging, coincident, leading_lagged, coincident_lagged), as.numeric), 
    leading_detrended = leading - leading_lagged,
    coincident_detrended = coincident - coincident_lagged) %>%
    filter(time_period >= lubridate::ymd("1992-01-01") & time_period <= lubridate::ymd("2026-01-28")) %>%
    group_by(time_period)

sarb_gdp <- full_join(sarb_gdp, bci %>% select(time_period, coincident), by = "time_period")

gdp_ts <- ts(sarb_gdp$lgdp, 
             start = c(1992, 1), 
             frequency = 4)
bci_ts <- ts(sarb_gdp$coincident_detrended, 
             start = c(1992, 1), 
             frequency = 12)

fit <- tempdisagg::td(gdp_ts ~ bci_ts, method = "chow-lin-maxlog")
summary(fit)

View(sarb_gdp)
model_cl <- tempdisagg::td(sarb_gdp_ts ~ 1, 
    to = 12,
     method = "denton-cholette")


####

## Plot All Variables for each Sample

shock_cor <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
    mutate(shock_lag = lag(full$oilshock)) %>%
    select(oilshock, shock_lag) %>%
    cor(use = "complete.obs")
print(full)
####

samples <- c("new", "historical", "full")

df_wide <- readr::read_csv(here::here("data", "samples", "newsample.csv")) %>%
    select(-c("...1", "uvi2", "uvi34", "uvi5"))


for (sample in samples){
        file <- paste0(sample, "sample.csv")
        path <- here::here("descriptives", sample)
        Sample <- tools::toTitleCase(sample)
    name <- "stationarity.png"
    df <- readr::read_csv(here::here("data", "samples", file)) %>%
        select(-any_of(c("m_all", "m_manuf", "...1", "uvi2", "uvi34", "uvi5", "value", "Value", "rate"))) %>%
        mutate(outputgap_bci = outputgap_bci / 100, 
                oilshock = oilshock / 100) %>%
        pivot_longer(-time_period, names_to = "type", values_to = "value") %>%
        mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) 

    plot <- ggplot(data = df %>% filter(type %in% c("oilshock", "outputgap_bci", "usdzar_fred", "ppi_manuf", "cpi"), time_period <= as.Date("2025-06-01")), aes(x = time_period, y = value, colour = type)) +
    geom_line(linewidth = 0.6, alpha = 0.75) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.01)) +
    scale_colour_manual(values = erpt_palette, labels = c("oilshock" = "Oil Shock ", "outputgap_bci" = "Demand (SARB BCI)", "usdzar_fred" = "Exchange Rate", "ppi_manuf" = "PPI", "cpi" = "CPI")) +
    scale_y_continuous(expand = expansion(mult = 0.03)) +
    labs(
        x        = NULL,
        y        = NULL,
        colour   = NULL,
        title    = paste0("Variable Time Series — ", Sample, " sample"),
        caption  = "Sources: FRED, SARB, Stats SA, Känzig (2021)."
    ) +
    theme_publication()
    
  ggsave(filename = paste0("stationarity_", Sample,".png"), 
         plot     = plot, 
         path     = path,
         width    = 8, 
         height   = 5, 
         dpi      = 300)

    plot_prices <- ggplot(data = df %>% filter(type %in% c("usdzar_fred", "ppi_manuf", "cpi"), time_period <= as.Date("2025-06-01")), aes(x = time_period, y = value, colour = type)) +
    geom_line(linewidth = 0.6, alpha = 0.75) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.01)) +
    scale_colour_manual(values = erpt_palette, labels = c("usdzar_fred" = "Exchange Rate", "ppi_manuf" = "PPI", "cpi" = "CPI")) +
    scale_y_continuous(expand = expansion(mult = 0.03)) +
    labs(
        x        = NULL,
        y        = NULL,
        colour   = NULL,
        title    = paste0("Exchange Rate vs Price Shocks — ", Sample, " sample"),
        caption  = "Sources: FRED, Stats SA."
    ) +
    theme_publication()
    
  ggsave(filename = paste0("xr-prices_", Sample,".png"), 
         plot     = plot_prices, 
         path     = path,
         width    = 8, 
         height   = 5, 
         dpi      = 300)

    
}

########
## Check for uncorrelated shocks
#########


df_var <- full_filtered %>%
    select(oilshock, outputgap_dc, usdzar_fred, m, ppi, cpi) %>%
    as.data.frame()
n_lags <- 12

var_model <- do.call(VAR, list(y = df_var, p = as.integer(n_lags), type = "const"))
resids <- cbind(residuals(var_model), oilshock = tail(full$oilshock, nrow(residuals(var_model))))

residcor <- cor(resids) %>%
        as.data.frame() 
View(residcor)

openxlsx::write.xlsx(residcor, 
           file      = here::here("Tables", "residcor.xlsx"),
           sheetName = "Residuals",
           rowNames  = FALSE)

        

lags <- c(1:24)
print(lags)        
        


aic_var_df <- tibble(Lag = as.numeric(), 
                    AIC = as.numeric(), 
                    BIC = as.numeric())

for (lag in lags){

    oil_exog <- sapply(0:lag, function(l) dplyr::lag(df_var$oilshock, l))
    colnames(oil_exog) <- paste0("oil.l", 0:lag)
    
    var_model <- do.call(VAR, list(y = df_var %>% dplyr::select(-oilshock), p = as.integer(lag), type = "const", exogen = df_var %>% dplyr::select(oilshock) %>% as.matrix()))

    AIC <- as.numeric(AIC(var_model))
    Lag <- as.numeric(lag)
    BIC <- as.numeric(BIC(var_model))

    temp <- tibble(Lag = Lag, AIC = AIC, BIC = BIC)

    aic_var_df <- rbind(aic_var_df, temp)
}

print(aic_var_df)
View(aic_var_df)
View(residcor)

info <- VARselect(
    df_var %>% dplyr::select(-oilshock), 
    lag.max = 24, 
    type = "const",
    exogen = df_var %>% dplyr::select(oilshock) %>% as.matrix()
)

aic_var_df <- tibble(
    Lag = 1:24,
    AIC = info$criteria["AIC(n)", ]
)



cor(full$m_hist, full$uvi34, use = "complete.obs")





full <- full %>%
    arrange(time_period) %>%
    filter(time_period <= as.Date("2022-12-01"))


##############################################################################################################################################################################

################################################################################ Stationarity ################################################################################

##############################################################################################################################################################################


vars <- c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi")

export_adf_results <- function(data, vars, lags = 12,
                                outfile = here::here("Tables", "adf_results.xlsx")) {
  library(urca)
  library(openxlsx)
  
  specs <- c("trend", "drift", "none")
  tau_names <- c(trend = "tau3", drift = "tau2", none = "tau1")
  
  results <- lapply(vars, function(var) {
    row <- list(variable = var)
    for (spec in specs) {
      res  <- urca::ur.df(data[[var]], type = spec, lags = lags, selectlags = "AIC")
      s    <- summary(res)
      tau  <- tau_names[spec]
      stat <- s@teststat[1, tau]
      cv   <- s@cval[tau, ]
      row[[paste0(spec, "_stat")]]   <- round(stat, 4)
      row[[paste0(spec, "_cv1")]]    <- cv["1pct"]
      row[[paste0(spec, "_cv5")]]    <- cv["5pct"]
      row[[paste0(spec, "_cv10")]]   <- cv["10pct"]
      row[[paste0(spec, "_reject")]] <- ifelse(stat < cv["5pct"], "Yes", "No")
    }
    as.data.frame(row)
  })
  
  df <- do.call(rbind, results)
  write.xlsx(df, outfile, overwrite = TRUE)
  message("Saved: ", outfile)
}

export_adf_results(
  data = full_filtered,
  vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi"),
  lags = 12,
  outfile = here::here("Tables", "adf_results.xlsx")
)

View(full)



descriptive_table <- function(data, vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi")) {
  
  p1 <- data %>% filter(time_period >= as.Date("1990-02-01") & time_period <= as.Date("2008-12-01"))
  p2 <- data %>% filter(time_period >= as.Date("2009-01-01") & time_period <= as.Date("2022-12-01"))

  
  fmt <- function(x) paste0(round(mean(x, na.rm = TRUE), 3), " (", round(sd(x, na.rm = TRUE), 3), ")")
  
  rows <- lapply(vars, function(var) {
    tibble(
      Variable    = var,
      Full_Sample = fmt(data[[var]]),
      `1990-2008` = fmt(p1[[var]]),
      `2009-2023` = fmt(p2[[var]])
    )
  })
  
  bind_rows(rows)
}

desc <- descriptive_table(data = full_filtered)
View(desc)
writexl::write_xlsx(desc, path = here::here("Tables", "descriptives.xlsx"))


##############################################################################################################################################################################

################################################################################ Normality ###################################################################################

##############################################################################################################################################################################

normality_test <- function(data, vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi")) {
    table <- tibble()
    for (var in vars){
        jb <- tseries::jarque.bera.test(data[[var]])

        if (as.numeric(jb$p.value) < 0.001){
            jbp <- "p < 0.01"
        } else{
            jbp <- round(jb$p.value, 3)
        }
        row <- tibble(
        name = var,
        skewness = round(moments::skewness(data[[var]]), 3),
        kurtosis = round(moments::kurtosis(data[[var]]), 3),
        jbstat = round(jb$statistic, 3),
        jbp = jbp)
    table <- bind_rows(table, row)
    }
    return(table)
}

norm <- normality_test(data = full_filtered)


writexl::write_xlsx(norm, path = here::here("Tables", "normality.xlsx"))
