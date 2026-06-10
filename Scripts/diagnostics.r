##############################################################################################################################################################################

############################################################################## Run Diagnostics ##############################################################################

##############################################################################################################################################################################


    pacman::p_load(dplyr, tidyr, econdatar, ggplot2, showtext, tseries, urca, kableExtra)
library(showtext)
library(sysfonts)


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
    "oilshock"      = "#E69F00",   # amber
    "outputgap_bci" = "#56B4E9",   # sky blue
    "outputgap_dc"  = "#56B4E9",   # sky blue (same concept)
    "usdzar_fred"   = "#000000",   # black
    "neer_sarb"     = "#000000",   # black
    "m"             = "#009E73",   # teal green
    "ppi"           = "#0072B2",   # deep blue
    "ppi_manuf"     = "#0072B2",   # deep blue
    "cpi"           = "#D55E00",    # vermillion
    "cpi_adj"           = "#D55E00" ,
    "int_eff" = "purple"
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
      plot.title        = element_text(size = rel(1.5), face = "bold",
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



# full <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
#       mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) %>%
#     filter(time_period <= as.Date("2025-06-01"), time_period >= as.Date("1990-02-01")) 

# full_filtered <- full %>% filter(time_period < as.Date("2023-01-01"))

# short <- full_filtered %>% filter(time_period < as.Date("2013-01-01"))


####
# ## Show the 3 measures of output gap to support one another
####


# output_long <- outputgap %>%
#   pivot_longer(-time_period, names_to = "type", values_to = "value") %>%
#   group_by(type) %>%
#   mutate(value_n = scale(value))


# ##NEER from SARB
# neer <- readr::read_csv(here::here("data", "neer.csv"), skip = 2) %>%
#   mutate(time_period = lubridate::my(Date),
#   neer_sarb = log(Value) - log(lag(Value, n = 1))) %>%
#   select(-Date) %>%
#   filter(time_period >= as.Date("1992-01-01") & time_period <= as.Date("2026-03-01"))
# View(neer)
# full_corr <- readr::read_csv(here::here("data", "samples", "fullsample.csv"))  %>%
#   filter(time_period >= as.Date(2013)) %>%
# select(outputgap_bci, outputgap_cl, outputgap_dc) %>%
# rename("SARB BCI" = "outputgap_bci",
#       "Denton-Cholette" = "outputgap_dc",
#       "Chow-Lin" = "outputgap_cl"
# ) %>%
# cor(use = "complete.obs")
# print(full_corr)

# full_corr_diag <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
# select(time_period, outputgap_bci, outputgap_cl, outputgap_dc) %>%
# filter(time_period <= as.Date("2022-12-01")) %>%
# mutate(period = if_else(time_period <= as.Date("2010-01-01"), 
#                         "historical", "new")) %>%
# group_by(period) %>%
# summarise(
#   mean_bci = mean(outputgap_bci, na.rm = TRUE) / 100,
#   mean_cl  = mean(outputgap_cl,  na.rm = TRUE),
#   mean_dc  = mean(outputgap_dc,  na.rm = TRUE),
#   sd_bci   = sd(outputgap_bci,   na.rm = TRUE) / 100,
#   sd_cl    = sd(outputgap_cl,    na.rm = TRUE),
#   sd_dc    = sd(outputgap_dc,    na.rm = TRUE),
#   cor_bci_dc = cor(outputgap_bci, outputgap_dc, use = "complete.obs")
# )
  
# full_corr_diag <- full_corr_diag %>%
# mutate(
#   Period = tools::toTitleCase(period),
#   `SARB BCI` = paste0(
#     formatC(mean_bci, format = "f", digits = 4),
#     "\n(",
#     formatC(sd_bci, format = "f", digits = 4),
#     ")"
#   ),
#   `Chow-Lin` = paste0(
#     formatC(mean_cl, format = "f", digits = 4),
#     "\n(",
#     formatC(sd_cl, format = "f", digits = 4),
#     ")"
#   ),
#   `Denton-Cholette` = paste0(
#     formatC(mean_dc, format = "f", digits = 4),
#     "\n(",
#     formatC(sd_dc, format = "f", digits = 4),
#     ")"
#   ),
#   `BCI $\\times$ DC` = formatC(cor_bci_dc, format = "f", digits = 3)
# ) %>%
# select(Period, `SARB BCI`, `Chow-Lin`, `Denton-Cholette`, `BCI $\\times$ DC`) %>%
# kbl(
#   booktabs = TRUE,
#   escape   = FALSE,
#   align    = c("l", "c", "c", "c", "c"),
#   caption  = "Output Gap Proxy Comparison by Subsample"
# ) %>%
# kable_styling(
#   latex_options     = c("hold_position"),
#   bootstrap_options = c("striped", "hover"),  # HTML only
#   full_width        = FALSE,
#   position          = "center"
# ) %>%
# add_header_above(
#   c(" " = 1, "Mean (SD)" = 3, " " = 1),
#   bold   = TRUE,
#   line   = TRUE,
#   escape = FALSE
# ) %>%
# row_spec(0, bold = TRUE) %>%
# footnote(
#   general         = "HP-filtered output gap proxies ($\\lambda = 14{,}400$, monthly). Standard deviations in parentheses. Sample split at 2008-10. BCI $\\times$ CL denotes the Pearson correlation between the SARB BCI and Chow-Lin measures within each subsample.",
#   general_title   = "\\textit{Notes:}",
#   escape          = FALSE,
#   threeparttable  = TRUE
# )

# print(full_corr_diag)




# sarb_gdp <- read_dataset(id = "QB_NATLACC", 
#                       series_key = "KBP6006D.Q.R.S.LA") %>%
#                       as_tibble() %>%
#                       rename(gdp = "KBP6006D.Q.R.S.LA") %>%
#                       mutate(lgdp = log(gdp),
#           detrended_gdp = gdp - lag(gdp)) %>%
#   filter(time_period >= as.Date("1992-01-01") & 
#           time_period <= as.Date("2026-02-01")) %>%
#           mutate(gdp_cycle = mFilter::hpfilter(lgdp, freq = 1600)$cycle) 
          
            

# bci <- readxl::read_excel(here::here("data", "BCI DATA", "BCI_April_2026.xls")) %>%
#   rename(time_period = "...1", 
#           leading = "Leading\n indicator", 
#           lagging = "Lagging\n indicator",
#           coincident = "Coincident\n indicator") %>%
#   filter(!is.na(time_period)) %>%
#   mutate(time_period = lubridate::floor_date(lubridate::as_date(time_period), "month"),
#   leading_lagged = lag(leading, n = 1), 
#   coincident_lagged = lag(coincident, n = 1),
#   across(c(leading, lagging, coincident, leading_lagged, coincident_lagged), as.numeric), 
#   leading_detrended = leading - leading_lagged,
#   coincident_detrended = coincident - coincident_lagged) %>%
#   filter(time_period >= lubridate::ymd("1992-01-01") & time_period <= lubridate::ymd("2026-01-28")) %>%
#   group_by(time_period)

# sarb_gdp <- full_join(sarb_gdp, bci %>% select(time_period, coincident), by = "time_period")

# gdp_ts <- ts(sarb_gdp$lgdp, 
#             start = c(1992, 1), 
#             frequency = 4)
# bci_ts <- ts(sarb_gdp$coincident_detrended, 
#             start = c(1992, 1), 
#             frequency = 12)

# fit <- tempdisagg::td(gdp_ts ~ bci_ts, method = "chow-lin-maxlog")
# summary(fit)

# View(sarb_gdp)
# model_cl <- tempdisagg::td(sarb_gdp_ts ~ 1, 
#   to = 12,
#     method = "denton-cholette")


# ####

# ## Plot All Variables for each Sample

# shock_cor <- full_filtered %>%
#   mutate(shock_lag = lag(full$oilshock)) %>%
#   select(oilshock, shock_lag) %>%
#   cor(use = "complete.obs")
# print(full)
# ####

# samples <- c("new", "historical", "full")

# df_wide <- readr::read_csv(here::here("data", "samples", "newsample.csv")) %>%
#   select(-c("...1", "uvi2", "uvi34", "uvi5"))


##############################################################################################################################################################################



################################################################################ BEGIN HERE ################################################################################



##############################################################################################################################################################################







full <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
       mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) %>%
      filter(time_period <= as.Date("2025-06-01"), time_period >= as.Date("1990-02-01")) 


full_filtered <- full %>% filter(time_period < as.Date("2023-01-01"))


low_inflation <- full_filtered %>% filter(time_period >= as.Date("2010-02-01"))


early <- full_filtered %>% filter(time_period < as.Date("2010-02-01"))


##############################################################################################################################################################################

################################################################################ INTERPOLATION ################################################################################

##############################################################################################################################################################################



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

sarb_gdp <- full_join(sarb_gdp, bci %>% select(time_period, coincident, coincident_detrended), by = "time_period")

gdp_ts <- ts(sarb_gdp$lgdp, 
            start = c(1992, 1), 
            frequency = 4)
View(sarb_gdp)

bci_ts <- ts(sarb_gdp$coincident.x, 
            start = c(1992, 1), 
            frequency = 12)

bci_ts_detrended <- ts(sarb_gdp$coincident_detrended, 
            start = c(1992, 1), 
            frequency = 12)

gdp_ts_detrended <- ts(sarb_gdp$detrended_gdp, 
            start = c(1992, 1), 
            frequency = 4)

fit <- tempdisagg::td(gdp_ts ~ bci_ts, method = "chow-lin-maxlog")

fit_detrended <- tempdisagg::td(gdp_ts_detrended ~ bci_ts_detrended, method = "chow-lin-maxlog")

summary(fit)

summary(fit_detrended)

model_cl <- tempdisagg::td(sarb_gdp_ts ~ 1, 
  to = 12,
    method = "denton-cholette")

cl_coef <- as.data.frame(fit$coefficients) %>%
  tibble::rownames_to_column("Term") %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cl_coef %>%
    mutate(Term = gsub("_", "\\_", Term, fixed = TRUE)) %>%

  kable(
    format   = "latex",
    booktabs = TRUE,
    linesep  = "\\addlinespace",
    caption  = "Chow-Lin Temporal Disaggregation: GDP on BCI",
    label    = "tab:chow_lin",
    escape   = FALSE
  ) %>%
  kable_styling(
    latex_options = c("HOLD_position"),
    full_width    = FALSE,
    font_size     = 8
  ) %>%
  footnote(
    general           = paste0("{Note:} Rho = ", round(fit$rho, 4), 
                               ". R$^2$ = ", round(fit$r.squared, 4), "."),
    general_title     = "",
    footnote_as_chunk = FALSE,
    escape            = FALSE
  ) %>%
  writeLines(here::here("main", "chow_lin.txt"))

##############################################################################################################################################################################

################################################################################ ERROR CORRELATION ################################################################################

##############################################################################################################################################################################



lag <- 9
var_list <- c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi_adj")

df_var_full <- full_filtered %>%
  mutate(d_imp = ifelse(time_period == break_date, 1L, 0L)) %>%
  dplyr::select(time_period, all_of(var_list), d_imp) 
print(names(df_var_full))

oil_exog <- sapply(0:lag, function(l) dplyr::lag(df_var_full$oilshock, l))
colnames(oil_exog) <- paste0("oil.l", 0:lag)

exog_mat <- cbind(oil_exog, d_imp = df_var_full$d_imp)

valid_rows <- complete.cases(exog_mat)

keep <- df_var_full$time_period >= lubridate::ymd("1990-02-01")
exog_mat <- exog_mat[keep, ]
df_var <- df_var_full[keep, ] %>% dplyr::select(-time_period) %>% na.omit()


var_model <- do.call(VAR, list(
  y      = df_var %>% dplyr::select(-oilshock, -d_imp),
  p      = as.integer(lag),
  type   = "const",
  exogen = exog_mat
))

resids <- cbind(residuals(var_model), oilshock = tail(full$oilshock, nrow(residuals(var_model))))

 #oilshock = tail(df_var_full$oilshock[keep], nrow(resids)))
new_names <- c("Oil Shock", "Output Gap", "Exchange Rate", "Import Price Index", "Producer Price Index", "Consumer Price Index")
residcor <- cor(resids) %>%     
        as.data.frame() %>%
        setNames(new_names)
rownames(residcor) <- new_names
View(residcor)


residcor_txt <- residcor %>%
        kable(
          format = "latex",
          booktabs = TRUE,
          linesp = "", 
          digit = 3,
          caption = "Contemporaneous Correlation Between Errors"
        ) %>%
        kable_styling(
          latex_options = c("HOLD_position", "scale_down"),
          full_width    = TRUE,
          font_size     = 8
        )

writeLines(residcor_txt, here::here("main", "residcor_early.txt"))


plot_acf <- function(resids, n_lags = 24, file_name = "acf_resids.png"){
  
  var_labels <- c(
    "oilshock"          = "Oil Shock",
    "outputgap_dc" = "Output Gap",
    "neer_sarb"    = "Exchange Rate",
    "m"            = "Import Price Index",
    "ppi"          = "Producer Price Index",
    "cpi_adj"          = "Consumer Price Index"
  )

  acf_data <- lapply(colnames(resids), function(col){
    acf_obj <- acf(resids[, col], lag.max = n_lags, plot = FALSE)
    tibble(
      variable = var_labels[col],
      lag      = as.numeric(acf_obj$lag),
      acf      = as.numeric(acf_obj$acf)
    )
  }) %>% 
    bind_rows() %>%
    mutate(variable = factor(variable, levels = var_labels))

  ci <- qnorm(0.975) / sqrt(nrow(resids))

  acf_plot <- ggplot(acf_data, aes(x = lag, y = acf)) +
    geom_hline(yintercept =  0,       colour = "grey40", linewidth = 0.3) +
    geom_hline(yintercept =  c(-ci, ci), linetype = "dashed", 
               colour = "#C45E3E", linewidth = 0.4) +
    geom_segment(aes(xend = lag, yend = 0), 
                 colour = "#2C6E8A", linewidth = 0.5) +
    geom_point(size = 0.8, colour = "#2C6E8A") +
    facet_wrap(~variable, ncol = 3, scales = "free_y") +
    scale_x_continuous(breaks = seq(0, n_lags, by = 6)) +
    scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1)) +
    coord_cartesian(ylim = c(-0.3, 0.3)) +
    labs(
      x     = "Lag (months)",
      y     = "Autocorrelation",
      title  = NULL
    ) +
    theme_publication() +
    theme(
      strip.text    = element_text(size = 7, face = "bold"),
      panel.spacing = unit(0.8, "lines"),
      axis.text     = element_text(size = 6)
    )

  ggsave(
    filename = file_name,
    plot     = acf_plot,
    path     = file.path(here::here(), "main"),
    width    = 7,
    height   = 4,
    dpi      = 150,
    device   = "png"
  )

  return(acf_plot)
}

plot_acf(resids, n_lags = 24, file_name = "acf_lowinf.png")

cpi_box <- Box.test(resids[, "cpi"], lag = 12, type = "Ljung-Box") 

ljung_box_tbl <- tibble(
  Variable = "cpi",
  ) %>%
  mutate(
    lb_12 = purrr::map(Variable, ~ Box.test(resids[, .x], lag = 12, type = "Ljung-Box")),
    lb_24 = purrr::map(Variable, ~ Box.test(resids[, .x], lag = 24, type = "Ljung-Box")),
    `Stat (12)`    = purrr::map_dbl(lb_12, ~ round(.x$statistic, 3)),
    `p-value (12)` = purrr::map_chr(lb_12, ~ ifelse(.x$p.value < 0.001, "< 0.001", as.character(round(.x$p.value, 3)))),
    `Stat (24)`    = purrr::map_dbl(lb_24, ~ round(.x$statistic, 3)),
    `p-value (24)` = purrr::map_chr(lb_24, ~ ifelse(.x$p.value < 0.001, "< 0.001", as.character(round(.x$p.value, 3))))
  ) %>%
  select(-lb_12, -lb_24) %>%
  mutate(Variable = recode(Variable,
    "cpi"          = "Consumer Price Index"
  ))

lb_txt <- ljung_box_tbl %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    linesep  = "\\addlinespace",
    caption  = "Ljung-Box Tests for Residual Autocorrelation",
    label    = "tab:ljung_box",
    escape   = FALSE
  ) %>%
  add_header_above(
    c(" " = 1, "Lag 12" = 2, "Lag 24" = 2),
    bold = TRUE
  ) %>%
  kable_styling(
    latex_options = c("HOLD_position"),
    full_width    = TRUE,
    font_size     = 8
  ) %>%
  footnote(
    general           = "{Note:} Ljung-Box test for residual autocorrelation at lags 12 and 24.",
    general_title     = "",
    footnote_as_chunk = FALSE,
    escape            = FALSE
  )

writeLines(lb_txt, here::here("main", "ljung_box.txt"))

##############################################################################################################################################################################

################################################################################ AIC ################################################################################

##############################################################################################################################################################################




aic_var_df <- tibble(Lag = as.numeric(), 
                    AIC = as.numeric(), 
                    BIC = as.numeric())

View(full_filtered)

full <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
       mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) %>%
      filter(time_period <= as.Date("2022-12-01", format = "%Y-%m-%d"), time_period >= as.Date("1988-01-01", format = "%Y-%m-%d")) 

full_filtered <- full %>% filter(time_period < as.Date("2023-01-01", format = "%Y-%m-%d"))
var_list <- c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi_adj")


break_date <- as.Date("2010-02-01", format = "%Y-%m-%d")

df_var_full <- full_filtered %>%
  mutate(d_imp = ifelse(time_period == break_date, 1L, 0L)) %>%
  dplyr::select(time_period, all_of(var_list), d_imp) 
print(names(df_var_full))

aic_var_df <- tibble(Lag = integer(), AIC = numeric(), BIC = numeric())

for (lag in 1:24){
  oil_exog <- do.call(cbind, lapply(0:lag, function(l) dplyr::lag(df_var_full$oilshock, l)))
  colnames(oil_exog) <- paste0("oil.l", 0:lag)

  exog_mat_full <- cbind(oil_exog, d_imp = df_var_full$d_imp)

  keep     <- df_var_full$time_period >= lubridate::ymd("1990-02-01")
  df_var   <- df_var_full[keep, ] %>% dplyr::select(-time_period)
  exog_mat <- exog_mat_full[keep, ]

  complete <- complete.cases(df_var) & complete.cases(exog_mat)
  df_var   <- df_var[complete, ] %>% dplyr::select(-oilshock, -d_imp)
  exog_mat <- exog_mat[complete, , drop = FALSE]  # force matrix

  cat("lag:", lag, "| nrow df_var:", nrow(df_var), "| nrow exog_mat:", nrow(exog_mat), "\n")

  var_model <- do.call(VAR, list(
    y      = df_var,
    p      = as.integer(lag),
    type   = "const",
    exogen = exog_mat
  ))

  aic_var_df <- rbind(aic_var_df, tibble(
    Lag = lag,
    AIC = as.numeric(AIC(var_model)),
    BIC = as.numeric(BIC(var_model))
  ))
}


oil_exog <- sapply(0:lag, function(l) dplyr::lag(df_var_full$oilshock, l))
colnames(oil_exog) <- paste0("oil.l", 0:lag)

exog_mat <- cbind(oil_exog, d_imp = df_var_full$d_imp)
keep <- df_var_full$time_period >= lubridate::ymd("1990-02-01")
exog_mat <- exog_mat[keep, ]


mark_top3 <- function(x){
  ranks <- rank(x, ties.method = "first")
  stars <- case_when(
    ranks == 1 ~ paste0(round(x, 3), "***"),
    ranks == 2 ~ paste0(round(x, 3), "**"),
    ranks == 3 ~ paste0(round(x, 3), "*"),
    TRUE       ~ as.character(round(x, 3))
  )
  stars
}

lag_selection_txt <- aic_var_df %>%
  mutate(
    AIC = mark_top3(AIC),
    BIC = mark_top3(BIC)
  ) %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    linesep   = "",
    caption   = "VAR Lag Length Selection Criteria",
    label     = "lag_selection",
    escape    = FALSE,
    col.names = c("Lag", "AIC", "BIC")) %>%
    column_spec(1, width = "2cm") %>%
    column_spec(2, width = "4cm") %>%
    column_spec(3, width = "4cm") %>%
  kable_styling(
    latex_options = c("HOLD_position"),
    full_width    = FALSE,
    font_size     = 8
  ) %>%
  footnote(
    general           = "{Note:} AIC and BIC computed for VAR with oil shock as exogenous variable. $^{***}$ minimum, $^{**}$ second minimum, $^{*}$ third minimum.",
    general_title     = "",
    footnote_as_chunk = FALSE,
    escape            = FALSE
  )

writeLines(lag_selection_txt, here::here("main", "lag_selection.txt"))









full <- full %>%
    arrange(time_period) %>%
    filter(time_period <= as.Date("2022-12-01"))


##############################################################################################################################################################################

################################################################################ Stationarity ################################################################################

##############################################################################################################################################################################


vars <- c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi_adj", "int_eff")
export_adf_results <- function(data, vars, lags = 12,
                                outfile = here::here("main", "adf_results.txt")) {
  library(urca)
  library(knitr)
  library(kableExtra)
  
  specs     <- c("trend", "drift", "none")
  tau_names <- c(trend = "tau3", drift = "tau2", none = "tau1")
  spec_labels <- c(trend = "Trend", drift = "Drift", none = "Non-Zero Mean")
  
  results <- lapply(vars, function(var) {
    lapply(specs, function(spec) {
      res  <- urca::ur.df(data[[var]], type = spec, lags = lags, selectlags = "AIC")
      s    <- summary(res)
      tau  <- tau_names[spec]
      stat <- s@teststat[1, tau]
      cv   <- s@cval[tau, ]
      data.frame(
        Variable   = case_when(var == "oilshock" ~ "Oil Shock",
                                var == "outputgap_dc" ~ "Output Gap",
                                var == "neer_sarb" ~ "Exchange Rate",
                                var == "m" ~ "Importer Price Index",
                                var == "ppi" ~ "Producer Price Index",
                                var == "cpi_adj" ~ "Consumer Price Index",
                                var == "int_eff" ~ "Real Prime Rate"),
        Spec       = as.character(spec_labels[spec]),
        Statistic  = round(stat, 3),
        `1%`       = cv["1pct"],
        `5%`       = cv["5pct"],
        `10%`      = cv["10pct"],
        Reject     = ifelse(stat < cv["5pct"], "Yes", "No"),
        check.names = FALSE
      )
    }) %>% bind_rows()
  }) %>% bind_rows()

  tbl <- results %>%
    select(-Variable) %>%
    kable(
        format      = "latex",
        booktabs    = TRUE,
        linesep     = "",
        caption     = "Augmented Dickey-Fuller Unit Root Tests",
        label       = "adf",
        col.names   = c("Specification", "Statistic", "1\\%", "5\\%", "10\\%", "Reject"),
        escape      = FALSE,
        row.names = FALSE,
        table.envir = "table"
    ) %>%
    pack_rows("Oil Shock",             1, 3)  %>%
    pack_rows("Output Gap",                4, 6)  %>%
    pack_rows("Exchange Rate",         7, 9)  %>%
    pack_rows("Importer Price Index",   10, 12) %>%
    pack_rows("Producer Price Index", 13, 15) %>%
    pack_rows("Consumer Price Index", 16, 18) %>%
    pack_rows("Real Prime Rate", 19, 21) %>%
    row_spec(3,  hline_after = TRUE) %>%
    row_spec(6,  hline_after = TRUE) %>%
    row_spec(9,  hline_after = TRUE) %>%
    row_spec(12, hline_after = TRUE) %>%
    row_spec(15, hline_after = TRUE) %>%
    row_spec(18, hline_after = TRUE) %>%
    row_spec(21, hline_after = TRUE) %>%
    kable_styling(
        latex_options = c("HOLD_position"),
        font_size     = 8,
        full_width    = TRUE
    ) 
  tbl_text <- as.character(tbl)
  writeLines(tbl_text, outfile)
  message("Saved: ", outfile)
}

export_adf_results(
  data = full_filtered,
  vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi_adj", "int_eff"),
  lags = 9,
  outfile = here::here("main", "adf_results.txt")
)

View(full)



samples  <- c("full_filtered", "early", "low_inflation")
sample_names <- c("1990 - 2023", "1990 - 2010", "2010 - 2023")
stationarity <- list()


for (i in 1:length(samples)){
        sample <- samples[i]
        name <- sample_names[i]
        df <- get(sample)
        path <- here::here("main")
        Sample <- tools::toTitleCase(sample)
  
    df <- df %>%
        select(time_period, oilshock, outputgap_dc, neer_sarb, m, ppi, cpi_adj, int_eff) %>%
        mutate( 
                oilshock = oilshock / 100,
                int_eff = int_eff / 10) %>%
        pivot_longer(-time_period, names_to = "type", values_to = "value") %>%
        mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) 

    plot <- ggplot(data = df %>% filter(type %in% c("oilshock", "outputgap_dc", "neer_sarb", "m" ,"ppi", "cpi_adj", "int_eff"), time_period <= as.Date("2025-06-01")), aes(x = time_period, y = value, colour = type)) +
    geom_line(linewidth = 0.6, alpha = 0.75) +
    scale_x_date(date_breaks = "4 years", date_labels = "%Y", expand = expansion(mult = 0.01)) +
    scale_colour_manual(values = erpt_palette, labels = c("oilshock" = "Oil Shock ", "outputgap_dc" = "Output Gap", "neer_sarb" = "Exchange Rate", "m" = "Importer Price Index", "ppi" = "Producer Price Index", "cpi_adj" = "Consumer Price Index", "int_eff" = "Real Prime Rate")) +
    scale_y_continuous(expand = expansion(mult = 0.03)) +
    labs(
        x        = NULL,
        y        = NULL,
        colour   = NULL,
        title    = name
    ) +
    theme(
      text         = element_text(family = "source_serif"),
    plot.title   = element_text(family = "source_serif"),
    axis.text    = element_text(family = "source_serif"),
    legend.text  = element_text(family = "source_serif")
    ) +
    theme_publication(base_size = 11, base_family = "source_serif")
    
  ggsave(filename = paste0("stationarity_", sample,".png"), 
         plot     = plot, 
         path     = path,
         width    = 6.5, 
         height   = 4, 
         dpi      = 225)

         stationarity[[sample]] <- plot
}


combined_stationarity <- wrap_plots(stationarity, nrow = 3, ncol = 1) +
  plot_layout(guides = "collect", 
      axes = "collect", 
      heights = c(1, 1, 1),   # equal row heights — reduce total by adjusting ggsave height
       widths  = c(1, 1) ) +
  plot_annotation(
    theme = theme(
      plot.caption  = element_text(hjust = 0, size = 9, family = "source_sans"),
      legend.position = "bottom",
      legend.text  = element_text(family = "source_sans", size = rel(2.5)),
      legend.title  = element_text(family = "source_sans", ),
      text         = element_text(family = "source_sans"),  # catches everything
    plot.title   = element_text(family = "source_sans", size = rel(2.5)),
    legend.key.size = unit(2.5, "cm"),
    axis.text    = element_text(family = "source_sans"),
    axis.title   = element_text(family = "source_sans"),
    )
  ) &
  theme(
    text         = element_text(family = "source_sans"),  # catches everything
    plot.title   = element_text(family = "source_sans", size = rel(2.5)),
    axis.text    = element_text(family = "source_sans"),
    axis.title   = element_text(family = "source_sans"),
    legend.text  = element_text(family = "source_sans", size = rel(2.5)),
    legend.key.size = unit(2.5, "cm"),
    legend.title = element_text(family = "source_sans"),
    strip.text   = element_text(family = "source_sans"),
    plot.caption = element_text(family = "source_sans"),
    plot.margin  = margin(2, 6, 2, 4)
  )

ggsave(
    filename = "stationarity_combined.png",
    plot     = combined_stationarity,
    path     = here::here("main"),
    width    = 10,
    height   = 10,
    dpi      = 300,
    device   = "png"
)


##############################################################################################################################################################################

################################################################################ Descriptive Table ################################################################################

##############################################################################################################################################################################

descriptive_table <- function(data, 
vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi_adj", "int_eff")) {
  
  p1 <- data %>% filter(time_period >= as.Date("1990-02-01") & time_period <= as.Date("2010-01-01"))
  p2 <- data %>% filter(time_period >= as.Date("2010-02-01") & time_period <= as.Date("2022-12-01"))

  
  fmt <- function(x) paste0(round(mean(x, na.rm = TRUE), 3), " (", round(sd(x, na.rm = TRUE), 3), ")")
  
  desc_tbl <- tibble(Variable = character(),
                      `1990 - 2023` = character(),
                      `1990 - 2010` = character(),
                      `2010 - 2023` = character())
  for (var in vars){
      Variable   <- case_when(var == "oilshock" ~ "Oil Shock",
                                var == "outputgap_dc" ~ "Output Gap",
                                var == "neer_sarb" ~ "Exchange Rate",
                                var == "m" ~ "Importer Price Index",
                                var == "ppi" ~ "Producer Price Index",
                                var == "cpi_adj" ~ "Consumer Price Index", 
                                var == "int_eff" ~ "Real Prime Rate") 
      temp_row <- tibble(
  Variable      = as.character(Variable),
  `1990 - 2023` = as.character(fmt(data[[var]])),
  `1990 - 2010` = as.character(fmt(p1[[var]])),
  `2010 - 2023` = as.character(fmt(p2[[var]]))
)
    desc_tbl <- bind_rows(desc_tbl, temp_row)
  }


  desc_txt <- desc_tbl %>%
    kable(
      format   = "latex",
      booktabs = TRUE,
      linesep  = "\\addlinespace[6pt]",
      caption  = "Descriptive Statistics",
      label    = "desc_table"
    ) %>%
    kable_styling(
      latex_options = c("HOLD_position"),
      full_width    = TRUE,
      font_size     = 8
    ) %>%
    footnote(
      general           = "{Note:} Mean (standard deviation). Sample splits at 2010-02-01.",
      general_title     = "",
      footnote_as_chunk = FALSE,
      escape            = FALSE
    )

  writeLines(desc_txt, here::here("main", "desc_tbl.txt"))
writeLines(desc_txt, here::here("main", "desc_tbl.txt"))
}

desc <- descriptive_table(data = full_filtered, vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi_adj", "int_eff"))
View(desc)
writexl::write_xlsx(desc, path = here::here("Tables", "descriptives.xlsx"))


##############################################################################################################################################################################

################################################################################ Normality ###################################################################################

##############################################################################################################################################################################

normality_test <- function(data, vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi_adj", "int_eff")) {
  table <- tibble()
  for (var in vars){
    jb <- tseries::jarque.bera.test(na.omit(data[[var]]))

    jbp <- ifelse(as.numeric(jb$p.value) < 0.001, "p < 0.001", as.character(round(jb$p.value, 3)))

    row <- tibble(
      Variable = case_when(
        var == "oilshock"     ~ "Oil Shock",
        var == "outputgap_dc" ~ "Output Gap",
        var == "neer_sarb"    ~ "Exchange Rate",
        var == "m"            ~ "Import Price Index",
        var == "ppi"          ~ "Producer Price Index",
        var == "cpi_adj"          ~ "Consumer Price Index",
        var == "int_eff" ~ "Real Prime Rate",
        TRUE                  ~ var
      ),
      Skewness  = round(moments::skewness(data[[var]], na.rm = TRUE), 3),
      Kurtosis  = round(moments::kurtosis(data[[var]], na.rm = TRUE), 3),
      `JB Stat` = round(jb$statistic, 3),
      `p-value` = jbp
    )
    table <- bind_rows(table, row)
  }

  results_txt <- table %>%
    kable(
      format   = "latex",
      booktabs = TRUE,
      linesep  = "\\addlinespace",
      caption  = "Normality Tests",
      label    = "tab:normality"
    ) %>%
    kable_styling(
      latex_options = c("HOLD_position"),
      full_width    = TRUE,
      font_size     = 8
    ) %>%
    footnote(
      general           = "{Note:} Jarque-Bera test for normality. Skewness and excess kurtosis reported.",
      general_title     = "",
      footnote_as_chunk = FALSE,
      escape            = FALSE
    )

  writeLines(results_txt, here::here("main", "normality.txt"))
  return(table)
}
norm <- normality_test(data = full_filtered, vars = c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi_adj", "int_eff"))


writexl::write_xlsx(norm, path = here::here("Tables", "normality.xlsx"))




##############################################################################################################################################################################

########################################################################## Import Indices ####################################################################################

##############################################################################################################################################################################

overlap <- full_filtered %>%
    filter(time_period >= as.Date("2010-01-01"), 
            time_period <= as.Date("2012-12-01")) %>%
    arrange(desc(time_period))

ccf <- broom::tidy(ccf(overlap$uvi34, overlap$m_hist, na.action = na.omit, lag.max = 6)) %>%
        knitr::kable(format = "latex", digits = 4, booktabs = TRUE, col.names = c("k Lag", "Correlation"),
        caption = "Correlation between UVI(t+k) and SIC(t)")
print(ccf)             
writeLines(ccf, here::here("tables", "uvi_sic_ccf.tex"))

print(ccf(overlap$uvi34, overlap$m_hist, na.action = na.omit, lag.max = 6))

t_test <- broom::tidy(t.test(lag(overlap$uvi34, n = 1), overlap$m_hist, paired = TRUE)) %>%
          select(estimate, statistic, p.value, conf.low, conf.high, parameter) %>%
          rename("Mean diff." = estimate, "t" = statistic, "p" = p.value,
                "CI low" = conf.low, "CI high" = conf.high, "df" = parameter) %>%
          knitr::kable(format = "latex", digits = 4, booktabs = TRUE,
                caption = "Paired t-test: UVI vs SIC import price series") %>%
                kable_styling(latex_options = c("HOLD_position", "scale_down"),
          full_width    = TRUE,
          font_size     = 8
        ) 

print(t_test)

writeLines(t_test, here::here("main", "uvi_sic_ttest.txt"))



# VAR consumes first p=9 rows for lags; residuals start at row 13
# break_idx is the position within the residual sequence



lag <- 6
df_var_full <- full_filtered %>%
  mutate(d_imp = ifelse(time_period == break_date, 1L, 0L)) %>%
  dplyr::select(time_period, all_of(var_list), d_imp) 
print(names(df_var_full))

oil_exog <- sapply(0:lag, function(l) dplyr::lag(df_var_full$oilshock, l))
colnames(oil_exog) <- paste0("oil.l", 0:lag)

exog_mat <- cbind(oil_exog, d_imp = df_var_full$d_imp)

valid_rows <- complete.cases(exog_mat)

keep <- df_var_full$time_period >= lubridate::ymd("1990-02-01")
exog_mat <- exog_mat[keep, ]
df_var <- df_var_full[keep, ] %>% dplyr::select(-time_period) %>% na.omit()


var_model_chow <- VAR(df_var %>% select(-oilshock, -d_imp), 
                 p = 6, 
                 type = "const",
                 exogen = exog_mat)

df_var_cropped <- df_var_full %>% filter(time_period >= as.Date("1990-02-01"))
break_idx <- which(df_var_cropped$time_period >= break_date)[1] - lag  # adjust for lag consumption


chow_results <- lapply(names(var_model_chow$varresult), function(eq) {
  model   <- var_model_chow$varresult[[eq]]
  y       <- as.vector(fitted(model) + residuals(model))
  X       <- model.matrix(model)
  n       <- nrow(X)
  k       <- ncol(X)
  
  # break_idx must fall within the model's row space
  bi <- min(break_idx, n - k - 1)
  
  rss_r   <- sum(residuals(model)^2)
  rss_u   <- sum(lm.fit(X[seq_len(bi), ], y[seq_len(bi)])$residuals^2) +
             sum(lm.fit(X[seq(bi + 1, n), ], y[seq(bi + 1, n)])$residuals^2)
  F_stat  <- ((rss_r - rss_u) / k) / (rss_u / (n - 2 * k))
  data.frame(equation = eq, F_stat = F_stat,
             p_value  = pf(F_stat, k, n - 2 * k, lower.tail = FALSE))
}) %>% bind_rows()


chow_txt <- chow_results %>%
  mutate(
     equation = recode(equation,
      "outputgap_dc" = "Output Gap",
      "neer_sarb"    = "Exchange Rate",
      "m"            = "Import Price Index",
      "ppi"          = "Producer Price Index",
      "cpi"          = "Consumer Price Index",
      "cpi_adj"      = "Consumer Price Index "
    ),
    F_stat   = round(F_stat, 3),
    p_value  = case_when(
      p_value < 0.001 ~ "$<$ 0.001$^{\\ast\\ast\\ast}$",
      p_value < 0.01  ~ paste0(round(p_value, 3), "$^{\\ast\\ast\\ast}$"),
      p_value < 0.05  ~ paste0(round(p_value, 3), "$^{\\ast\\ast}$"),
      p_value < 0.10  ~ paste0(round(p_value, 3), "$^{\\ast}$"),
      TRUE            ~ as.character(round(p_value, 3))
    )
  ) %>%
  `rownames<-`(NULL) %>%
  select(equation, F_stat, p_value) %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    linesep   = "\\addlinespace",
    caption   = "Chow Test for Structural Break at 2010-02",
    label     = "chow_test",
    escape    = FALSE,
    col.names = c("Equation", "F-Statistic", "p-value")
  ) %>%
  kable_styling(
    latex_options = c("HOLD_position"),
    full_width    = FALSE,
    font_size     = 8
  ) %>%
  footnote(
    general           = "{Note:} Chow test for parameter stability at 2010-02-01. $^{***}$ $p<0.01$, $^{**}$ $p<0.05$, $^{*}$ $p<0.10$.",
    general_title     = "",
    footnote_as_chunk = FALSE,
    escape            = FALSE
  )

writeLines(chow_txt, here::here("main", "chow_test.txt"))

imports <- full_filtered %>%
  select(time_period, m_hist, uvi34) %>%
  pivot_longer(-time_period, names_to = "series", values_to = "value") %>%
  mutate(series = recode(series, "m_hist" = "SIC", "uvi34" = "UVI (Arithmetic)")) %>%
  ggplot(aes(x = time_period, y = value, colour = series)) +
  scale_colour_manual(values = c("SIC" = "#2C6E8A", "UVI (Arithmetic)" = "#C45E3E")) +
  geom_line(linewidth = 1.2) +
  coord_cartesian(xlim = c(as.Date("2009-01-01"), as.Date("2014-01-01"))) +
  labs(
    x      = "Time Period",
    y      = "",
    colour = NULL
  ) +
  theme_publication() +
  theme(
    axis.text    = element_text(size = 12),
    axis.title   = element_text(size = 13),
    legend.text  = element_text(size = 12),
    plot.title   = element_text(size = 14),
    legend.key.width = unit(1.5, "cm")
  )

ggsave(filename = "sic_uvi.png", path = here::here("main"), width = 6, height = 6)


####
### Arithmetic vs Geometric
####


folders <- c("xm20102022.xlsx", "uvi16_25.xlsx")
codes <- c("UVI20000", "UVI34000", "UVI50000")
m <- tibble(time_period = character())
uvi <- tibble(time_period = as.Date(character()))

geom <- readxl::read_excel(here::here("data", "uvi16_25.xlsx"))
View(geom)

uvi <- tibble(time_period = as.Date(character()))

for (folder in folders){
  tag <- ifelse(folder == "xm20102022.xlsx", "arit", "geom")
  
  m_temp <- tibble(time_period = as.Date(character()))

  for (code in codes){
    path <- paste0(here::here(), "/data")
    temp_m <- readxl::read_excel(paste0(path, "/", folder)) %>%
      filter(H03 == code) %>%
      rename(type = H03) %>%
      select(-c("H01", "H02", "H04", "H05", "H17", "H18", "H25")) %>%
      pivot_longer(-type, names_to = "period", values_to = code) %>%
      mutate(time_period = lubridate::ymd(paste0(
        stringr::str_sub(period, 5, 8), "-",
        stringr::str_sub(period, 3, 4), "-01"
      ))) %>%
      select(time_period, !!sym(code)) %>%
      mutate(!!sym(code) := as.numeric(!!sym(code))) %>%
      arrange(time_period)

    m_temp <- full_join(m_temp, temp_m, by = "time_period")
  }

  m_temp <- m_temp %>%
    mutate(
      uvi2  = log(UVI20000) - log(lag(UVI20000, n = 1)),
      uvi34 = log(UVI34000) - log(lag(UVI34000, n = 1)),
      uvi5  = log(UVI50000) - log(lag(UVI50000, n = 1))
    ) %>%
    rename_with(~ paste0(.x, ".", tag), -time_period)

  uvi <- full_join(uvi, m_temp, by = "time_period")
}
View(uvi)

overlap_uvi <- uvi %>%
    filter(time_period >= as.Date("2016-02-01"), 
            time_period <= as.Date("2022-12-01")) %>%
    arrange(desc(time_period)) %>%
    select(where(~ !any(is.na(.))))


View(overlap_uvi)

cor_uvi <- cor(overlap_uvi$uvi34.arit, overlap_uvi$uvi34.geom, use = "complete.obs")
print(cor_uvi)  # [1] 0.7657967

t_test_uvi <- broom::tidy(t.test(overlap_uvi$uvi34.arit, overlap_uvi$uvi34.geom, paired = TRUE)) %>%
          select(estimate, statistic, p.value, conf.low, conf.high, parameter) %>%
          rename("Mean diff." = estimate, "t" = statistic, "p" = p.value,
                "CI low" = conf.low, "CI high" = conf.high, "df" = parameter) %>%
          
          knitr::kable(format = "latex", digits = 4, booktabs = TRUE,
                caption = "Paired t-test: Geometric vs Arithmetic UVI Series") %>%
                kable_styling(latex_options = c("HOLD_position", "scale_down"),
          full_width    = TRUE,
          font_size     = 8
        ) 

writeLines(t_test_uvi, here::here("main", "arit_geom_ttest.txt"))


geom_arit <- uvi %>%
  select(time_period, uvi34.geom, uvi34.arit) %>%
  pivot_longer(-time_period, names_to = "series", values_to = "value") %>%
  mutate(series = recode(series, "uvi34.geom" = "Geometric", "uvi34.arit" = "Arithmetic")) %>%
  ggplot(aes(x = time_period, y = value, colour = series)) +
  scale_colour_manual(values = c("Geometric" = "#2C6E8A", "Arithmetic" = "#C45E3E")) +
  geom_line(linewidth = 1.2) +
  coord_cartesian(xlim = c(as.Date("2015-06-01"), as.Date("2023-06-01"))) +
  labs(
    x      = "Time Period",
    y      = "",
    colour = NULL
  ) +
  theme_publication() +
  theme(
    axis.text    = element_text(size = 12),
    axis.title   = element_text(size = 13),
    legend.text  = element_text(size = 12),
    plot.title   = element_text(size = 14),
    legend.key.width = unit(1.5, "cm")
  )

ggsave(filename = "arit_geom.png", path = here::here("main"), width = 6, height = 6)


