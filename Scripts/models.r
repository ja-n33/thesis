##############################################################################################################################################################################

############################################################################## Run VAR Models ##############################################################################

##############################################################################################################################################################################

pacman::p_load(dplyr, tidyr, econdatar, ggplot2, vars, conflicted, lpirfs, patchwork, magick, cowplot)


conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# ── Theme ──────────────────────────────────────────────────────────────────────

sysfonts::font_add_google("Source Serif 4", "source_serif")
sysfonts::font_add_google("Source Sans 3",  "source_sans")


showtext::showtext_auto()
showtext::showtext_opts(dpi = 150)
sysfonts::font_families()

extrafont::loadfonts(device = "pdf")

# Download Source Sans 3
download.file(
  "https://github.com/google/fonts/raw/main/ofl/sourcesans3/SourceSans3%5Bwght%5D.ttf",
  destfile = "~/SourceSans3.ttf"
)

# Download Source Serif 4
download.file(
  "https://github.com/google/fonts/raw/main/ofl/sourceserif4/SourceSerif4%5Bwght%5D.ttf",
  destfile = "~/SourceSerif4.ttf"
)

systemfonts::register_font(name = "source_sans", plain = "~/SourceSans3.ttf")
systemfonts::register_font(name = "source_serif", plain = "~/SourceSerif4.ttf")

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

##############################################################################################################################################################################

######################################################################### Historical Sample (1992 - 2012) ####################################################################

##############################################################################################################################################################################


here::here()


full <- readr::read_csv(here::here("data", "samples", "fullsample.csv")) %>%
       mutate(time_period = as.Date(time_period, format = "%Y-%m-%d")) %>%
      filter(time_period <= as.Date("2025-06-01"), time_period >= as.Date("1992-02-01")) 

full_filtered <- full %>% filter(time_period < as.Date("2023-01-01"))

low_inflation <- full_filtered %>% filter(time_period >= as.Date("2009-01-01"))

early <- full_filtered %>% filter(time_period < as.Date("2009-01-01"))



View(full)
##############################################################################################################################################################################

######################################################################### Full Sample (1992-02 - 2025-06) ####################################################################

##############################################################################################################################################################################


plot_var <- function(sample = full, 
                var_list = c("oilshock", "outputgap_dc", "neer_sarb", "ppi", "cpi"),
                n_lags = 12, 
                name = "Full Sample", 
                imp      = var_list[3],
                resp = c("ppi_full", "cpi")){

                coefficients_m <- NULL

                name <- as.character(name)

                if (imp == "usdzar_fred"){
                  measure <- "Currency Measure is the USD/ZAR Exchange Rate.\nShaded regions reflect bootstrapped 95% confidence intervals."
                } else{
                  measure <- "Currency Measure is the SARB Nominal Effective Exchange Rate.\nShaded Regions reflect bootstrapped 95% confidence intervals."
                }
                if ("m" %in% var_list){
                file_name <- paste0(gsub(" ", "", tolower(name)), "_p_", as.character(n_lags), ".png")
                } else {
                file_name <- paste0(gsub(" ", "", tolower(name)), "_p_", as.character(n_lags), ".png")
                }
                acfpath <- file.path(here::here(), "acf", file_name)

                irfpath <- file.path(here::here(), "irf", file_name)

                plot_title <- name

                df_var <- sample %>%
                  dplyr::select(all_of(var_list)) %>%
                  na.omit()

              
                oil_exog <- sapply(0:as.numeric(n_lags), function(l) dplyr::lag(df_var$oilshock, l))
                colnames(oil_exog) <- paste0("oil.l", 0:as.numeric(n_lags)) 

                #if (var_list[1] == "oilshock"){
                       #       var_model <- do.call(VAR, list(y = df_var %>% dplyr::select(-oilshock), p = as.integer(n_lags), type = "const", exogen = oil_exog %>% as.matrix()))
                  #} else {
                              var_model <- do.call(VAR, list(y = df_var , p = as.integer(n_lags), type = "const"))
                              #}

              
                var_summary <- summary(var_model)

                coefficients_cpi  <- var_summary$varresult$cpi

                if ("ppi_full" %in% var_list){
                  coefficients_ppi <- var_summary$varresult$ppi_full
                } else 
                coefficients_ppi <- var_summary$varresult$ppi_all

                if ("uvi34" %in% var_list){
                  coefficients_m <- var_summary$varresult$uvi34
                } else if ("m_all" %in% var_list){
                  coefficients_m <- var_summary$varresult$m_all
                } else if ("m_manuf" %in% var_list){
                  coefficients_m <- var_summary$varresult$m_manuf
                }
                resids <- residuals(var_model)
                if ("oil" %in% var_list){
                  if (length(var_list) == 5 ){
                  resids_df <- residuals(var_model) %>%
                    as.data.frame() %>%
                    rename("Oil Shock" = 1, 
                            "Demand Shock" = 2,
                            "Exchange Rate Shock" = 3, 
                            "PPI Shock" = 4, 
                            "CPI Shock" = 5) 
                  } else {resids_df <- residuals(var_model) %>%
                    as.data.frame() %>%
                    rename("Oil Shock" = 1, 
                            "Demand Shock" = 2,
                            "Exchange Rate Shock" = 3, 
                            "Import Price Shock" = 4,
                            "PPI Shock" = 5, 
                            "CPI Shock" = 6)
                  }} else if ("oilshock" %in% var_list){ 
                    if (length(var_list) == 5 ){
                  resids_df <- residuals(var_model) %>%
                    as.data.frame() %>%
                    rename("Demand Shock" = 1,
                            "Exchange Rate Shock" = 2, 
                            "PPI Shock" = 3, 
                            "CPI Shock" = 4) 
                  } else {resids_df <- residuals(var_model) %>%
                    as.data.frame() %>%
                    rename("Demand Shock" = 1,
                            "Exchange Rate Shock" = 2, 
                            "Import Price Shock" = 3,
                            "PPI Shock" = 4, 
                            "CPI Shock" = 5)
                  }}

                png(filename = acfpath, width = 1200, 
                    height = 800, 
                    res = 150)

                par(mfrow = c(2, 3))
                for (col in colnames(resids_df)) {
                  acf(resids_df[, col], main = col, lag.max = 24)
                }
                
                dev.off()

                imp_sd       <- sd(resids[, imp], na.rm = TRUE)
                scale_factor <- 0.01 / imp_sd

                oil_var <- var_list[1]
                
                if (oil_var == "oilshock") {
                  exog_fc <- matrix(0, nrow = 18, ncol = (as.numeric(n_lags) + 1),
                        dimnames = list(NULL, colnames(oil_exog)))
                  irf_results <- do.call(irf, list(
                    x          = var_model,
                    impulse    = imp,
                    response   = c(imp, resp),
                    n.ahead    = 18,
                    boot       = TRUE,
                    ci         = 0.95,
                    cumulative = TRUE,
                    runs       = 500,
                    exogen     = exog_fc
                  ))
                } else {
                  irf_results <- do.call(irf, list(
                    x          = var_model,
                    impulse    = imp,
                    response   = c(imp, resp),
                    n.ahead    = 18,
                    boot       = TRUE,
                    ci         = 0.95,
                    cumulative = TRUE,
                    runs       = 500
                  ))
                }
                # Rescale all components
                irf_results$irf[[imp]]   <- irf_results$irf[[imp]]  * scale_factor
                irf_results$Lower[[imp]] <- irf_results$Lower[[imp]] * scale_factor
                irf_results$Upper[[imp]] <- irf_results$Upper[[imp]] * scale_factor

                xr_response <- irf_results$irf[[imp]][, imp]

                if ("ppi" %in% var_list){
                  if ("m" %in% var_list){

                irf_df <- bind_rows(
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "ppi"] * 100,
                    lower    = irf_results$Lower[[imp]][, "ppi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "ppi"] * 100,
                    response = "Producer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "cpi"] * 100,
                    lower    = irf_results$Lower[[imp]][, "cpi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "cpi"] * 100,
                    response = "Consumer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "m"] * 100,
                    lower    = irf_results$Lower[[imp]][, "m"] * 100,
                    upper    = irf_results$Upper[[imp]][, "m"] * 100,
                    response = "Import Price Index"
                  )
                 )}
                else if ("m_manuf" %in% var_list) {
                irf_df <- bind_rows(
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "ppi"] * 100,
                    lower    = irf_results$Lower[[imp]][, "ppi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "ppi"] * 100,
                    response = "Producer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "cpi"] * 100,
                    lower    = irf_results$Lower[[imp]][, "cpi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "cpi"] * 100,
                    response = "Consumer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:24,
                    estimate = irf_results$irf[[imp]][, "m_manuf"] * 100,
                    lower    = irf_results$Lower[[imp]][, "m_manuf"] * 100,
                    upper    = irf_results$Upper[[imp]][, "m_manuf"] * 100,
                    response = "Import Price Index"
                  )
                 )} else {
                irf_df <- bind_rows(
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "ppi"] * 100,
                    lower    = irf_results$Lower[[imp]][, "ppi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "ppi"] * 100,
                    response = "Producer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "cpi"] * 100,
                    lower    = irf_results$Lower[[imp]][, "cpi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "cpi"] * 100,
                    response = "Consumer Price Index"
                  ))
                 }} else if ("ppi_full" %in% var_list) {
                  if ("m" %in% var_list) {
                irf_df <- bind_rows(
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "ppi_full"] * 100,
                    lower    = irf_results$Lower[[imp]][, "ppi_full"] * 100,
                    upper    = irf_results$Upper[[imp]][, "ppi_full"] * 100,
                    response = "Producer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "cpi"] * 100,
                    lower    = irf_results$Lower[[imp]][, "cpi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "cpi"] * 100,
                    response = "Consumer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "m"] * 100,
                    lower    = irf_results$Lower[[imp]][, "m"] * 100,
                    upper    = irf_results$Upper[[imp]][, "m"] * 100,
                    response = "Import Price Index"
                  )
                 )} else {
                irf_df <- bind_rows(
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "ppi_full"] * 100,
                    lower    = irf_results$Lower[[imp]][, "ppi_full"] * 100,
                    upper    = irf_results$Upper[[imp]][, "ppi_full"] * 100,
                    response = "Producer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "cpi"] * 100,
                    lower    = irf_results$Lower[[imp]][, "cpi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "cpi"] * 100,
                    response = "Consumer Price Index"
                  ))
                 }
                 } else if ("ppi_manuf" %in% var_list) {
                  if ("uvi34" %in% var_list) {
                irf_df <- bind_rows(
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "ppi_manuf"] * 100,
                    lower    = irf_results$Lower[[imp]][, "ppi_manuf"] * 100,
                    upper    = irf_results$Upper[[imp]][, "ppi_manuf"] * 100,
                    response = "Producer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "cpi"] * 100,
                    lower    = irf_results$Lower[[imp]][, "cpi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "cpi"] * 100,
                    response = "Consumer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "m"] * 100,
                    lower    = irf_results$Lower[[imp]][, "m"] * 100,
                    upper    = irf_results$Upper[[imp]][, "m"] * 100,
                    response = "Import Price Index"
                  )
                 )} else {
                irf_df <- bind_rows(
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "ppi_manuf"] * 100,
                    lower    = irf_results$Lower[[imp]][, "ppi_manuf"] * 100,
                    upper    = irf_results$Upper[[imp]][, "ppi_manuf"] * 100,
                    response = "Producer Price Index"
                  ),
                  data.frame(
                    horizon  = 0:18,
                    estimate = irf_results$irf[[imp]][, "cpi"] * 100,
                    lower    = irf_results$Lower[[imp]][, "cpi"] * 100,
                    upper    = irf_results$Upper[[imp]][, "cpi"] * 100,
                    response = "Consumer Price Index"
                  ))
                 }
                 }
                y_min <- max(-0.05, min(irf_df$lower, na.rm = TRUE))

                irf_plot <- ggplot(irf_df, aes(x = horizon, y = estimate, 
                                  fill = response, colour = response)) +
                geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
                geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.10, colour = NA) +
                geom_line(linewidth = 0.8) +
                scale_colour_manual(values = c(
                  "Producer Price Index" = "#2C6E8A",
                  "Consumer Price Index" = "#C45E3E",
                  "Import Price Index" = "#4A9A6F"
                )) +
                scale_fill_manual(values = c(
                  "Producer Price Index" = "#2C6E8A",
                  "Consumer Price Index" = "#C45E3E",
                  "Import Price Index" = "#4A9A6F"
                                  )) +
                scale_x_continuous(breaks = seq(0, 18, by = 3), limits = c(0, 18), expand = c(0,0)) +
                scale_y_continuous(labels = scales::label_percent(scale = 1)) +
                coord_cartesian(ylim = c(y_min, NA)) +
                labs(x       = "Months after shock",
                    y       = "Cumulative Response (%)",
                    title   = plot_title,
                    #caption = measure,
                    colour  = NULL,
                    fill    = NULL) +
                theme_publication() 

                ggsave(
                  filename = file_name,
                  plot     = irf_plot,
                  path     = file.path(here::here(), "irf"),
                  width    = 10,
                  height   = 6,
                  dpi      = 150,
                  device   = "png"
                )
return(irf_plot)
                  }

sets <- c("full_filtered", "early", "low_inflation")


plots <- list()


for (set in sets){
  temp_resp <- c("m", "ppi", "cpi")

  if (set == "full"){
    temp_name <- "Full Sample (1990-02 - 2025-06)"
    temp_list <- c("oilshock", "outputgap_dc", "neer_sarb", "ppi", "cpi")
    temp_resp <- c("ppi", "cpi")
  }  else {temp_list <- c("oilshock", "outputgap_dc", "neer_sarb", "m","ppi", "cpi")
          temp_resp <- c("m", "ppi", "cpi")
  }
  if (set == "full_filtered"){
    temp_name <- "Full Sample (1990-02 - 2022-12)"
    
  } else if (set == "low_inflation"){
    temp_name <- "Anchored Inflation (2009-01 - 2022-12)"
  } else if (set == "early"){
    temp_name <- "Early Sample (1990-02 - 2008-12)"
  }


plots[[set]] <- plot_var(sample = get(set), 
      var_list = temp_list,
      name = temp_name, 
      resp = temp_resp, 
      n_lags = 12)
}


combined_plot <- wrap_plots(plots, nrow = 3, ncol = 1) +
  plot_layout(guides = "collect", axes = "collect") +
  plot_annotation(
    caption = "Exchange Rate Measure is the Nominal Effective Exchange Rate.\nShaded regions reflect bootstrapped 95% confidence intervals.",
    theme = theme(
      plot.caption  = element_text(hjust = 0, size = 9, family = "source_sans"),
      legend.position = "bottom",
      legend.text   = element_text(family = "source_sans"),
      legend.title  = element_text(family = "source_sans"),
      text         = element_text(family = "source_sans"),  # catches everything
    plot.title   = element_text(family = "source_serif", face = "bold"),
    axis.text    = element_text(family = "source_sans"),
    axis.title   = element_text(family = "source_sans"),
    )
  ) &
  theme(
    text         = element_text(family = "source_sans"),  # catches everything
    plot.title   = element_text(family = "source_serif", face = "bold"),
    axis.text    = element_text(family = "source_sans"),
    axis.title   = element_text(family = "source_sans"),
    legend.text  = element_text(family = "source_sans"),
    legend.title = element_text(family = "source_sans"),
    strip.text   = element_text(family = "source_sans"),
    plot.caption = element_text(family = "source_sans"),
    plot.margin  = margin(6, 14, 6, 12)
  )
library(ragg)
ggsave(here::here("irf", "combined.png"), combined_plot, width = 12, height = 18, dpi = 150, device   = "png", type = "cairo")


plot <- ggdraw(combined_plot) + ##this creates a blank canvas with the combined plot on top of it. I then add codera logo
        draw_image(here::here("logo-transparent.png"),
               x = 0.8, y = -0.41,       # bottom right
               width = 0.19,
               hjust = 0, vjust = 0)

ggsave(here::here("import_composition", "food_indices.png"), plot, width = 8, height = 5)
var_table <- function(dat = new,
                             var_list = c("oil", "outputgap_dc", "neer_sarb", "ppi", "cpi"),
                             n_lags   = 12,
                             name     = "New Sample",
                             imp      = var_list[3],
                             resp     = c("ppi", "cpi"),
                             horizons = c(3, 6, 12, 18, 24),
                             B        = 1000) {

  # ── Labels and paths ──────────────────────────────────────────────────────────
  if (imp == "usdzar_fred") {
    measure <- "Currency Measure is the USD/ZAR Exchange Rate"
  } else {
    measure <- "Currency Measure is the SARB Nominal Effective Exchange Rate"
  }

  file_name  <- paste0(gsub(" ", "", tolower(name)), ".png")
  acfpath    <- file.path(here::here(), "acf", file_name)
  irfpath    <- file.path(here::here(), "irf", file_name)
  plot_title <- paste0("Exchange Rate Pass-Through (", name, ")")

  # ── Estimate VAR ──────────────────────────────────────────────────────────────
  df_var <- dat %>%
    dplyr::select(all_of(var_list)) %>%
    na.omit()

  p_val     <- as.integer(n_lags)


  oil_exog <- sapply(0:as.numeric(n_lags), function(l) dplyr::lag(df_var$oilshock, l))
  colnames(oil_exog) <- paste0("oil.l", 0:as.numeric(n_lags)) 

  if (var_list[1] == "oilshock"){
                var_model <- do.call(VAR, list(y = df_var %>% dplyr::select(-oilshock), p = as.integer(n_lags), type = "const", exogen = oil_exog %>% as.matrix()))
    } else {
                var_model <- do.call(VAR, list(y = df_var , p = as.integer(n_lags), type = "const"))
                }

  var_summary <- summary(var_model)

  coefficients_cpi <- var_summary$varresult$cpi

  if ("ppi_full" %in% var_list) {
    coefficients_ppi <- var_summary$varresult$ppi_full
  } else if ("ppi_all" %in% var_list){
    coefficients_ppi <- var_summary$varresult$ppi_all
  } else if ("ppi_manuf" %in% var_list){
    coefficients_ppi <- var_summary$varresult$ppi_manuf
  } 

  if ("uvi34" %in% var_list) {
    coefficients_m <- var_summary$varresult$uvi34
  } else if ("m_all" %in% var_list) {
    coefficients_m <- var_summary$varresult$m_all
  } else if ("m_manuf" %in% var_list) {
    coefficients_m <- var_summary$varresult$m_manuf
  }

  # ── ACF plot of residuals ─────────────────────────────────────────────────────
  resids <- residuals(var_model)

  png(filename = acfpath, width = 1200, height = 800, res = 150)
  par(mfrow = c(2, 3))
  for (col in colnames(resids)) {
    acf(resids[, col], main = col, lag.max = 24)
  }
  dev.off()

  # ── IRF for plotting ──────────────────────────────────────────────────────────
  imp_sd       <- sd(resids[, imp], na.rm = TRUE)
  scale_factor <- 0.01 / imp_sd

  
    oil_var <- var_list[1]
    
    if (oil_var == "oilshock") {
      exog_fc <- matrix(0, nrow = 25, ncol = (as.numeric(n_lags) + 1),
                        dimnames = list(NULL, colnames(oil_exog)))
      irf_results <- do.call(irf, list(
        x          = var_model,
        impulse    = imp,
        response   = c(imp, resp),
        n.ahead    = 24,
        boot       = FALSE,
        ci         = 0.95,
        cumulative = TRUE,
        runs       = 500,
        exogen     = exog_fc
      ))
    } else {
      irf_results <- do.call(irf, list(
        x          = var_model,
        impulse    = imp,
        response   = c(imp, resp),
        n.ahead    = 24,
        boot       = FALSE,
        ci         = 0.95,
        cumulative = TRUE,
        runs       = 500
      ))
    }
  # ── Helper: cumulative PT ratio and response at each horizon ───────────────────────────────
  get_pt <- function(model, imp, resp, horizons) {

    irf_obj <- irf(model,
                   impulse  = imp,
                   response = c(imp, resp),
                   n.ahead  = max(horizons),
                   ortho    = TRUE,
                   boot     = FALSE)

    irf_mat <- irf_obj$irf[[imp]]   # (max(horizons)+1) rows × n_vars cols
    irf_s   <- irf_mat[, imp]       # exchange rate own-response (denominator)

    pt <- mapply(function(r, h) {
      irf_p <- irf_mat[, r]
      sum(irf_p[1:(h+1)]) / sum(irf_s[1:(h+1)])
    },
    r = rep(resp,     each  = length(horizons)),
    h = rep(horizons, times = length(resp)))

    return(pt)
  }

   get_resp <- function(model, imp, resp, horizons) {

    irf_obj <- irf(model,
                   impulse  = imp,
                   response = c(imp, resp),
                   n.ahead  = max(horizons),
                   ortho    = TRUE,
                   boot     = FALSE)

    irf_mat <- irf_obj$irf[[imp]]   # (max(horizons)+1) rows × n_vars cols

    response <- mapply(function(r, h) {
      irf_p <- irf_mat[, r]
      sum(irf_p[1:(h + 1)])
    },
    r = rep(resp,     each  = length(horizons)),
    h = rep(horizons, times = length(resp)))
  }

  # ── Point estimates ───────────────────────────────────────────────────────────
  pt_point <- get_pt(var_model, imp, resp, horizons)
  r_point <- get_resp(var_model, imp, resp, horizons) * scale_factor


  # ── Bootstrap standard errors ─────────────────────────────────────────────────
  n_obs       <- nrow(resids)
  fitted_vals <- fitted(var_model)
  n_col_pt    <- length(resp) * length(horizons)
  pt_boot     <- matrix(NA_real_, nrow = B, ncol = n_col_pt)
  r_boot      <- matrix(NA_real_, nrow = B, ncol = n_col_pt)

  for (b in seq_len(B)) {
  resid_boot <- resids[base::sample(n_obs, n_obs, replace = TRUE), ]
  y_boot     <- fitted_vals + resid_boot

  tryCatch({
    if (var_list[1] == "oilshock") {
      exog_boot <- oil_exog[-(1:var_model$p), , drop = FALSE]
      var_boot  <- do.call(VAR, list(y      = y_boot,
                                     p      = var_model$p,
                                     type   = "const",
                                     exogen = exog_boot))
    } else {
      var_boot <- do.call(VAR, list(y    = y_boot,
                                    p    = var_model$p,
                                    type = "const"))
    }
    pt_boot[b, ] <- get_pt(var_boot, imp, resp, horizons)
    r_boot[b, ]  <- get_resp(var_boot, imp, resp, horizons)
  }, error = function(e) NULL)
}
  pt_se <- apply(pt_boot, 2L, sd, na.rm = TRUE)
  r_se <- apply(r_boot, 2L, sd, na.rm = TRUE) * scale_factor

  # ── Assemble table ────────────────────────────────────────────────────────────
  resp_labels <- dplyr::case_when(
    resp == "ppi_full" ~ "PPI",
    resp == "ppi_all"  ~ "PPI",
    resp == "ppi_manuf"  ~ "PPI",
    resp == "cpi"      ~ "CPI",
    resp == "uvi34"    ~ "Import prices",
    resp == "m_all"    ~ "Import prices",
    resp == "m_manuf"  ~ "Import prices",
    TRUE               ~ resp
  )

  result <- data.frame(
    Variable = rep(resp_labels, each = length(horizons)),
    Horizon  = paste0("Month ", horizons),
    Response       = paste0(round(r_point * 100, 4)," (", round(r_se * 100, 4), ")"),
    PT       = paste0(round(pt_point, 4)," (", round(pt_se, 4), ")"),
    stringsAsFactors = FALSE
  )

  print(
    knitr::kable(result,
                 col.names = c("Price stage", "Horizon", "Cumulative Response (%)","Pass-through"),
                 align     = c("l", "l", "r", "r"),
                 caption   = paste0("Cumulative ERPT — ", name, " (B = ", B, ")"))
  )

  invisible(list(table = result, var_model = var_model, irf = irf_results))

}




sd(full$ppi_manuf, na.rm = TRUE)

sd(full$finalmanufgoods_full, na.rm = TRUE)


for (set in sets){
  if (set == "full"){
    temp_list <- c("oilshock", "outputgap_dc", "neer_sarb", "ppi", "cpi")
    temp_name <- "Full Sample"
    temp_resp <- c("ppi", "cpi")
  }  else {
    temp_list <- c("oilshock", "outputgap_dc", "neer_sarb", "m", "ppi", "cpi")
    temp_name <- "Full Import Sample"
    temp_resp <- c("m", "ppi", "cpi")
  }
full_var <- var_table(dat = get(set), 
      var_list = temp_list,
      name = temp_name, 
      resp = temp_resp)
}


sd(full$uvi2, na.rm = TRUE)
sd(full$m_hist, na.rm = TRUE)

summary(full_var$var_model)
serial.test(full_var$var_model, lags.pt = 12, type = "PT.asymptotic")  # serial correlation
normality.test(full_var$var_model)                                       # normality of residuals
stability(full_var$var_model)                                            # eigenvalue stability

imports_var <- var_table(dat = full %>% filter(time_period > as.Date("1999-06-01"), time_period < as.Date("2023-01-01")), 
      var_list = c("oilshock", "outputgap_dc", "neer_sarb", "uvi34", "ppi", "cpi"),
      n_lags = 6, 
      name = "Model with Imports", 
      imp = "neer_sarb", 
      resp = c("ppi", "cpi", "uvi34"))

summary(imports_var$var_model)
      

cor(full$neer_sarb, full$usdzar_fred)

openxlsx::write.xlsx(full_var$table, 
           file      = here::here("Tables", "full_var_table.xlsx"),
           sheetName = "Pass-Through",
           rowNames  = FALSE)


filename_var <- file.path(here::here(), "Tables", "full_var_table.csv")
write.csv(full_var$table, filename_var)


################################################################################################################################################################################
########################################################################   Linear Projections Estimates  ######################################################################
################################################################################################################################################################################


lp_table <- function(set    = full,
                     var_list  = c("oilshock", "outputgap_dc", "neer_sarb", "ppi_manuf", "cpi"),
                     n_lags    = 12,
                     name      = "Full Sample",
                     imp       = "usdzar_fred",
                     resp      = c("ppi_manuf", "cpi"),
                     horizons  = c(3, 6, 12, 18, 24)) {

  oil <- var_list[1]   # exogenous, ordered first
  dem <- var_list[2]   # demand, ordered second (before the exchange rate)

  measure <- if (imp == "usdzar_fred")
    "Currency Measure is the USD/ZAR exchange Rate" else
    "Currency Measure is the SARB Nominal Effective Exchange Rate"

  # ── Data: contemporaneous + lags of every variable ──────────────────────
  df_var <- set %>%
    dplyr::select(all_of(var_list)) %>%
    na.omit() %>%
    as.data.frame()

  bd <- df_var
  for (v in var_list) for (l in seq_len(n_lags))
    bd[[paste0(v, ".l", l)]] <- dplyr::lag(df_var[[v]], l)

  lag_cols <- grep("\\.l[0-9]+$", names(bd), value = TRUE)

  # ── Recursive exchange-rate shock ───────────────────────────────────────
  # Orthogonalise the exchange rate wrt contemporaneous oil & demand and all
  # lags (ordering oil > demand > exchange rate). na.exclude pads the dropped
  # first n_lags rows with NA so 'shock' lines up with bd row-for-row.
  xr_fit <- lm(reformulate(c(oil, dem, lag_cols), response = imp),
               data = bd, na.action = na.exclude)
  bd$shock <- residuals(xr_fit)

  # ── Forward cumulative sum of a (differenced) series ────────────────────
  cum_lead <- function(x, h) {
    s <- x
    if (h > 0) for (j in seq_len(h)) s <- s + dplyr::lead(x, j)
    s
  }

  # ── One cumulative LP: returns cumulative coef + Newey-West SE ───────────
  lp_cum <- function(outcome, h) {
    d        <- bd
    d$cum_y  <- cum_lead(d[[outcome]], h)
    fit      <- lm(reformulate(c("shock", oil, dem, lag_cols), response = "cum_y"),
                   data = d)
    V        <- sandwich::NeweyWest(fit, lag = h + 1, prewhite = FALSE)
    c(coef = unname(coef(fit)["shock"]),
      se   = unname(sqrt(V["shock", "shock"])))
  }

  # ── Normalisation: shock that moves the exchange rate 1% on impact ──────
  xr_impact    <- unname(lp_cum(imp, 0)["coef"])     # ≈ 1 by construction
  scale_factor <- 0.01 / xr_impact

  # ── Exchange-rate cumulative response (PT denominator) ──────────────────
  xr <- vapply(horizons, function(h) lp_cum(imp, h), numeric(2))   # 2 x H

  # ── Price rows ──────────────────────────────────────────────────────────
  est <- dplyr::bind_rows(lapply(resp, function(r) {
    pr <- vapply(horizons, function(h) lp_cum(r, h), numeric(2))   # 2 x H

    gp <- pr["coef", ]; sp <- pr["se", ]    # price cumulative coef / se
    gx <- xr["coef", ]; sx <- xr["se", ]    # exchange-rate cumulative coef / se

    data.frame(
      resp     = r,
      horizon  = horizons,
      resp_val = gp * scale_factor,                              # scaled to 1% deprec.
      resp_se  = sp * scale_factor,
      pt_val   = gp / gx,                                        # scale-free ratio
      pt_se    = sqrt((1 / gx)^2 * sp^2 + (gp / gx^2)^2 * sx^2)  # delta method
    )
  }))

  # ── Format (identical layout to var_table) ──────────────────────────────
  est$Variable <- dplyr::case_when(
    est$resp %in% c("ppi_manuf", "ppi_full", "ppi_all") ~ "PPI",
    est$resp == "cpi"                                   ~ "CPI",
    est$resp %in% c("uvi34", "m_all", "m_manuf")         ~ "Import prices",
    TRUE                                                ~ est$resp)

  result <- data.frame(
    Variable = est$Variable,
    Horizon  = paste0("Month ", est$horizon),
    Response = paste0(round(est$resp_val * 100, 4), " (", round(est$resp_se * 100, 4), ")"),
    PT       = paste0(round(est$pt_val, 4),         " (", round(est$pt_se, 4), ")"),
    stringsAsFactors = FALSE
  )

  print(knitr::kable(result,
        col.names = c("Price stage", "Horizon", "Cumulative response (%)", "Pass-through"),
        align     = c("l", "l", "r", "r"),
        caption   = paste0("LP Cumulative ERPT — ", name, " (Newey-West HAC)")))

  invisible(list(table = result, est = est, scale_factor = scale_factor, xr_fit = xr_fit))
}


full_lp <- lp_table(sample = full, 
      var_list = c("oilshock", "outputgap_dc", "usdzar_fred", "ppi_manuf", "cpi"),
      name = "Full Sample", 
      resp = c("ppi_manuf", "cpi"))

View(full_lp$table)

openxlsx::write.xlsx(full_lp$table, 
           file      = here::here("Tables", "full_lp_table.xlsx"),
           sheetName = "Pass-Through",
           rowNames  = FALSE)


filename_lp <- file.path(here::here(), "Tables", "full_lp_table.csv")
write.csv(full_lp$table, filename_lp)

plot_lp <- function(dat   = new,
                    var_list = c("oilshock", "outputgap_dc", "usdzar_fred", "ppi_full", "cpi"),
                    n_lags   = 12,
                    name     = "New Sample",
                    imp      = var_list[3],
                    resp     = c("ppi_full", "cpi"),
                    hor      = 18) {

  name <- as.character(name)

  measure <- if (imp == "usdzar_fred"){
    "Currency Measure is the USD/ZAR exchange Rate.\nDashed lines reflect bootstrapped 95% confidence intervals."}
     else{
    "Currency Measure is the SARB Nominal Effective Exchange Rate.\nDashed lines reflect bootstrapped 95% confidence intervals."}

  file_name  <- paste0(gsub(" ", "", tolower(name)), "_lp_p_", as.character(n_lags), ".png")
  acfpath    <- file.path(here::here(), "acf", file_name)
  plot_title <- paste0("Impulse Response Functions using Linear Projections  of the ", name, " using p = ", n_lags)

  oil <- var_list[1]
  dem <- var_list[2]

  # ── Data: contemporaneous + lags ─────────────────────────────────────────
  df_var <- set %>%
    dplyr::select(all_of(var_list)) %>%
    na.omit() %>%
    as.data.frame()

  bd <- df_var
  for (v in var_list) for (l in seq_len(n_lags))
    bd[[paste0(v, ".l", l)]] <- dplyr::lag(df_var[[v]], l)

  lag_cols <- grep("\\.l[0-9]+$", names(bd), value = TRUE)

  # ── Recursive exchange rate shock ─────────────────────────────────────────
  xr_fit  <- lm(reformulate(c(oil, dem, lag_cols), response = imp),
                data = bd, na.action = na.exclude)
  bd$shock <- residuals(xr_fit)

  # ── ACF of residuals ──────────────────────────────────────────────────────
  resids_df <- bd %>%
    dplyr::select(all_of(var_list)) %>%
    na.omit() %>%
    as.data.frame()

  if ("oilshock" %in% var_list) {
    if (length(var_list) == 5) {
      colnames(resids_df) <- c("Oil Shock", "Demand Shock",
                               "Exchange Rate Shock", "PPI Shock", "CPI Shock")
    } else {
      colnames(resids_df) <- c("Oil Shock", "Demand Shock", "Exchange Rate Shock",
                               "Import Price Shock", "PPI Shock", "CPI Shock")
    }
  } else {
    if (length(var_list) == 5) {
      colnames(resids_df) <- c("Demand Shock", "Exchange Rate Shock",
                               "PPI Shock", "CPI Shock")
    } else {
      colnames(resids_df) <- c("Demand Shock", "Exchange Rate Shock",
                               "Import Price Shock", "PPI Shock", "CPI Shock")
    }
  }

  png(filename = acfpath, width = 1200, height = 800, res = 150)
  par(mfrow = c(2, 3))
  for (col in colnames(resids_df)) acf(resids_df[, col], main = col, lag.max = 24)
  dev.off()

  # ── Helpers identical to lp_table ─────────────────────────────────────────
  cum_lead <- function(x, h) {
    s <- x
    if (h > 0) for (j in seq_len(h)) s <- s + dplyr::lead(x, j)
    s
  }

  lp_cum <- function(outcome, h) {
    d       <- bd
    d$cum_y <- cum_lead(d[[outcome]], h)
    fit     <- lm(reformulate(c("shock", oil, dem, lag_cols), response = "cum_y"),
                  data = d)
    V       <- sandwich::NeweyWest(fit, lag = h + 1, prewhite = FALSE)
    c(coef  = unname(coef(fit)["shock"]),
      se    = unname(sqrt(V["shock", "shock"])))
  }

  # ── Normalisation ─────────────────────────────────────────────────────────
  xr_impact    <- unname(lp_cum(imp, 0)["coef"])
  scale_factor <- 0.01 / xr_impact

  # ── Compute IRF over all horizons 0:hor ───────────────────────────────────
  horizons <- 0:hor

  make_irf_df <- function(outcome, label) {
    pr <- vapply(horizons, function(h) lp_cum(outcome, h), numeric(2))

    coefs <- pr["coef", ] * scale_factor * 100
    ses   <- pr["se",   ] * scale_factor * 100

    data.frame(
      horizon  = horizons,
      estimate = coefs,
      lower    = coefs - 1.96 * ses,
      upper    = coefs + 1.96 * ses,
      response = label
    )
  }

  # ── Build irf_df ──────────────────────────────────────────────────────────
  if ("ppi" %in% var_list) {
    if ("m" %in% var_list) {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi", "Producer Price Index"),
        make_irf_df("cpi",     "Consumer Price Index"),
        make_irf_df("m_all",   "Import Price Index"))
    } else if ("m_manuf" %in% var_list) {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi", "Producer Price Index"),
        make_irf_df("cpi",     "Consumer Price Index"),
        make_irf_df("m_manuf", "Import Price Index"))
    } else {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi", "Producer Price Index"),
        make_irf_df("cpi",     "Consumer Price Index"))
    }
  } else if ("ppi_full" %in% var_list) {
    if ("uvi34" %in% var_list) {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi_full", "Producer Price Index"),
        make_irf_df("cpi",      "Consumer Price Index"),
        make_irf_df("uvi34",    "Import Price Index"))
    } else {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi_full", "Producer Price Index"),
        make_irf_df("cpi",      "Consumer Price Index"))
    }
  } else if ("ppi_manuf" %in% var_list) {
    if ("uvi34" %in% var_list) {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi_manuf", "Producer Price Index"),
        make_irf_df("cpi",       "Consumer Price Index"),
        make_irf_df("uvi34",     "Import Price Index"))
    } else {
      irf_df <- dplyr::bind_rows(
        make_irf_df("ppi_manuf", "Producer Price Index"),
        make_irf_df("cpi",       "Consumer Price Index"))
    }
  }

  irf_df <- irf_df %>% dplyr::mutate(response = as.character(response))

  # ── Plot ──────────────────────────────────────────────────────────────────
  irf_plot <- ggplot(irf_df, aes(x = horizon, y = estimate,
                                  fill = response, colour = response)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    geom_line(aes(y = lower), linewidth = 0.4, linetype = "dashed") +
    geom_line(aes(y = upper), linewidth = 0.4, linetype = "dashed") +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = c(
      "Producer Price Index"         = "#2C6E8A",
      "Consumer Price Index"          = "#C45E3E",
      "Import Price Index"  = "#4A9A6F")) +
    scale_fill_manual(values = c(
      "Producer Price Index"         = "#2C6E8A",
      "Consumer Price Index"          = "#C45E3E",
      "Import Prices Index"  = "#4A9A6F")) +
    scale_x_continuous(breaks = seq(0, hor, by = 3),
                       limits = c(0, hor),
                       expand = c(0, 0)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs(x       = "Months after shock",
         y       = "Cumulative response (%)",
         title   = plot_title,
         caption = measure,
         colour  = NULL,
         fill    = NULL) +
    theme_publication()

  ggsave(
    filename = file_name,
    plot     = irf_plot,
    path     = file.path(here::here(), "irf"),
    width    = 10,
    height   = 6,
    dpi      = 150
  )

  invisible(list(irf_df = irf_df, irf_plot = irf_plot))
}
 
for (lag in c(3, 6, 12, 18)){

  for (set in sets){
    temp_resp <- c("m", "ppi", "cpi")

    if (set == "full"){
      temp_name <- "Full Sample (1990-02 - 2025-06)"
      temp_list <- c("oilshock", "outputgap_dc", "neer_sarb", "ppi", "cpi")
      temp_resp <- c("ppi", "cpi")
    }  else {temp_list <- c("oilshock", "outputgap_dc", "neer_sarb", "m","ppi", "cpi")
            temp_resp <- c("m", "ppi", "cpi")
    }
    if (set == "full_filtered"){
      temp_name <- "Full Import Sample (1990-02 - 2022-12)"
      
    } else if (set == "low_inflation"){
      temp_name <- "2009-01 - 2022-12"
    } else if (set == "early"){
      temp_name <- "1990-02 - 2008-12"
    }
  plot_lp(sample = get(set), 
        var_list = temp_list,
        name = temp_name, 
        resp = temp_resp, 
        n_lags = lag)
  }
  }
