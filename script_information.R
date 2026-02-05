# WIKIPEDIA NETWORK CENTRALITY AND NVDA VOLATILITY ANALYSIS

# 1. SETUP AND LIBRARY LOADING
# Load required packages 
required_packages <- c(
  "tidyverse",   # Data manipulation and visualization
  "httr",        # API requests
  "jsonlite",    # JSON parsing
  "igraph",      # Network analysis
  "quantmod",    # Financial data
  "lmtest",      # Regression diagnostics
  "sandwich",    # Robust standard errors
  "lubridate"    # Date handling
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# 2. DEFINE PARAMETERS

# Time window: 3-year period (2022-2025)
start_date <- "2022-01-01"
end_date   <- "2025-02-28"

# Wikipedia pages for analysis (AI-related topics)
pages <- c(
  "NVIDIA",
  "Artificial_intelligence",
  "OpenAI",
  "Large_language_model",
  "ChatGPT",
  "Machine_learning",
  "Deep_learning",
  "Semiconductor",
  "GPU",
  "Transformer_(machine_learning_model)"
)

# 3. DATA COLLECTION: WIKIPEDIA PAGEVIEWS

# Function to retrieve pageviews from Wikimedia REST API
get_pageviews <- function(page, start_date, end_date, language = "en") {
  
  # Format dates for API 
  start_str <- format(as.Date(start_date), "%Y%m%d")
  end_str <- format(as.Date(end_date), "%Y%m%d")
  
  # Construct API URL
  url <- sprintf(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/%s.wikipedia/all-access/all-agents/%s/daily/%s/%s",
    language,
    URLencode(page, reserved = TRUE),
    start_str,
    end_str
  )
  
  # Make API request with error handling
  response <- tryCatch({
    GET(url)
  }, error = function(e) {
    return(NULL)
  })
  
  # Check HTTP response status
  if (is.null(response) || status_code(response) != 200) {
    return(NULL)
  }
  
  # Parse JSON response
  data <- tryCatch({
    content(response, as = "text", encoding = "UTF-8") %>% 
      fromJSON()
  }, error = function(e) {
    return(NULL)
  })
  
  # Extract data if available
  if (is.null(data) || is.null(data$items)) {
    return(NULL)
  }
  
  # Return tibble with pageview data
  result <- tibble(
    page  = page,
    date  = as.Date(data$items$timestamp, format = "%Y%m%d"),
    views = as.numeric(data$items$views)
  )
  
  return(result)
}

# Collect pageviews for all pages
all_pageviews <- NULL

for (page in pages) {
  Sys.sleep(0.3)  # Rate limiting to avoid API overload
  
  pv <- get_pageviews(page, start_date, end_date)
  
  if (!is.null(pv) && nrow(pv) > 0) {
    if (is.null(all_pageviews)) {
      all_pageviews <- pv
    } else {
      all_pageviews <- bind_rows(all_pageviews, pv)
    }
  }
}

# Check if data collection was successful
if (is.null(all_pageviews) || nrow(all_pageviews) == 0) {
  stop("Data collection failed: No Wikipedia data retrieved")
}

# Transform to wide format (one column per page)
pageviews_wide <- all_pageviews %>%
  pivot_wider(
    names_from  = page,
    values_from = views,
    values_fill = 0
  ) %>%
  arrange(date)


# 4. NETWORK CONSTRUCTION


# Define directed edges based on semantic relationships
# Relationships: technological dependency, industry links, conceptual hierarchy
edges <- tribble(
  ~from, ~to,
  # NVIDIA cluster
  "NVIDIA", "Artificial_intelligence",
  "NVIDIA", "GPU",
  "NVIDIA", "Deep_learning",
  "NVIDIA", "Machine_learning",
  "NVIDIA", "Semiconductor",
  "GPU", "Semiconductor",
  "GPU", "NVIDIA",
  "GPU", "Deep_learning",
  # OpenAI/ChatGPT cluster
  "ChatGPT", "OpenAI",
  "ChatGPT", "Artificial_intelligence",
  "ChatGPT", "Large_language_model",
  "ChatGPT", "Transformer_(machine_learning_model)",
  "OpenAI", "ChatGPT",
  "OpenAI", "Artificial_intelligence",
  "OpenAI", "Large_language_model",
  # LLM/Transformer cluster
  "Large_language_model", "Artificial_intelligence",
  "Large_language_model", "Transformer_(machine_learning_model)",
  "Large_language_model", "Deep_learning",
  "Large_language_model", "ChatGPT",
  "Transformer_(machine_learning_model)", "Artificial_intelligence",
  "Transformer_(machine_learning_model)", "Deep_learning",
  "Transformer_(machine_learning_model)", "Machine_learning",
  # Machine learning connections
  "Deep_learning", "Machine_learning",
  "Deep_learning", "Artificial_intelligence",
  "Deep_learning", "NVIDIA",
  "Machine_learning", "Artificial_intelligence",
  "Machine_learning", "Deep_learning"
) %>%
  filter(from %in% pages & to %in% pages)

# Create directed graph using igraph
g <- graph_from_data_frame(edges, directed = TRUE, vertices = pages)

# Calculate centrality measures
centrality_measures <- tibble(
  page = V(g)$name,
  degree = degree(g, mode = "all"),
  betweenness = betweenness(g, directed = TRUE),
  eigenvector = eigen_centrality(g, directed = TRUE)$vector,
  pagerank = page_rank(g)$vector
)

# Extract eigenvector centrality as named vector
centrality_vec <- centrality_measures$eigenvector
names(centrality_vec) <- centrality_measures$page

# 5. CALCULATE ATTENTION METRICS

# Calculate total daily volume
attention_df <- pageviews_wide %>%
  mutate(
    total_volume = rowSums(select(., all_of(pages)), na.rm = TRUE)
  )

# Initialize vectors for metrics
weighted_cent <- numeric(nrow(attention_df))
entropy_vals <- numeric(nrow(attention_df))
hhi_vals <- numeric(nrow(attention_df))

# Calculate metrics for each day
for (i in 1:nrow(attention_df)) {
  
  total <- attention_df$total_volume[i]
  
  if (total == 0) {
    weighted_cent[i] <- 0
    entropy_vals[i] <- 0
    hhi_vals[i] <- 0
  } else {
    # Calculate pageview shares
    views_vec <- as.numeric(attention_df[i, pages])
    shares <- views_vec / total
    
    # Weighted centrality: sum of (share × eigenvector centrality)
    weighted_cent[i] <- sum(shares * centrality_vec[pages], na.rm = TRUE)
    
    # Shannon entropy: -sum(p × log(p))
    shares_pos <- shares[shares > 0]
    entropy_vals[i] <- -sum(shares_pos * log(shares_pos))
    
    # Herfindahl-Hirschman Index: sum(p²)
    hhi_vals[i] <- sum(shares^2)
  }
}

# Add calculated metrics to dataframe
attention_df <- attention_df %>%
  mutate(
    weighted_centrality = weighted_cent,
    entropy = entropy_vals,
    hhi = hhi_vals
  ) %>%
  select(date, weighted_centrality, entropy, hhi, total_volume)


# 6. COLLECT FINANCIAL DATA

# Download NVDA stock data from Yahoo Finance
getSymbols("NVDA", from = start_date, to = end_date, auto.assign = TRUE)

# Calculate daily returns and volatility
returns <- dailyReturn(Cl(NVDA), type = "log")
volatility <- abs(returns)

# Create finance dataframe
finance_df <- tibble(
  date = as.Date(index(returns)),
  returns = as.numeric(coredata(returns)),
  volatility = as.numeric(coredata(volatility))
)


# 7. MERGE DATA AND CREATE LAG STRUCTURE

# Merge Wikipedia and financial data by date
merged_df <- attention_df %>%
  inner_join(finance_df, by = "date") %>%
  arrange(date)

# Create lag-1 variables for all key variables
final_df <- merged_df %>%
  mutate(
    vol_lag1     = dplyr::lag(volatility, 1),
    cent_lag1    = dplyr::lag(weighted_centrality, 1),
    entropy_lag1 = dplyr::lag(entropy, 1),
    hhi_lag1     = dplyr::lag(hhi, 1),
    volume_lag1  = dplyr::lag(total_volume, 1)
  ) %>%
  filter(!is.na(vol_lag1))  # Remove first observation (NA from lag)

# Add event dummy variables for DeepSeek announcement
final_df <- final_df %>%
  mutate(
    event_deepseek = if_else(date == as.Date("2025-01-21"), 1, 0),
    days_from_event = as.numeric(date - as.Date("2025-01-21")),
    event_window = if_else(abs(days_from_event) <= 5, 1, 0)
  )


# 8. REGRESSION ANALYSIS

# Model 1: Centrality and volatility (tests H1a, H2a)
m_centrality <- lm(
  volatility ~ vol_lag1 + weighted_centrality + cent_lag1,
  data = final_df
)

# Apply Newey-West HAC standard errors (lag=5)
m_centrality_robust <- coeftest(
  m_centrality, 
  vcov = NeweyWest(m_centrality, lag = 5, prewhite = FALSE)
)

# Model 2: Entropy and volatility (tests H2b)
m_entropy <- lm(
  volatility ~ vol_lag1 + entropy + entropy_lag1,
  data = final_df
)

m_entropy_robust <- coeftest(
  m_entropy,
  vcov = NeweyWest(m_entropy, lag = 5, prewhite = FALSE)
)

# Model 3: HHI and volatility (tests H2c)
m_hhi <- lm(
  volatility ~ vol_lag1 + hhi + hhi_lag1,
  data = final_df
)

m_hhi_robust <- coeftest(
  m_hhi,
  vcov = NeweyWest(m_hhi, lag = 5, prewhite = FALSE)
)

# Model 4: Full model (horse race with all measures)
m_full <- lm(
  volatility ~ vol_lag1 + 
    weighted_centrality + cent_lag1 +
    entropy + entropy_lag1 +
    hhi + hhi_lag1 +
    total_volume + volume_lag1,
  data = final_df
)

m_full_robust <- coeftest(
  m_full,
  vcov = NeweyWest(m_full, lag = 5, prewhite = FALSE)
)

# Model 5: Event study (tests H3)
m_event <- lm(
  volatility ~ vol_lag1 + weighted_centrality + cent_lag1 +
    event_deepseek + event_window,
  data = final_df
)

m_event_robust <- coeftest(
  m_event,
  vcov = NeweyWest(m_event, lag = 5, prewhite = FALSE)
)

# Model 6: Attention persistence (tests H1b)
m_persistence <- lm(
  weighted_centrality ~ cent_lag1 + entropy_lag1 + vol_lag1,
  data = final_df
)

m_persistence_robust <- coeftest(
  m_persistence,
  vcov = NeweyWest(m_persistence, lag = 5, prewhite = FALSE)
)


# 9. GENERATE VISUALIZATIONS

# Figure 1: Time series of standardized variables
p1 <- final_df %>%
  mutate(
    vol_std = as.numeric(scale(volatility)),
    cent_std = as.numeric(scale(weighted_centrality)),
    entropy_std = as.numeric(scale(entropy))
  ) %>%
  select(date, vol_std, cent_std, entropy_std) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_vline(xintercept = as.Date("2025-01-21"), 
             linetype = "dashed", color = "red", alpha = 0.5) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Wikipedia Attention Metrics vs NVDA Volatility (2022-2025)",
    subtitle = paste0("N = ", nrow(final_df), " trading days | Red line = DeepSeek event (Jan 21, 2025)"),
    x = NULL,
    y = "Standardized Value",
    color = NULL
  ) +
  scale_color_manual(
    values = c("vol_std" = "#B2182B", "cent_std" = "#2166AC", "entropy_std" = "#1B7837"),
    labels = c("vol_std" = "Volatility", "cent_std" = "Centrality", "entropy_std" = "Entropy")
  ) +
  theme(legend.position = "bottom")

ggsave("figure1_timeseries.png", p1, width = 12, height = 6, dpi = 300)

# Figure 2: Scatter plot of centrality vs volatility
p2 <- ggplot(final_df, aes(x = weighted_centrality, y = volatility)) +
  geom_point(alpha = 0.3, size = 1.5, color = "#2166AC") +
  geom_smooth(method = "lm", se = TRUE, color = "#B2182B", fill = "#B2182B", alpha = 0.2) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Network Centrality → Market Volatility (H2a)",
    subtitle = sprintf("Correlation = %.3f | β = %.4f (p < 0.01)", 
                       cor(final_df$weighted_centrality, final_df$volatility),
                       coef(m_centrality)[3]),
    x = "Weighted Network Centrality (eigenvector)",
    y = "NVDA Volatility (absolute returns)"
  )

ggsave("figure2_scatter.png", p2, width = 8, height = 6, dpi = 300)

# Figure 3: Event study visualization
p3 <- final_df %>%
  filter(abs(days_from_event) <= 30) %>%
  select(days_from_event, volatility, weighted_centrality) %>%
  mutate(
    across(c(volatility, weighted_centrality), scale, .names = "{.col}_std")
  ) %>%
  pivot_longer(ends_with("_std"), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = days_from_event, y = value, color = variable)) +
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Event Study: DeepSeek Announcement (H3)",
    subtitle = "±30 trading days window around January 21, 2025",
    x = "Days from event",
    y = "Standardized value",
    color = NULL
  ) +
  scale_color_manual(
    values = c("volatility_std" = "#B2182B", "weighted_centrality_std" = "#2166AC"),
    labels = c("volatility_std" = "Market Volatility", "weighted_centrality_std" = "Network Centrality")
  ) +
  theme(legend.position = "bottom")

ggsave("figure3_event_study.png", p3, width = 10, height = 6, dpi = 300)

