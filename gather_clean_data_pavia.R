# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(lubridate)
library(jsonlite)
library(magrittr)
library(tidyr)
library(httr)

# Variables ----------------------------------------------------------------------------------------
policy_id <- "4bf184e01e0f163296ab253edd60774e2d34367d0e7b6cbc689b567d"
project <- "Pavia"
time_now <- as_datetime(now())


# Databases ----------------------------------------------------------------------------------------
# Functions
loj <- function (X = NULL, Y = NULL, onCol = NULL) {
  if (truelength(X) == 0 | truelength(Y) == 0) 
    stop("setDT(X) and setDT(Y) first")
  n <- names(Y)
  X[Y, `:=`((n), mget(paste0("i.", n))), on = onCol]
}


# Extract information from cnft.io -----------------------------------------------------------------
api_link_cnft <- "https://api.cnft.io/market/listings"

query <- function(page, url, project, sold) {
  httr::content(httr::POST(
    url = url, 
    body = list(
      search = "", 
      types = c("listing", "offer"), 
      project = project, 
      sort = list(`_id` = -1L), 
      priceMin = NULL, 
      priceMax = NULL, 
      page = page, 
      verified = TRUE, 
      nsfw = FALSE, 
      sold = sold, 
      smartContract = NULL
    ), 
    encode = "json"
  ), simplifyVector = TRUE)
}

query_n <- function(url, project, sold, n = "all") {
  if (n == "all") n <- query(1L, url, project, sold)[["count"]]
  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- query(i, url, project, sold)[["results"]]
    if (length(out[[i]]) < 1L) return(out[seq_len(i - 1L)])
  }
  out
}

CNFT <- query_n(api_link_cnft, project, sold = FALSE) %>%
  lapply(data.table) %>% rbindlist(fill = TRUE)

CNFT <- CNFT[asset.policyId == policy_id]
CNFT[, asset        := asset.metadata.name]
CNFT[, link         := paste0("https://cnft.io/token/", X_id)]
CNFT[, price        := price/10**6]
CNFT[, sc           := ifelse(is.na(smartContractTxid), "no", "yes")]
CNFT[, market       := "cnft.io"]

for (i in 1:nrow(CNFT)) {
  .offers <- CNFT[i, offers[[1]]]
  if (nrow(.offers) == 0) CNFT[i, last_offer := 0]
  if (nrow(.offers) > 0) CNFT[i, last_offer := max(.offers$offer/10**6)]
}
CNFT[type == "listing", last_offer := NA]

CNFT <- CNFT[, .(asset, type, price, last_offer, sc, market, link)]


# CNFT sales ---------------------------------------------------------------------------------------
CNFTS <- query_n(api_link_cnft, project, 11, sold = TRUE) %>%
  lapply(data.table) %>% rbindlist(fill = TRUE)

CNFTS[, asset         := asset.metadata.name]
CNFTS[, price         := price/10**6]
CNFTS[, market        := "cnft.io"]
CNFTS[, sold_at       := as_datetime(soldAt)]
CNFTS[, sold_at_hours := difftime(time_now, sold_at, units = "hours")]
CNFTS[, sold_at_days  := difftime(time_now, sold_at, units = "days")]

CNFTS <- CNFTS[order(-sold_at), .(asset, price, sold_at, sold_at_hours, sold_at_days, market)]
CNFTS <- CNFTS[sold_at_hours <= 24*3]


# Extract information from jpg.store ---------------------------------------------------------------
# jpg.store/api/policy - all supported policies
# jpg.store/api/policy/[id]/listings - listings for a given policy
# jpg.store/api/policy/[id]/sales - sales for a given policy

api_link <- sprintf("jpg.store/api/policy/%s/listings", policy_id)

JPG <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
JPG[, link         := paste0("https://www.jpg.store/asset/", asset)]
JPG[, price        := price_lovelace]
JPG[, asset        := asset_display_name]
JPG[, price        := price/10**6]
JPG[, sc           := "yes"]
JPG[, market       := "jpg.store"]

JPG <- JPG[, .(asset, type = "listing", price, last_offer = NA, sc, market, link)]


# JPG sales ----------------------------------------------------------------------------------------
api_link <- sprintf("jpg.store/api/policy/%s/sales", policy_id)

JPGS <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
JPGS[, price         := price_lovelace]
JPGS[, asset         := asset_display_name]
JPGS[, price         := price/10**6]
JPGS[, market        := "jpg.store"]
JPGS[, sold_at       := as_datetime(purchased_at)]
JPGS[, sold_at_hours := difftime(time_now, sold_at, units = "hours")]
JPGS[, sold_at_days  := difftime(time_now, sold_at, units = "days")]

JPGS <- JPGS[order(-sold_at), .(asset, price, sold_at, sold_at_hours, sold_at_days, market)]
JPGS <- JPGS[sold_at_hours <= 24*3]


# Merge markets data -------------------------------------------------------------------------------
# Listings
DT <- rbindlist(list(CNFT, JPG), fill = TRUE, use.names = TRUE)

# Sales
DTS <- rbindlist(list(CNFTS, JPGS), fill = TRUE, use.names = TRUE)

# Add data collection timestamp
DT[, data_date := time_now]
DTS[, data_date := time_now]


# Save ---------------------------------------------------------------------------------------------
saveRDS(DT, file = "data/DT.rds")
saveRDS(DTS, file = "data/DTS.rds")


# Database evolution -------------------------------------------------------------------------------
DTE <- copy(DT)
if (file.exists("data/DTE_pavia.rds")) {
  cat("File data/DTE exists:", file.exists("data/DTE_pavia.rds"), "\n")
  DTE_old <- readRDS("data/DTE_pavia.rds")
  DTE <- rbindlist(list(DTE, DTE_old))
  DTE <- DTE[difftime(time_now, data_date, units = "hours") <= 24] # Only retain last 24 hours
}
saveRDS(DTE, file = "data/DTE_pavia.rds")
