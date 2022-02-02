# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(lubridate)
library(jsonlite)
library(tidyr)
library(httr)

# Variables ----------------------------------------------------------------------------------------
policy_id <- "c364930bd612f42e14d156e1c5410511e77f64cab8f2367a9df544d1"
project <- "Boss Cat Rocket Club"
time_now <- as_datetime(now())


# Databases ----------------------------------------------------------------------------------------
RAR <- readRDS("data/RAR_bcrc.rds")


# Functions ----------------------------------------------------------------------------------------
extract_num <- function(x) as.numeric(regmatches(x, regexpr("[[:digit:]]+", x)))

loj <- function (X = NULL, Y = NULL, onCol = NULL) {
  if (truelength(X) == 0 | truelength(Y) == 0) 
    stop("setDT(X) and setDT(Y) first")
  n <- names(Y)
  X[Y, `:=`((n), mget(paste0("i.", n))), on = onCol]
}


# CNFT listings ------------------------------------------------------------------------------------
api_link_cnft <- "https://api.cnft.io/market/listings"

query <- function(page, url, project, sold) {
  httr::content(httr::POST(
    url = url, 
    body = list(
      search = "", 
      types = c("listing", "offer", "bundle"),
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

.CNFT <- query_n(api_link_cnft, project, sold = FALSE) |>
  lapply(data.table) |> rbindlist(fill = TRUE)

.CNFT[, link := paste0("https://cnft.io/token/", ifelse(is.na(X_id), `_id`, X_id))]

# Initialize data.table
CNFT <- data.table(asset = NA, asset_number = NA, type = NA, price = NA,
                   last_offer = NA, sc = NA, market = NA, link = NA)

for (i in 1:nrow(.CNFT)) {
  CNFT <- rbind(CNFT, data.table(asset        = .CNFT[i, assets[[1]]$metadata$name],
                                 asset_number = as.numeric(NA),
                                 type         = .CNFT[i, type],
                                 price        = .CNFT[i, price],
                                 last_offer   = .CNFT[i, offers],
                                 sc           = .CNFT[i, smartContractTxid],
                                 market       = "cnft.io",
                                 link         = .CNFT[i, link]))
}

CNFT <- CNFT[2:nrow(CNFT)] # Clear first row from initialization
CNFT[, asset_number := extract_num(asset)]
CNFT[, price        := price/10**6]
CNFT[, sc           := ifelse(is.na(sc), "no", "yes")]

for (i in 1:nrow(CNFT)) {
  .offers <- CNFT[i, last_offer[[1]]]
  if (nrow(.offers) == 0) {
    CNFT[i, last_offer := NA]
  } else {
    CNFT[i, last_offer := max(.offers$offer/10**6)]
  }
}


# CNFT sales ---------------------------------------------------------------------------------------
.CNFTS <- query_n(api_link_cnft, project, sold = TRUE, n = 11) |>
  lapply(data.table) |> rbindlist(fill = TRUE)

.CNFTS <- .CNFTS[!is.na(soldAt)]

# Initialize data.table
CNFTS <- data.table(asset = NA, asset_number = NA, price = NA,
                    market = NA, sold_at = NA)

for (i in 1:nrow(.CNFTS)) {
  CNFTS <- rbind(CNFTS, data.table(asset        = .CNFTS[i, assets[[1]]$metadata$name],
                                   asset_number = as.numeric(NA),
                                   price        = .CNFTS[i, price],
                                   market       = "cnft.io",
                                   sold_at      = .CNFTS[i, soldAt]))
}

CNFTS <- CNFTS[2:nrow(CNFTS)] # Clear first row from initialization
CNFTS[, asset_number  := extract_num(asset)]
CNFTS[, price         := price/10**6]
CNFTS[, market        := "cnft.io"]
CNFTS[, sold_at       := as_datetime(sold_at)]
CNFTS[, sold_at_hours := difftime(time_now, sold_at, units = "hours")]
CNFTS[, sold_at_days  := difftime(time_now, sold_at, units = "days")]

CNFTS <- CNFTS[order(-sold_at), .(asset, asset_number, price, sold_at, sold_at_hours, 
                                  sold_at_days, market)]
CNFTS <- CNFTS[sold_at_hours <= 24*3]


# JPG listings -------------------------------------------------------------------------------------
# jpg.store/api/policy - all supported policies
# jpg.store/api/policy/[id]/listings - listings for a given policy
# jpg.store/api/policy/[id]/sales - sales for a given policy
api_link <- sprintf("jpg.store/api/policy/%s/listings", policy_id)

JPG <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
JPG[, link         := paste0("https://www.jpg.store/asset/", asset)]
JPG[, price        := price_lovelace]
JPG[, asset        := asset_display_name]
JPG[, asset_number := as.numeric(gsub("Boss Cat Rocket Club #", "", asset))]
JPG[, price        := price/10**6]
JPG[, sc           := "yes"]
JPG[, market       := "jpg.store"]

JPG <- JPG[, .(asset, asset_number, type = "listing", price, last_offer = NA, sc, market, link)]


# JPG sales ----------------------------------------------------------------------------------------
api_link <- sprintf("jpg.store/api/policy/%s/sales", policy_id)

JPGS <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
JPGS[, price         := price_lovelace]
JPGS[, asset         := asset_display_name]
JPGS[, asset_number  := as.numeric(gsub("Boss Cat Rocket Club #", "", asset))]
JPGS[, price         := price/10**6]
JPGS[, market        := "jpg.store"]
JPGS[, sold_at       := as_datetime(purchased_at)]
JPGS[, sold_at_hours := difftime(time_now, sold_at, units = "hours")]
JPGS[, sold_at_days  := difftime(time_now, sold_at, units = "days")]

JPGS <- JPGS[order(-sold_at), .(asset, asset_number, price, sold_at, sold_at_hours,
                                sold_at_days, market)]
JPGS <- JPGS[sold_at_hours <= 24*3]


# Merge markets data -------------------------------------------------------------------------------
# Listings
DT <- rbindlist(list(CNFT, JPG), fill = TRUE, use.names = TRUE)

# Sales
DTS <- rbindlist(list(CNFTS, JPGS), fill = TRUE, use.names = TRUE)

# Add data collection timestamp
DT[, data_date := time_now]
DTS[, data_date := time_now]


# Rarity and ranking -------------------------------------------------------------------------------
setDT(DT); setDT(RAR)
loj(DT, RAR, "asset_number")

setDT(DTS); setDT(RAR)
loj(DTS, RAR, "asset_number")

DT[, rank_range := fcase(asset_rank %between% c(1,    10),   "1-10",
                         asset_rank %between% c(11,   100),  "11-100",
                         asset_rank %between% c(101,  1000), "101-1000",
                         asset_rank %between% c(1001, 1500), "1001-2000",
                         asset_rank %between% c(2001, 3000), "2001-3000",
                         asset_rank %between% c(3001, 4000), "3001-4000",
                         asset_rank %between% c(4001, 5000), "4001-5000",
                         asset_rank %between% c(5001, 6000), "5001-6000",
                         asset_rank %between% c(6001, 7000), "6001-7000",
                         asset_rank %between% c(7001, 8000), "7001-8000",
                         asset_rank %between% c(8001, 9000), "8001-9000",
                         asset_rank %between% c(9001, 9999), "9001-9999") %>% 
     factor(levels = c("1-10",
                       "11-100",
                       "101-1000",
                       "1001-2000",
                       "2001-3000",
                       "3001-4000",
                       "4001-5000",
                       "5001-6000",
                       "6001-7000",
                       "7001-8000",
                       "8001-9000",
                       "9001-9999"))]

DTS[, rank_range := fcase(asset_rank %between% c(1,    10),   "1-10",
                          asset_rank %between% c(11,   100),  "11-100",
                          asset_rank %between% c(101,  1000), "101-1000",
                          asset_rank %between% c(1001, 1500), "1001-2000",
                          asset_rank %between% c(2001, 3000), "2001-3000",
                          asset_rank %between% c(3001, 4000), "3001-4000",
                          asset_rank %between% c(4001, 5000), "4001-5000",
                          asset_rank %between% c(5001, 6000), "5001-6000",
                          asset_rank %between% c(6001, 7000), "6001-7000",
                          asset_rank %between% c(7001, 8000), "7001-8000",
                          asset_rank %between% c(8001, 9000), "8001-9000",
                          asset_rank %between% c(9001, 9999), "9001-9999") %>% 
      factor(levels = c("1-10",
                        "11-100",
                        "101-1000",
                        "1001-2000",
                        "2001-3000",
                        "3001-4000",
                        "4001-5000",
                        "5001-6000",
                        "6001-7000",
                        "7001-8000",
                        "8001-9000",
                        "9001-9999"))]


# Remove BRCR for which we do not have the rank/rarity score ---------------------------------------
DT <- DT[!(asset_number %in% c(3097, 7210, 9535, 8114))]
DTS <- DTS[!(asset_number %in% c(3097, 7210, 9535, 8114))]


# Large format -------------------------------------------------------------------------------------
.cols <- names(DT)[names(DT) %like% "background_|earring_|hat_|eyes_|mouth_|clothes_|fur_"]
DTL <- data.table(gather(DT, raw_trait, has_trait, all_of(.cols)))
DTL <- DTL[has_trait == 1]

DTL[, trait_category := strsplit(raw_trait, "_")[[1]][1], 1:nrow(DTL)]
DTL[, trait          := gsub(trait_category, "", raw_trait), 1:nrow(DTL)]
DTL[, trait          := gsub("_", " ", trait), 1:nrow(DTL)]
DTL[, trait          := gsub("^ ", "", trait), 1:nrow(DTL)]

DTL[, trait_category_and_trait := paste0(trait_category, "_", trait)]


# Save ---------------------------------------------------------------------------------------------
saveRDS(DT, file = "data/DT.rds")
saveRDS(DTL, file = "data/DTL.rds")
saveRDS(DTS, file = "data/DTS.rds")


# Database evolution -------------------------------------------------------------------------------
DTE <- copy(DT)
if (file.exists("data/DTE_bcrc.rds")) {
  cat("File data/DTE exists:", file.exists("data/DTE_bcrc.rds"), "\n")
  DTE_old <- readRDS("data/DTE_bcrc.rds")
  DTE <- rbindlist(list(DTE, DTE_old))
  DTE <- DTE[difftime(time_now, data_date, units = "hours") <= 24] # Only retain last 24 hours
}
saveRDS(DTE, file = "data/DTE_bcrc.rds")
