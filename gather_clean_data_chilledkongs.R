# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(jsonlite)
library(tidyr)
library(httr)

RAR <- readRDS("data/RAR_chilledkongs.rds")

policy_id <- "c56d4cceb8a8550534968e1bf165137ca41e908d2d780cc1402079bd"

# Functions
loj <- function (X = NULL, Y = NULL, onCol = NULL) {
  if (truelength(X) == 0 | truelength(Y) == 0) 
    stop("setDT(X) and setDT(Y) first")
  n <- names(Y)
  X[Y, `:=`((n), mget(paste0("i.", n))), on = onCol]
}


# Extract information from cnft.io -----------------------------------------------------------------
api_link <- "https://api.cnft.io/market/listings"
project <- "ChilledKongs"

query <- function(page, url, project) {
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
      sold = FALSE, 
      smartContract = NULL
    ), 
    encode = "json"
  ), simplifyVector = TRUE)
}

query_all <- function(url, project) {
  n <- query(1L, url, project)[["count"]]
  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- query(i, url, project)[["results"]]
    if (length(out[[i]]) < 1L)
      return(out[seq_len(i - 1L)])
  }
  out
}

CNFT <- query_all(api_link, project) |> lapply(data.table) |> rbindlist(fill = TRUE)

CNFT <- CNFT[!(asset.metadata.name %like% "Christmas|Collectible")]
CNFT[, asset        := gsub("ChilledKong", "Chilled Kong #", asset.metadata.name)]
CNFT[, link         := paste0("https://cnft.io/token/", X_id)]
CNFT[, asset_number := as.numeric(gsub("Chilled Kong #", "", asset))]
CNFT[, price        := price/10**6]
CNFT[, sc           := ifelse(is.na(smartContractTxid), "no", "yes")]
CNFT[, market       := "cnft.io"]

for (i in 1:nrow(CNFT)) {
  .offers <- CNFT[i, offers[[1]]]
  if (nrow(.offers) == 0) CNFT[i, last_offer := 0]
  if (nrow(.offers) > 0) CNFT[i, last_offer := max(.offers$offer/10**6)]
}
CNFT[type == "listing", last_offer := NA]

CNFT[, asset_traits_raw := paste0(
  "—", "Body_", asset.metadata.body,
  "—", "Eyes_", asset.metadata.eyes,
  "—", "Mouth_", asset.metadata.mouth,
  "—", "Clothes_", asset.metadata.clothes,
  "—", "Special_", asset.metadata.special,
  "—", "Earrings_", asset.metadata.earrings,
  "—", "Background_", asset.metadata.background, "—"
)]

CNFT[, asset_traits := paste0(
  "Body:", asset.metadata.body,
  " — ", "Eyes:", asset.metadata.eyes,
  " — ", "Mouth:", asset.metadata.mouth,
  " — ", "Clothes:", asset.metadata.clothes,
  " — ", "Special:", asset.metadata.special,
  " — ", "Earrings:", asset.metadata.earrings,
  " — ", "Background:", asset.metadata.background
)]

CNFT <- CNFT[, .(asset, asset_number, type, price, last_offer, sc, market, link,
                 asset_traits_raw, asset_traits)]


# Extract information from jpg.store ---------------------------------------------------------------
# jpg.store/api/policy - all supported policies
# jpg.store/api/policy/[id]/listings - listings for a given policy
# jpg.store/api/policy/[id]/sales - sales for a given policy


api_link <- sprintf("jpg.store/api/policy/%s/listings", policy_id)

JPG <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
JPG[, link         := paste0("https://www.jpg.store/asset/", asset)]
JPG[, price        := price_lovelace]
JPG[, asset        := gsub("ChilledKong", "Chilled Kong #", asset_display_name)]
JPG[, asset_number := as.numeric(gsub("Chilled Kong #", "", asset))]
JPG[, price        := price/10**6]
JPG[, sc           := "yes"]
JPG[, market       := "jpg.store"]

JPG <- JPG[, .(asset, asset_number, type = "listing", price, last_offer = NA, sc, market, link)]

# Merge info
DT <- rbindlist(list(CNFT, JPG), fill = TRUE, use.names = TRUE)


# Rarity and ranking -------------------------------------------------------------------------------
setDT(DT); setDT(RAR)
loj(DT, RAR, "asset_number")

DT[, rank_range := fcase(asset_rank %between% c(1, 100), "1-100",
                         asset_rank %between% c(101, 250), "101-250",
                         asset_rank %between% c(251, 500), "251-500",
                         asset_rank %between% c(501, 750), "501-750",
                         asset_rank %between% c(751, 1000), "751-1000",
                         asset_rank %between% c(1001, 1500), "1001-1500",
                         asset_rank %between% c(1501, 2000), "1501-2000",
                         asset_rank %between% c(2001, 3000), "2001-3000",
                         asset_rank %between% c(3001, 4000), "3001-4000",
                         asset_rank %between% c(4001, 5000), "4001-5000",
                         asset_rank %between% c(5001, 6000), "5001-6000",
                         asset_rank %between% c(6001, 7000), "6001-7000",
                         asset_rank %between% c(7001, 8000), "7001-8000",
                         asset_rank %between% c(8001, 9000), "8001-9000",
                         asset_rank %between% c(9001, 9999), "9001-9999") %>% 
     factor(levels = c("1-100",
                       "101-250",
                       "251-500",
                       "501-750",
                       "751-1000",
                       "1001-1500",
                       "1501-2000",
                       "2001-3000",
                       "3001-4000",
                       "4001-5000",
                       "5001-6000",
                       "6001-7000",
                       "7001-8000",
                       "8001-9000",
                       "9001-9999"))]


DT[, rarity_range := fcase(asset_rarity %between% c(10, 15), "10-15",
                           asset_rarity %between% c(15.001, 20), "15-20",
                           asset_rarity %between% c(20.001, 25), "20-25",
                           asset_rarity %between% c(25.001, 30), "25-30",
                           asset_rarity %between% c(30.001, 35), "30-35",
                           asset_rarity %between% c(35.001, 40), "35-40",
                           asset_rarity %between% c(40.001, 45), "40-45",
                           asset_rarity %between% c(45.001, 50), "45-50",
                           asset_rarity %between% c(50.001, 55), "50-55",
                           asset_rarity %between% c(100.001, 120), "100-120",
                           asset_rarity %between% c(737, 737), "737") %>% 
     factor(levels = c("10-15",
                       "15-20",
                       "20-25",
                       "25-30",
                       "30-35",
                       "35-40",
                       "40-45",
                       "45-50",
                       "50-55",
                       "100-120",
                       "737"))]


# Large format -------------------------------------------------------------------------------------
# .cols <- names(DT)[names(DT) %like% "background_|earring_|hat_|eyes_|mouth_|clothes_|fur_"]
# DTL <- data.table(gather(DT, raw_trait, has_trait, all_of(.cols)))
# DTL <- DTL[has_trait == 1]
# 
# DTL[, trait_category := strsplit(raw_trait, "_")[[1]][1], 1:nrow(DTL)]
# DTL[, trait          := strsplit(raw_trait, "_")[[1]][2], 1:nrow(DTL)]

DT[, `:=`(asset_traits_num = "NA", asset_traits = "NA")] # REMOVE THISSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS


# Save ---------------------------------------------------------------------------------------------
saveRDS(DT, file = "data/DT.rds")
# saveRDS(DTL, file = "data/DTL.rds")

