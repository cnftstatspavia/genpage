# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(jsonlite)
library(tidyr)
library(httr)

RAR <- readRDS("data/RAR_sac.rds")

policy_id <- "ee6da4b71e0913cbebec02edc23653f9b970af69324fdddfed1285d9"

# Functions
loj <- function (X = NULL, Y = NULL, onCol = NULL) {
  if (truelength(X) == 0 | truelength(Y) == 0) 
    stop("setDT(X) and setDT(Y) first")
  n <- names(Y)
  X[Y, `:=`((n), mget(paste0("i.", n))), on = onCol]
}


# Extract information from cnft.io -----------------------------------------------------------------
api_link <- "https://api.cnft.io/market/listings"
project <- "Space Ape Club - SAC Apes"

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

CNFT <- CNFT[asset.policyId == policy_id]

CNFT[, asset        := asset.metadata.name]
CNFT[, link         := paste0("https://cnft.io/token/", X_id)]
CNFT[, asset_number := as.numeric(gsub("SpaceApe #", "", asset))]
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
  "—", "Eyes_", asset.metadata.traits.Eyes,
  "—", "FurColor_", asset.metadata.traits.Fur.Color,
  "—", "SpaceSuit_", asset.metadata.traits.Space.Suit,
  "—", "Mouth_", asset.metadata.traits.Mouth,
  "—", "BackAccessory_", asset.metadata.traits.Back.Accessory,
  "—", "NormalClothing_", asset.metadata.traits.Normal.Clothing, "—"
)]

CNFT[, asset_traits := paste0(
  "Eyes:", asset.metadata.traits.Eyes,
  " — ", "FurColor:", asset.metadata.traits.Fur.Color,
  " — ", "SpaceSuit:", asset.metadata.traits.Space.Suit,
  " — ", "Mouth:", asset.metadata.traits.Mouth,
  " — ", "BackAccessory:", asset.metadata.traits.Back.Accessory,
  " — ", "NormalClothing:", asset.metadata.traits.Normal.Clothing
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
JPG[, asset        := asset_display_name]
JPG[, asset_number := as.numeric(gsub("SpaceApe #", "", asset))]
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

DT[, rarity_range := trunc(asset_rarity/50)*50]
DT[, rarity_range := paste0(rarity_range, "-", rarity_range + (50-1))]
.levels_order <- DT[order(asset_rarity), unique(rarity_range)]
DT[, rarity_range := factor(rarity_range, levels = .levels_order)]


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

