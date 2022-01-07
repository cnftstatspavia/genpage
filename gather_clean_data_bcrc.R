# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(jsonlite)
library(tidyr)
library(httr)

RAR <- readRDS("data/RAR_bcrc.rds")

# Functions
loj <- function (X = NULL, Y = NULL, onCol = NULL) {
  if (truelength(X) == 0 | truelength(Y) == 0) 
    stop("setDT(X) and setDT(Y) first")
  n <- names(Y)
  X[Y, `:=`((n), mget(paste0("i.", n))), on = onCol]
}


# Extract information from cnft.io -----------------------------------------------------------------
api_link <- "https://api.cnft.io/market/listings"
project <- "Boss Cat Rocket Club"

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

CNFT[, asset        := asset.metadata.name]
CNFT[, link         := paste0("https://cnft.io/token/", X_id)]
CNFT[, asset_number := as.numeric(gsub("Boss Cat Rocket Club #", "", asset))]
CNFT[, price        := price/10**6]
CNFT[, sc           := ifelse(is.na(smartContractTxid), "no", "yes")]
CNFT[, market       := "cnft.io"]

for (i in 1:nrow(CNFT)) {
  .offers <- CNFT[i, offers[[1]]]
  if (nrow(.offers) == 0) CNFT[i, last_offer := 0]
  if (nrow(.offers) > 0) CNFT[i, last_offer := max(.offers$offer/10**6)]
}
CNFT[type == "listing", last_offer := NA]


CNFT <- CNFT[, .(asset, asset_number, type, price, last_offer, sc, market, link)]


# Extract information from jpg.store ---------------------------------------------------------------
# jpg.store/api/policy - all supported policies
# jpg.store/api/policy/[id]/listings - listings for a given policy
# jpg.store/api/policy/[id]/sales - sales for a given policy


api_link <- "jpg.store/api/policy/c364930bd612f42e14d156e1c5410511e77f64cab8f2367a9df544d1/listings"

JPG <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
JPG[, link         := paste0("https://www.jpg.store/asset/", asset)]
JPG[, price        := price_lovelace]
JPG[, asset        := asset_display_name]
JPG[, asset_number := as.numeric(gsub("Boss Cat Rocket Club #", "", asset))]
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

DT[, rarity_range := fcase(asset_rarity %between% c(70, 90), "70-90",
                           asset_rarity %between% c(90.001, 110), "90-110",
                           asset_rarity %between% c(110.001, 130), "110-130",
                           asset_rarity %between% c(130.001, 150), "130-150",
                           asset_rarity %between% c(150.001, 170), "150-170",
                           asset_rarity %between% c(170.001, 190), "170-190",
                           asset_rarity %between% c(190.001, 210), "190-210",
                           asset_rarity %between% c(210.001, 230), "210-230",
                           asset_rarity %between% c(230.001, 250), "230-250",
                           asset_rarity %between% c(250.001, 270), "250-270",
                           asset_rarity %between% c(270.001, 280), "270-280",
                           asset_rarity %between% c(280.001, 290), "280-290",
                           asset_rarity %between% c(290.001, 300), "290-300",
                           asset_rarity %between% c(300.001, 310), "300-310",
                           asset_rarity %between% c(310.001, 320), "310-320") %>% 
     factor(levels = c("70-90",
                       "90-110",
                       "110-130",
                       "130-150",
                       "150-170",
                       "170-190",
                       "190-210",
                       "210-230",
                       "230-250",
                       "250-270",
                       "270-280",
                       "280-290",
                       "290-300",
                       "300-310",
                       "310-320"))]


# Remove BRCR for which we do not have the rank/rarity score ---------------------------------------
DT <- DT[!(asset_number %in% c(3097, 7210, 9535, 8114))]


# Large format -------------------------------------------------------------------------------------
.cols <- names(DT)[names(DT) %like% "background_|earring_|hat_|eyes_|mouth_|clothes_|fur_"]
DTL <- data.table(gather(DT, raw_trait, has_trait, all_of(.cols)))
DTL <- DTL[has_trait == 1]

DTL[, trait_category := strsplit(raw_trait, "_")[[1]][1], 1:nrow(DTL)]
DTL[, trait          := strsplit(raw_trait, "_")[[1]][2], 1:nrow(DTL)]


# Save ---------------------------------------------------------------------------------------------
saveRDS(DT, file = "data/DT.rds")
saveRDS(DTL, file = "data/DTL.rds")


# Task schedule ------------------------------------------------------------------------------------
# taskscheduleR::taskscheduler_create(taskname = "BCRC_check",
#                                     rscript = normalizePath(list.files(pattern = "main.R")),
#                                     schedule = "MINUTE", modifier = 7)

## Delete task
# taskscheduleR::taskscheduler_delete(taskname = "BCRC_check")


# Notification -------------------------------------------------------------------------------------
# DT_latest <- fread("data/DT_latest.csv")
# is_super_rare <- any(DT$index_rank_z > 3) # check is there very good deal
# is_new <- DT_latest[1, asset] != DT[1, asset] # Check if asset is new
# 
# if(is_super_rare & is_new) {
#   beepr::beep(2)
#   DT[1, browseURL(link)]
#   fwrite(DT, "data/DT_latest.csv")
# }
# https://towardsdatascience.com/effective-notification-mechanisms-in-r-82db9cb8816
