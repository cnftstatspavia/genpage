# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(jsonlite)
library(tidyr)
library(httr)

policy_id <- "4bf184e01e0f163296ab253edd60774e2d34367d0e7b6cbc689b567d"

# Functions
loj <- function (X = NULL, Y = NULL, onCol = NULL) {
  if (truelength(X) == 0 | truelength(Y) == 0) 
    stop("setDT(X) and setDT(Y) first")
  n <- names(Y)
  X[Y, `:=`((n), mget(paste0("i.", n))), on = onCol]
}


# Extract information from cnft.io -----------------------------------------------------------------
api_link <- "https://api.cnft.io/market/listings"
project <- "Pavia"

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

# Merge info
DT <- rbindlist(list(CNFT, JPG), fill = TRUE, use.names = TRUE)


# Save ---------------------------------------------------------------------------------------------
saveRDS(DT, file = "data/DT.rds")
