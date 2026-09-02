library(tidyverse)
library(png)
library(countrycode)
library(jsonlite)

# 1. List all flag files
files <- list.files("data", pattern = "\\.png$", full.names = TRUE)
codes <- sub("\\.png$", "", basename(files))

# 2. Extract downsampled pixel color features (16x24 grid = 384 pixels x 3 RGB channels)
extract_features <- function(file, h = 16, w = 24) {
  img <- readPNG(file)
  if (length(dim(img)) == 3) {
    img <- img[, , 1:min(3, dim(img)[3])]
  } else {
    img <- array(rep(img, 3), dim = c(dim(img), 3))
  }
  r_idx <- round(seq(1, dim(img)[1], length.out = h))
  c_idx <- round(seq(1, dim(img)[2], length.out = w))
  as.vector(img[r_idx, c_idx, 1:3])
}

message("Extracting flag image features...")
feat_list <- lapply(files, extract_features)
feat_mat <- do.call(rbind, feat_list)
rownames(feat_mat) <- codes

# 3. Compute normalized Euclidean distance matrix (0 = identical, 1 = maximum distance)
raw_dist <- as.matrix(dist(feat_mat))
max_d <- sqrt(ncol(feat_mat))
norm_dist <- raw_dist / max_d

# 4. Map country names cleanly
names_vec <- countrycode(
  codes,
  origin = "iso2c",
  destination = "country.name",
  custom_match = c(
    "gb-eng" = "England",
    "gb-nir" = "Northern Ireland",
    "gb-sct" = "Scotland",
    "gb-wls" = "Wales",
    "xk" = "Kosovo"
  ),
  warn = FALSE
)
names_vec <- coalesce(names_vec, toupper(codes))
names(names_vec) <- codes

# 5. Build structured dataset with top 3 most and least similar flags for each country
flag_records <- map(codes, \(code) {
  dists <- norm_dist[code, ]
  # Exclude self
  other_dists <- sort(dists[names(dists) != code])
  
  closest_codes <- head(names(other_dists), 3)
  furthest_codes <- tail(names(other_dists), 3)
  
  tibble(
    code = code,
    name = names_vec[code],
    most_similar_1_code = closest_codes[1],
    most_similar_1_name = names_vec[closest_codes[1]],
    most_similar_2_code = closest_codes[2],
    most_similar_2_name = names_vec[closest_codes[2]],
    most_similar_3_code = closest_codes[3],
    most_similar_3_name = names_vec[closest_codes[3]],
    least_similar_1_code = furthest_codes[3],
    least_similar_1_name = names_vec[furthest_codes[3]],
    least_similar_2_code = furthest_codes[2],
    least_similar_2_name = names_vec[furthest_codes[2]],
    least_similar_3_code = furthest_codes[1],
    least_similar_3_name = names_vec[furthest_codes[1]]
  )
}) |> list_rbind() |> arrange(name)

# 6. Save data
saveRDS(flag_records, "data/flag_similarity.rds")
saveRDS(norm_dist, "data/dist_matrix.rds")
write_json(flag_records, "data/flag_similarity.json", pretty = TRUE)

message("Saved similarity data to data/flag_similarity.rds and data/flag_similarity.json")
