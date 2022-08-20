# Reads prodpad swagger api, then save as internal data

url <- "https://app.swaggerhub.com/apiproxy/registry/ProdPad/prodpad/1.0"
prodpad_api_v1 <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)

usethis::use_data(prodpad_api_v1, internal = TRUE, version = 3)
