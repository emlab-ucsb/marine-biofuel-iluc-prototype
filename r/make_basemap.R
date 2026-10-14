#################################
# make_basemap.R
# Country outlines for the report's maps, as a plain CSV
#################################
#
# Writes data/natural_earth/ne_110m_admin_0_countries.csv.gz: Natural Earth 1:110m Admin 0
# country polygons (public domain, https://www.naturalearthdata.com/), as vertices that
# ggplot2::geom_polygon() can draw directly. Storing plain coordinates means the report needs
# only ggplot2, not sf, so the GitHub render needs no GDAL/GEOS/PROJ system libraries.
#
# Each country also gets model_country, its name in the land-use model results, so the report
# can join country results onto the map. The model pools a few countries into one region for
# estimation; each member country is mapped to its region.
#
# Run once, locally (needs rnaturalearth and sf, which are not in renv.lock):
#   Rscript r/make_basemap.R

ne <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
ne <- ne[ne$admin != "Antarctica", c("admin", "geometry")]

# Model regions that pool several countries, and names that differ from Natural Earth's.
model_country_crosswalk <- c(
  "Afghanistan" = "Afg_Pak_Turk_Uzb_Kyrg_Taji",
  "Pakistan"    = "Afg_Pak_Turk_Uzb_Kyrg_Taji",
  "Turkmenistan" = "Afg_Pak_Turk_Uzb_Kyrg_Taji",
  "Uzbekistan"  = "Afg_Pak_Turk_Uzb_Kyrg_Taji",
  "Kyrgyzstan"  = "Afg_Pak_Turk_Uzb_Kyrg_Taji",
  "Tajikistan"  = "Afg_Pak_Turk_Uzb_Kyrg_Taji",
  "Eritrea"     = "Eritrea_Ethiopia",
  "Ethiopia"    = "Eritrea_Ethiopia",
  "Mauritania"  = "Mauritania_Senegal",
  "Senegal"     = "Mauritania_Senegal",
  "Niger"       = "Niger_Chad",
  "Chad"        = "Niger_Chad",
  "Yemen"       = "Yemen_Oman_Jordan_Iraq_Iran",
  "Oman"        = "Yemen_Oman_Jordan_Iraq_Iran",
  "Jordan"      = "Yemen_Oman_Jordan_Iraq_Iran",
  "Iraq"        = "Yemen_Oman_Jordan_Iraq_Iran",
  "Iran"        = "Yemen_Oman_Jordan_Iraq_Iran",
  "North Macedonia" = "Macedonia",
  "Somaliland"  = "Somalia" # the model assigns Somaliland's cells to Somalia
)
stopifnot("Crosswalk names a country not in Natural Earth" =
            all(names(model_country_crosswalk) %in% ne$admin))
ne$model_country <- ifelse(
  ne$admin %in% names(model_country_crosswalk),
  model_country_crosswalk[ne$admin],
  ne$admin
)

# Vertices: for MULTIPOLYGONs, L3 = feature, L2 = polygon within it, L1 = ring (1 = outer).
coords <- as.data.frame(sf::st_coordinates(sf::st_cast(ne, "MULTIPOLYGON")))
basemap <- data.frame(
  admin = ne$admin[coords$L3],
  model_country = ne$model_country[coords$L3],
  piece = paste(coords$L3, coords$L2, sep = "_"), # one closed polygon
  ring = paste(coords$L3, coords$L2, coords$L1, sep = "_"), # holes share a piece
  lon = round(coords$X, 4),
  lat = round(coords$Y, 4)
)

dir.create(here::here("data/natural_earth"), showWarnings = FALSE)
out <- here::here("data/natural_earth/ne_110m_admin_0_countries.csv.gz")
readr::write_csv(basemap, out)
cat("Wrote", nrow(basemap), "vertices for", length(unique(basemap$admin)), "countries to", out, "\n")
