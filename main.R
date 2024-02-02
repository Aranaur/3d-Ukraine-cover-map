# 1. Packages
library(pacman)

p_load(
  terra,
  giscoR,
  sf,
  tidyverse,
  ggtern,
  elevatr,
  png,
  rayshader,
  magick,
  furrr,
  here,
  future,
  rvest,
  RSelenium,
  netstat,
  janitor,
  extrafont
)

# 2. Load UA borders

country_sf <- gisco_get_countries(
  country = "UA",
  resolution = "1"
)

plot(sf::st_geometry(country_sf))
png("ua-borders.png")

# 3. Get land cover data

## https://livingatlas.arcgis.com/landcoverexplorer

urls <- c(
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/34U_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/35U_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/36U_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/37U_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/35T_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/36T_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/37T_20220101-20230101.tif"
)

options(timeout = 480)

download_if_not_exists <- function(url) {
  if (!file.exists(basename(url))) {
    download.file(url, basename(url), mode = "wb")
  }
}

map(urls, download_if_not_exists)

# 4. Load tiles

raster_files <- list.files(
  path = here(),
  pattern = "20230101.tif$",
  full.names = TRUE
)

crs <- "EPSG:4326"

if(!file.exists("ua_land_cover_vrt.vrt")){
  for(raster in raster_files){
    cat("Start with:", raster, "\n")
    rasters <- terra::rast(raster)
    country <- country_sf %>% 
      sf::st_transform(
        crs = terra::crs(
          rasters
        )
      )
    land_cover <- terra::crop(
      rasters,
      terra::vect(
        country
      ),
      snap = "in",
      mask = TRUE
    ) %>% 
      terra::aggregate(
        fact = 5,
        fun = "modal"
      ) %>% 
      terra::project(crs)
    # write raster
    terra::writeRaster(
      land_cover,
      paste0(
        raster,
        "_ua",
        ".tif"
      ),
      overwrite=FALSE
    )
  }
}

# 5. Load virtual layer

r_list <- list.files(
  path = here(),
  pattern = "_ua.tif",
  full.names = TRUE
)

land_cover_vrt <- terra::vrt(
  r_list,
  "ua_land_cover_vrt.vrt",
  overwrite = TRUE
)

# 6. Get original colors

ras <- terra::rast(
  raster_files[[1]]
)

raster_color_table <- do.call(
  data.frame,
  terra::coltab(ras)
)

hex_code <- ggtern::rgb2hex(
  r = raster_color_table[,2],
  g = raster_color_table[,3],
  b = raster_color_table[,4]
)

# 7. Assign colors

cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))
land_cover_vrt <- na.omit(land_cover_vrt)

land_cover_ua <- terra::subst(
  land_cover_vrt,
  from = from,
  to = to,
  names = cols
)

terra::plotRGB(land_cover_ua)

img_file <- "land_cover_ua_2.png"

terra::writeRaster(
  land_cover_ua,
  img_file,
  overwrite = TRUE,
  NAflag = 255
)

# 8. Digital elevation model

elev <- elevatr::get_elev_raster(
  locations = country_sf,
  z = 8,
  clip = "locations"
)

crs_lambert <-
  "+proj=tmerc +lat_0=0 +lon_0=21 +k=1 +x_0=300000 +y_0=0 +ellps=krass +towgs84=24,-121,-76,0,0,0,0 +units=m +no_defs +type=crs" # Lambert Azimuthal Equal Area

land_cover_ua_resampled <- terra::resample(
  x = land_cover_ua,
  y = terra::rast(elev),
  method = "near"
) %>% 
  terra::project(crs_lambert)

terra::plotRGB(land_cover_ua_resampled)

img_file <- "land_cover_ua.png"

terra::writeRaster(
  land_cover_ua_resampled,
  img_file,
  overwrite = TRUE,
  NAflag = 255
)

img <- png::readPNG(img_file)

# 9. Create 3D map

elev_lambert <- elev %>%
  terra::rast() %>%
  terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
  elev_lambert
)

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat %>% 
  rayshader::height_shade(
    texture = colorRampPalette(
      cols[9]
    )(256)
  ) %>% 
  rayshader::add_overlay(
    img,
    alphalayer = 1
  ) %>% 
  rayshader::plot_3d(
    elmat,
    zscale = 12,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      w / 10, h / 10
    ),
    zoom = .5,
    phi = 85,
    theta = 0
  )

rayshader::render_camera(
  zoom = .58
)

# 10. Render image

filename <- "3d_land_cover_ua-dark.png"

rayshader::render_highquality(
  filename = filename,
  preview = TRUE,
  light = FALSE,
  environment_light = here('air_museum_playground_4k.hdr'),
  intensity_env = 1,
  rotate_env = 90,
  interactive = FALSE,
  parallel = TRUE,
  width = w * 1.5,
  height = h * 1.5
)

# 11. Get zonal multiclas statistics
driver <- rsDriver(browser = "firefox",
                   chromever = "114.0.5735.90",
                   verbose = FALSE,
                   port = free_port())

remDr <- driver$client
remDr$open()
remDr$navigate("https://www.arcgis.com/home/item.html?id=cfcb7609de5f478eb7666240902d4d3d")
data_table <- remDr$findElement(using = "xpath", '/html/body/div[3]/div/div[2]/div/div[2]/div/main/div[2]/div[2]/div[1]/div/div/div/div[8]/table/tbody')

class_tbl <- data_table$getPageSource() %>% 
  unlist() %>%
  read_html() %>% 
  html_table() %>% 
  .[[1]] %>%
  row_to_names(row_number = 1) %>% 
  mutate(Value = as.factor(Value))
write_csv("class_tbl.csv")

country_sf$id <- 1:nrow(country_sf)

classes <- class_tbl$Value

zonal_stats_ukr <- exactextractr::exact_extract(
  land_cover_vrt,
  country_sf
)

plan(multisession)

ukraine_multiclass <- future_map_dfr(zonal_stats_ukr, function(x) {
  as.data.frame(
    prop.table(
      table(factor(x[, 1], levels = classes))
    )
  )
})

ukraine_multiclass_sf <- ukraine_multiclass %>% 
  as_tibble() %>%
  left_join(
    class_tbl,
    by = c("Var1" = "Value")
  ) %>% 
  mutate(colors = c(
    "#419bdf", "#397d49", "#7a87c6", 
    "#e49635", "#c4281b", "#a59b8f", 
    "#a8ebff", "#616161", "#e3e2c3"
  ), .after = Name,
  perc = scales::percent(Freq / sum(Freq), accuracy = .01, trim = FALSE)) %>% 
  arrange(desc(Freq)) %>% 
  filter(!(Name %in% c("Bare ground", 'Snow/Ice', 'Clouds')))

# 12. Put all together

plot <- ukraine_multiclass_sf %>% 
  ggplot(aes(x = Freq, y = fct_reorder(Name, Freq))) +
  geom_col(aes(fill = fct_reorder(Name, Freq))) +
  geom_text(
    aes(label = perc), size = 8, hjust = -0.1, vjust = 0.5, family = "Fira Sans", fontface = "bold") +
  scale_fill_manual(values = rev(ukraine_multiclass_sf$colors)) +
  theme_void() +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(.01, .01)) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 28, hjust = 1, family = "Georgia"),
    plot.margin = margin(15, 85, 15, 15))

legend_name <- "land_cover_legend.png"
ggsave(legend_name, plot, width = 10, height = 6)

lc_img <- magick::image_read(
  filename
)

my_legend <- magick::image_read(
  legend_name
)

my_legend_scaled <- magick::image_scale(
  magick::image_background(
    my_legend, "none"
  ), 2500
)

p <- magick::image_composite(
  magick::image_scale(
    lc_img, "x7000" 
  ),
  my_legend_scaled,
  gravity = "southwest",
  offset = "+1000+1000"
)

p <- p %>% 
  image_annotate(
    "Land cover in 2022",
    size = 150,
    color = alpha("#e49635", .5),
    font = "Georgia",
    gravity = "north",
    location = "+0+300"
  ) %>% 
  image_annotate(
    "Ukraine",
    size = 300,
    color = "#e49635",
    font = "Georgia",
    gravity = "north",
    location = "+0+500"
  ) %>% 
  image_annotate(
    "©2024 Ihor Miroshnychenko (https://aranaur.rbind.io)",
    size = 100,
    color = alpha("grey20", .75),
    font = "Georgia",
    gravity = "southeast",
    location = "+100+200"
  ) %>% 
  image_annotate(
    "Data: Esri | Sentinel-2 Land Cover Explorer",
    size = 100,
    color = alpha("grey20", .75),
    font = "Georgia",
    gravity = "southeast",
    location = "+100+100"
  )

magick::image_write(
  p, "3d_ua_land_cover_final_ua.png"
)
