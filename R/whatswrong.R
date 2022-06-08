## mercator
## https://twitter.com/neilrkaye/status/1050722881657864192
library(sf)
sf::sf_use_s2(FALSE)  ## that's what's wrong lol ...
proj <- "+proj=ortho +lon_0=147 +lat_0=-42"
map <- st_geometry(rnaturalearth::ne_countries(returnclass = "sf"))
## just crop slightly in longitude to avoid the wrap
## but crop a lot to avoid the infinity at the south pole
map <- st_cast(st_cast(st_crop(map, st_bbox(map) + c(-1/1e6, 1.5, -1/1e6, 0))), "POLYGON")
## (and break up multipolygons so each gets moved by its own centre)
projd <- st_transform(map, proj)
proj_cent <- st_centroid(projd)
projmat <- st_coordinates(proj_cent)
ll_cent <- st_transform(proj_cent, "OGC:CRS84")
llmat <- st_coordinates(ll_cent)

bad <- is.na(llmat[,1])

if (any(bad)) {
  map <- map[!bad]
  llmat <- llmat[!bad, ]
}

lp <- lapply(seq_along(map), function(i)
  st_transform(map[i], sprintf("+proj=laea +lon_0=%f +lat_0=%f", llmat[i, 1], llmat[i, 2]))[[1]])

new_cent <- do.call(rbind, lapply(lp, function(cc) st_coordinates(st_centroid(cc))))

poly_on_proj <- st_sfc(lapply(seq_along(lp), function(i) lp[[i]] + (  projmat[i, 1:2] - new_cent[i,1:2] )))

par(mar = rep(0, 4))
plot(projd, reset = TRUE, col = "grey")
for (i in seq_along(poly_on_proj)) try(plot(poly_on_proj[i], add = T, col = rgb(0, 0, 0.5, alpha = 0.6)))
