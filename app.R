# =============================================================================
#  Fuel·ES  |  Spain fuel prices by station (R / Shiny, single-file app)
#
#  Data     : MITECO "Geoportal de Gasolineras" REST API (the same source the
#             fuel-stations-spain-api project wraps), no key required
#             https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/help
#  Map      : Leaflet (open source) + OpenStreetMap standard tiles
#             (tile.openstreetmap.org; optional dark rendering via CSS filter)
#  Geocoding: Nominatim (OpenStreetMap), forward + reverse
#  Routing  : OSRM (open source, OSM data). Default = public demo server, fine
#             for testing; point OSRM_URL at your own instance for real use.
#
#  Modes    : Nearby  -> nearest / cheapest N stations around a point
#             Route   -> optimal fuel stops along a drive (tank, consumption,
#                        reserve, detour cost), Tesla-trip-planner style
#
#  Run      : shiny::runApp("app.R")
#             Rscript -e 'shiny::runApp("app.R", launch.browser = TRUE)'
#  Note     : "Use my location" needs a secure context (localhost or https).
# =============================================================================

## ---- Packages (auto-install if missing) -------------------------------------
pkgs <- c("shiny", "bslib", "httr2", "leaflet", "htmltools")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(httr2); library(leaflet); library(htmltools)
})

## ---- Config -----------------------------------------------------------------
API_URL    <- "https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/"
NOMINATIM  <- "https://nominatim.openstreetmap.org"
OSM_TILES  <- "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
OSM_ATTR   <- "&copy; <a href='https://www.openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors"
OSRM_URL   <- Sys.getenv("OSRM_URL", "https://router.project-osrm.org")
APP_UA     <- "FuelES-Shiny/1.0 (personal R/Shiny app; httr2)"
BROWSER_UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0 Safari/537.36"
CACHE_TTL  <- 30 * 60   # seconds; the Ministry refreshes roughly every 30 min
CACHE_FILE <- file.path(tools::R_user_dir("fuel_es_shiny", "cache"), "stations.rds")
DEFAULT_FUEL <- "Gasolina 95 E5"
FUEL_PREF  <- c("Gasolina 95 E5", "Gasoleo A", "Gasolina 98 E5", "Gasoleo Premium",
                "Gasolina 95 E10", "Gasolina 95 E5 Premium", "Gasolina 98 E10", "Gasoleo B")

## ---- Helpers ----------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

num_es <- function(x) suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))

title_case <- function(x) gsub("(*UCP)\\b(\\p{L})", "\\U\\1", tolower(x), perl = TRUE)

# lower-case + strip Spanish accents, for accent-insensitive search
fold <- function(x) {
  x <- tolower(enc2utf8(as.character(x)))
  tryCatch(
    chartr("áàäâéèëêíìïîóòöôúùüûñç",
           "aaaaeeeeiiiioooouuuunc", x),
    error = function(e) tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = ""))  # non-UTF-8 locales
  )
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}

fmt_price  <- function(p) formatC(p, format = "f", digits = 3)
fmt_km     <- function(d) ifelse(d < 1, paste0(round(d * 1000), " m"), paste0(formatC(d, format = "f", digits = 1), " km"))
fmt_int    <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_coords <- function(lat, lon) sprintf("%.5f, %.5f", lat, lon)
short_label <- function(x, n = 3) paste(head(trimws(strsplit(x, ",")[[1]]), n), collapse = ", ")

# "40.4168, -3.7038" / "40,4168; -3,7038" / "40.4168 -3.7038"  ->  list(lat, lon)
parse_coords <- function(x) {
  m <- regmatches(x, regexec(
    "^\\s*(-?\\d{1,3}(?:[.,]\\d+)?)\\s*(?:[,;]\\s*|\\s+)(-?\\d{1,3}(?:[.,]\\d+)?)\\s*$",
    x, perl = TRUE))[[1]]
  if (length(m) != 3) return(NULL)
  lat <- num_es(m[2]); lon <- num_es(m[3])
  if (anyNA(c(lat, lon)) || abs(lat) > 90 || abs(lon) > 180) return(NULL)
  list(lat = lat, lon = lon)
}

price_colors <- function(p) {
  rng <- range(p, na.rm = TRUE)
  t <- if (diff(rng) > 0) (p - rng[1]) / diff(rng) else rep(0, length(p))
  rgb(colorRamp(c("#4cb782", "#f2c94c", "#eb5757"))(t), maxColorValue = 255)
}

price_tier <- function(p) {
  rng <- range(p, na.rm = TRUE)
  t <- if (diff(rng) > 0) (p - rng[1]) / diff(rng) else rep(0, length(p))
  ifelse(t < 1 / 3, "lo", ifelse(t < 2 / 3, "mid", "hi"))
}

osm_link <- function(lat, lon, origin = NULL) {
  if (is.null(origin)) {
    sprintf("https://www.openstreetmap.org/?mlat=%.6f&mlon=%.6f#map=17/%.6f/%.6f", lat, lon, lat, lon)
  } else {
    sprintf("https://www.openstreetmap.org/directions?engine=fossgis_osrm_car&route=%.6f%%2C%.6f%%3B%.6f%%2C%.6f",
            origin$lat, origin$lon, lat, lon)
  }
}

## ---- Data: Ministry API with in-memory + on-disk cache ----------------------
normalize_stations <- function(body) {
  raw <- body$ListaEESSPrecio
  if (is.null(raw) || !NROW(raw))
    stop("API returned no stations (", body$ResultadoConsulta %||% "unknown status", ")", call. = FALSE)

  pick <- function(pattern) {
    nm <- grep(pattern, names(raw), value = TRUE, perl = TRUE)
    if (length(nm)) as.character(raw[[nm[1]]]) else rep(NA_character_, nrow(raw))
  }

  brand_raw <- trimws(pick("^R.{1,2}tulo$"))
  brand_raw[is.na(brand_raw) | !nzchar(brand_raw)] <- "Sin rótulo"

  st <- data.frame(
    id           = pick("^IDEESS$"),
    brand        = ifelse(nchar(brand_raw) <= 4, toupper(brand_raw), title_case(brand_raw)),
    address      = title_case(trimws(pick("^Direcci"))),
    town         = title_case(trimws(pick("^Localidad$"))),
    municipality = title_case(trimws(pick("^Municipio$"))),
    province     = title_case(trimws(pick("^Provincia$"))),
    cp           = pick("^C\\.P\\.$"),
    hours        = trimws(pick("^Horario$")),
    sale         = trimws(pick("^Tipo Venta$")),
    lat          = num_es(pick("^Latitud$")),
    lon          = num_es(pick("^Longitud")),
    stringsAsFactors = FALSE
  )

  # every "Precio ..." column becomes a fuel (new products appear automatically)
  pcols  <- grep("^Precio ", names(raw), value = TRUE)
  prices <- vapply(pcols, function(cn) num_es(raw[[cn]]), numeric(nrow(raw)))
  if (is.null(dim(prices))) prices <- matrix(prices, nrow = nrow(raw))
  colnames(prices) <- sub("^Precio ", "", pcols)
  prices[!is.na(prices) & prices <= 0] <- NA
  prices <- prices[, colSums(!is.na(prices)) > 0, drop = FALSE]

  ok <- !is.na(st$lat) & !is.na(st$lon)
  st <- st[ok, , drop = FALSE]
  prices <- prices[ok, , drop = FALSE]
  rownames(st) <- NULL

  st$hay <- fold(paste(st$brand, st$address, st$town, st$municipality, st$province, st$cp))

  pref <- intersect(FUEL_PREF, colnames(prices))
  list(
    st      = st,
    prices  = prices,
    fuels   = c(pref, sort(setdiff(colnames(prices), pref))),
    updated = body$Fecha %||% "unknown",
    fetched = Sys.time(),
    stale   = FALSE
  )
}

fetch_stations <- function() {
  resp <- request(API_URL) |>
    req_user_agent(BROWSER_UA) |>
    req_headers(Accept = "application/json",
                `Accept-Language` = "es-ES,es;q=0.9",
                Referer = "https://sedeaplicaciones.minetur.gob.es/") |>
    req_timeout(120) |>
    req_retry(max_tries = 3, backoff = function(i) 2 * i,
              is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
    req_perform()
  normalize_stations(resp_body_json(resp, simplifyVector = TRUE, check_type = FALSE))
}

.cache <- new.env(parent = emptyenv())

get_stations <- function(force = FALSE) {
  d <- .cache$data
  if (!force && !is.null(d) && !isTRUE(d$stale) &&
      difftime(Sys.time(), d$fetched, units = "secs") < CACHE_TTL) return(d)

  fresh <- tryCatch(fetch_stations(), error = function(e) e)
  if (!inherits(fresh, "error")) {
    .cache$data <- fresh
    try({
      dir.create(dirname(CACHE_FILE), recursive = TRUE, showWarnings = FALSE)
      saveRDS(fresh, CACHE_FILE)
    }, silent = TRUE)
    return(fresh)
  }

  # API down or blocking: fall back to the last good copy (memory, then disk)
  fallback <- d
  if (is.null(fallback) && file.exists(CACHE_FILE))
    fallback <- tryCatch(readRDS(CACHE_FILE), error = function(e) NULL)
  if (is.null(fallback))
    stop("Could not reach the Ministry API: ", conditionMessage(fresh), call. = FALSE)
  fallback$stale <- TRUE
  fallback$error <- conditionMessage(fresh)
  .cache$data <- fallback
  fallback
}

## ---- Geocoding (Nominatim / OpenStreetMap) ----------------------------------
nominatim <- function(path, query) {
  request(NOMINATIM) |>
    req_url_path_append(path) |>
    req_url_query(!!!query, format = "jsonv2") |>
    req_user_agent(APP_UA) |>
    req_headers(`Accept-Language` = "es,en") |>
    req_timeout(15) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}

geocode_address <- function(q) {
  js <- nominatim("search", list(q = q, limit = 1, countrycodes = "es"))
  if (!length(js) || !NROW(js)) return(NULL)
  list(lat = as.numeric(js$lat[1]), lon = as.numeric(js$lon[1]), label = js$display_name[1])
}

reverse_label <- function(lat, lon) {
  lbl <- tryCatch(nominatim("reverse", list(lat = lat, lon = lon, zoom = 17))$display_name,
                  error = function(e) NULL)
  if (is.null(lbl) || !nzchar(lbl)) fmt_coords(lat, lon) else short_label(lbl)
}

## ---- Routing + fuel-stop optimiser -------------------------------------------
# Resolve free text (address or "lat, lon") to a point
resolve_place <- function(q) {
  xy <- parse_coords(q)
  if (!is.null(xy)) return(list(lat = xy$lat, lon = xy$lon, label = fmt_coords(xy$lat, xy$lon)))
  g <- geocode_address(q)
  if (is.null(g)) stop(sprintf("No match for “%s” in Spain.", q), call. = FALSE)
  g$label <- short_label(g$label)
  g
}

# Driving route from OSRM (GeoJSON geometry), resampled every `step_km`
osrm_route <- function(from, to, step_km = 0.5) {
  url <- sprintf("%s/route/v1/driving/%.6f,%.6f;%.6f,%.6f", OSRM_URL, from$lon, from$lat, to$lon, to$lat)
  js <- request(url) |>
    req_url_query(overview = "full", geometries = "geojson", steps = "false", alternatives = "false") |>
    req_user_agent(APP_UA) |>
    req_timeout(45) |>
    req_error(is_error = function(r) FALSE) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  if (!identical(js$code, "Ok")) stop("Routing failed: ", js$message %||% js$code %||% "unknown error", call. = FALSE)

  xy  <- js$routes$geometry$coordinates[[1]]          # matrix: lon, lat
  lon <- xy[, 1]; lat <- xy[, 2]
  n   <- length(lat)
  cum <- c(0, cumsum(haversine_km(lat[-n], lon[-n], lat[-1], lon[-1])))
  keep <- !duplicated(cum)
  s <- unique(c(seq(0, max(cum), by = step_km), max(cum)))
  list(
    s   = s,                                            # km along route
    lat = approx(cum[keep], lat[keep], xout = s)$y,
    lon = approx(cum[keep], lon[keep], xout = s)$y,
    km  = js$routes$distance[1] / 1000,                 # OSRM road distance
    min = js$routes$duration[1] / 60                    # OSRM drive time
  )
}

# Stations within `corridor_km` of the route: km position along the route and
# straight-line offset from it. Two passes (coarse 5 km, then fine) keep it fast.
corridor_candidates <- function(d, rt, fuel, corridor_km = 2, public_only = TRUE, only24 = FALSE,
                                per_bin = 3, bin_km = 10) {
  p <- d$prices[, fuel]
  keep <- !is.na(p)
  if (isTRUE(public_only)) keep <- keep & (d$st$sale %in% c("P", "", NA))
  if (isTRUE(only24))      keep <- keep & grepl("24H", d$st$hours, ignore.case = TRUE)

  lat0 <- mean(rt$lat); kx <- 111.32 * cos(lat0 * pi / 180); ky <- 110.574
  pad  <- corridor_km + 6
  keep <- keep &
    d$st$lat >= min(rt$lat) - pad / ky & d$st$lat <= max(rt$lat) + pad / ky &
    d$st$lon >= min(rt$lon) - pad / kx & d$st$lon <= max(rt$lon) + pad / kx
  idx <- which(keep)
  empty <- data.frame(idx = integer(), km = numeric(), off = numeric(), price = numeric())
  if (!length(idx)) return(empty)

  nearest <- function(sx, sy, rx, ry) {
    out_i <- integer(length(sx)); out_d <- numeric(length(sx))
    for (ch in split(seq_along(sx), ceiling(seq_along(sx) / 400))) {
      d2 <- outer(sx[ch], rx, "-")^2 + outer(sy[ch], ry, "-")^2
      j  <- max.col(-d2, ties.method = "first")
      out_i[ch] <- j
      out_d[ch] <- sqrt(d2[cbind(seq_along(ch), j)])
    }
    list(i = out_i, d = out_d)
  }

  sx <- d$st$lon[idx] * kx; sy <- d$st$lat[idx] * ky
  rx <- rt$lon * kx;        ry <- rt$lat * ky

  coarse <- seq(1, length(rx), by = max(1, round(5 / (rt$s[2] - rt$s[1]))))
  nc <- nearest(sx, sy, rx[coarse], ry[coarse])
  near <- nc$d <= corridor_km + 3.5
  idx <- idx[near]; sx <- sx[near]; sy <- sy[near]
  if (!length(idx)) return(empty)

  nf <- nearest(sx, sy, rx, ry)
  ok <- nf$d <= corridor_km
  if (!any(ok)) return(empty)
  cand <- data.frame(idx = idx[ok], km = rt$s[nf$i[ok]], off = nf$d[ok], price = p[idx[ok]])

  # prune: cheapest `per_bin` stations in every `bin_km` stretch (keeps the optimiser fast)
  cand <- cand[order(cand$km), ]
  bin  <- floor(cand$km / bin_km)
  rk   <- ave(cand$price + cand$off * 1e-3, bin, FUN = function(x) rank(x, ties.method = "first"))
  cand <- cand[rk <= per_bin, ]
  rownames(cand) <- NULL
  cand
}

# Exact fuel-stop optimisation by dynamic programming over (stop, litres in tank).
# Minimises fuel spend + value of time lost (detours + fixed time per stop).
#   cand: data.frame(idx, km, off, price) sorted by km     D: route km
#   tank_l, start_l: litres          cons: L per km
#   reserve_l: never arrive anywhere below this    arrive_l: min litres at destination
optimize_fuel_plan <- function(cand, D, tank_l, start_l, cons, reserve_l, arrive_l,
                               stop_min = 5, eur_per_h = 15, detour_factor = 1.3, detour_kmh = 40) {
  Tl <- floor(tank_l); L <- Tl + 1; lv <- 0:Tl
  n <- nrow(cand); N <- n + 2
  km    <- c(0, cand$km, D)
  off   <- c(0, cand$off * detour_factor, 0)                  # road km off the route, one way
  price <- c(NA, cand$price, NA)
  det_min <- c(0, 2 * off[-c(1, N)] / detour_kmh * 60, 0)     # detour minutes, there and back
  stopc   <- c(0, (stop_min + det_min[-c(1, N)]) / 60 * eur_per_h, 0)

  cost <- matrix(Inf, N, L)            # min cost to ARRIVE at node j with g litres
  pn   <- matrix(NA_integer_, N, L)    # parent node
  ph   <- matrix(NA_integer_, N, L)    # litres when leaving parent
  dg   <- matrix(NA_integer_, N, L)    # litres on arrival at node i given departure level h
  h0 <- min(Tl, max(0, floor(start_l)))

  for (i in 1:(N - 1)) {
    depv <- rep(Inf, L)
    if (i == 1) {
      depv[h0 + 1] <- 0; dg[1, h0 + 1] <- h0
    } else {
      a <- cost[i, ]
      if (!any(is.finite(a))) next
      v <- a - lv * price[i]
      best <- Inf; bg <- NA_integer_
      for (h in 1:Tl) {                 # buy > 0 litres: leave with h > g
        if (v[h] < best) { best <- v[h]; bg <- h - 1L }
        if (is.finite(best)) { depv[h + 1] <- best + h * price[i] + stopc[i]; dg[i, h + 1] <- bg }
      }
    }
    fin <- which(is.finite(depv))
    if (!length(fin)) next
    for (j in (i + 1):N) {
      if (cons * (km[j] - km[i]) > Tl) break
      need <- cons * (km[j] - km[i] + off[i] + off[j])
      minA <- if (j == N) arrive_l else reserve_l
      arr  <- floor(lv[fin] - need + 1e-9)
      ok   <- arr >= minA & arr >= 0
      if (!any(ok)) next
      hh <- fin[ok]; aa <- arr[ok] + 1
      better <- depv[hh] < cost[j, aa]
      if (any(better)) {
        cost[j, aa[better]] <- depv[hh[better]]
        pn[j, aa[better]]   <- i
        ph[j, aa[better]]   <- hh[better] - 1L
      }
    }
  }

  if (!any(is.finite(cost[N, ]))) return(NULL)

  # backtrack
  j <- N; a <- which.min(cost[N, ]) - 1L
  arrive_dest <- a
  stops <- list()
  while (j != 1) {
    i <- pn[j, a + 1]; h <- ph[j, a + 1]; g <- dg[i, h + 1]
    if (i != 1) stops[[length(stops) + 1]] <- data.frame(
      node = i, km = km[i], arrive_l = g, buy_l = h - g, leave_l = h,
      price = price[i], cost_eur = (h - g) * price[i], detour_min = det_min[i])
    j <- i; a <- g
  }
  stops <- if (length(stops)) do.call(rbind, rev(stops)) else
    data.frame(node = integer(), km = numeric(), arrive_l = numeric(), buy_l = numeric(),
               leave_l = numeric(), price = numeric(), cost_eur = numeric(), detour_min = numeric())
  stops$cand_row <- stops$node - 1L
  list(stops = stops, arrive_l = arrive_dest, start_l = h0, D = D, tank_l = Tl,
       fuel_eur = sum(stops$cost_eur), litres = sum(stops$buy_l),
       extra_min = sum(stops$detour_min) + nrow(stops) * stop_min,
       objective = min(cost[N, ]))
}

# Inline SVG of tank level along the route (the "battery curve")
fuel_curve_svg <- function(plan, reserve_l) {
  W <- 600; H <- 120; padx <- 12; top <- 10; bot <- 22
  X <- function(k) padx + k / max(plan$D, 1e-6) * (W - 2 * padx)
  Y <- function(l) top + (1 - l / plan$tank_l) * (H - top - bot)
  s <- plan$stops
  px <- X(0); py <- Y(plan$start_l)
  for (k in seq_len(nrow(s))) {
    px <- c(px, X(s$km[k]), X(s$km[k])); py <- c(py, Y(s$arrive_l[k]), Y(s$leave_l[k]))
  }
  px <- c(px, X(plan$D)); py <- c(py, Y(plan$arrive_l))
  pts <- paste(sprintf("%.1f,%.1f", px, py), collapse = " ")
  area <- paste0(sprintf("%.1f,%.1f ", X(0), Y(0)), pts, sprintf(" %.1f,%.1f", X(plan$D), Y(0)))
  dots <- if (nrow(s)) paste(sprintf(
    "<circle cx='%.1f' cy='%.1f' r='9' class='fc-dot'/><text x='%.1f' y='%.1f' class='fc-num'>%d</text>",
    X(s$km), Y(s$leave_l), X(s$km), Y(s$leave_l) + 3.5, seq_len(nrow(s))), collapse = "") else ""
  ticks <- pretty(c(0, plan$D), n = 5); ticks <- ticks[ticks <= plan$D]
  tick_txt <- paste(sprintf("<text x='%.1f' y='%d' class='fc-tick'>%s km</text>", X(ticks), H - 6, ticks), collapse = "")
  HTML(paste0(
    "<svg class='fuel-curve' viewBox='0 0 ", W, " ", H, "' role='img' aria-label='Fuel level along the route'>",
    "<line x1='", padx, "' x2='", W - padx, "' y1='", Y(reserve_l), "' y2='", Y(reserve_l), "' class='fc-res'/>",
    "<polygon points='", area, "' class='fc-area'/>",
    "<polyline points='", pts, "' class='fc-line'/>",
    dots, tick_txt, "</svg>"))
}

## ---- Popup HTML -------------------------------------------------------------
popup_html <- function(d, i, fuel, origin = NULL) {
  s  <- d$st[i, ]
  pr <- d$prices[i, ]
  pr <- pr[!is.na(pr)]
  others <- pr[names(pr) != fuel]
  rows <- if (length(others)) {
    paste0("<tr><td>", htmlEscape(names(others)), "</td><td>", fmt_price(others), "</td></tr>", collapse = "")
  } else ""
  dist <- if (!is.null(origin)) paste0(" · ", fmt_km(haversine_km(origin$lat, origin$lon, s$lat, s$lon))) else ""
  paste0(
    "<div class='pp'>",
    "<div class='pp-t'>", htmlEscape(s$brand), "</div>",
    "<div class='pp-s'>", htmlEscape(s$address), ", ", htmlEscape(s$town),
    " (", htmlEscape(s$province), ")", dist, "</div>",
    "<div class='pp-price'>", fmt_price(pr[fuel]), " €/L<span>", htmlEscape(fuel), "</span></div>",
    if (nzchar(rows)) paste0("<table class='pp-tbl'>", rows, "</table>") else "",
    "<div class='pp-s'>", htmlEscape(s$hours), if (identical(s$sale, "R")) " · restricted sale" else "", "</div>",
    "<div class='pp-links'><a target='_blank' rel='noopener' href='", osm_link(s$lat, s$lon, origin), "'>",
    if (is.null(origin)) "Open in OpenStreetMap" else "Directions (OSM)", " ↗</a></div>",
    "</div>"
  )
}

## ---- Theme (Linear-inspired) ------------------------------------------------
app_theme <- bs_theme(
  version = 5,
  bg = "#08090a", fg = "#f7f8f8",
  primary = "#5e6ad2", secondary = "#2a2b30",
  success = "#4cb782", warning = "#f2c94c", danger = "#eb5757",
  base_font = font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
  code_font = font_collection("JetBrains Mono", "ui-monospace", "SFMono-Regular", "Menlo", "monospace"),
  "font-size-base" = "0.8125rem",
  "border-radius" = "6px"
)

app_css <- r"---(
:root{
  --bg:#08090a; --panel:#0f1011; --surface:#151619; --surface-2:#1c1d21;
  --hover:rgba(255,255,255,.035); --border:rgba(255,255,255,.08); --border-2:rgba(255,255,255,.14);
  --text:#f7f8f8; --muted:#8a8f98; --subtle:#62666d;
  --accent:#5e6ad2; --accent-2:#7c86ea; --green:#4cb782; --amber:#f2c94c; --red:#eb5757;
  --mono:"JetBrains Mono",ui-monospace,SFMono-Regular,Menlo,monospace;
}
html,body{background:var(--bg);color:var(--text)}
body{font-family:Inter,system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;font-size:13px;letter-spacing:-.006em;
     font-feature-settings:"cv01","cv11","ss03";-webkit-font-smoothing:antialiased}
a{color:var(--accent-2);text-decoration:none} a:hover{color:#fff}

/* top bar */
.bslib-page-sidebar > .navbar{background:var(--panel)!important;border-bottom:1px solid var(--border)!important;min-height:48px;padding:0 6px}
.bslib-page-sidebar > .navbar .container-fluid{flex-wrap:nowrap}
.app-title{display:flex;align-items:center;gap:8px;font-size:13px;font-weight:600;width:100%}
.app-title .crumb{color:var(--subtle);font-weight:400}
.app-title .crumb-2{color:var(--muted);font-weight:500}
.app-title .meta{margin-left:auto;color:var(--muted);font-weight:400;font-size:12px;display:flex;align-items:center;gap:6px}
.live-dot{width:7px;height:7px;border-radius:50%;background:var(--green);box-shadow:0 0 0 3px rgba(76,183,130,.15);display:inline-block;margin-right:4px}
.live-dot.stale{background:var(--amber);box-shadow:0 0 0 3px rgba(242,201,76,.15)}
.sep{color:var(--subtle);margin:0 4px}

/* layout */
.bslib-sidebar-layout{border:none!important;--_sidebar-bg:var(--panel)}
.bslib-sidebar-layout > .sidebar{background:var(--panel)!important;border-right:1px solid var(--border)!important}
.bslib-sidebar-layout > .main{background:var(--bg);padding:14px 16px!important;gap:12px}
.sidebar-content{padding:12px 14px 16px!important;gap:0!important}
.sec{padding:12px 0;border-bottom:1px solid var(--border)}
.sec:first-child{padding-top:2px}
.sec-t{font-size:11px;font-weight:500;color:var(--muted);letter-spacing:.02em;text-transform:uppercase;margin-bottom:8px}
.shiny-input-container{margin-bottom:8px;width:100%!important}
label,.control-label{font-size:12px;color:var(--muted);font-weight:500;margin-bottom:4px}

/* inputs */
.form-control,.selectize-control.single .selectize-input,.selectize-input{
  background:var(--surface)!important;border:1px solid var(--border)!important;color:var(--text)!important;
  border-radius:6px!important;min-height:32px;padding:6px 10px;font-size:13px;box-shadow:none!important}
.form-control::placeholder{color:var(--subtle)}
.form-control:focus,.selectize-input.focus{border-color:var(--accent)!important;box-shadow:0 0 0 3px rgba(94,106,210,.18)!important}
.selectize-input > input{color:var(--text)!important}
.selectize-control.single .selectize-input:after{border-color:var(--muted) transparent transparent transparent!important}
.selectize-dropdown{background:var(--surface-2)!important;border:1px solid var(--border-2)!important;color:var(--text)!important;
  border-radius:8px!important;box-shadow:0 12px 32px rgba(0,0,0,.5)!important;margin-top:4px}
.selectize-dropdown .option{padding:6px 10px}
.selectize-dropdown .active,.selectize-dropdown .selected{background:rgba(94,106,210,.18)!important;color:var(--text)!important}
.loc-row{display:flex;gap:6px;align-items:flex-start}
.loc-row .shiny-input-container{flex:1;margin:0}
.search-wrap{position:relative}
.search-wrap kbd{position:absolute;right:8px;top:7px;pointer-events:none}
.search-wrap .form-control{padding-right:28px}

/* buttons */
.btn{font-size:13px;font-weight:500;border-radius:6px;padding:5px 10px;transition:background .12s,border-color .12s}
.btn-primary{background:var(--accent);border-color:var(--accent);color:#fff}
.btn-primary:hover,.btn-primary:focus{background:var(--accent-2);border-color:var(--accent-2)}
.btn-icon{width:32px;height:32px;padding:0;display:inline-flex;align-items:center;justify-content:center;flex:none}
.btn-ghost{background:transparent;border:1px solid var(--border);color:var(--text)}
.btn-ghost:hover,.btn-ghost:focus{background:var(--hover);border-color:var(--border-2);color:var(--text)}
#geolocate{margin-top:6px} #geolocate.loading{opacity:.6}
.btn-sm{font-size:12px;padding:4px 8px}

/* origin pill */
.origin-pill{margin-top:8px;display:flex;align-items:center;gap:8px;padding:6px 8px 6px 10px;background:rgba(94,106,210,.1);
  border:1px solid rgba(94,106,210,.3);border-radius:6px;font-size:12px}
.origin-pill .lbl{flex:1;min-width:0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.origin-pill .x{color:var(--muted)} .origin-pill .x:hover{color:var(--text)}
.dot-accent{width:8px;height:8px;border-radius:50%;background:var(--accent);flex:none;box-shadow:0 0 0 3px rgba(94,106,210,.25)}
.hint{margin-top:8px;font-size:12px;color:var(--subtle)}

/* checkboxes */
.form-check-input,.checkbox input{accent-color:var(--accent)}
.form-check-input{background-color:var(--surface);border-color:var(--border-2)}
.form-check-input:checked{background-color:var(--accent);border-color:var(--accent)}
.checkbox label{color:var(--text);font-size:13px}
.checkbox{margin:2px 0}

/* segmented radio (.seg wraps a radioButtons(inline = TRUE)) */
.seg .shiny-options-group{display:flex;background:var(--surface);border:1px solid var(--border);border-radius:6px;padding:2px;gap:2px}
.seg .radio-inline,.seg .form-check{flex:1;margin:0!important;padding:0!important}
.seg input{position:absolute;opacity:0;pointer-events:none}
.seg label span,.seg .radio-inline span{display:block;text-align:center;padding:4px 0;border-radius:4px;color:var(--muted);cursor:pointer;font-size:12.5px}
.seg input:checked + span{background:var(--surface-2);color:var(--text);box-shadow:0 0 0 1px var(--border-2)}
.seg-mode{margin-bottom:4px}
.seg-mode .shiny-options-group{padding:3px}
.seg-mode label span,.seg-mode .radio-inline span{padding:6px 0;font-weight:500;font-size:13px}

/* route mode */
.two-col{display:grid;grid-template-columns:1fr 1fr;gap:8px}
.two-col .shiny-input-container{margin-bottom:6px}
.inline-links{display:flex;justify-content:space-between;font-size:12px;margin:-2px 0 8px}
.inline-links a{color:var(--muted)} .inline-links a:hover{color:var(--text)}
#plan_route{margin-top:4px}
.fuel-curve-wrap{padding:12px 14px 6px;border-bottom:1px solid var(--border)}
.fuel-curve-wrap .k{font-size:11px;color:var(--muted);font-weight:500;text-transform:uppercase;letter-spacing:.02em;margin-bottom:6px;display:flex;justify-content:space-between}
.fuel-curve{width:100%;height:auto;display:block;overflow:visible}
.fc-area{fill:rgba(94,106,210,.12)} .fc-line{fill:none;stroke:var(--accent-2);stroke-width:2;vector-effect:non-scaling-stroke;stroke-linejoin:round}
.fc-res{stroke:var(--red);stroke-dasharray:4 4;stroke-width:1;opacity:.6;vector-effect:non-scaling-stroke}
.fc-dot{fill:var(--accent);stroke:var(--panel);stroke-width:2}
.fc-num{fill:#fff;font-size:10px;font-weight:600;text-anchor:middle;font-family:Inter,system-ui,sans-serif}
.fc-tick{fill:var(--subtle);font-size:10px;text-anchor:middle;font-family:var(--mono)}
.st-step{display:inline-flex;align-items:center;justify-content:center;width:20px;height:20px;border-radius:50%;
  background:var(--accent);color:#fff;font-size:11px;font-weight:600;font-family:Inter,system-ui,sans-serif}
.st-step.end{background:var(--surface-2);border:1px solid var(--border-2);color:var(--muted)}
.st-row.static{cursor:default} .st-row.static:hover{background:transparent}
.lvl{font-family:var(--mono);font-size:11.5px;color:var(--muted)}
.lvl b{color:var(--text);font-weight:600}
.buy{font-family:var(--mono);font-size:12px;color:var(--text)}
.leaflet-tooltip.stop-label{font-family:var(--mono);font-size:11px;font-weight:600;padding:1px 6px;border-color:var(--accent)!important}

/* sliders */
.irs--shiny .irs-line{background:var(--surface-2)!important;border:none!important}
.irs--shiny .irs-bar{background:var(--accent)!important;border:none!important}
.irs--shiny .irs-handle{background:#fff!important;border:none!important;box-shadow:0 0 0 3px rgba(94,106,210,.35)!important}
.irs--shiny .irs-single{background:var(--surface-2)!important;color:var(--text)!important;font-family:var(--mono);font-size:11px;border-radius:4px}
.irs--shiny .irs-single:before{display:none}
.irs--shiny .irs-min,.irs--shiny .irs-max{background:transparent!important;color:var(--subtle)!important;font-size:10px}

/* footer */
.side-foot{display:flex;gap:6px;padding-top:12px}
.side-foot .btn{flex:1}
.source{font-size:11px;color:var(--subtle);margin-top:10px;line-height:1.5}
.source a{color:var(--muted)} .source a:hover{color:var(--text)}
kbd{background:var(--surface-2);border:1px solid var(--border-2);color:var(--muted);font-family:var(--mono);font-size:10.5px;padding:0 5px;border-radius:4px;box-shadow:none}

/* stats */
.stats{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:12px}
.stat{background:var(--panel);border:1px solid var(--border);border-radius:10px;padding:12px 14px;min-width:0}
.stat .k{font-size:11.5px;color:var(--muted);font-weight:500}
.stat .v{font-size:20px;font-weight:600;letter-spacing:-.02em;margin-top:4px;font-variant-numeric:tabular-nums;font-family:var(--mono)}
.stat .v small{font-family:Inter,system-ui,sans-serif;font-size:12px;color:var(--muted);font-weight:500;margin-left:4px}
.stat .v.good{color:var(--green)}
.stat .s{font-size:12px;color:var(--subtle);margin-top:2px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
@media (max-width:991px){.stats{grid-template-columns:repeat(2,minmax(0,1fr))}}

/* cards */
.card{background:var(--panel)!important;border:1px solid var(--border)!important;border-radius:10px!important;box-shadow:none!important;overflow:hidden}
.card-header{background:transparent!important;border-bottom:1px solid var(--border)!important;padding:10px 14px!important;font-size:13px;font-weight:600}
.card-header > .shiny-html-output{display:flex;align-items:center;gap:8px;width:100%}
.card-header .chip:last-child{margin-left:auto}
.chip{display:inline-flex;align-items:center;gap:4px;font-size:11px;font-weight:500;color:var(--muted);border:1px solid var(--border-2);
  border-radius:999px;padding:1px 7px;line-height:16px;white-space:nowrap}
.chip.warn{color:var(--amber);border-color:rgba(242,201,76,.35)}
.list-body{padding:0!important}
.map-card{min-height:420px}
.map-card .card-body{padding:0!important}

/* station rows */
.st-row{display:grid;grid-template-columns:22px minmax(0,1fr) auto;gap:12px;align-items:center;padding:10px 14px;
  border-bottom:1px solid var(--border);cursor:pointer;position:relative;transition:background .1s}
.st-row:hover{background:var(--hover)}
.st-row.active{background:rgba(94,106,210,.1)}
.st-row.active:before{content:"";position:absolute;left:0;top:0;bottom:0;width:2px;background:var(--accent)}
.st-rank{font-family:var(--mono);font-size:11px;color:var(--subtle)}
.st-title{display:flex;align-items:center;gap:6px;font-weight:600;font-size:13px;min-width:0}
.st-title .name{white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.st-sub{color:var(--muted);font-size:12px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;margin-top:2px}
.st-right{display:flex;align-items:center;gap:10px}
.st-num{text-align:right}
.st-price{font-family:var(--mono);font-size:14px;font-weight:600;font-variant-numeric:tabular-nums;display:flex;align-items:center;gap:6px;justify-content:flex-end}
.st-price .u{font-family:Inter,system-ui,sans-serif;font-size:10.5px;color:var(--subtle);font-weight:500}
.st-dist{font-size:11.5px;color:var(--muted);margin-top:2px;font-variant-numeric:tabular-nums}
.pdot{width:6px;height:6px;border-radius:50%;display:inline-block}
.lo .pdot{background:var(--green)} .mid .pdot{background:var(--amber)} .hi .pdot{background:var(--red)}
.lo .st-price{color:var(--green)}
.st-go{color:var(--subtle);width:26px;height:26px;display:inline-flex;align-items:center;justify-content:center;border-radius:6px;border:1px solid transparent}
.st-go:hover{color:var(--text);border-color:var(--border-2);background:var(--surface)}
.empty{padding:48px 20px;text-align:center;color:var(--muted)}
.empty .t{color:var(--text);font-weight:600;margin-bottom:4px}

/* leaflet, dark */
.leaflet-tile.osm-dark,.osm-dark{filter:invert(1) hue-rotate(180deg) brightness(.92) contrast(.88) saturate(.6)}
.leaflet-container{background:var(--bg)!important;font-family:inherit}
.leaflet-bar{border:1px solid var(--border-2)!important;box-shadow:none!important;border-radius:6px!important;overflow:hidden}
.leaflet-bar a{background:var(--surface)!important;color:var(--text)!important;border-color:var(--border)!important}
.leaflet-bar a:hover{background:var(--surface-2)!important}
.leaflet-control-layers{background:var(--surface)!important;color:var(--text)!important;border:1px solid var(--border-2)!important;border-radius:8px!important;box-shadow:0 8px 24px rgba(0,0,0,.4)!important}
.leaflet-control-layers-toggle{filter:invert(1) brightness(1.4)}
.leaflet-control-attribution{background:rgba(8,9,10,.75)!important;color:var(--subtle)!important}
.leaflet-control-attribution a{color:var(--muted)!important}
.leaflet-popup-content-wrapper,.leaflet-popup-tip{background:var(--surface-2)!important;color:var(--text)!important;box-shadow:0 12px 32px rgba(0,0,0,.55)!important}
.leaflet-popup-content-wrapper{border:1px solid var(--border-2);border-radius:10px!important}
.leaflet-popup-content{margin:12px 14px!important;font-size:12.5px;line-height:1.45;min-width:220px}
.leaflet-container a.leaflet-popup-close-button{color:var(--muted)}
.leaflet-tooltip{background:var(--surface-2)!important;color:var(--text)!important;border:1px solid var(--border-2)!important;box-shadow:0 4px 12px rgba(0,0,0,.4)!important;border-radius:5px!important}
.leaflet-tooltip.price-label{font-family:var(--mono);font-size:11px;font-weight:600;padding:1px 5px}
.leaflet-tooltip-top:before{border-top-color:var(--border-2)!important}
.pp-t{font-weight:600;font-size:13.5px}
.pp-s{color:var(--muted);margin-top:2px}
.pp-price{font-family:var(--mono);font-size:18px;font-weight:600;margin:8px 0 6px;color:var(--green)}
.pp-price span{font-family:Inter,system-ui,sans-serif;font-size:11px;color:var(--muted);font-weight:500;margin-left:8px}
.pp-tbl{width:100%;border-collapse:collapse;margin:4px 0 8px}
.pp-tbl td{padding:2px 0;border-top:1px solid var(--border);color:var(--muted)}
.pp-tbl td:last-child{text-align:right;font-family:var(--mono);color:var(--text)}
.pp-links{margin-top:8px}
.leaflet-container .pp a{color:var(--accent-2)}

/* notifications, progress, scrollbars */
#shiny-notification-panel .shiny-notification{background:var(--surface-2);color:var(--text);border:1px solid var(--border-2);border-radius:8px;
  box-shadow:0 12px 32px rgba(0,0,0,.5);opacity:1;font-size:12.5px}
.shiny-notification-error{border-left:3px solid var(--red)!important}
.shiny-notification-warning{border-left:3px solid var(--amber)!important}
.shiny-notification .progress{background:var(--surface)} .progress-bar{background:var(--accent)!important}
::-webkit-scrollbar{width:10px;height:10px}
::-webkit-scrollbar-thumb{background:rgba(255,255,255,.08);border-radius:10px;border:2px solid var(--panel)}
::-webkit-scrollbar-track{background:transparent}
)---"

app_js <- r"---(
$(function(){
  // Enter in the location box = Find
  $(document).on('keydown', '#loc', function(e){
    if (e.key === 'Enter') { e.preventDefault(); $('#locate').trigger('click'); }
  });

  // Browser geolocation
  $(document).on('keydown', '#r_from, #r_to', function(e){
    if (e.key === 'Enter') { e.preventDefault(); $('#plan_route').trigger('click'); }
  });

  $(document).on('click', '#geolocate, .js-geolocate', function(e){
    e.preventDefault();
    if (!navigator.geolocation) {
      Shiny.setInputValue('geo_err', 'geolocation not supported by this browser', {priority: 'event'});
      return;
    }
    var b = $(this); b.prop('disabled', true).addClass('loading');
    navigator.geolocation.getCurrentPosition(function(p){
      b.prop('disabled', false).removeClass('loading');
      Shiny.setInputValue('geo', {lat: p.coords.latitude, lon: p.coords.longitude, acc: p.coords.accuracy}, {priority: 'event'});
    }, function(err){
      b.prop('disabled', false).removeClass('loading');
      Shiny.setInputValue('geo_err', err.message || 'permission denied', {priority: 'event'});
    }, {enableHighAccuracy: true, timeout: 12000, maximumAge: 60000});
  });

  // Row click -> focus station on the map (ignore clicks on links)
  $(document).on('click', '.st-row', function(e){
    if ($(e.target).closest('a').length) return;
    Shiny.setInputValue('pick', $(this).attr('data-id'), {priority: 'event'});
  });

  // "/" focuses search, Esc clears it
  $(document).on('keydown', function(e){
    var typing = $(e.target).is('input, textarea, select, [contenteditable]');
    if (e.key === '/' && !typing) { e.preventDefault(); $('#q').trigger('focus'); }
    if (e.key === 'Escape' && $(e.target).is('#q')) { $('#q').val('').trigger('change').trigger('blur'); }
  });

  // Highlight + scroll a row (from map marker clicks or list picks)
  Shiny.addCustomMessageHandler('focusRow', function(id){
    $('.st-row').removeClass('active');
    var el = $('.st-row').filter(function(){ return $(this).attr('data-id') === String(id); });
    el.addClass('active');
    if (el[0]) el[0].scrollIntoView({block: 'nearest', behavior: 'smooth'});
  });
});
)---"

logo_svg <- HTML(paste0(
  "<svg width='18' height='18' viewBox='0 0 24 24' aria-hidden='true'>",
  "<defs><linearGradient id='lg' x1='0' y1='0' x2='1' y2='1'>",
  "<stop offset='0' stop-color='#8b94f0'/><stop offset='1' stop-color='#5e6ad2'/></linearGradient></defs>",
  "<rect width='24' height='24' rx='6' fill='url(#lg)'/>",
  "<path d='M7.5 17.5V8a1.5 1.5 0 0 1 1.5-1.5h3.5A1.5 1.5 0 0 1 14 8v9.5M6.5 17.5h8.5M14 11h1.25a1 1 0 0 1 1 1v3a1 1 0 0 0 2 0V9.5l-2-2' ",
  "fill='none' stroke='#fff' stroke-width='1.6' stroke-linecap='round' stroke-linejoin='round'/>",
  "<path d='M9.5 9h3' stroke='#fff' stroke-width='1.6' stroke-linecap='round'/></svg>"
))

section <- function(title, ...) div(class = "sec", div(class = "sec-t", title), ...)

## ---- UI ---------------------------------------------------------------------
ui <- page_sidebar(
  window_title = "Fuel·ES — Spain fuel prices",
  theme = app_theme,
  fillable = TRUE,
  title = div(
    class = "app-title", logo_svg,
    span("Fuel·ES"), span(class = "crumb", "/"), span(class = "crumb-2", "Spain fuel prices"),
    div(class = "meta", uiOutput("meta", inline = TRUE))
  ),
  sidebar = sidebar(
    width = 320, open = "always",
    div(class = "seg seg-mode",
        radioButtons("mode", NULL, c("Nearby" = "nearby", "Route" = "route"), inline = TRUE)),

    # ---- Route mode ----
    conditionalPanel(
      "input.mode == 'route'",
      section(
        "Trip",
        textInput("r_from", NULL, placeholder = "From: address or lat, lon (blank = pin)"),
        textInput("r_to", NULL, placeholder = "To: address or lat, lon"),
        div(class = "inline-links",
            actionLink("geolocate_r", "Use my location as start", class = "js-geolocate"),
            actionLink("clear_route", "Clear")),
        actionButton("plan_route", "Plan fuel stops", icon = icon("route"), class = "btn-primary w-100")
      ),
      section(
        "Vehicle",
        div(class = "two-col",
            numericInput("tank", "Tank (L)", value = 50, min = 10, max = 200, step = 1),
            numericInput("cons", "Use (L/100 km)", value = 6.5, min = 2, max = 50, step = 0.1)),
        sliderInput("level", "Fuel now (%)", min = 0, max = 100, value = 35, step = 1, ticks = FALSE, post = "%"),
        div(class = "two-col",
            numericInput("reserve", "Reserve (%)", value = 10, min = 0, max = 50, step = 1),
            numericInput("arrive", "Arrive with (%)", value = 20, min = 0, max = 100, step = 1))
      ),
      section(
        "Plan options",
        sliderInput("corridor", "Max distance off route (km)", min = 0.5, max = 10, value = 2, step = 0.5, ticks = FALSE),
        div(class = "two-col",
            numericInput("eur_h", "Your time (€/h)", value = 15, min = 0, max = 500, step = 1),
            numericInput("stop_min", "Per stop (min)", value = 5, min = 0, max = 60, step = 1))
      )
    ),

    # ---- Nearby mode ----
    conditionalPanel(
      "input.mode == 'nearby'",
    section(
      "Location",
      div(class = "loc-row",
          textInput("loc", NULL, placeholder = "Address, place or lat, lon"),
          actionButton("locate", NULL, icon = icon("magnifying-glass"),
                       class = "btn-primary btn-icon", title = "Find (Enter)")),
      actionButton("geolocate", "Use my current location", icon = icon("location-arrow"),
                   class = "btn-ghost w-100"),
      uiOutput("origin_ui")
    )),

    # ---- Shared ----
    section("Fuel", selectInput("fuel", NULL, choices = FUEL_PREF, selected = DEFAULT_FUEL)),
    conditionalPanel(
      "input.mode == 'nearby'",
      section(
        "Results",
        sliderInput("n", "Stations (N)", min = 1, max = 50, value = 10, step = 1, ticks = FALSE),
        sliderInput("radius", "Max radius, km (0 = no limit)", min = 0, max = 100, value = 25, step = 5, ticks = FALSE),
        div(class = "seg", radioButtons("sort", "Sort by", c("Distance" = "distance", "Price" = "price"), inline = TRUE))
      )
    ),
    section(
      "Filter",
      conditionalPanel("input.mode == 'nearby'", div(class = "search-wrap",
          textInput("q", NULL, placeholder = "Brand, street, town, postcode…"),
          tags$kbd("/"))),
      checkboxInput("public_only", "Public sale only", TRUE),
      checkboxInput("only24", "Open 24 h", FALSE)
    ),
    div(class = "side-foot",
        actionButton("refresh", "Refresh", icon = icon("rotate"), class = "btn-ghost btn-sm"),
        downloadButton("download", "CSV", class = "btn-ghost btn-sm")),
    div(class = "source", HTML(paste0(
      "Prices: <a href='https://geoportalgasolineras.es' target='_blank' rel='noopener'>MITECO Geoportal</a>. ",
      "Map data © <a href='https://www.openstreetmap.org/copyright' target='_blank' rel='noopener'>OpenStreetMap</a> contributors. ",
      "Geocoding: Nominatim."
    ))),
    tags$div(
      style = "margin-top: auto; padding-top: .75rem; border-top: 1px solid var(--bs-border-color, rgba(128,128,128,.25)); font-size: .75rem; line-height: 1.5; opacity: .8;",
      "Built by ", tags$a(href = "https://jfer2pi.github.io", target = "_blank", rel = "noopener", "Fernando Rodríguez Alvarado"),
      tags$br(),
      tags$a(href = "https://jfer2pi.github.io", target = "_blank", rel = "noopener", "Portfolio"), " · ",
      tags$a(href = "https://www.linkedin.com/in/jfer2pi", target = "_blank", rel = "noopener", "LinkedIn"), " · ",
      tags$a(href = "https://github.com/jfer2pi/combustibl_es", target = "_blank", rel = "noopener", "Source code")
    )
  ),

  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@500;600&display=swap"),
    tags$style(HTML(app_css)),
    tags$script(HTML(app_js))
  ),

  uiOutput("stats"),
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(5, 7)),
    fill = TRUE, gap = "12px",
    card(
      card_header(uiOutput("list_title", inline = TRUE)),
      card_body(class = "list-body", fillable = FALSE, uiOutput("list"))
    ),
    card(
      class = "map-card", full_screen = TRUE,
      card_body(padding = 0, leafletOutput("map", height = "100%"))
    )
  )
)

## ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  store  <- reactiveVal(NULL)
  origin <- reactiveVal(NULL)

  # -- data loading ------------------------------------------------------------
  load_data <- function(force = FALSE) {
    d <- withProgress(message = "Loading station prices…", value = 0.4, {
      tryCatch(get_stations(force), error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL)
        NULL
      })
    })
    if (is.null(d)) return(invisible())
    if (isTRUE(d$stale))
      showNotification(paste("Live API unavailable, showing cached data.", d$error %||% ""),
                       type = "warning", duration = 10)
    store(d)
    sel <- isolate(input$fuel)
    updateSelectInput(session, "fuel", choices = d$fuels,
                      selected = if (!is.null(sel) && sel %in% d$fuels) sel
                                 else if (DEFAULT_FUEL %in% d$fuels) DEFAULT_FUEL else d$fuels[1])
  }
  observe({
    invalidateLater(CACHE_TTL * 1000)
    isolate(load_data(FALSE))
  })
  observeEvent(input$refresh, load_data(TRUE))

  # -- origin: address / coords / browser / map click --------------------------
  set_origin <- function(lat, lon, label = NULL, src = "manual") {
    origin(list(lat = lat, lon = lon, label = label %||% fmt_coords(lat, lon), src = src))
  }

  observeEvent(input$locate, {
    q <- trimws(input$loc %||% "")
    if (!nzchar(q)) {
      showNotification("Type an address or 'lat, lon'.", type = "message")
      return()
    }
    xy <- parse_coords(q)
    if (!is.null(xy)) return(set_origin(xy$lat, xy$lon, src = "coords"))
    g <- tryCatch(geocode_address(q), error = function(e) e)
    if (inherits(g, "error")) {
      showNotification(paste("Geocoding failed:", conditionMessage(g)), type = "error")
    } else if (is.null(g)) {
      showNotification(sprintf("No match for “%s” in Spain.", q), type = "warning")
    } else {
      set_origin(g$lat, g$lon, short_label(g$label), "address")
    }
  })

  observeEvent(input$geo, {
    g <- input$geo
    set_origin(g$lat, g$lon, reverse_label(g$lat, g$lon), "device")
    updateTextInput(session, "loc", value = fmt_coords(g$lat, g$lon))
    updateTextInput(session, "r_from", value = fmt_coords(g$lat, g$lon))
  })
  observeEvent(input$geo_err, showNotification(paste("Location unavailable:", input$geo_err), type = "warning"))

  observeEvent(input$map_click, {
    cl <- input$map_click
    if (identical(input$mode, "route")) {
      # Route mode: a map click sets the destination and plans straight away
      to_q <- fmt_coords(cl$lat, cl$lng)
      updateTextInput(session, "r_to", value = to_q)
      do_plan(input$r_from, to_q)
      return()
    }
    set_origin(cl$lat, cl$lng, reverse_label(cl$lat, cl$lng), "map")
    updateTextInput(session, "loc", value = fmt_coords(cl$lat, cl$lng))
  })

  observeEvent(input$clear_origin, {
    origin(NULL)
    updateTextInput(session, "loc", value = "")
  })

  output$origin_ui <- renderUI({
    o <- origin()
    if (is.null(o)) return(div(class = "hint", "Tip: click anywhere on the map to drop a point."))
    div(class = "origin-pill",
        span(class = "dot-accent"),
        span(class = "lbl", title = o$label, o$label),
        actionLink("clear_origin", icon("xmark"), class = "x", title = "Clear location"))
  })

  # -- results -----------------------------------------------------------------
  q_debounced <- debounce(reactive(input$q), 300)

  results <- reactive({
    d <- store(); req(d)
    fuel <- input$fuel
    req(fuel %in% colnames(d$prices))
    p <- d$prices[, fuel]

    keep <- !is.na(p)
    if (isTRUE(input$public_only)) keep <- keep & (d$st$sale %in% c("P", "", NA))
    if (isTRUE(input$only24))      keep <- keep & grepl("24H", d$st$hours, ignore.case = TRUE)
    q <- trimws(q_debounced() %||% "")
    if (nzchar(q)) {
      for (tok in strsplit(fold(q), "\\s+")[[1]]) keep <- keep & grepl(tok, d$st$hay, fixed = TRUE)
    }
    idx <- which(keep)

    o <- origin()
    dist <- if (!is.null(o)) haversine_km(o$lat, o$lon, d$st$lat[idx], d$st$lon[idx]) else rep(NA_real_, length(idx))
    if (!is.null(o) && isTRUE(input$radius > 0)) {
      inr <- dist <= input$radius
      idx <- idx[inr]; dist <- dist[inr]
    }
    pool_prices <- p[idx]

    ord <- if (!is.null(o) && identical(input$sort, "distance")) order(dist, pool_prices) else order(pool_prices, dist)
    ord <- head(ord, input$n)

    list(idx = idx[ord], dist = dist[ord], price = pool_prices[ord], fuel = fuel,
         pool = length(idx), pool_median = if (length(idx)) median(pool_prices) else NA_real_,
         origin = o)
  })

  # -- route mode: route, corridor, plan ----------------------------------------
  route_store <- reactiveVal(NULL)
  route_err   <- reactiveVal(NULL)   # last planning problem, shown in the list panel

  fail_route <- function(msg) {
    route_err(msg)
    showNotification(msg, type = "error", duration = 8)
  }

  # Geocode both ends, route with OSRM, store. Called by the button, Enter, and map clicks.
  do_plan <- function(from_q, to_q) {
    from_q <- trimws(from_q %||% ""); to_q <- trimws(to_q %||% "")
    if (!nzchar(to_q))
      return(fail_route("Enter a destination (or click the map in Route mode)."))
    if (!nzchar(from_q) && is.null(origin()))
      return(fail_route("Enter a start (address or lat, lon) or press “Use my location as start”."))
    route_err(NULL)
    res <- withProgress(message = "Planning route…", value = 0.2, tryCatch({
      from <- if (nzchar(from_q)) resolve_place(from_q) else origin()
      incProgress(0.2, detail = "Geocoding destination")
      to <- resolve_place(to_q)
      incProgress(0.2, detail = "Routing (OSRM)")
      rt <- osrm_route(from, to)
      list(from = from, to = to, rt = rt)
    }, error = function(e) e))
    if (inherits(res, "error")) return(fail_route(conditionMessage(res)))
    route_store(res)
  }

  observeEvent(input$plan_route, do_plan(input$r_from, input$r_to))

  observeEvent(input$clear_route, {
    route_store(NULL); route_err(NULL)
    updateTextInput(session, "r_to", value = "")
  })

  route_cands <- reactive({
    r <- route_store(); req(r)
    d <- store(); req(d, input$fuel %in% colnames(d$prices))
    corridor_candidates(d, r$rt, input$fuel, corridor_km = input$corridor,
                        public_only = input$public_only, only24 = input$only24)
  })

  route_inputs <- debounce(reactive(list(
    tank = input$tank, cons = input$cons, level = input$level, reserve = input$reserve,
    arrive = input$arrive, eur_h = input$eur_h, stop_min = input$stop_min)), 400)

  route_plan <- reactive({
    r <- route_store(); req(r)
    cand <- route_cands()
    v <- route_inputs()
    ok <- vapply(v, function(x) is.numeric(x) && length(x) == 1 && is.finite(x), logical(1))
    validate(need(all(ok), "Fill in all vehicle and plan inputs."))
    validate(need(v$tank >= 10 && v$cons > 0, "Tank must be at least 10 L and consumption above 0."))
    tank <- v$tank
    plan <- optimize_fuel_plan(
      cand, D = r$rt$km, tank_l = tank, start_l = tank * v$level / 100, cons = v$cons / 100,
      reserve_l = tank * v$reserve / 100, arrive_l = tank * v$arrive / 100,
      stop_min = v$stop_min, eur_per_h = v$eur_h)
    med <- if (nrow(cand)) median(cand$price) else NA_real_
    list(plan = plan, cand = cand, route = r, v = v, corridor_median = med,
         reserve_l = tank * v$reserve / 100)
  })

  # -- header meta -------------------------------------------------------------
  output$meta <- renderUI({
    d <- store(); req(d)
    tagList(
      span(class = paste("live-dot", if (isTRUE(d$stale)) "stale")),
      span(if (isTRUE(d$stale)) "Cached · " else "", "Prices as of ", d$updated),
      span(class = "sep", "·"),
      span(fmt_int(nrow(d$st)), " stations")
    )
  })

  # -- stat tiles --------------------------------------------------------------
  tile <- function(k, v, s, cls = NULL, unit = NULL)
    div(class = "stat", div(class = "k", k),
        div(class = paste("v", cls), v, if (!is.null(unit)) tags$small(unit)),
        div(class = "s", s))

  route_stats <- function() {
    if (is.null(route_store()))
      return(div(class = "stats",
                 tile("Distance", "—", "Set a start and destination"),
                 tile("Fuel cost", "—", "Optimised plan"),
                 tile("Saving", "—", "vs. corridor median price"),
                 tile("Stops", "—", "Extra time incl. detours")))
    rp <- route_plan(); p <- rp$plan; rt <- rp$route$rt
    hm <- sprintf("%d h %02d min", floor(rt$min / 60), round(rt$min %% 60))
    if (is.null(p))
      return(div(class = "stats",
                 tile("Distance", formatC(rt$km, format = "f", digits = 0), hm, unit = "km"),
                 tile("Fuel cost", "—", "No feasible plan"),
                 tile("Saving", "—", "—"),
                 tile("Stops", "—", "—")))
    base_l <- max(0, p$litres)
    saving <- if (is.finite(rp$corridor_median) && base_l > 0) base_l * rp$corridor_median - p$fuel_eur else NA
    div(class = "stats",
      tile("Distance", formatC(rt$km, format = "f", digits = 0), paste0(hm, " drive"), unit = "km"),
      tile("Fuel cost", sprintf("%.2f", p$fuel_eur),
           sprintf("%s L bought · %.1f L used", fmt_int(p$litres), rt$km * rp$v$cons / 100), unit = "€"),
      tile("Saving", if (is.finite(saving)) sprintf("%.2f", max(0, saving)) else "—",
           "vs. same litres at corridor median", cls = "good", unit = if (is.finite(saving)) "€"),
      tile("Stops", nrow(p$stops), sprintf("+%d min incl. detours", round(p$extra_min))))
  }

  output$stats <- renderUI({
    if (identical(input$mode, "route")) return(route_stats())
    r <- results(); d <- store()
    has <- length(r$idx) > 0
    o <- r$origin
    k_min <- if (has) which.min(r$price) else NA_integer_
    k_near <- if (has && !is.null(o)) which.min(r$dist) else NA_integer_
    div(class = "stats",
      tile("Cheapest shown",
           if (has) fmt_price(r$price[k_min]) else "—",
           if (has) paste0(d$st$brand[r$idx[k_min]],
                           if (!is.na(r$dist[k_min])) paste0(" · ", fmt_km(r$dist[k_min])) else "")
           else "No stations match",
           cls = "good", unit = if (has) "€/L"),
      tile("Median",
           if (is.finite(r$pool_median)) fmt_price(r$pool_median) else "—",
           sprintf("across %s matching stations", fmt_int(r$pool)),
           unit = if (is.finite(r$pool_median)) "€/L"),
      tile("Nearest",
           if (!is.na(k_near)) fmt_km(r$dist[k_near]) else "—",
           if (!is.na(k_near)) d$st$brand[r$idx[k_near]] else "Set a location"),
      tile("Saving on 50 L",
           if (has && is.finite(r$pool_median)) sprintf("%.2f", max(0, (r$pool_median - min(r$price)) * 50)) else "—",
           "cheapest shown vs. median",
           unit = if (has && is.finite(r$pool_median)) "€")
    )
  })

  # -- list ----------------------------------------------------------------------
  route_title <- function() {
    if (is.null(route_store())) return(tagList(span("Route plan"), span(class = "chip", input$fuel)))
    rp <- route_plan(); rt <- rp$route$rt
    tagList(
      span("Route plan"),
      span(class = "chip", sprintf("%s candidates ≤ %s km off route", fmt_int(nrow(rp$cand)), input$corridor)),
      span(class = "chip", input$fuel)
    )
  }

  route_list <- function() {
    r <- route_store()
    err <- route_err()
    if (!is.null(err))
      return(div(class = "empty",
                 div(class = "t", style = "color:var(--red)", "Couldn’t plan this route"),
                 div(err)))
    if (is.null(r))
      return(div(class = "empty",
                 div(class = "t", "Plan fuel stops along a drive"),
                 div("Enter a start and destination, set your tank and consumption, then press Plan.",
                     tags$br(), "Tip: in Route mode a map click sets the destination and plans the route.")))
    rp <- route_plan(); p <- rp$plan; d <- store()
    if (is.null(p))
      return(div(class = "empty",
                 div(class = "t", "No feasible plan"),
                 div("No combination of stations keeps you above the reserve. Try more fuel now, a lower reserve,",
                     " a wider corridor, or turn off the 24 h / public-only filters.")))
    tank <- p$tank_l
    pct  <- function(l) paste0(round(100 * l / tank), "%")
    s    <- p$stops
    rows <- list(
      div(class = "st-row static",
          div(span(class = "st-step end", "A")),
          div(div(class = "st-title", span(class = "name", r$from$label %||% "Start")),
              div(class = "st-sub", "Start · km 0")),
          div(class = "st-right", div(class = "st-num",
              div(class = "lvl", "Tank ", tags$b(pct(p$start_l))),
              div(class = "st-dist", sprintf("%d L", p$start_l)))))
    )
    for (k in seq_len(nrow(s))) {
      i  <- rp$cand$idx[s$cand_row[k]]
      st <- d$st[i, ]
      rows[[length(rows) + 1]] <- div(
        class = "st-row lo", `data-id` = st$id,
        div(span(class = "st-step", k)),
        div(
          div(class = "st-title",
              span(class = "name", st$brand),
              span(class = "chip", sprintf("km %s", round(s$km[k]))),
              if (grepl("24H", st$hours, ignore.case = TRUE)) span(class = "chip", "24h")),
          div(class = "st-sub", title = st$hours,
              sprintf("%s · %s · +%d min detour", st$address, st$town, round(s$detour_min[k]))),
          div(class = "lvl", "Arrive ", tags$b(pct(s$arrive_l[k])), " → leave ", tags$b(pct(s$leave_l[k])))
        ),
        div(class = "st-right",
            div(class = "st-num",
                div(class = "st-price", span(class = "pdot"), fmt_price(s$price[k]), span(class = "u", "€/L")),
                div(class = "buy", sprintf("%d L · %.2f €", s$buy_l[k], s$cost_eur[k]))),
            tags$a(class = "st-go", href = osm_link(st$lat, st$lon), target = "_blank", rel = "noopener",
                   title = "Open in OpenStreetMap", icon("route")))
      )
    }
    rows[[length(rows) + 1]] <- div(
      class = "st-row static",
      div(span(class = "st-step end", "B")),
      div(div(class = "st-title", span(class = "name", r$to$label %||% "Destination")),
          div(class = "st-sub", sprintf("Arrive · km %s", round(p$D)))),
      div(class = "st-right", div(class = "st-num",
          div(class = "lvl", "Tank ", tags$b(pct(p$arrive_l))),
          div(class = "st-dist", sprintf("%d L", p$arrive_l)))))

    tagList(
      div(class = "fuel-curve-wrap",
          div(class = "k", span("Fuel level along the route"),
              span(sprintf("reserve %s", pct(rp$reserve_l)))),
          fuel_curve_svg(p, rp$reserve_l)),
      tagList(rows)
    )
  }

  output$list_title <- renderUI({
    if (identical(input$mode, "route")) return(route_title())
    r <- results(); o <- r$origin; n <- length(r$idx)
    lead <- if (is.null(o)) sprintf("Cheapest %d in selection", n)
            else if (identical(input$sort, "price")) sprintf("Cheapest %d nearby", n)
            else sprintf("Nearest %d", n)
    tagList(
      span(lead),
      if (!is.null(o) && isTRUE(input$radius > 0)) span(class = "chip", paste0("≤ ", input$radius, " km")),
      span(class = "chip", r$fuel)
    )
  })

  output$list <- renderUI({
    if (identical(input$mode, "route")) return(route_list())
    r <- results(); d <- store()
    if (!length(r$idx)) {
      return(div(class = "empty",
                 div(class = "t", "No stations match"),
                 div("Try a wider radius, another fuel, or clear the search.")))
    }
    tiers <- price_tier(r$price)
    o <- r$origin
    tagList(lapply(seq_along(r$idx), function(k) {
      i <- r$idx[k]; s <- d$st[i, ]
      div(
        class = paste("st-row", tiers[k]), `data-id` = s$id,
        div(class = "st-rank", sprintf("%02d", k)),
        div(
          div(class = "st-title",
              span(class = "name", s$brand),
              if (grepl("24H", s$hours, ignore.case = TRUE)) span(class = "chip", "24h"),
              if (identical(s$sale, "R")) span(class = "chip warn", "Restricted")),
          div(class = "st-sub", title = s$hours, paste0(s$address, " · ", s$town))
        ),
        div(class = "st-right",
            div(class = "st-num",
                div(class = "st-price", span(class = "pdot"), fmt_price(r$price[k]), span(class = "u", "€/L")),
                div(class = "st-dist", if (!is.na(r$dist[k])) fmt_km(r$dist[k]) else s$province)),
            tags$a(class = "st-go", href = osm_link(s$lat, s$lon, o), target = "_blank", rel = "noopener",
                   title = if (is.null(o)) "Open in OpenStreetMap" else "Directions (OpenStreetMap)",
                   icon("route")))
      )
    }))
  })

  # -- map ---------------------------------------------------------------------
  output$map <- renderLeaflet({
    # Both layers are the standard OpenStreetMap tiles; "dark" is the same
    # tiles with a CSS filter applied in the browser (see .osm-dark in app_css)
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles(urlTemplate = OSM_TILES, attribution = OSM_ATTR, group = "OpenStreetMap",
               options = tileOptions(maxZoom = 19)) |>
      addTiles(urlTemplate = OSM_TILES, attribution = OSM_ATTR, group = "OpenStreetMap (dark)",
               options = tileOptions(maxZoom = 19, className = "osm-dark")) |>
      addLayersControl(baseGroups = c("OpenStreetMap", "OpenStreetMap (dark)"),
                       position = "topright",
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(lng = -3.70, lat = 40.30, zoom = 6)
  })

  # Nearby layers
  observe({
    if (identical(input$mode, "route")) {
      leafletProxy("map") |> clearGroup("stations") |> clearGroup("origin") |> clearGroup("sel")
      return()
    }
    r <- results(); d <- store()
    o <- r$origin
    pr <- leafletProxy("map") |> clearGroup("stations") |> clearGroup("origin") |> clearGroup("sel")

    if (!is.null(o)) {
      if (isTRUE(input$radius > 0)) {
        pr <- pr |> addCircles(lng = o$lon, lat = o$lat, radius = input$radius * 1000, group = "origin",
                               color = "#5e6ad2", weight = 1, opacity = 0.6, fillOpacity = 0.04,
                               dashArray = "4 4", options = pathOptions(interactive = FALSE))
      }
      pr <- pr |> addCircleMarkers(lng = o$lon, lat = o$lat, radius = 7, group = "origin",
                                   color = "#ffffff", weight = 2, fillColor = "#5e6ad2", fillOpacity = 1,
                                   label = o$label)
    }

    if (length(r$idx)) {
      s <- d$st[r$idx, ]
      few <- length(r$idx) <= 15
      pops <- vapply(r$idx, function(i) popup_html(d, i, r$fuel, o), character(1))
      lbl <- if (few) fmt_price(r$price) else sprintf("%s · %s €", s$brand, fmt_price(r$price))
      pr <- pr |>
        addCircleMarkers(lng = s$lon, lat = s$lat, layerId = s$id, group = "stations",
                         radius = 6, color = "#08090a", weight = 1.5,
                         fillColor = price_colors(r$price), fillOpacity = 0.95,
                         popup = pops, label = lbl,
                         labelOptions = labelOptions(noHide = few, direction = "top", offset = c(0, -6),
                                                     className = if (few) "price-label" else NULL))
      lng <- c(s$lon, o$lon); lat <- c(s$lat, o$lat)
      pr |> fitBounds(min(lng), min(lat), max(lng), max(lat),
                      options = list(padding = c(40, 40), maxZoom = 15))
    } else if (!is.null(o)) {
      pr |> setView(o$lon, o$lat, zoom = 13)
    }
  })

  # Route layers: line, corridor candidates, planned stops, A/B markers
  observe({
    pr <- leafletProxy("map") |> clearGroup("route") |> clearGroup("route_c") |> clearGroup("route_stops")
    if (!identical(input$mode, "route") || is.null(route_store())) return()
    r  <- route_store(); rt <- r$rt
    d  <- store(); req(d)
    pr <- pr |>
      addPolylines(lng = rt$lon, lat = rt$lat, group = "route", color = "#5e6ad2", weight = 5, opacity = 0.9,
                   options = pathOptions(interactive = FALSE)) |>
      addCircleMarkers(lng = r$from$lon, lat = r$from$lat, group = "route", radius = 8, color = "#ffffff",
                       weight = 2, fillColor = "#1c1d21", fillOpacity = 1, label = paste("A ·", r$from$label)) |>
      addCircleMarkers(lng = r$to$lon, lat = r$to$lat, group = "route", radius = 8, color = "#ffffff",
                       weight = 2, fillColor = "#5e6ad2", fillOpacity = 1, label = paste("B ·", r$to$label))

    rp <- route_plan()
    cand <- rp$cand
    if (nrow(cand)) {
      sc <- d$st[cand$idx, ]
      pr <- pr |> addCircleMarkers(
        lng = sc$lon, lat = sc$lat, layerId = paste0("c_", sc$id), group = "route_c",
        radius = 4, stroke = FALSE, fillColor = price_colors(cand$price), fillOpacity = 0.55,
        label = sprintf("%s · %s € · km %d", sc$brand, fmt_price(cand$price), round(cand$km)),
        popup = vapply(cand$idx, function(i) popup_html(d, i, input$fuel, NULL), character(1)))
    }
    p <- rp$plan
    if (!is.null(p) && nrow(p$stops)) {
      s  <- p$stops
      ii <- cand$idx[s$cand_row]
      ss <- d$st[ii, ]
      pr <- pr |> addCircleMarkers(
        lng = ss$lon, lat = ss$lat, layerId = ss$id, group = "route_stops",
        radius = 9, color = "#ffffff", weight = 2, fillColor = "#4cb782", fillOpacity = 1,
        popup = vapply(seq_along(ii), function(k) paste0(
          popup_html(d, ii[k], input$fuel, NULL),
          sprintf("<div class='pp-s'>Plan: stop %d, buy %d L (%.2f €)</div>", k, s$buy_l[k], s$cost_eur[k])),
          character(1)),
        label = sprintf("%d · %s", seq_len(nrow(s)), fmt_price(s$price)),
        labelOptions = labelOptions(noHide = TRUE, direction = "top", offset = c(0, -8), className = "stop-label"))
    }
    pr |> fitBounds(min(rt$lon), min(rt$lat), max(rt$lon), max(rt$lat), options = list(padding = c(30, 30)))
  })

  # list click -> fly + popup; marker click -> highlight row
  observeEvent(input$pick, {
    d <- store(); req(d)
    i <- match(as.character(input$pick), d$st$id); req(!is.na(i))
    s <- d$st[i, ]
    leafletProxy("map") |>
      clearGroup("sel") |>
      addPopups(s$lon, s$lat, popup_html(d, i, input$fuel, origin()), group = "sel",
                options = popupOptions(closeButton = TRUE, autoPan = TRUE)) |>
      flyTo(s$lon, s$lat, zoom = 15, options = list(duration = 0.6))
    session$sendCustomMessage("focusRow", s$id)
  })

  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    if (!is.null(id)) session$sendCustomMessage("focusRow", id)
  })

  # -- CSV export ----------------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() sprintf("%s_%s_%s.csv",
                                  if (identical(input$mode, "route")) "plan_ruta" else "gasolineras",
                                  gsub("\\W+", "_", input$fuel), format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      d <- store()
      if (identical(input$mode, "route")) {
        rp <- route_plan(); p <- rp$plan
        if (is.null(p) || !nrow(p$stops)) {
          write.csv(data.frame(note = "No fuel stops needed or no feasible plan"), file, row.names = FALSE)
          return()
        }
        ii <- rp$cand$idx[p$stops$cand_row]
        out <- cbind(stop = seq_along(ii),
                     d$st[ii, c("id", "brand", "address", "town", "province", "hours", "lat", "lon"), drop = FALSE],
                     p$stops[, c("km", "arrive_l", "buy_l", "leave_l", "price", "cost_eur", "detour_min")],
                     fuel = input$fuel)
        write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
        return()
      }
      r <- results()
      s <- d$st[r$idx, c("id", "brand", "address", "town", "municipality", "province",
                         "cp", "hours", "sale", "lat", "lon"), drop = FALSE]
      out <- cbind(rank = seq_along(r$idx), s, fuel = rep(r$fuel, length(r$idx)),
                   price_eur_l = r$price, distance_km = round(r$dist, 3))
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
