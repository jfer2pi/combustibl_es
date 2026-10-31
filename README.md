# combustibl_es

**Fuel·ES** is a single-file R/Shiny app for finding fuel prices at every service station in Spain. It has two modes:

- **Nearby**: the nearest or cheapest *N* stations around an address, a `lat, lon` pair, your current location, or a point you click on the map.
- **Route**: plans fuel stops along a drive, in the spirit of in-car EV trip planners. Give it a start, a destination and your tank, and it works out where to stop, how many litres to buy at each stop, and what the trip will cost.

All prices come from the Spanish government's open data. Every map, routing and geocoding service the app uses is open source and built on OpenStreetMap.

## Why

Spain publishes the price at every station in an open, key-free feed: the Ministry for the Ecological Transition's *Geoportal de Gasolineras*. Most apps built on it answer "where is fuel cheapest near me?". This project asks a different question: **on this trip, where should I stop and how much should I buy?**

The answer depends on:

- how much fuel you have now and how big your tank is;
- your car's consumption;
- the reserve you never want to drop below;
- how many minutes a detour or a stop is worth to you.

## Features

### Nearby mode

- Set the search centre by address, `lat, lon` (Spanish decimal commas also work), browser geolocation, or a click on the map.
- Choose N (1 to 50), a maximum radius, and whether to sort by distance or by price.
- Search by brand, street, town or postcode. It ignores accents, and every word you type must match.
- Filter to public-sale stations only, or to stations open 24 h.
- Markers are coloured green to red by price, and popups list every fuel the station sells.
- Directions open in OpenStreetMap.
- Export the results to CSV.

### Route mode

- The route comes from the [OSRM](https://project-osrm.org/) routing engine.
- **Corridor search:** finds every station within *x* km of the route and records its km position along it.
- **Exact optimiser:** a dynamic programme over every combination of stops and litres in the tank. It minimises fuel spend plus the value of time lost to detours and stops, and it never lets you arrive anywhere below your reserve.
- **Plan output:**
  - a tank-level chart along the route;
  - a timeline from A to B with, for each stop: litres to buy, cost, and tank % on arrival and departure;
  - the saving compared with paying the median price along the route.
- The plan recalculates as soon as you change any vehicle or plan input.

### Interface

- Linear-inspired dark UI built with `bslib`.
- Map: Leaflet with OpenStreetMap tiles, plus a dark rendering of the same OSM tiles.
- Press `/` to jump to the search box.

## Run it

```r
shiny::runApp("app.R")
# or from a terminal:
# Rscript -e 'shiny::runApp("app.R", launch.browser = TRUE)'
```

The app installs any missing packages (`shiny`, `bslib`, `httr2`, `leaflet`, `htmltools`) on first run.

- The first load downloads all ~12,000 stations and takes a few seconds.
- Data is cached for 30 minutes and saved to disk, so the app keeps working, showing the last good copy, if the Ministry API goes down.
- "Use my location" needs a secure context: `localhost` or https.

### Configuration

| Variable   | Default                          | Purpose                                                                |
|------------|----------------------------------|------------------------------------------------------------------------|
| `OSRM_URL` | `https://router.project-osrm.org` | Routing server. The public demo is for testing only; use your own instance for real use. |

## Data sources and services

| What        | Source                                                                                                  | Notes                                                                    |
|-------------|---------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|
| Fuel prices | [MITECO, Geoportal de Gasolineras REST API](https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/help) | Open data, no key needed. Refreshed about every 30 min by the Ministry. |
| Map tiles   | [OpenStreetMap](https://www.openstreetmap.org/copyright)                                                 | © OpenStreetMap contributors. Subject to the [OSM tile usage policy](https://operations.osmfoundation.org/policies/tiles/). |
| Geocoding   | [Nominatim](https://nominatim.org/)                                                                      | Subject to the [Nominatim usage policy](https://operations.osmfoundation.org/policies/nominatim/): at most 1 request per second, no autocomplete. |
| Routing     | [OSRM](https://project-osrm.org/)                                                                        | Open source, runs on OSM data.                                           |

Prices are reported by the stations themselves and can be out of date or contain errors. Treat every plan as an estimate. This project is not affiliated with, or endorsed by, the Spanish Ministry.

## Limitations and next ideas

- Only Spanish stations are covered for now. France, Portugal and Italy also publish open per-station price data, which would make cross-border routes ("fill up before the border") possible.
- Opening hours are only checked for "24H". Fully parsing the `Horario` field would make night-time plans more realistic.
- Detours are estimated as 1.3 × the straight-line distance, driven at 40 km/h. Exact detour times could come from OSRM for the shortlisted stations.
- A real deployment needs its own tile, geocoding and routing servers instead of the public OSM and OSRM ones.

## Credits

- **Idea and product concept:** Fernando ([@jfer2pi](https://github.com/jfer2pi)). He came up with the app and with route-based fuel-stop planning modelled on in-car EV route planners.
- **Code:** written with [Claude](https://claude.ai) (Anthropic) from Fernando's specifications.
