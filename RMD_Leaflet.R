library(leaflet)

my_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(lat=37.434424, lng=127.128252, popup="Jihan's clinic")
my_map
