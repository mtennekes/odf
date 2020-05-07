get_od_endpoints <- function(E, U = NULL) {

  if (!inherits(E, "sf")) stop("Either specify U, or let E be an sf object with lines.")

  origs <- E %>%
    group_by(get(od_o(E))) %>%
    slice(1L) %>%
    ungroup() %>%
    transmute(id = get(od_o(E)))

  dests <- E %>%
    group_by(get(od_d(E))) %>%
    slice(1L) %>%
    ungroup() %>%
    transmute(id = get(od_d(E)))

  if (is.factor(origs$id) && is.factor(dests$id)) {
    if (!identical(levels(origs$id), levels(dests$id))) {
      stop("Levels orig and dest from E are not identical.")
    }
  }

  orig_df <- st_coordinates(origs) %>%
    as_tibble() %>%
    select(c(names(.)[1:2], tail(names(.), 1))) %>%
    rename(X = 1, Y = 2, L = 3) %>%
    group_by(L) %>%
    slice(1L) %>%
    ungroup() %>%
    st_as_sf(coords = c("X", "Y"), crs = st_crs(E))

  dest_df <- st_coordinates(dests) %>%
    as_tibble() %>%
    select(c(names(.)[1:2], tail(names(.), 1))) %>%
    rename(X = 1, Y = 2, L = 3) %>%
    group_by(L) %>%
    slice(n()) %>%
    ungroup() %>%
    st_as_sf(coords = c("X", "Y"), crs = st_crs(E))

  origs$geometry <- orig_df$geometry
  dests$geometry <- dest_df$geometry

  U <- rbind(origs, dests)

  if (is.numeric(U$id) && !is.integer(U$id)) {
    U$id <- as.integer(U$id)
  }
  if (is.character(U$id)) {
    U$id <- as.factor(U$id)
  }
  U <- U[!duplicated(U$id), ]
  attr(U, "od_id") <- "id"
  U
}
