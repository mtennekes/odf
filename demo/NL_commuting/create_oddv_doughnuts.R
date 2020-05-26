create_oddv_doughnuts <- function(x, highlighted, other = "other", edges_direction = "in", doughnut_type = "out", pal, size_min = 20000, size_max = 250000, doughnut_scale = 1.5, flow_min = 500, flow_max = 20000, flow_scale = 10, view_args = NULL, clip = NULL) {
  stopifnot(length(pal) == (length(highlighted) + 1L))
  names(pal) <- c(highlighted, other)


  ###########################################################################################
  #### misc functions
  ###########################################################################################

  add_city_class <- function(col, levels, other) {
    x <- match(col, levels)
    x[is.na(x)] <- length(levels) + 1L

    factor(x, levels = 1:(length(levels) + 1L), labels = c(levels, other))
  }
  create_grobs <- function(U, pal, scale = 1) {
    if (inherits(U, "sf")) U <- sf::st_drop_geometry(U)
    Ulong <- U %>%
      select(!!(c("name", names(pal)))) %>%
      pivot_longer(-name, names_to = "class", values_to = "value") %>%
      replace_na(list(value = 0))

    grobs <- lapply(U$name, function(nm) {
      df <- Ulong %>%
        filter(name == nm)
      singleCat <- sum(df$value != 0) <= 1L
      ggplotGrob(ggplot(df, aes(x=2, y=value, fill = class)) +
                   geom_bar(stat="identity", width=1, size = ifelse(singleCat, 0, 2 * scale), color = "white", show.legend = FALSE) +
                   geom_vline(xintercept = 2.5, color = "white", size = 5 * scale) +
                   geom_rect(xmin = 0, xmax = .75, ymin = 0, ymax = sum(df$value), size = 0, color = "white", fill = "grey90") +
                   geom_vline(xintercept = 1.5, color = "white", size = 5 * scale ) +
                   scale_fill_manual(values = pal) +
                   coord_polar("y", start=0) +
                   xlim(.75, 2.5) +
                   theme_void())
    })
    names(grobs) <- U$name
    grobs
  }

  doughnut_group_by <- ifelse(doughnut_type == "out", "name_from", "name_to")
  edge_group_by <- ifelse(edges_direction == "out", "name_from", "name_to")
  doughnut_size <- ifelse(doughnut_type == "out", "value_out", "value_in")

  doughnut_class <- ifelse(doughnut_type == "out", "class_to", "class_from")
  edge_class <- ifelse(edges_direction == "out", "class_from", "class_to")

  edge_range <- if (edges_direction == "in") c(.5, 1) else c(0, .5)
  edge_trunc <- if (edges_direction == "in") units::set_units(c(500, 0), "m") else units::set_units(c(0, 500), "m")


  # transform to mercator (needed later to draw straight edges in interactive mode) and put name colunm first
  x$U <- x$U %>%
    sf::st_transform(3857) %>%
    select(name, everything())

  # create labels ("a to b") and add class_to variable (needed later to color edges)
  x$E <- x$E %>%
    filter(muni_from != muni_to) %>%
    group_by(muni_from, muni_to) %>%
    summarize(value = sum(value), show = show[1]) %>%
    ungroup() %>%
    mutate(value = as.integer(value),
           name_from = x$U$name[match(muni_from, x$U$id)],
           name_to = x$U$name[match(muni_to, x$U$id)],
           label = paste(name_from, "to", name_to),
           class_from = add_city_class(name_from, highlighted, other),
           class_to = add_city_class(name_to, highlighted, other)) %>%
    select(label, everything())

  # calculate inflow and outflow
  x <- od_sum_out(x, "value")
  x <- od_sum_in(x, "value")

  # filter out Waddeneilanden (except Texel) and set lowerbound to outflow (needed to determine size of doughnuts)
  x$U <- x$U %>%
    filter(!(name %in% c("Vlieland", "Terschelling",  "Ameland", "Schiermonnikoog"))) %>%
    mutate(size = pmin(size_max, pmax(size_min, !!sym(doughnut_size))))

  x$E <- x$E %>%
    filter(muni_from %in% x$U$id, muni_to %in% x$U$id)


  uExtra <- x$E %>%
    group_by(!!sym(doughnut_group_by)) %>%
    mutate(value = round(value / sum(value) * 100)) %>%
    ungroup() %>%
    mutate(value = replace(value, !!sym(doughnut_class) =="other", NA)) %>%
    group_by(!!sym(doughnut_group_by), !!sym(doughnut_class)) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    complete(!!sym(doughnut_group_by), !!sym(doughnut_class), fill = list(value = 0)) %>%
    mutate(value = replace(value, !!sym(doughnut_group_by) == as.character(!!sym(doughnut_class)), NA)) %>%  #needed for the popups
    pivot_wider(names_from = !!sym(doughnut_class), values_from = value) %>%
    mutate(other = 100-rowSums(.[,-1], na.rm = TRUE))


  x$U <- x$U %>%
    left_join(uExtra, by = c("name" = doughnut_group_by))

  ###########################################################################################
  #### create lines and doughnuts
  ###########################################################################################

  grobs <- create_grobs(x$U, pal, scale = .25)

  # filter edges by flow
  x$E <- x$E %>%
    filter(value >= flow_min)

  # create straight lines from midpoint to endpoints
  x <- od_add_lines(x, angle = 0, range = edge_range, trunc = edge_trunc, min_trunc_dist = units::set_units(1000, "m"))

  ###########################################################################################
  #### process data for popups
  ###########################################################################################

  vars <- c(highlighted, other)
  names(vars) <- paste0(ifelse(doughnut_type == "out", "to ", "from "), c(highlighted, other))

  repl <- function(x) ifelse(is.na(x), "n.a.", paste0(x, "%"))

  lns <- x$E %>%
    filter(show) %>%
    mutate(width = pmin(value, flow_max)) %>%
    select(label, value, width, !!sym(edge_class)) %>%
    arrange(desc(value))
  pnts <- x$U %>%
    filter(show) %>%
    rename(!!vars) %>%
    rename(outflow = value_out,
           inflow = value_in) %>%
    mutate_at(names(vars), repl) %>%
    select( !!c("name", "outflow", "inflow", "size", names(vars)))

  # clip
  if (!is.null(clip)) {
    clip <- sf::st_transform(clip, 3857)
    lns <- sf::st_intersection(lns, clip)
    pnts <- sf::st_intersection(pnts, clip)
  }


  set_precision <- function(x, precision = 4) {
    sf::st_geometry(x) <- sf::st_as_sfc(lapply(sf::st_geometry(x), function(y) {
      y[] <- round(y[], precision)
      y
    }))
    x
  }


  lns <- lns %>%
    sf::st_transform(4326) %>%
    set_precision(4)


  pnts <- pnts %>%
    sf::st_transform(4326) %>%
    set_precision(4)



  ###########################################################################################
  #### tmap
  ###########################################################################################

  tm <- tm_basemap(c("Esri.WorldGrayCanvas", "OpenStreetMap")) +
    tm_shape(lns) +
    tm_lines(lwd = "width", scale = flow_scale, col = edge_class, id = "label", popup.vars = "value", palette = pal, title.col = "Municipality", group = "Flows") +
    tm_shape(pnts) +
    tm_symbols(size = "size", scale = doughnut_scale, id = "name", popup.vars = c("inflow", "outflow", names(vars)),
               shape = "name", shapes = grobs, legend.shape.show = FALSE, grob.dim = c(width = 48, height = 48, render.width = 96, render.height = 96),
               group = "Doughnut charts")

  if (!is.null(view_args)) tm <- tm + do.call(tm_view, view_args)

  tm
}


filter_Limburg <- function(x, NL_muni_poly) {
  LI_muni <- (NL_muni_poly %>%
    st_drop_geometry() %>%
    filter(NUTS3_name %in% c("Midden-Limburg", "Noord-Limburg")) %>%
    select(id))$id
  NL_muni <- (NL_muni_poly %>%
                st_drop_geometry() %>%
                filter(NUTS2_name != "Limburg (NL)") %>%
                select(id))$id

  x$U <- x$U %>%
    mutate(id = replace(id, name=="Venlo", "L"),
           geometry = replace(geometry, name == "Venlo", sf::st_point(c(225000, 450000))),
           name = replace(name, name=="Venlo", "Limburg (other)")) %>%
    mutate(id = replace(id, name=="Eindhoven", "NL"),
           geometry = replace(geometry, name == "Eindhoven", sf::st_point(c(130000, 450000))),
           name = replace(name, name=="Eindhoven", "Outside Limburg")) %>%
    mutate(show = !(id %in% c("L", "NL"))) %>%
    filter(!(id %in% LI_muni), !(id %in% NL_muni))

  x$E <- x$E %>%
    mutate(muni_from = case_when(muni_from %in% LI_muni ~ "L",
                                 muni_from %in% NL_muni ~ "NL",
                                 TRUE ~ muni_from),
           muni_to = case_when(muni_to %in% LI_muni ~ "L",
                                 muni_to %in% NL_muni ~ "NL",
                                 TRUE ~ muni_to)) %>%
    group_by(muni_from, muni_to, mode) %>%
    summarize(value = sum(value), show = show[1]) %>%
    ungroup() %>%
    mutate(show = !(muni_to %in% c("L", "NL")))
  x
}

filter_Noord <- function(x, NL_muni_poly) {
  GF_muni <- (NL_muni_poly %>%
                st_drop_geometry() %>%
                filter(NUTS2_name %in% c("Groningen", "Friesland (NL)", "Drenthe")) %>%
                select(id))$id
  OG_muni <- (NL_muni_poly %>%
                st_drop_geometry() %>%
                filter(NUTS2_name %in% c("Overijssel", "Gelderland")) %>%
                select(id))$id
  NL_muni <- (NL_muni_poly %>%
                st_drop_geometry() %>%
                filter(!(NUTS2_name %in% c("Groningen", "Friesland (NL)", "Drenthe", "Overijssel", "Gelderland"))) %>%
                select(id))$id


  x$U <- x$U %>%
    mutate(id = replace(id, name=="Zwolle", "OG"),
           geometry = replace(geometry, name == "Zwolle", sf::st_point(c(250000, 400000))),
           name = replace(name, name=="Zwolle", "Overijssel and Gelderland")) %>%
    mutate(id = replace(id, name=="Utrecht", "NL"),
           geometry = replace(geometry, name == "Utrecht", sf::st_point(c(100000, 400000))),
           name = replace(name, name=="Utrecht", "West and south Netherlands")) %>%
    mutate(id = replace(id, name=="Eindhoven", "NL2"),
           geometry = replace(geometry, name == "Eindhoven", sf::st_point(c(-50000, 400000))),
           name = replace(name, name=="Eindhoven", "West and south Netherlands")) %>%
    mutate(show = !(id %in% c("OG", "NL", "NL2"))) %>%
    filter(!(id %in% OG_muni), !(id %in% NL_muni))

  x$E <- x$E %>%
    mutate(muni_from = case_when(muni_from %in% OG_muni ~ "OG",
                                 muni_from %in% NL_muni & muni_to != "GM0114" ~ "NL",
                                 muni_from %in% NL_muni & muni_to == "GM0114" ~ "NL2",
                                 TRUE ~ muni_from),
           muni_to = case_when(muni_to %in% OG_muni ~ "OG",
                               muni_to %in% NL_muni ~ "NL",
                               TRUE ~ muni_to)) %>%
    group_by(muni_from, muni_to, mode) %>%
    summarize(value = sum(value), show = show[1]) %>%
    ungroup() %>%
    mutate(show = !(muni_to %in% c("OG", "NL", "NL2")))
  x
}
