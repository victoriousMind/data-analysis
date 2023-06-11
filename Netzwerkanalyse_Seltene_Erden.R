# Netzwerkanalyse Handelsdaten Seltener Erden
# erstellt von Victoria Haase am 11.06.2023
# E-Mail: stu210491@mail.uni-kiel.de

# Infos über aktuelle Sitzung:
###

sessionInfo()

# Arbeitsverzeichnis festlegen (bestimmen, wo Daten gesucht werden sollen):
###

setwd("~/Desktop/Bachelorarbeit/R-Analyse/Seltene Erden")

# Daten einlesen:
###

trade_raw_data <- read.table("2023-03-18_REE_trade_data_USA_world_edited.csv", # öffne das .csv file,
                             # um die Struktur der CSV Datei zu erkennen
                             # Parameter entsprechend unterhalb angeben 
                             header=T,           # Spaltenüberschrift wahr oder falsch?
                             sep=";",            # Trennzeichenzwischen den Werten (; oder ,?)
                             dec=",",            # Dezimaltrennzeichen (, oder .?)
                             stringsAsFactors=T) # Buchstabenkette als Faktoren erkennen?

# Kontrollieren der eingelesenen Daten:
###

str(trade_raw_data)

# Origin und Destination Code von int zu Factor ändern und kontrollieren:
###

trade_raw_data[,'origin_code'] = as.factor(trade_raw_data[,'origin_code'])
levels(trade_raw_data$origin_code)

trade_raw_data[,'destination_code'] = as.factor(trade_raw_data[,'destination_code'])
levels(trade_raw_data$destination_code)

# bestimmte Spalten umbenennen:
###

names(trade_raw_data)[names(trade_raw_data) == 'quantity_.kg.'] <- 'quantity_kg'
names(trade_raw_data)[names(trade_raw_data) == 'quantity_.US..'] <- 'quantity_US_Dollar'

# ANPASSEN (bei anderen Daten)! alle Gütermengen desselben Herkunftslandes oder Ziellandes zusammenfassen (aggregieren), damit nur ein Pfeil und nicht für jede Güterklasse einzelne Pfeile; ab hier zwei Data frames:
###

updatedTrade_raw_data_origin <- aggregate(.~origin, trade_raw_data[c(1:96),], sum) # Zeilennummer an Herkunftsland Zeilennunmmer anpassen 
updatedTrade_raw_data_destination <- aggregate(.~destination, trade_raw_data[c(97:357),], sum) # Zeilennummer an Zielland Zeilennummer anpassen 

# Alle Zeilen mit quantity_kg == 0 löschen:
###

updatedTrade_raw_data_origin <- updatedTrade_raw_data_origin[!(updatedTrade_raw_data_origin$quantity_kg==0),]
updatedTrade_raw_data_destination <- updatedTrade_raw_data_destination[!(updatedTrade_raw_data_destination$quantity_kg==0),]

# ANPASSEN! Daten von "China" (Zeile 5) und "China, Hong Kong SAR" (Zeile 6) (Zeilen 5 and 6 in originalem Datensatz) zu einem werden lassen:
###

updatedTrade_raw_data_origin[6, 1] <- "China"
updatedTrade_raw_data_origin <- aggregate(.~origin, updatedTrade_raw_data_origin, sum)

# ANPASSEN! Falls "Zielland Data frame" auch "China" and "China, Hong Kong SAR" hat, dasselbe tun (dann die beiden folgenden Zeilen zu einem Befehl umwandeln mit shift+command+c:
###

# updatedTrade_raw_data_destination[6, 1] <- "China"
# updatedTrade_raw_data_destination <- aggregate(.~origin, updatedTrade_raw_data_destination, sum)

# Mitgliedsstaaten der EU zusammenfassen:
###

# Listen der EU definieren:
###

listEU27 <- c("Austria", "Belgium", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Lithuania", "Netherlands", "Poland", "Slovakia", "Spain", "Sweden", "Slovenia", "Luxembourg", "Bulgaria", "Cyprus", "Croatia", "Portugal", "Romania", "Malta", "Latvia")

# Funktion "replaceEU()", die alle 27 Länder der EU zu "EU-27" werden lässt:
###

replaceEU <- function(df, list_values) {
  for (i in 1: nrow(df)) {
    if (df[i,1] %in% list_values) {
      levels(df[,1]) <- c(levels(df[,1]), "EU-27")
      df[i,1] <- "EU-27"
    }
    else {
      levels(df[i,1]) <- levels(df[i,1])
    }
  }
  return(df)
}

updatedTrade_raw_data_origin <- replaceEU(updatedTrade_raw_data_origin, listEU27)

updatedTrade_raw_data_destination <- replaceEU(updatedTrade_raw_data_destination, listEU27)


updatedTrade_raw_data_origin <- aggregate(.~origin, updatedTrade_raw_data_origin, sum)

updatedTrade_raw_data_destination <- aggregate(.~destination, updatedTrade_raw_data_destination, sum)

# -> Nun haben wir zwei data frames:einen für das gleiche Herkunfsland (außer USA), einen für dasselbe Zielland (außer USA)

# Nicht korrekte Spalten rausnehmen (year, commodity_code, commodity, origin_code, destination_code), mit str (structure) nochmal anzeigen lassen, wie es jetzt aussieht:
###

updatedTrade_raw_data_origin <- subset(updatedTrade_raw_data_origin, select = -c(destination, year, commodity_code, commodity, origin_code, destination_code) )
str(updatedTrade_raw_data_origin)

updatedTrade_raw_data_destination <- subset(updatedTrade_raw_data_destination, select = -c(origin, year, commodity_code, commodity, origin_code, destination_code) )
str(updatedTrade_raw_data_destination)

# ANPASSEN! year and destination/origin zu beiden data frames hinzufügen:
###

year = c(rep(2021, each=22)) # Hinweis: bei anderen Daten Anzahl der Zeilen anpassen
destination = c(rep("USA", each=22)) # Hinweis: bei anderen Daten Anzahl der Zeilen anpassen
updatedTrade_raw_data_origin <- cbind(updatedTrade_raw_data_origin, year) # mit 22 Zeilen
updatedTrade_raw_data_origin <- cbind(updatedTrade_raw_data_origin, destination) # mit 22 Zeilen
updatedTrade_raw_data_origin[,'year'] = as.factor(updatedTrade_raw_data_origin[,'year'])
updatedTrade_raw_data_origin[,'destination'] = as.factor(updatedTrade_raw_data_origin[,'destination'])
updatedTrade_raw_data_origin <- updatedTrade_raw_data_origin[,c(1,5,2:4)] # Spalten neu anordnen
str(updatedTrade_raw_data_origin)

year = c(rep(2021, each=76)) # Hinweis: bei anderen Daten Anzahl der Zeilen anpassen
origin = c(rep("USA", each=76)) # Hinweis: bei anderen Daten Anzahl der Zeilen anpassen
updatedTrade_raw_data_destination <- cbind(updatedTrade_raw_data_destination, year) # mit 76 Zeilen
updatedTrade_raw_data_destination <- cbind(updatedTrade_raw_data_destination, origin) # mit 76 Zeilen
updatedTrade_raw_data_destination[,'year'] = as.factor(updatedTrade_raw_data_destination[,'year'])
updatedTrade_raw_data_destination[,'origin'] = as.factor(updatedTrade_raw_data_destination[,'origin'])
updatedTrade_raw_data_destination <- updatedTrade_raw_data_destination[,c(5,1:4)] # Spalten neu anordnen
str(updatedTrade_raw_data_destination)

# Die zwei Data frames kombinieren:
###

final_trade_data <- rbind(updatedTrade_raw_data_origin, updatedTrade_raw_data_destination)

# Zeilen löschen von "Other Asia, nes" (nes = not elsewhere specified) weil es beim späteren geologischen Coding nicht benutzt werden kann
###

final_trade_data <- subset(final_trade_data, origin != "Other Asia, nes")
final_trade_data <- subset(final_trade_data, destination != "Other Asia, nes")

# Netzwerk / Netzwerkgrafik:
###

library(igraph) # igraph package laden

# Funktion "get_graph()" erstellen, die die "final_trade_data" als input nimmt
###

get_graph <- function(final_trade_data) { 
  
  graph <- graph_from_data_frame(final_trade_data) # Erstellung der Grundlagengrafik
  
  # plot(graph) # Überprüfung der Grunlagengrafik optional
  
  # ANPASSEN! quantity_kg wird momentan benutzt - ändern zu quantity_US_Dollar falls notwendig! quantity_kg im Moment ausgewählt, weil der Wert des US $ und der REES schwanken kann:
  # E() (aus dem Paket i graph) spuckt die Kanten des Graphens aus (https://www.rdocumentation.org/packages/igraph/versions/1.3.5/topics/E)
  # quantity_kg wird als das Gewicht der Kanten des Graphens hinzugefügt:
  E(graph)$weight <- as.numeric(E(graph)$quantity_kg)
  
  # plot(graph) # plot (zeige) den geänderten Graphen (Gewichte hinzugefügt)
  # Mein jetziger Wissensstand: Je kürzer der Pfeil, desto höher quantity_kg.
  
  # Den Ländern geologische codes zuweisen
  ###
  
  library(readr)
  
  # Einlesen der geologischen Codes der Länder:
  ###
  
  countriesgeocodes <- read_csv("geocode_gaza_from_nature_paper_Czechia_Rep. of Korea_USA_North Macedonia_Rep. of Moldova_United Rep. of Tanzania_Viet Nam_EU-27_changed.csv", na = "null")
  # Benutzen der adaptierten Version des Nature papers (manche Namen angepasst, damit sie zu den UN Daten passen)
  # ("Gaza Strip" verwendet, nicht "Palestinian Territories")
  
  # Neupositionierung des "World" Knotens Längengrades von -160 zu 179
  ###
  
  countriesgeocodes[which(countriesgeocodes$name == "World"), "longitude"] <- 179
  
  # Erstelle data frame "vertices" mit den Ländern von "graph" und der "countriesgeocodes" .csv Datei
  ###
  
  vertices      <- data.frame(id = as.character(V(graph)$name)) # V() extrahiert die vertices aus dem graph Objekt, die Namen der Vertices werden dann genommmen und in einen neuen Data frame gepackt in die Spalte "id"
  vertices$isoAlpha2 <- NaN # hinzufügen einer leeren Spalte "isoAlpha2" zum data frame "vertices" - Der "isoAlpha2" code eines Landes ist ein Zwei-Buchstaben code, der Teil des ISO 3166 Standards ist, welcher betreut wird von der " International Organization for Standardization" (ISO)
  vertices$latitude  <- NaN # hinzufügen einer leeren Spalte "latitude" zum data frame "vertices"
  vertices$longitude  <- NaN # hinzufügen einer leeren Spalte "longitude" zum data frame "vertices"
  
  for (i in 1:nrow(vertices)) { # "for"-Schleife, um alle isoAlpha2-, latitude- und longitude-Werte aus contriesgeocodes in den vertices data frame in die entsprechende Zeile zu schreiben
    idx <- which(countriesgeocodes$name == vertices$id[i]) 
    vertices$isoAlpha2[i]  <- countriesgeocodes$isoAlpha2[idx] 
    vertices$latitude[i]   <- countriesgeocodes$latitude[idx] 
    vertices$longitude[i]   <- countriesgeocodes$longitude[idx] 
  } # für alle Länder in vertices data frame
  
  # Berechnung der centrality
  ###
  
  # Stelle Funktion auf, um Daten zu normalisieren
  ###
  
  normalize_func <- function(x) {
    a <- min(x)
    b <- max(x)
    (x - a) / (b - a)
  }
  
  # Erstelle Graph als Liste
  ###
  
  gs <- list() # Die Liste wird in "gs" geschrieben.
  gs[[1]] <- graph # Der Graph ist ist zugeordnet als erstes Element in der Liste
  
  # jetzt: wirkliche Berechnung der centrality measures nach der Vorbereitung
  ###
  
  # Festlegen, dass "strength" die Stärke der Verbindung ist, d.h. in meinem Fall "weight" des "graph", und, dass die "cost" die Inverse des "weight" des "graph" ist
  
  gs <- lapply(gs, function(x) { 
    E(x)$strength <- E(x)$weight 
    E(x)$cost <- mean(E(x)$weight) / E(x)$weight 
    return(x)
  })
  
  # Lege fest, dass "weight" die Kosten der Verbindung darstellt, was offenbar der Standardweg ist, wie igraph funktioniert
  
  gs <- lapply(gs, function(x) {
    E(x)$weight <- E(x)$cost # Festlegen, dass "cost" Werte (gerade oben berechnet) die neuen "weight" Werte der edges E() sind.
    return(x)
  })
  
  # Ab hier habe ich nicht mehr jeden einzelnen Schritt nachvollzogen, die Kommentare stammen von OSPINA-ALVAREZ, ANDRES ET.AL.2022."A NETWORK ANALYSIS OF GLOBAL CEPHALOPED TRADE"
  ###
  # Dennoch Werte an meine Funktion angepasst
  
  gs <- lapply(gs, function(x) {
    V(x)$isoAlpha2 <- vertices$isoAlpha2 # create column "isoAlpha2" in gs and assign isoAlpha2 value of vertices to it
    
    
    # Geo-position for each node
    V(x)$x                          <- vertices$longitude
    V(x)$y                          <- vertices$latitude
    
    # Centrality measures
    V(x)$degree       <- degree(x, mode = "all")
    V(x)$in_degree    <- degree(x, mode = "in")
    V(x)$out_degree   <- degree(x, mode = "out")
    
    V(x)$strength     <- strength(x,
                                  weights = E(x)$strength,
                                  mode = "all",
                                  loops = T)
    V(x)$in_strength  <- strength(x,
                                  weights = E(x)$strength,
                                  mode = "in",
                                  loops = T)
    V(x)$out_strength <- strength(x,
                                  weights = E(x)$strength,
                                  mode = "out",
                                  loops = T)
    
    V(x)$evcent                    <- evcent(x, weights = E(x)$strength)$vector
    V(x)$hub_score                 <- hub.score(x, weights = E(x)$strength)$vector
    V(x)$auth_score                <- authority.score(x, weights = E(x)$strength)$vector
    V(x)$page_rank                 <- page_rank(x, weights = E(x)$strength)$vector
    
    # Here we calculate node betweenness with respect to the cost
    V(x)$betweenness <- betweenness(x)
    # Here we calculate edge betweenness with respect to the cost
    E(x)$edge_betweenness <- edge.betweenness(x)
    
    # Normalized Centrality measures
    V(x)$n.degree              <- normalize_func(V(x)$degree)
    V(x)$n.strength            <- normalize_func(V(x)$strength)
    V(x)$n.in_strength         <- normalize_func(V(x)$in_strength)
    V(x)$n.out_strength        <- normalize_func(V(x)$out_strength)
    V(x)$n.betweenness         <- normalize_func(V(x)$betweenness)
    V(x)$n.evcent              <- normalize_func(V(x)$evcent+1E-16)
    V(x)$n.page_rank           <- normalize_func(V(x)$page_rank)
    V(x)$n.auth_score          <- normalize_func(V(x)$auth_score)
    V(x)$n.hub_score           <- normalize_func(V(x)$hub_score)
    
    E(x)$n.edge_betweenness    <- normalize_func(E(x)$edge_betweenness)
    E(x)$n.edge_weight         <- normalize_func(as.numeric(E(x)$weight))
    E(x)$n.edge_strength       <- normalize_func(as.numeric(E(x)$strength ))
    E(x)$n.edge_cost           <- normalize_func(as.numeric(E(x)$cost))
    
    if (is.connected(x) == TRUE){
      V(x)$closeness             <- closeness(x)
      V(x)$n.closeness           <- normalize_func(V(x)$closeness)
    }
    return(x)
  })
  
  
  vstats <- do.call('rbind', lapply(1:length(gs), function(x) {
    o <- get.data.frame(gs[[x]], what = 'vertices')
    o$network <- get.graph.attribute(gs[[x]], "name")
    o$time <- x
    return(o)
  }))
  
  
  estats <- do.call('rbind', lapply(1:length(gs), function(x) {
    o <- get.data.frame(gs[[x]], what = 'edges')
    o$network <- get.graph.attribute(gs[[x]], "name")
    o$time <- x
    return(o)
  }))
  
  
  gstats <- do.call('rbind', lapply(gs, function(y) {
    ga <- list.graph.attributes(y)
    ga <- ga[1:length(ga)]
    sapply(ga, function(x) {
      get.graph.attribute(y, x)
    })
  }))
  
  
  library(dplyr)
  
  # ------------------------------- #
  # Making some calculations in tnet
  # ------------------------------- #
  
  # https://cran.r-project.org/web/packages/tnet/tnet.pdf
  
  library(tnet)
  mat_q <- as.matrix(as_adjacency_matrix(graph, attr = "weight"))
  mat_n <- data.frame(id = c(1:ncol(mat_q)), name = colnames(mat_q))
  graph.tnet <- as.tnet(mat_q, type = "weighted one-mode tnet")
  tnet.vstat <-
    full_join(mat_n, as.data.frame(betweenness_w(graph.tnet)), by = c("id" = "node"))
  if (is.connected(graph) == TRUE){
    tnet.vstat <- full_join(tnet.vstat, as.data.frame(closeness_w(graph.tnet, gconly = FALSE)), by = c("id" = "node"))
    V(gs[[1]])$closeness.tnet               <- tnet.vstat$closeness
    V(gs[[1]])$n.closeness.tnet             <- normalize_func(tnet.vstat$closeness)
  }
  
  # ------------------------------- #
  # Writing tnet calculations
  # ------------------------------- #
  
  V(gs[[1]])$betweenness.tnet             <- tnet.vstat$betweenness
  V(gs[[1]])$n.betweenness.tnet           <-
    normalize_func(tnet.vstat$betweenness)
  
  
  return(
    list(
      gt1 = gs[[1]],
      ggg = gs[1],
      vstats = vstats,
      estats = estats,
      gstats = gstats
    )
  )
  
} # end of function "get_graph()" which takes the "final_trade_data" as its input for "final_trade_data" input parameter

# (Erstellen des Graphen ohne server() function):
###
  
graph_obj <- get_graph(final_trade_data = final_trade_data)

# The Network Graph ####

# ------------------ #
# Map theme config.
# ------------------ #
################# GEOPOSITIONATED NETWORK PLOT (WHITE) #################
library(mapdata)
library(ggraph)

maptheme <-
  theme_minimal(base_size = 24) %+replace% #Relative size of plot
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0, 0),
    legend.justification = c(0, 0),
    legend.background = element_blank(),
    # Remove overall border
    legend.key = element_blank(),
    # Remove border around each item
    panel.background = element_rect(fill = "#ffffff"),
    plot.margin = unit(c(0, 0, 0, 0), 'cm')
  )

country_shapes <-
  geom_polygon(
    aes(x = long, y = lat, group = group),
    data = map_data('world'),
    fill = "#f1f2ef",
    color = "#515151",
    size = 0.15
  )
mapcoords <- coord_fixed(xlim = c(-180, 180), ylim = c(-78, 80))


gt1 <- graph_obj$gt1

###########################################
# ANPASSEN! Entscheiden, welche node measure nutzen!
###########################################

node_measure_to_use <- "Strength"
# node_measure_to_use <- "In-strength"
# node_measure_to_use <- "Out-strength"
# node_measure_to_use <- "Betweenness"
# node_measure_to_use <- "Closeness"
# node_measure_to_use <- "Eigenvector"
# node_measure_to_use <- "Page's Rank"
# node_measure_to_use <- "Authority score"
# node_measure_to_use <- "Hub score"

node_measure <- (
  if (node_measure_to_use == "Strength") {
    V(gt1)$n.strength }
  else if (node_measure_to_use == "In-strength") {
    V(gt1)$n.in_strength }
  else if (node_measure_to_use == "Out-strength") {
    V(gt1)$n.out_strength }
  else if (node_measure_to_use == "Betweenness") {
    V(gt1)$n.betweenness }
  else if (node_measure_to_use == "Closeness") {
    V(gt1)$n.closeness.tnet }
  else if (node_measure_to_use == "Eigenvector") {
    V(gt1)$n.evcent }
  else if (node_measure_to_use == "Page's Rank") {
    V(gt1)$n.page_rank }
  else if (node_measure_to_use == "Authority score") {
    V(gt1)$n.auth_score }
  else if (node_measure_to_use == "Hub score") {
    V(gt1)$n.hub_score }
)

###########################################
# ANPASSEN! Entscheiden, welche edge measure nutzen!
###########################################

edge_measure_to_use <- "Edge strength"
# edge_measure_to_use <- "Edge betweenness"

edge_measure <- (
  if (edge_measure_to_use == "Edge strength") {
    E(gt1)$n.edge_strength }
  else if (edge_measure_to_use == "Edge betweenness") {
    E(gt1)$n.edge_betweenness }
)

node_legend <- paste("normalised", node_measure_to_use)
edge_legend <- paste("norm.", edge_measure_to_use)

###########
# ANPASSEN!
###########
legend_1 <- "Globales Handelsnetzwerk Seltener Erden"

###########
# ANPASSEN!
###########
legend_2  <- "Normalisierte Menge in kg | 2021"

###########
# ANPASSEN!
###########
legend_3 <- "Data from https://comtrade.un.org | United Nations"


library(ggraph)
library(RColorBrewer)
library(scales)
# library(shiny)


### Which colorblind-friendly brewer palettes for nodes and arcs do you want to use?
cbpalette_arcs_low <- "#87CEFF" # Light blue
cbpalette_arcs_high <- "#27408B" # Dark blue
cbpalette_arcs <- "GnBu"
cbpalette_nodes <- "RdPu"

### Color of the nodes labels on the network graph.
df_col_lab <- data.frame(measure = node_measure, color = "NA")
df_col_lab$color <-
  ifelse(df_col_lab$measure >= 0.51, "white", "black")
col_label_1 <- df_col_lab$color


### Legend position (top, bottom, right, left)
legend_pos <- "right"

# ---------------------------------
# Make geo-positioned network graph
# ggg <- graph_obj()$ggg
ggg <- graph_obj$ggg

lapply(ggg, function(x) {
  g <- x
  
  ggraph(g,
         layout = "manual",
         x = V(g)$x,
         y = V(g)$y) +
    country_shapes +
    
    geom_edge_arc(
      aes(
        edge_color = edge_measure,
        edge_alpha = edge_measure,
        edge_width = edge_measure
      ),
      arrow = arrow(
        length = unit(1.6, 'mm'),
        type = "closed",
        angle = 15
      ),
      start_cap = circle(0.8, 'mm'),
      end_cap = circle(2.4, 'mm'),
      strength = 0.30
    ) +
    
    scale_edge_colour_gradient(low = cbpalette_arcs_low,
                               high = cbpalette_arcs_high,
                               limits = c(0, 1)) +
    
    scale_edge_alpha(range = c(0.1, 1),
                     limits = c(0, 1)) +
    
    scale_edge_width_continuous(range = c(0.8, 1.6),
                                limits = c(0, 1)) +
    
    geom_node_point(aes(size = node_measure, color = node_measure)) +
    scale_color_gradientn(colours = brewer_pal(palette = cbpalette_nodes)(5),
                          limits = c(0, 1)) +
    scale_size_continuous(range = c(3, 6),
                          limits = c(0, 1)) +
    scale_alpha_continuous(range = c(0.4, 1),
                           limits = c(0, 1)) +
    
    geom_node_text(aes(label = isoAlpha2),
                   color = col_label_1,
                   size = 2) +
    
    guides(
      size = guide_legend(paste(node_legend), title.theme = element_text(size = 12)),
      color = guide_legend(paste(node_legend), title.theme = element_text(size = 12)),
      alpha = guide_legend(paste(node_legend), title.theme = element_text(size = 12)),
      edge_alpha = guide_legend(paste(edge_legend), title.theme = element_text(size = 12)),
      edge_width = guide_legend(paste(edge_legend), title.theme = element_text(size = 12)),
      edge_color = guide_legend(paste(edge_legend), title.theme = element_text(size = 12))
    ) +
    
    mapcoords +
    maptheme +
    # theme(legend.position = "none")
    theme(
      legend.text = element_text(size = 9),
      legend.position = "bottom",
      legend.box.spacing = unit(0.001, "cm"),
      legend.box.margin = unit(c(0, 0, 0, 0), 'cm'),
      legend.box = "vertical",
      legend.box.just = "bottom",
      legend.margin = margin(0, 0, 0, 0, 'cm'),
      legend.spacing = unit(0.2, "cm")
    ) +
    ggplot2::annotate(
      "text",
      x = -190,
      y = -53,
      hjust = 0,
      size = 3.,
      label = paste(legend_1),
      color = "black"
    ) +
    
    ggplot2::annotate(
      "text",
      x = -190,
      y = -60,
      hjust = 0,
      size = 2.4,
      label = paste(legend_2),
      color = "black"
    ) +
    
    ggplot2::annotate(
      "text",
      x = -190,
      y = -67,
      hjust = 0,
      size = 2.0,
      label = paste(legend_3),
      color = "black",
      alpha = 0.5
    )
  
})

# finale Grafik im aktuellen Arbeitsverzeichnis speichern:
# https://ggplot2.tidyverse.org/reference/ggsave.html
###

ggsave(
  filename = "Karte_REE.png",
  plot = last_plot(),
  device = "png",
  scale = 1,
  width = 2532,
  height = 1800,
  units = "px",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

# Grafik-Modul bei Fehlern ausschalten:
###

# dev.off()

# Arbeitsraum leeren:
###

# rm(list = ls())

######
# Tabellen mit Werten der Netzwerkanalyse erstellen:
######

######
# Top-Exporteure
######
vstats <- graph_obj$vstats # hier stecken alle Daten drinnen, wenn man sie als .csv speichern will, kann man folgende Zeile ausführen
write.csv(vstats, "~/Downloads/vertices-stats.csv", row.names = F) # vor "/vstats.csv"" das Arbeitsverzeichnis / den Speicherort richtig setzen

# Optional, wenn es um t (Tonnen) gehen soll (dann bei head() auch "kg" zu "t" ändern)
# vstats$out_strength <- vstats$out_strength / 1E3 # 1E3 = 1000 -> durch 1000 um es auf t zu bekommen

head(vstats %>% arrange(desc(out_strength)) %>% select(name, out_strength) %>% rename (trader = name, kg = out_strength))



######
# Top-Importeure
######
vstats <- graph_obj$vstats # hier stecken alle Daten drinnen, wenn man sie als .csv speichern will, kann man folgende Zeile ausführen
# write.csv(vstats, "~/Downloads/vertices-stats.csv", row.names = F) # vor "/vstats.csv"" das Arbeitsverzeichnis / den Speicherort richtig setzen

# Optional, wenn es um t (Tonnen) gehen soll (dann bei head() auch "kg" zu "t" ändern)
# vstats$in_strength <- vstats$in_strength / 1E3

head(vstats %>% arrange(desc(in_strength)) %>% select(name, in_strength) %>% rename (trader = name, kg = in_strength))



######
# Top-Strength (oder welches anderes Maß eben gerade gewählt ist)
######
vstats <- graph_obj$vstats # hier stecken alle Daten drinnen, wenn man sie als .csv speichern will, kann man folgende Zeile ausführen
# write.csv(vstats, "~/Downloads/vertices-stats.csv", row.names = F) # vor "/vstats.csv"" das Arbeitsverzeichnis / den Speicherort richtig setzen

head(vstats %>% arrange(desc(strength)) %>% select(name) %>% rename (trader = name))

# head(vstats %>% arrange(desc(in_strength)) %>% select(name) %>% rename (trader = name))

# head(vstats %>% arrange(desc(out_strength)) %>% select(name) %>% rename (trader = name))

# head(vstats %>% arrange(desc(betweenness)) %>% select(name) %>% rename (trader = name))

# head(tnet.vstat %>% arrange(desc(closeness)) %>% select(name) %>% rename (trader = name))

# head(vstats %>% arrange(desc(evcent)) %>% select(name) %>% rename (trader = name))

# head(vstats %>% arrange(desc(auth_score)) %>% select(name) %>% rename (trader = name))

# head(vstats %>% arrange(desc(hub_score)) %>% select(name) %>% rename (trader = name))



######
# Top-Warenflüsse zwischen Handelspartnern
######
estats <- graph_obj$estats # hier stecken alle Daten drinnen, wenn man sie als .csv speichern will, kann man folgende Zeile ausführen
write.csv(estats, "~/Downloads/edges-stats.csv", row.names = F) # vor "/estats.csv"" das Arbeitsverzeichnis / den Speicherort richtig setzen

# Optional, wenn es um t (Tonnen) gehen soll (dann bei head() auch "kg" zu "t" ändern)
# estats$quantity_kg <- estats$quantity_kg / 1E3

head(
  estats %>% arrange(desc(quantity_kg)) %>% select(from, to, quantity_kg) %>% rename (
    exporter = from,
    importer = to,
    kg = quantity_kg
  )
)

######
# Anzahl der Handelsverbindungen (insgesamt, Exporte, Importe) - macht bei meinem Datensatz nicht so viel Sinn
######

# vstats <- graph_obj$vstats # hier stecken alle Daten drinnen, wenn man sie als .csv speichern will, kann man folgende Zeile ausführen
# # write.csv(vstats, "~/Downloads/vertices-stats.csv", row.names = F) # vor "/vstats.csv"" das Arbeitsverzeichnis / den Speicherort richtig setzen
# 
# head(

#   vstats %>% arrange(desc(degree)) %>% select(name, degree, out_degree, in_degree) %>% rename (
#     trader = name,
#     'total links' = degree,
#     'export links' = out_degree,
#     'import links' = in_degree
#   )
# )

