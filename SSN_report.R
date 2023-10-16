# Mini-projet


# Chargement des packages -------------------------------------------------

library(igraph)
library(tidyverse)
library(kableExtra)

# Travail sur les données de contacts des foyers kenyans (Kiti, 2016)
# Note de lecture du script : le nombre de "#" correspond la plupart du temps à la subdivision (par ex, ### est une sous partie de ##)


# Contacts au sein des (intra) foyers ------------------------------------------------------

## Importation des données ("within_households)

contacts_within <- read_csv("Pesah_b/scc2034_kilifi_all_contacts_within_households.csv")

## Observations basiques

### Classement des foyers par nombre de membres

foyers_par_membres <- contacts_within |> 
  select(h1, m1) |> 
  group_by(h1) |> 
  distinct(m1) |> 
  count() |> 
  arrange(desc(n)) |> 
  rename(nombre_membres = n)

### Classement des foyers par nombre de contacts parmi/entre ses membres

foyers_par_contacts <- contacts_within |> 
  group_by(h1) |> 
  count() |> 
  ungroup() |>
  arrange(fct(h1, levels = c("H", "E", "B", "F", "L"))) |> 
  select(n) |> 
  rename(nombre_contacts_intra_groupe = n)


### Fusion des deux tableaux précédents : nombre de membres et nombres de contacts par foyer

bind_cols(foyers_par_membres, foyers_par_contacts) |>
  rename(Foyer = h1) |> 
  kbl(booktabs = TRUE, col.names = c("Foyer", "Nombre de membres", "Nombre de contacts au sein du foyer")) |> 
  kable_styling(full_width = FALSE)


### Répartition des membres des foyers par âge


nodes_contacts_within |> # Le tableau des individus "nodes_contacts_within" est créé ci-dessous
  mutate(h1m1 = fct(str_sub(h1m1, 1, 1), levels = c("H", "E", "B", "F", "L"))) |> 
  count(h1m1, age1) |> 
  pivot_wider(names_from = age1, values_from = n, values_fill = 0) |> 
  kbl(booktabs = TRUE, col.names = c("Foyer", "0-5 ans", "6-14 ans", "15-19 ans", "20-49 ans", "50  et plus")) |> 
  kable_styling(full_width = FALSE)

### Répartition des membres des foyers par sexe

nodes_contacts_within |>
  mutate(h1m1 = fct(str_sub(h1m1, 1, 1), levels = c("H", "E", "B", "F", "L"))) |> 
  rename(foyer = h1m1) |>
  count(foyer, sex1) |> 
  pivot_wider(names_from = sex1, values_from = n, values_fill = 0) |> 
  kbl(booktabs = TRUE, col.names = c("Foyer", "Nombre de femmes", "Nombre d'hommes")) |> 
  kable_styling(full_width = FALSE)

## Graphe

### Création de la edgelist

links_contacts_within <- contacts_within |>
  select(h1, m1, h2, m2, duration, day, hour) |> 
  unite("h1m1", h1, m1, sep = "") |> 
  unite("h2m2", h2, m2, sep = "")

#### Et des edgelists par foyer

links_contacts_within_H <- links_contacts_within |> filter(str_detect(h1m1, "^H"))
links_contacts_within_E <- links_contacts_within |> filter(str_detect(h1m1, "^E"))
links_contacts_within_B <- links_contacts_within |> filter(str_detect(h1m1, "^B"))
links_contacts_within_F <- links_contacts_within |> filter(str_detect(h1m1, "^F"))
links_contacts_within_L <- links_contacts_within |> filter(str_detect(h1m1, "^L"))

### Création du tableau des individus (la liste des sommets)

nodes_contacts_within <- contacts_within |> 
  distinct(h1, m1, age1, sex1) |> 
  unite("h1m1", h1, m1, sep = "") |>
  arrange(h1m1)

#### Et des tableaux des individus par foyer

nodes_contacts_within_H <- nodes_contacts_within |> filter(str_detect(h1m1, "^H"))
nodes_contacts_within_E <- nodes_contacts_within |> filter(str_detect(h1m1, "^E"))
nodes_contacts_within_B <- nodes_contacts_within |> filter(str_detect(h1m1, "^B"))
nodes_contacts_within_F <- nodes_contacts_within |> filter(str_detect(h1m1, "^F"))
nodes_contacts_within_L <- nodes_contacts_within |> filter(str_detect(h1m1, "^L"))

### Création du graphe (on utilise le foyer H pour l'exemple)

reseau_within_H <- graph_from_data_frame(d = links_contacts_within_H, vertices = nodes_contacts_within_H, directed = F)
reseau_within_simp_H <- igraph::simplify(reseau_within_H, remove.multiple = T, edge.attr.comb = c(duration = "sum", day = "ignore", hour = "ignore")) # "Simplification" du graphe : entre autres, les liens multiples sont fusionnés en un seul avec les durées de contact sommées

### Fixation des attributs (foyer H)

V(reseau_within_simp_H)$color <- c("orange3", "dodgerblue4")[1 + (V(reseau_within_simp_H)$sex1=="M")] # Femmes en orange et hommes en bleu
V(reseau_within_simp_H)$size <- V(reseau_within_simp_H)$age1 * 4 + 8 # La taille des sommets est proportionnelle à l'âge du membre du foyer
### J'ai hésité à fixer la taille du sommet proportionnelle au nombre de contacts, mais cette information est déjà présente à travers la centralité du sommet dans le graphe

E(reseau_within_simp_H)$width <- E(reseau_within_simp_H)$duration/3000 # La taille des edges correspond à durée totale des contacts entre deux individus


### Affichage du graphe (foyer H)

par(mar=c(1.5, 1.5, 1.5, 1.5)) # fixe les marges

plot(reseau_within_simp_H, 
     vertex.label = NA, vertex.color = V(reseau_within_simp_H)$color, vertex.size = V(reseau_within_simp_H)$size,
     edge.color = "darkgrey", edge.curved = 0.1, edge.width = E(reseau_within_simp_H)$width,
     main = "Contacts entre les membres du foyer H", frame = T, margin = c(0.1, 0.1, 0.1, 0.1)) # On pourrait éventuellement rajouter "layout = layout_in_circle" pour un small world model (cf p.18 tutoriel 2018 kateto.net)

legend(x = -1.4, y = -1, c("Femme", "Homme"), pch = 21, # pch fait en sort qu'à côté de Homme et Femme il y ait un cercle de la couleur définie avec pt.bg
       pt.bg = c("orange3", "dodgerblue4"), pt.cex = 1.8, cex = 0.8)

### Pour les autres foyers (E, B, F et L) : Création du graphe, fixation des attributs et affichage du graphe

#### Foyer E
reseau_within_E <- graph_from_data_frame(d = links_contacts_within_E, vertices = nodes_contacts_within_E, directed = F)
reseau_within_simp_E <- igraph::simplify(reseau_within_E, remove.multiple = T, edge.attr.comb = c(duration = "sum", day = "ignore", hour = "ignore"))
V(reseau_within_simp_E)$color <- c("orange3", "dodgerblue4")[1 + (V(reseau_within_simp_E)$sex1=="M")]
V(reseau_within_simp_E)$size <- V(reseau_within_simp_E)$age1 * 4 + 8
E(reseau_within_simp_E)$width <- E(reseau_within_simp_E)$duration/3000
plot(reseau_within_simp_E, 
     vertex.label = NA, vertex.color = V(reseau_within_simp_E)$color, vertex.size = V(reseau_within_simp_E)$size,
     edge.color = "darkgrey", edge.curved = 0.1, edge.width = E(reseau_within_simp_E)$width,
     main = "Contacts entre les membres du foyer E", frame = T, margin = c(0.1, 0.1, 0.1, 0.1))


#### Foyer B
reseau_within_B <- graph_from_data_frame(d = links_contacts_within_B, vertices = nodes_contacts_within_B, directed = F)
reseau_within_simp_B <- igraph::simplify(reseau_within_B, remove.multiple = T, edge.attr.comb = c(duration = "sum", day = "ignore", hour = "ignore"))
V(reseau_within_simp_B)$color <- c("orange3", "dodgerblue4")[1 + (V(reseau_within_simp_B)$sex1=="M")]
V(reseau_within_simp_B)$size <- V(reseau_within_simp_B)$age1 * 4 + 8
E(reseau_within_simp_B)$width <- E(reseau_within_simp_B)$duration/3000
plot(reseau_within_simp_B, 
     vertex.label = NA, vertex.color = V(reseau_within_simp_B)$color, vertex.size = V(reseau_within_simp_B)$size,
     edge.color = "darkgrey", edge.curved = 0.1, edge.width = E(reseau_within_simp_B)$width,
     main = "Contacts entre les membres du foyer B", frame = T, margin = c(0.1, 0.1, 0.1, 0.1))


#### Foyer F
reseau_within_F <- graph_from_data_frame(d = links_contacts_within_F, vertices = nodes_contacts_within_F, directed = F)
reseau_within_simp_F <- igraph::simplify(reseau_within_F, remove.multiple = T, edge.attr.comb = c(duration = "sum", day = "ignore", hour = "ignore"))
V(reseau_within_simp_F)$color <- c("orange3", "dodgerblue4")[1 + (V(reseau_within_simp_F)$sex1=="M")]
V(reseau_within_simp_F)$size <- V(reseau_within_simp_F)$age1 * 4 + 8
E(reseau_within_simp_F)$width <- E(reseau_within_simp_F)$duration/3000
plot(reseau_within_simp_F, 
     vertex.label = NA, vertex.color = V(reseau_within_simp_F)$color, vertex.size = V(reseau_within_simp_F)$size,
     edge.color = "darkgrey", edge.curved = 0.1, edge.width = E(reseau_within_simp_F)$width,
     main = "Contacts entre les membres du foyer F", frame = T, margin = c(0.1, 0.1, 0.1, 0.1))


#### Foyer L
reseau_within_L <- graph_from_data_frame(d = links_contacts_within_L, vertices = nodes_contacts_within_L, directed = F)
reseau_within_simp_L <- igraph::simplify(reseau_within_L, remove.multiple = T, edge.attr.comb = c(duration = "sum", day = "ignore", hour = "ignore"))
V(reseau_within_simp_L)$color <- c("orange3", "dodgerblue4")[1 + (V(reseau_within_simp_L)$sex1=="M")]
V(reseau_within_simp_L)$size <- V(reseau_within_simp_L)$age1 * 4 + 8
E(reseau_within_simp_L)$width <- E(reseau_within_simp_L)$duration/3000
plot(reseau_within_simp_L, 
     vertex.label = NA, vertex.color = V(reseau_within_simp_L)$color, vertex.size = V(reseau_within_simp_L)$size,
     edge.color = "darkgrey", edge.curved = 0.1, edge.width = E(reseau_within_simp_L)$width,
     main = "Contacts entre les membres du foyer L", frame = T, margin = c(0.1, 0.1, 0.1, 0.1))



## Mesures du graphique (foyer H)

### Densité
edge_density(reseau_within_simp_H)

### Transitivité globale et moyenne des transitivités locales
### Pour l'interprétation, voir notamment slide 36 du cours du 21 octobre

transitivity(reseau_within_simp_H, type = "global")
mean(transitivity(reseau_within_simp_H, type = "local"))

### Diamètre
diameter(reseau_within_simp_H)

### Degré (et centralisation)
degree(reseau_within_simp_H)
centr_degree(reseau_within_simp_H)$centralization # "Group Degree Centralization: it equals 1 when one actor chooses all other actors (star graph). It equals 0 when all degrees are equal (circle graph)"

### Extraction des trois membres avec le plus haut degré

top_three_members <- degree(reseau_within_simp_H) |> 
  enframe() |> 
  slice_max(value, n = 3) |> 
  select(name)

nodes_contacts_within_H |> 
  filter(h1m1 %in% deframe(top_three_members))

### Proximité, eigenvector et intermediarité

centr_clo(reseau_within_simp_H)$centralization
centr_eigen(reseau_within_simp_H)$centralization
centr_betw(reseau_within_simp_H)$centralization

### Distances

mean_distance(reseau_within_simp_H)

### Cliques

largest_cliques(reseau_within_simp_H) # Sort 50 cliques de 12 sommets


clique_num(reseau_within_simp_H) # Calcul du nombre de sommets de la clique la plus large

### Assortativité

assortativity_nominal(reseau_within_simp_H, as.numeric(as.factor(V(reseau_within_simp_H)$sex1))) # Astuce d'utiliser as.numeric et as.factor trouvée ici : https://stackoverflow.com/questions/37840680/assortativity-nominal-in-igraph (et voir https://ona-book.org/similarity.html pour l'interprétation de l'assortivity)
assortativity_nominal(reseau_within_simp_H, as.numeric(as.factor(V(reseau_within_simp_H)$age1)))

### Tableau récapitulatif

#### On crée la liste des graphe pour les cinq foyers
reseaux_within_simp <- list(reseau_within_simp_H, reseau_within_simp_E, reseau_within_simp_B, reseau_within_simp_F, reseau_within_simp_L)

tibble(
  Foyer = c("H", "E", "B", "F", "L"),
  "Dens" = map(reseaux_within_simp, edge_density) |> unlist() |> round(2),
  "Trans glob" = map(reseaux_within_simp, ~transitivity(.x, type = "global")) |> unlist() |> round(2),
  "Moy trans loc" = map(reseaux_within_simp, ~mean(transitivity(.x, type = "local"))) |> unlist() |> round(2),
  "Diam" = map(reseaux_within_simp, diameter) |> unlist() |> round(2),
  "Degr" = map(reseaux_within_simp, ~centr_degree(.x)$centralization) |> unlist() |> round(2),
  "Prox" = map(reseaux_within_simp, ~centr_clo(.x)$centralization) |> unlist() |> round(2),
  "Eigen" = map(reseaux_within_simp, ~centr_eigen(.x)$centralization) |> unlist() |> round(2),
  "Interm" = map(reseaux_within_simp, ~centr_betw(.x)$centralization) |> unlist() |> round(2),
  "Dist" = map(reseaux_within_simp, mean_distance) |> unlist() |>  round(2),
  "Clique max" = map(reseaux_within_simp, clique_num) |> unlist(),
  "Assort genre" = map(reseaux_within_simp, ~assortativity_nominal(.x, as.numeric(as.factor(V(.x)$sex1)))) |>  unlist()|> round(3),
  "Assort âge" = map(reseaux_within_simp, ~assortativity_nominal(.x, as.numeric(as.factor(V(.x)$age1)))) |>  unlist()|> round(3)
) |> 
  kbl(booktabs = TRUE) |> 
  kable_styling(full_width = FALSE)
  

###################################################################################################################


# Contacts entre (inter) les membres des foyers -------------------------------------------------------

## Importation des données des contacts inter-foyers (across households)

contacts_across <- read_csv("Pesah_b/scc2034_kilifi_all_contacts_across_households.csv")

## Observations basiques

### Nombre total de contacts inter-foyers par foyer

contacts_across |> 
  group_by(h1, h2) |> 
  count() |> 
  ungroup() |> 
  mutate(
    E = cumsum(if_else(h1 == "E" | h2 == "E", n , 0)),
    F = cumsum(if_else(h1 == "F" | h2 == "F", n , 0)),
    L = cumsum(if_else(h1 == "L" | h2 == "L", n , 0))
  ) |> 
  select(E, F, L) |> 
  slice_tail() |> 
  pivot_longer(everything(), names_to = "foyer", values_to = "nb_cont_ext") |> 
  kbl(booktabs = TRUE, col.names = c("Foyer", "Contacts extérieurs")) |> 
  kable_styling(full_width = FALSE)


### Répartition des contacts entre les trois foyers
  
contacts_across |> 
  group_by(h1, h2) |> 
  count() |> 
  ungroup() |> 
  mutate(
    E_F = cumsum(if_else((h1 == "E" & h2 == "F") | (h1 == "F" & h2 == "E"), n , 0)),
    E_L = cumsum(if_else((h1 == "E" & h2 == "L") | (h1 == "L" & h2 == "E"), n , 0)),
    F_L = cumsum(if_else((h1 == "F" & h2 == "L") | (h1 == "L" & h2 == "L"), n , 0))
  ) |> 
  select(E_F, E_L, F_L) |> 
  slice_tail() |> 
  pivot_longer(everything(), names_to = "foyers_impliques", values_to = "nb_cont_ext") |> 
  kbl(booktabs = TRUE, col.names = c("Foyers impliqués", "Contacts entre membres")) |> 
  kable_styling(full_width = FALSE)

## Graphe

### Création de la edgelist

links_contacts_across <- contacts_across |>
  select(h1, m1, h2, m2, duration, day, hour) |> 
  unite("h1m1", h1, m1, sep = "") |> 
  unite("h2m2", h2, m2, sep = "")


### Création du tableau des individus (la liste des sommets)

nodes_contacts_across <- contacts_within |> # on part du tableau "contacts_within" et pas "contacts_across" car c'est seulement dans le premier que tous les individus sont présents directement dans la colonne h1 
  filter(h1 == "E" | h1 == "F" | h1 == "L") |> # d'après le dictionnaire des variables, L, F et E sont les trois seuls foyers dont les périodes de mesure se chevauchent (on a donc pu mesurer leurs contacts)
  distinct(h1, m1, age1, sex1) |> 
  unite("h1m1", h1, m1, sep = "") |>
  arrange(h1m1) |> 
  filter(h1m1 %in% links_contacts_across$h1m1 | h1m1 %in% links_contacts_across$h2m2) |>  # On garde dans le tableau seulement les individus qui ont des contacts inter-foyers (étant donné que le tableau contient à ce stade tous les individus)
  mutate(foyer_num = case_when(
    str_sub(h1m1, 1, 1) == "E" ~ 1,
    str_sub(h1m1, 1, 1) == "F" ~ 2,
    str_sub(h1m1, 1, 1) == "L" ~ 3
  ))

### Classement des foyers par nombre de membres qui ont eu des contacts inter-foyers
### (On affiche pas ce tableau dans le rapport)
nodes_contacts_across |> 
  select(h1m1) |> 
  count(foyer = str_sub(h1m1, 1, 1)) # On extrait la première lettre du membre (par ex E de E1) et on compte le nombre d'occurence de la lettre dans la colonne

### Création du graph

reseau_across <- graph_from_data_frame(d = links_contacts_across, vertices = nodes_contacts_across, directed = F)
reseau_across_simp <- igraph::simplify(reseau_across, remove.multiple = T, edge.attr.comb = c(duration = "sum", day = "ignore", hour = "ignore"))

### Fixation des attributs

V(reseau_across_simp)$shape <- c("circle", "square")[1 + (V(reseau_across_simp)$sex1=="M")] # Femmes en cercle et hommes en rectangle
V(reseau_across_simp)$color <- c("gray50", "tomato", "gold")[(V(reseau_across_simp)$foyer_num)]
V(reseau_across_simp)$size <- V(reseau_across_simp)$age1 * 4 + 8 # La taille des sommets est proportionnelle à l'âge du membre du foyer
# J'ai hésité à fixer la taille du sommet proportionnelle au nombre de contacts, mais cette information est déjà présente à travers la centralité du sommet dans le graphe

E(reseau_across_simp)$width <- E(reseau_across_simp)$duration/200 # La taille des edges correspond à durée totale des contacts entre deux individus


### Affichage du graphe

par(mar=c(1.5, 1.5, 1.5, 1.5)) # fixe les marges

plot(reseau_across_simp, 
     vertex.label = NA, vertex.color = V(reseau_across_simp)$color, vertex.size = V(reseau_across_simp)$size, vertex.shape = V(reseau_across_simp)$shape,
     edge.color = "darkgrey", edge.curved = 0.1, edge.width = E(reseau_across_simp)$width,
     main = "Contacts entre les foyers E, F et L", frame = T, margin = c(0.1, 0.1, 0.1, 0.1))

legend(x = -1.4, y = -1, c("Foyer E", "Foyer F", "Foyer L", "Femme (carré pour Homme)"), pch = 21, # pch fait en sort qu'à côté de Homme et Femme il y ait un cercle de la couleur définie avec pt.bg
       pt.bg = c("gray50", "tomato", "gold", "white"), pt.cex = 1.8, cex = 0.8)





