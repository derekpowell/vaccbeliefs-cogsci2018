# library(GGally)
# library(network)
# library(sna)
library(ggthemes)
set.seed(123)
# make function to get edge weights out of fitted network
get_weights <- function(bnFitted) {
  arcDf <- arcs(bnFitted)
  
  apply(arcDf, 1, function(x){
    from <- x[1]
    to <- x[2]
    
    val <- bnFitted[[to]][["coefficients"]][[from]]
  })
  
}

train.scaled <- train %>% scale() %>% as_tibble()
net.fit.std <- bn.fit(net.bd.hc, train.scaled)
# make function to label nodes with abstractness tiers
recode_nodes <- function(x){
  recode(x,
         "diseaseRare" = "disease\nrarity",
         "diseaseSevere" = "disease\nseverity",
         "hb" = "holistic\nbalance",
         "infantImmLimCap" = "IIS:\nlimited\ncapacity",
         "infantImmWeak" = "IIS:\nweakness",
         "medSkept" = "medical\nskepticism",
         "nat" = "naturalism",
         "overpar" = "parental\nprotective-\nness",
         "parentExpert" = "parental\nexpertise",
         "vaccDanger" = "vaccine\ndanger",
         "vaccEff" = "vaccine\neffective-\nness",
         "vaccIntent" = "vaccination\nintentions",
         "vaccStrain" = "IIS:\nvaccines\nstrain",
         "vaccTox" = "vacc. toxic\nadditives")}

tiers <- c("claims", "worldviews", "claims", 
           "theories", "worldviews", "theories", 
           "theories", "claims", "claims", 
           "claims", "claims", "claims", 
           "claims", "intentions")

# make dataframe for plotting
temp_df <- arcs(net.fit.std) %>%
  data.frame() %>%
  mutate(edge_param = get_weights(net.fit.std),
         edge_weight = edge_param,
         pprint_edge_weight = pprint_cor(edge_param)
         # edge_weight = abs(edge_param),
         # edge_sign = factor(sign(edge_param))
         ) %>%
  mutate_at(vars(from, to), funs(recode_nodes))

# manually flag arbitrarily directed arcs
temp_df[4,5] <- temp_df[4,5] %>% paste0("*")
temp_df[22,5] <- temp_df[22,5] %>% paste0("*")
temp_df[24,5] <- temp_df[24,5] %>% paste0("*")

# manually tweak label positions

temp_df[23,5] <- temp_df[23,5] %>% paste0("             ")
temp_df[24,5] <- temp_df[24,5] %>% paste0("             ")
temp_df[18,5] <- temp_df[18,5] %>% paste0("             ")
temp_df[8,5] <- temp_df[8,5] %>% paste0("    ")

# convert dataframe to igraph
temp <- graph_from_data_frame(temp_df, directed = TRUE) %>%
  set_vertex_attr("tier", 
                  # we should make this more reproducible rather than 
                  # doing it by hand, but i can't make 'recode()' play nicely
                  # with 'set_vertex_attr()' yet 
                  value = tiers) %>%
  set_vertex_attr("tierCode",
                   value = recode(tiers, 
                                  "claims" = "\n(c)",
                                  "worldviews" = "\n(w)",
                                  "theories" = "\n(t)",
                                  "intentions" = "\n(i)"
                                  ))


kara_layout <- readRDS("../cogsci2018-vacc/kw_layout.rds")
kw_layout <- create_layout(temp, layout="kk")

kw_layout$x <- kara_layout$x 
kw_layout$y <- kara_layout$y
# do some manual moving of nodes
kw_layout[12,2] <- -.75 # disease rarity up
kw_layout[14,2] <- -1.4 # vacc intent down
kw_layout[9,1] <- -.75 # vacc intent left and down
kw_layout[9,2] <- -1 # vacc intent down
kw_layout[11,2] <- -.1 # toxins down
kw_layout[8,1] <- .25



# plot it - Derek's version: vertices as big circles
# some issues with edge weights signs
plt.net <- ggraph(kw_layout) +
  geom_edge_link(
                  aes(label=pprint_edge_weight, colour=edge_weight),
                  angle_calc="along",
                  label_dodge = unit(3, 'mm'),
                  # label_push= unit(-6, "mm"),
                  label_alpha=1,
                  arrow = arrow(length = unit(3, 'mm')),
                 end_cap = circle(32/2-3, 'mm'),
                 width=1.5
                 ) +
  geom_node_point(size = 34, color="gray20") +
  geom_node_point(size = 32, color="white") +
  scale_edge_color_distiller(palette="RdYlBu", direction=1, guide="none") +
  # scale_edge_color_viridis(direction=1) +
  # scale_color_colorblind(name = "Abstractness tier",
  #                        breaks = c("worldviews", "theories",
  #                                   "claims", "intentions"),
  #                        guide = "legend") +
  # scale_edge_width("Stength of connection",
  #                  range = c(0.25, 1.5), guide="none") +
  # scale_edge_color_manual("Sign of connection",
  #                         breaks = c("1", "-1"),
  #                         labels = c("positive", "negative"),
  #                         values = c("red3", "gray50"),
  #                         guide="none") +
  geom_node_text(aes(label = paste0(name,tierCode)),
                 color = "black",
                 size = 11*.35) +
  xlim(-2.15,2.15) +
  ylim(-1.6,1.9) +
  theme(aspect.ratio=1/2) +
  theme_void() +
    guides(color = "none",
           edge_color = "none",
           edge_width = "none") +
  theme(text = element_text(size = 10))

# # plot it - version two: vertices as small circles, labeled

# 
# plt.net <- ggraph(temp, layout="kk") +
#   geom_edge_link(arrow = arrow(length = unit(3, 'mm')),
#                  end_cap = circle(4, 'mm'),
#                  alpha = 0.8,
#                  aes(width = edge_weight, color = edge_sign)) +
#   geom_node_point(size = 4, aes(color = tier)) +
#   scale_color_colorblind(name = "Abstractness tier",
#                          breaks = c("worldviews", "theories",
#                                     "claims", "intentions"),
#                          guide = "legend") +
#   scale_edge_width("Stength",
#                    range = c(0.3, 1.3)) +
#   scale_edge_color_manual("Sign",
#                           breaks = c("1", "-1"),
#                           labels = c("positive", "negative"),
#                           values = c("red3", "steelblue3"),
#                           guide = "none") +
#   geom_node_label(aes(label = gsub("- ", "", gsub("\n", " ", name)),
#                       color = tier),
#                   alpha = 0.1,
#                   label.size = 0,
#                   # repel = TRUE,
#                   nudge_y = -0.1,
#                   # nudge_x = 0.1,
#                   size = 10*0.352777778) +
#   theme_void() +
#   xlim(-1.5, 2.8) +
#   # xlim()
#   guides(color = guide_legend(order = 3),
#          edge_color = "none",
#          edge_width = guide_legend(order = 1)) +
#   theme(text = element_text(size = 10),
#         # legend.position = "right")
#         legend.position = c(.02, .02),
#         legend.justification = c(.02, .02),
#         legend.direction = "vertical",
#         legend.box = "vertical",
#         legend.background = element_rect(color = "black", fill = "white"))


# plot it - version one: vertices as big circles
# plt.net <- ggraph(kw_layout) +
#   geom_edge_link(arrow = arrow(length = unit(5, 'mm')),
#                  end_cap = circle(15, 'mm'),
#                  aes(width = edge_weight, color = edge_sign)) +
#   geom_node_point(size = 27.5, aes(color = tier)) +
#   scale_color_colorblind(name = "Abstractness tier",
#                          breaks = c("worldviews", "theories",
#                                     "claims", "intentions"),
#                          guide = "legend") +
#   scale_edge_width("Stength of connection",
#                    range = c(0.25, 1.5), guide="none") +
#   scale_edge_color_manual("Sign of connection",
#                           breaks = c("1", "-1"),
#                           labels = c("positive", "negative"),
#                           values = c("red3", "gray50"),
#                           guide="none") +
#   geom_node_text(aes(label = name),
#                  color = "white",
#                  size = 3.5) +
#   theme_void() +
#   guides(color = "none",
#          edge_color = "none",
#          edge_width = "none") +
#   theme(text = element_text(size = 10))

# ggsave("../cogsci2018-vacc/figs/vaccine-network-plot.png",plt.net, width = 7*1.5, height=3.5*1.5)
