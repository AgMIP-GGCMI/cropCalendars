#-----------------------------------------------------------#
# Climate-driven sowing and harvest dates                   #
# R-Code                                                    #
# Written by Sara Minoli                                    #
# Based on Minoli et al. (2019) Global and Planetary Change #
#-----------------------------------------------------------#

#-----------------------------------------------------------#
# CONFIGURATION of GRAPHICS PARAMETERS #
#-----------------------------------------------------------#
# PAPER SIZE ----
# 1 inch == 2.54 cm
a4w <- 8.27   #inches
a4h <- 11.69  #inches


# COLOR PALETTE ----
seasonality_types <- c("NO_SEASONALITY", "PREC", "PRECTEMP", "TEMP", "TEMPPREC")
seasonality_types_levs <- c(1:5)
seasonality_types_labels <- c("No seasonality"="NO_SEASONALITY",
                              "Precipitation"="PREC",
                              "Both with prevailing precipitation"="PRECTEMP",
                              "Temperature"="TEMP",
                              "Both with prevailing temperature"="TEMPPREC")
seasonality_types_cols <- c("#4d4d4d",
                            "#448888",
                            "#43b3b3",
                            "#992D2D",
                            "#ff6666")
names(seasonality_types_cols) <- names(seasonality_types)

seasonality_types3 <- c("NO_SEAS", "PREC_SEAS", "MIXED_SEAS")
seasonality_types3_levs <- c(1:3)




harvest_rule_labels <- paste(rep(c(1:3), times=3),
                             rep(c("a", "b", "c"), each=3), sep=".")
harvest_rule_cols <- c("#f0f0f0","#deebf7","#fee0d2",
                      "#bdbdbd","#9ecae1","#fc9272",
                      "#636363", "#3182bd","#de2d26")
names(harvest_rule_cols) <- harvest_rule_labels



harvest_reason_labels <- c("GPmin", "GPmaxrp", "GPmax",
                           "w. lim", "mid. t.", "high t.")
harvest_reason_labels_ext <- c("Earliest-maturing cultivar (GPmin)",
                               "Cultivar with longest grain filling (GPmaxrp)",
                               "Latest-maturing cultivar (GPmax)",
                               "Escape terminal water stress (w. lim)",
                               "Grain filling in warmest period (mid. t.)",
                               "Escape high temperature (high t.)")
harvest_reason_cols <- c("#d9f0d3", "#7fbf7b", "#1b7837", "#d95f02", "#af8dc3", "#762a83")
names(harvest_reason_cols) <- harvest_reason_labels
