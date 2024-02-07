library("psychotree")
library("PlackettLuce")
data("Topmodel2007", package = "psychotree")
R = as.grouped_rankings(Topmodel2007$preference)

tm_tree = pltree(R ~ ., data = Topmodel2007[, -1],
                 minsize = 5,
                 npseudo = 0)

worth_map(tm_tree)

##########################################

# Ranking of preference on four fruits
# based on traits taste, texture,
# price and storability

# taste
R1 = matrix(c(1, 2, 3, 4,
              4, 1, 3, 2,
              4, 1, 2, 3,
              1, 2, 0, 3), nrow = 4, byrow = TRUE)
colnames(R1) = c("apple", "banana", "orange", "pear")
mod1 = PlackettLuce(R1)

# texture
R2 = matrix(c(1, 4, 2, 3,
              1, 4, 3, 2,
              1, 4, 2, 3,
              1, 4, 2, 3), nrow = 4, byrow = TRUE)
colnames(R2) = c("apple", "banana", "orange", "pear")
mod2 = PlackettLuce(R2)

# price
R3 = matrix(c(2, 4, 3, 1,
              4, 1, 2, 3,
              3, 4, 2, 1,
              4, 3, 1, 2), nrow = 4, byrow = TRUE)
colnames(R3) = c("apple", "banana", "orange", "pear")
mod3 = PlackettLuce(R3)

# storability
R4 = matrix(c(1, 4, 3, 2,
              3, 4, 1, 2,
              1, 3, 2, 4,
              2, 3, 4, 1), nrow = 4, byrow = TRUE)
colnames(R4) = c("apple", "banana", "orange", "pear")
mod4 = PlackettLuce(R4)

# models in a list
mods = list(mod1, mod2, mod3, mod4)

# name for each model
labels = c("Taste", "Texture", "Price", "Storability")

worth_map(mods, ref = "banana", labels)

summary(mods[[1]], ref = NULL)

# plot only one model as bar
worth_bar(mod1)

library("gosset")
library("PlackettLuce")
library("climatrends")
library("chirps")
library("ggplot2")
data("nicabean", package = "gosset")
dat <- nicabean$trial
covar <- nicabean$covar
traits <- unique(dat$trait)

R <- vector(mode = "list", length = length(traits))
for (i in seq_along(traits)) {
  dat_i <- subset(dat, dat$trait == traits[i])
  R[[i]] <- rank_numeric(data = dat_i,
                         items = "item",
                         input = "rank",
                         id = "id",
                         ascending = TRUE)
}
head(dat_i)
str(dat_i)
baseline <- which(grepl("OverallAppreciation", traits))
kendall <- lapply(R[-baseline], function(X){
  kendallTau(x = X, y = R[[baseline]])
})
kendall <- do.call("rbind", kendall)
kendall$trait <- traits[-baseline]
kendall

[Package gosset version 1.0 Index]
