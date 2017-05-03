library(arules)
library(arulesViz)

transactions <- read.transactions("transactions.txt", format = "single", sep = ",",
                                  cols = c(1,2), rm.duplicates = TRUE, skip = 0)
summary(transactions)

itemFrequency(transactions)
itemFrequencyPlot(transactions, support=0.05, cex.names=0.75)

rules1 <- apriori(transactions, parameter = list(support=0.007, confidence=0.75))
summary(rules1)

rules2 <- apriori(transactions, parameter = list(support=0.01, confidence=0.8))
summary(rules2)

# Wine
WineRules <- subset(rules1, subset = lhs %ain% "Wine")
summary(WineRules)
inspect(WineRules)

WineRules2 <- subset(rules1, subset = rhs %ain% "Wine")
summary(WineRules2)
inspect(WineRules2)

# Beer
BeerRules <- subset(rules1, subset = lhs %ain% "Beer" & lift>8.0)
summary(BeerRules)
inspect(BeerRules)

BeerRules2 <- subset(rules1, subset = rhs %ain% "Beer")
summary(BeerRules2)
inspect(BeerRules2)

# Canned vs Fresh
CannedVegRules <- subset(rules2, subset = lhs %ain% "Canned Vegetables" & lift>8.172)
summary(CannedVegRules)
inspect(head(CannedVegRules, n = 5, by = "lift"))

CannedVegRules2 <- subset(rules2, subset = rhs %ain% "Canned Vegetables")
summary(CannedVegRules2)
inspect(head(CannedVegRules2, n = 5, by = "lift"))

FreshVegRules <- subset(rules2, subset = lhs %ain% "Fresh Vegetables" & lift>11.878)
summary(FreshVegRules)
inspect(head(FreshVegRules, n = 5, by = "lift"))

FreshVegRules2 <- subset(rules2, subset = rhs %ain% "Fresh Vegetables" & lift>3.0)
summary(FreshVegRules2)
inspect(head(FreshVegRules2, n = 5, by = "lift"))

# Fruit Rules
FruitRules <- subset(rules2, subset = lhs %pin% "Fruit")
summary(FruitRules)
inspect(head(FruitRules, by = "lift"))

FruitRules2 <- subset(rules2, subset = rhs %pin% "Fruit")
summary(FruitRules2)
inspect(head(FruitRules2, by = "lift"))

FruitRules3 <- subset(rules2, subset = rhs %ain% "Canned Fruit")
summary(FruitRules3)

# Large vs Small Baskets
largeTransactions <- rules1[size(items(rules1)) > 6]
summary(largeTransactions)
inspect(largeTransactions)

smallTransactions <- rules1[size(items(rules1)) < 4]
summary(smallTransactions)

largeSupport <- quality(largeTransactions)$support
summary(largeSupport)

smallSupport <- quality(smallTransactions)$support
summary(smallSupport)

largeSupport <- quality(largeTransactions)$lift
summary(largeSupport)

smallSupport <- quality(smallTransactions)$lift
summary(smallSupport)

# Paper Wipes
WipesRules <- subset(rules2, subset = lhs %pin% "Wipes" & lift >10)
summary(WipesRules)
inspect(head(WipesRules, n = 5, by = "lift"))

# Vizualisations

plot(WineRules2, method="paracoord")
plot(BeerRules2, method="paracoord")

plot(smallTransactions)
plot(largeTransactions)

plot(WipesRules2, measure=c("support", "lift"), shading="confidence", interactive=FALSE)
plot(WipesRules2, method="grouped", control=list(k=5), interactive=FALSE)