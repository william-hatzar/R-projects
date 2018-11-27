summary(OrchardSprays)
max_decrease <- which.max(OrchardSprays$decrease)
boxplot(OrchardSprays$decrease ~ OrchardSprays$treatment)