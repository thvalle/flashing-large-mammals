# bayestestR

library(bayestestR)
library(rstanarm)

model <- lm(Sepal.Length~Petal.Length, data=iris)
summary(model)

model <- stan_glm(Sepal.Length ~ Petal.Length, data=iris)
describe_posterior(model)
