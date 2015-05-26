
pmf1 <- random.pmf(10, rexp)

pmf1

colour.values <- c("red", "orange", "yellow", "green", "blue", "indigo", "violet", "brown", "white", "black")

rpmf1 <- rpmf.generator(pmf1, colour.values)

rpmf1(1)

rpmf1(1)

rpmf1(1)

c1 <- rpmf1(20)

c2 <- rpmf1(200)

c3 <- rpmf1(2000)

pmf2 <- random.pmf(10, rexp)

pmf2

rpmf2 <- rpmf.generator(pmf2, colour.values)

c4 <- rpmf2(20)
c4
c5 <- rpmf2(200)
c5
c6 <- rpmf2(2000)
c6

city.values <- c("Guelph", "Toronto", "Montreal", "Vancouver", "Ottawa", "Halifax", "Edmonton", "Calgary", "Winnipeg", "Regina")

rpmf.cities <- rpmf.generator(pmf2, city.values)

rpmf2(1)
rpmf2(1)
rpmf.cities(1)
rpmf.cities(1)
