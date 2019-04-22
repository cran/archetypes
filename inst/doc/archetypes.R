### R code from vignette source 'archetypes.Rnw'

###################################################
### code chunk number 1: archetypes.Rnw:70-71
###################################################
options(width=80, prompt='R> ')


###################################################
### code chunk number 2: archetypes.Rnw:298-299
###################################################
library(archetypes)


###################################################
### code chunk number 3: archetypes.Rnw:322-324 (eval = FALSE)
###################################################
## data("toy")
## plot(toy)


###################################################
### code chunk number 4: archetypes.Rnw:329-334
###################################################
data("toy")

par(mar = c(2,2,0,0) + 0.1, ps = 9)
plot(toy, xlab = "", ylab = "", xlim = c(0,20), ylim = c(0,20),
     pch = 19, col = gray(0.7), cex = 0.6)


###################################################
### code chunk number 5: archetypes.Rnw:343-346
###################################################
suppressWarnings(RNGversion("3.5.0"))
set.seed(1986)
a <- archetypes(toy, 3, verbose = TRUE)


###################################################
### code chunk number 6: archetypes.Rnw:357-358
###################################################
a


###################################################
### code chunk number 7: archetypes.Rnw:361-362
###################################################
parameters(a)


###################################################
### code chunk number 8: archetypes.Rnw:368-370 (eval = FALSE)
###################################################
## xyplot(a, toy, chull = chull(toy))
## xyplot(a, toy, adata.show = TRUE)


###################################################
### code chunk number 9: archetypes.Rnw:375-380
###################################################
par(mfrow = c(1,2), mar = c(2,2,0,0)+0.1, ps = 9)
xyplot(a, toy, chull = chull(toy),
       xlab = "", ylab = "", xlim = c(0, 20), ylim = c(0, 20), cex = 0.6)
xyplot(a, toy, adata.show = TRUE,
       xlab = "", ylab = "", xlim = c(0, 20), ylim = c(0, 20), cex = 0.6)


###################################################
### code chunk number 10: archetypes.Rnw:400-401 (eval = FALSE)
###################################################
## movieplot(a, toy)


###################################################
### code chunk number 11: archetypes.Rnw:406-412
###################################################
par(mfrow = c(2, 4), mar = c(0, 0, 0, 0) + 0.1, ps = 9)
movieplot(a, toy, xlim = c(0, 20), ylim = c(0, 20), cex = 0.6, axes = FALSE,
          postfn = function(iter) {
            box()
            text(1, 19, paste(iter + 1, ".", sep = ""), cex = 1)
          })


###################################################
### code chunk number 12: archetypes.Rnw:431-433
###################################################
set.seed(1986)
a4 <- stepArchetypes(data = toy, k = 3, verbose = FALSE, nrep = 4)


###################################################
### code chunk number 13: archetypes.Rnw:436-437
###################################################
a4


###################################################
### code chunk number 14: archetypes.Rnw:441-442
###################################################
summary(a4)


###################################################
### code chunk number 15: archetypes.Rnw:448-449 (eval = FALSE)
###################################################
## xyplot(a4, toy)


###################################################
### code chunk number 16: archetypes.Rnw:454-457
###################################################
par(mar = c(2, 2, 0, 0) + 0.1, ps = 9)
xyplot(a4, toy, cex = 0.6, xlim = c(0, 20), ylim = c(0, 20),
       xlab = "", ylab = "")


###################################################
### code chunk number 17: archetypes.Rnw:464-465 (eval = FALSE)
###################################################
## bestModel(a4)


###################################################
### code chunk number 18: archetypes.Rnw:467-468
###################################################
print(bestModel(a4), full = FALSE)


###################################################
### code chunk number 19: archetypes.Rnw:481-489
###################################################
file <- "as.RData"
if ( file.exists(file) ) {
  load(file = file)
} else {
  set.seed(1986)
  as <- stepArchetypes(data = toy, k = 1:10, verbose = FALSE, nrep = 4)
  save(as, file = file)
}


###################################################
### code chunk number 20: archetypes.Rnw:519-520
###################################################
rss(as)


###################################################
### code chunk number 21: archetypes.Rnw:523-524
###################################################
t(sapply(as, function(a) sapply(a, '[[', 'iters')))


###################################################
### code chunk number 22: archetypes.Rnw:535-536 (eval = FALSE)
###################################################
## screeplot(as)


###################################################
### code chunk number 23: archetypes.Rnw:541-546
###################################################
par(mar = c(3, 4, 0.1, 0) + 0.1, ps = 9)
screeplot(as, cex = 0.6, ylim = c(0, 0.08), axes = FALSE)
mtext("Archetypes", side = 1, line = 2)
axis(2, las = 2)
box()


###################################################
### code chunk number 24: archetypes.Rnw:554-557 (eval = FALSE)
###################################################
## a7 <- bestModel(as[[7]])
## xyplot(a7, toy, chull = chull(toy))
## xyplot(a7, toy, adata.show = TRUE)


###################################################
### code chunk number 25: archetypes.Rnw:562-569
###################################################
a7 <- bestModel(as[[7]])

par(mfrow = c(1, 2), mar = c(2, 2, 0, 0) + 0.1, ps = 9)
xyplot(a7, toy, chull = chull(toy),
       xlim = c(0, 20), ylim = c(0, 20), cex = 0.6, xlab = "", ylab = "")
xyplot(a7, toy, adata.show = TRUE,
       xlim = c(0, 20), ylim = c(0, 20), cex = 0.6, xlab = "", ylab = "")


###################################################
### code chunk number 26: archetypes.Rnw:583-594
###################################################
file <- "gas.RData"
if ( file.exists(file) ) {
  load(file = file)
} else {
  set.seed(1986)
  gas <- stepArchetypes(data = toy, k = 1:10,
                        family = archetypesFamily("original",
                        zalphasfn = archetypes:::ginv.zalphasfn),
                        verbose = FALSE, nrep = 4)
  save(gas, file = file)
}


###################################################
### code chunk number 27: archetypes.Rnw:633-634
###################################################
rss(gas)


###################################################
### code chunk number 28: archetypes.Rnw:638-639 (eval = FALSE)
###################################################
## movieplot(gas[[9]][[3]], toy)


###################################################
### code chunk number 29: archetypes.Rnw:644-650
###################################################
par(mfrow = c(1, 4), mar = c(0, 0, 0, 0) + 0.1, ps = 9)
movieplot(gas[[9]][[3]], toy, xlim = c(0, 20), ylim = c(0, 20), cex = 0.6,
          axes = FALSE, postfn = function(iter) {
            box()
            text(1, 19, paste(iter + 1, ".", sep = ""), cex = 1)
          })


###################################################
### code chunk number 30: archetypes.Rnw:668-671 (eval = FALSE)
###################################################
## ga7 <- bestModel(gas[[7]])
## xyplot(ga7, toy, chull = chull(toy))
## xyplot(ga7, toy, adata.show = TRUE)


###################################################
### code chunk number 31: archetypes.Rnw:676-683
###################################################
ga7 <- bestModel(gas[[7]])

par(mfrow = c(1, 2), mar = c(2, 2, 0, 0) + 0.1, ps = 9)
xyplot(ga7, toy, chull = chull(toy),
       xlim = c(0, 20), ylim = c(0, 20), cex = 0.6, xlab = "", ylab = "")
xyplot(ga7, toy, adata.show = TRUE,
       xlim = c(0, 20), ylim = c(0, 20), cex = 0.6, xlab = "", ylab = "")


###################################################
### code chunk number 32: archetypes.Rnw:693-694
###################################################
apply(coef(ga7, 'alphas'), 2, range)


###################################################
### code chunk number 33: archetypes.Rnw:796-798
###################################################
data("skel")
skel2 <- subset(skel, select = -Gender)


###################################################
### code chunk number 34: archetypes.Rnw:817-818 (eval = FALSE)
###################################################
## jd()


###################################################
### code chunk number 35: archetypes.Rnw:823-825
###################################################
par(mar = c(1, 4, 0, 0) + 0.1, ps = 9)
jd()


###################################################
### code chunk number 36: archetypes.Rnw:833-834 (eval = FALSE)
###################################################
## pcplot(skel2)


###################################################
### code chunk number 37: archetypes.Rnw:839-841
###################################################
datacol <- rgb(178, 178, 178, maxColorValue = 255,
               alpha = round(255 * 0.2))


###################################################
### code chunk number 38: archetypes.Rnw:843-845 (eval = FALSE)
###################################################
## par(mar = c(5, 0.4, 0, 0.4) + 0.1, ps = 9)
## pcplot(skel2, las = 2, col = datacol)


###################################################
### code chunk number 39: archetypes.Rnw:847-853
###################################################
png(filename = "body-pcplot-raw.png", units = "px",
    width = 590, height = 430, pointsize = 12)
par(mar = c(5.5, 0.4, 0, 0.4) + 0.1)
pcplot(skel2, las = 2, col = datacol)
graphics.off()
cat("\\includegraphics{body-pcplot-raw.png}\n")


###################################################
### code chunk number 40: archetypes.Rnw:862-870
###################################################
file <- "bas.RData"
if ( file.exists(file) ) {
  load(file = file)
} else {
  set.seed(1981)
  as <- stepArchetypes(skel2, k = 1:15, verbose = FALSE)
  save(as, file = file)
}


###################################################
### code chunk number 41: archetypes.Rnw:885-886 (eval = FALSE)
###################################################
## screeplot(as)


###################################################
### code chunk number 42: archetypes.Rnw:891-896
###################################################
par(mar = c(3, 4, 0.4, 0) + 0.1, ps = 9)
screeplot(as, cex = 0.6, axes = FALSE)
mtext("Archetypes", side = 1, line = 2)
axis(2, las = 2)
box()


###################################################
### code chunk number 43: archetypes.Rnw:904-905
###################################################
a3 <- bestModel(as[[3]])


###################################################
### code chunk number 44: archetypes.Rnw:908-909
###################################################
t(parameters(a3))


###################################################
### code chunk number 45: archetypes.Rnw:912-913 (eval = FALSE)
###################################################
## barplot(a3, skel2, percentiles = TRUE)


###################################################
### code chunk number 46: archetypes.Rnw:918-922
###################################################
par(mar = c(5, 4, 0.4, 0) + 0.1, ps = 9)
barplot(a3, skel2, percentiles = TRUE,
        below.compressed.height = 0.4,
        below.compressed.srt = 90)


###################################################
### code chunk number 47: archetypes.Rnw:934-935 (eval = FALSE)
###################################################
## pcplot(a3, skel2, data.col = as.numeric(skel$Gender))


###################################################
### code chunk number 48: archetypes.Rnw:940-944
###################################################
datacol <- c(rgb(0, 205, 0, maxColorValue = 255,
                 alpha = round(255 * 0.2)),
             rgb(0, 0, 255, maxColorValue = 255,
                 alpha=round(255 * 0.2)))


###################################################
### code chunk number 49: archetypes.Rnw:946-948 (eval = FALSE)
###################################################
## par(mar = c(5, 0.4, 0, 0.4) + 0.1, ps = 9)
## pcplot(a3, skel2, las = 2, data.col = datacol[skel$Gender])


###################################################
### code chunk number 50: archetypes.Rnw:950-956
###################################################
png(filename = "body-pcplot-gender.png", units = "px",
    width = 590, height = 430, pointsize = 12)
par(mar = c(5.5, 0.4, 0, 0.4) + 0.1)
pcplot(a3, skel2, las = 2, data.col = datacol[skel$Gender])
graphics.off()
cat("\\includegraphics{body-pcplot-gender.png}\n")


###################################################
### code chunk number 51: archetypes.Rnw:969-970 (eval = FALSE)
###################################################
## ternaryplot(coef(a3, 'alphas'), col = as.numeric(skel$Gender))


###################################################
### code chunk number 52: archetypes.Rnw:975-980
###################################################
library("vcd")
ternaryplot(coef(a3, 'alphas'), dimnames = 1:3, cex = 0.3,
            col = datacol[skel$Gender], main = NULL,
            labels = "none", grid = FALSE)
grid.text("1", x = 3, y = 3)


###################################################
### code chunk number 53: archetypes.Rnw:996-997 (eval = FALSE)
###################################################
## skeletonplot(parameters(a3))


###################################################
### code chunk number 54: archetypes.Rnw:1002-1004
###################################################
par(mar = c(1, 4, 0, 0) + 0.1, ps = 9)
skeletonplot(parameters(a3), skel.height = 190)


###################################################
### code chunk number 55: archetypes.Rnw:1054-1055
###################################################
sessionInfo()


