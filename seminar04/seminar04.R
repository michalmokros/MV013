setwd("~/Documents/school/MV013/seminar04")

#TASK 1
eyes_hair = matrix(c(0.12, 0.15, 0.22, 0.34, 0.06, 0.11), nrow=2)
colnames(eyes_hair) = c('blonde', 'brown', 'red')
rownames(eyes_hair) = c('blue', 'brown')

abs.freq <- eyes_hair * 1000
abs.eyes <- margin.table(abs.freq, 1) #rowSums
abs.hair <- margin.table(abs.freq, 2) #colSums
cond.hair <- prop.table(abs.freq, 2)
cond.eyes <- prop.table(abs.freq, 1)
barplot(abs.freq, space=0, ylab='absolute frequencies', legend.text = rownames(abs.freq),
        args.legend = list(x='topleft', bty='n'))
barplot(cond.hair, space=0, ylab='relative frequencies')
barplot(t(abs.freq), space=0, ylab='absolute frequencies', legend.text=colnames(abs.freq),
        args.legend = list(x='topleft', bty='n'), col=c('dodgerblue', 'brown', 'orangered2'))
barplot(t(cond.eyes), space=0, ylab='relative frequencies')
barplot(abs.freq, space=0, ylab='absolute frequencies', legend.text = rownames(abs.freq),
        args.legend = list(x='topleft', bty='n'), col=c('blue', 'brown'))
#COLORS IN R
eye.labels <- paste(names(abs.eyes), ' (', abs.eyes/sum(abs.eyes) * 100, '%)', sep='')
#paste0(names(abs.eyes), ' (', abs.eyes/sum(abs.eyes) * 100, '%)') same as above with no sep
pie(abs.eyes, col=rownames(eyes_hair), labels=eye.labels, main='Eye color', init.angle = 45)
hair.labels <- paste0(names(abs.hair), ' (', abs.hair/sum(abs.hair) * 100, '%)')
pie(abs.hair, col=c('yellow', 'brown', 'red'), labels=hair.labels, main='Hair color')


#TASK 2
pie(rep(1,24), col=rainbow(24))
pie(rep(1,24), col=heat.colors(24))
pie(rep(1,24), col=topo.colors(24))
pie(rep(1,24), col=terrain.colors(24))
pie(rep(1,24), col=grey(seq(from = 0, to = 1, by = 1/24)))


#TASK 3
legend(iris)
plot(iris$Petal.Length, iris$Petal.Width, pch=c(1,2,3)[unclass(iris$Species)], 
     col=c('Red', 'Green', 'Blue')[unclass(iris$Species)], main='Petal Scatter', 
     xlab='Petal Length (cm)', ylab='Petal Width (cm)', asp = 1)
