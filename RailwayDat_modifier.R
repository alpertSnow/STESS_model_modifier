########### STESS model modifier ###########
### Modify Railway.dat                   ###
### Date: 2017-09-08                     ###
### By: xf                               ###
############################################

# read Railway.dat
railway.dat <- readLines('Railway.dat')
# find out the length of the header
head.length <- grep('^$',railway.dat)
N <- (length(railway.dat) - head.length - 1)/5
stopifnot((length(railway.dat) - head.length - 1)%%10==0)   # 行数不对
# modify the body
body0 <- railway.dat[-c((1:head.length),length(railway.dat))]
n.node <- as.numeric(body0[(1:N-1) * 5 + 1])
mat.list <- list()
for (i in 1:N){
        mat.list[[i]] <- body0[(i-1)*5+2:5]
        mat.list[[i]] <- matrix(as.numeric(unlist(lapply(mat.list[[i]], strsplit,' '))),ncol = n.node[i],byrow = TRUE)
}
for (i in 1:(N/2)){
        mat.list[[2*i]] <- cbind(mat.list[[(i-1)*2+1]][,n.node[(i-1)*2+1]], mat.list[[2*i]])
        mat.list[[(i-1)*2+1]] <- cbind(rep(0,4), mat.list[[(i-1)*2+1]])
}
for (i in 1:N){
        mat.list[[i]][1:2,] <- sprintf('%.2f',mat.list[[i]][1:2,])
        mat.list[[i]] <- apply(mat.list[[i]], 1, paste, collapse=' ')
}
# read header and tail
head <- railway.dat[1:head.length]
tail <- railway.dat[length(railway.dat)]
# output
write(head,'railway_modified.dat',append = FALSE)
for (i in 1:N){
        write(n.node[i]+1,'railway_modified.dat',append = TRUE)
        write(mat.list[[i]],'railway_modified.dat',append = TRUE)
}
write(tail,'railway_modified.dat',append = TRUE)
