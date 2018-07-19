#' @title The 2048 Game
#'
#' @description The adjacent numbers can be added if they are equal. When the maximum number reaches 2048, you win.
#'
#' @examples run2048()
#'
#' @export run2048
#'
#' @importFrom graphics grid par plot points rect text


run2048 <- function(){
  options("e"=new.env())
  stage0()
  getGraphicsEvent(prompt="2048",onKeybd=keydown)
}

draw_bg <- function(){
  plot(0,0,xlim=c(0,0.8),ylim=c(0,0.8),type='n',xaxs="i", yaxs="i",xaxt="n", yaxt="n",xlab = "",ylab = "")
  for (i in c(1:4)){
    for (j in c(1:4)){
      points(0.1+0.2*(i-1),0.9-0.2*(j),col="coral1",pch=15,cex=16)}}
  grid(col="gray")
}

draw_num <- function(){
  e <- getOption("e")
  m <- get("m",envir = e)
  for (i in c(1:4)){
    for (j in c(1:4)){
      if (m[i,j] != 0){
        text(0.1+(j-1)*0.2,0.7-(i-1)*0.2,font=2,label=m[i,j],cex=2)
      }
    }
  }
}

init <- function(){
  e <- getOption("e")
  assign('stage',1,envir = e)
  mt <- matrix(c(sample(c(2,4),1),rep(0,15)),nrow=4)
  assign("m",mt[sample(4),sample(4)],envir = e)
  draw_bg()
  draw_num()
}

rm_zero <- function(){
  e <- getOption("e")
  x <- get("x",envir = e)
  dir <- get("dir",envir = e)
  m <- get("m",envir = e)
  if (x==0){
    if (dir=="up"){
      for (c in 1:4) m[,c] <- c(m[,c][which(m[,c]!=0)],rep(0,4-length(m[,c][which(m[,c]!=0)])))
    }
    if (dir=="down"){
      for (c in 1:4) m[,c] <- c(rep(0,4-length(e$m[,c][which(e$m[,c]!=0)])),e$m[,c][which(e$m[,c]!=0)])
    }
    if (dir=="left"){
      for (r in 1:4) m[r,] <- c(m[r,][which(m[r,]!=0)],rep(0,4-length(m[r,][which(m[r,]!=0)])))
    }

    if (dir=="right"){
      for (r in 1:4) m[r,] <- c(rep(0,4-length(m[r,][which(m[r,]!=0)])),m[r,][which(m[r,]!=0)])
    }
  }
  else{
    if (dir=="up"){
      c <- x
      m[,c] <- c(m[,c][which(m[,c]!=0)],rep(0,4-length(m[,c][which(m[,c]!=0)])))
    }
    if (dir=="down"){
      c <- x
      m[,c] <- c(rep(0,4-length(m[,c][which(m[,c]!=0)])),m[,c][which(m[,c]!=0)])
    }
    if (dir=="left"){
      r <- x
      m[r,] <- c(m[r,][which(m[r,]!=0)],rep(0,4-length(m[r,][which(m[r,]!=0)])))
    }

    if (dir=="right"){
      r <- x
      m[r,] <- c(rep(0,4-length(m[r,][which(m[r,]!=0)])),m[r,][which(m[r,]!=0)])
    }
  }
  assign("m", m, envir = e)
}

new_mt <- function(){
  e <- getOption("e")
  m <- get("m",envir = e)
  if(length(which(m==0))!=0){
    m[sample(which(m==0),1)] <- sample(c(2,4),1)
    assign("m",m,envir = e)
  }
}

fail <- function(){
  e <- getOption("e")
  m <- get("m",envir = e)
  x <- get("x",envir = e)
  if (length(m[which(m==0)])==0){
    assign("x",0,envir = e)
    for (r in 1:3){
      for (c in 1:3){
        if (m[r,c] == m[r,c+1] | m[r,c] == m[r+1,c]){
          assign("x",1,envir = e)
        }
      }
    }
    if (x==0){
      stage2()
    }}
}

stage1 <- function(){
  e <- getOption("e")
  assign("stage", 1, envir = e)
  assign("x", 0,envir = e)
  dir <- get("dir", envir = e)
  m <- get("m",envir = e)
  rm_zero()
  if (dir=="left"){
    i=1
    while (i<=4){
      if (m[i,1] != 0 & m[i,1]==m[i,2] & m[i,1]==m[i,3] & m[i,1]==m[i,4]){
        m[i,]=rep(c(2*e$m[i,1],0),each=2)
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[i,2]!=0 & m[i,3] != 0 & m[i,2]==m[i,1] & m[i,3]==m[i,4]){
        m[i,]=c(2*m[i,1],0,2*m[i,3],0)
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[i,2] != 0 & m[i,2]==m[i,1]){
        m[i,]=c(2*m[i,1],0,m[i,3],m[i,4])
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[i,3] != 0 & m[i,3]==m[i,4]){
        m[i,]=c(m[i,1],m[i,2],2*m[i,3],0)
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[i,2] != 0 & m[i,2]==m[i,3]){
        m[i,]=c(m[i,1],2*m[i,2],0,m[i,4])
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      i=i+1
    }
    rm_zero()
    fail()
    new_mt()
    draw_bg()
    draw_num()
    fail()
  }
  if (dir=="right"){
    i=1
    while (i<=4){
      if (m[i,1] != 0 & m[i,1]==m[i,2] & m[i,1]==m[i,3] & m[i,1]==m[i,4]){
        m[i,]=rep(c(0,2*m[i,1]),each=2)
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[i,2] != 0 & m[i,3] != 0 & m[i,2]==m[i,1] & m[i,3]==m[i,4]){
        m[i,]=c(0,2*m[i,1],0,2*m[i,3])
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[i,2] != 0 & m[i,2]==m[i,1]){
        m[i,]=c(0,2*m[i,1],m[i,3],m[i,4])
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[i,3] != 0 & m[i,3]==m[i,4]){
        m[i,]=c(m[i,1],m[i,2],0,2*m[i,3])
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[i,2] != 0 & m[i,2]==m[i,3]){
        m[i,]=c(m[i,1],0,2*m[i,2],m[i,4])
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      i=i+1
    }
    rm_zero()
    fail()
    new_mt()
    draw_bg()
    draw_num()
    fail()
  }

  if (dir=="up"){
    j=1
    while (j<=4){
      if (m[1,j] != 0 & m[1,j]==m[2,j] & m[1,j]==m[3,j] & m[1,j]==m[4,j]){
        m[,j]=rep(c(2*m[1,j],0),each=2)
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[2,j] != 0 & m[3,j] != 0 & m[2,j]==m[1,j] & m[3,j]==m[4,j]){
        m[,j]=c(2*m[1,j],0,2*m[3,j],0)
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[2,j] != 0 & m[2,j]==m[1,j]){
        m[,j]=c(2*m[1,j],0,m[3,j],m[4,j])
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[3,j] != 0 & m[3,j]==m[4,j]){
        m[,j]=c(m[1,j],m[2,j],2*m[3,j],0)
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[2,j] != 0 & m[2,j]==m[3,j]){
        m[,j]=c(m[1,j],2*m[2,j],0,m[4,j])
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      j=j+1
    }
    rm_zero()
    fail()
    new_mt()
    draw_bg()
    draw_num()
    fail()
  }
  if (dir == "down"){
    j=1
    while (j <= 4){
      if (m[1,j] != 0 & m[1,j]==m[2,j] & m[1,j]==m[3,j] & m[1,j]==m[4,j]){
        m[,j] = rep(c(0,2*m[1,j]),each=2)
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[2,j] != 0 & m[3,j] != 0 & m[2,j]==m[1,j] & m[3,j]==m[4,j]){
        m[,j]=c(0,2*m[1,j],0,2*m[3,j])
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[2,j] != 0 & m[2,j]==m[1,j]){
        m[,j]=c(0,2*m[1,j],m[3,j],m[4,j])
        assign("m", m, envir = e)
        assign("x",1,envir = e)
      }
      else if (m[3,j] != 0 & m[3,j]==m[4,j]){
        m[,j] = c(m[1,j],m[2,j],0,2*m[3,j])
        assign("m", m,envir = e)
        assign("x",1,envir = e)
      }
      else if (m[2,j] != 0 & m[2,j]==m[3,j]){
        m[,j] = c(m[1,j],0,2*m[2,j],m[4,j])
        assign("m", m,envir = e)
        assign("x",1,envir = e)
      }
      j = j+1
    }
    rm_zero()
    fail()
    new_mt()
    draw_bg()
    draw_num()
    fail()
  }
}
stage2 <- function(){
  e <- getOption("e")
  assign("stage", 2,envir = e)
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i",xaxt="n", yaxt="n",xlab = "",ylab = "")
  text(0.5,0.7,label="Game Over",cex=2)
  text(0.5,0.4,label="Space to restart, q to quit.",cex=2,col=4)
}

stage0 <- function(e = parent.frame()){
  assign("stage", 0,envir = e)
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i",xaxt="n", yaxt="n",xlab = "",ylab = "")
  text(0.5, 0.7, label = "welcome to 2048!", cex = 2)
  text(0.5, 0.4, label = "Any keyboard to start", cex = 2, col = 3)
  text(0.5, 0.3, label = "Up,Down,Left,Rigth to control direction", cex = 1, col = 4)
}

## input
keydown <- function(K){
  e <- getOption("e")
  stage <- get("stage",envir = e)
  print(paste("keydown:",K,",stage:",stage));
  if(stage == 0){
    init()
    return(NULL)
  }
  if(stage == 2){ #game over
    if(K == "q") q()
    else if(K == ' ') stage0()
    return(NULL)
  }
  if(stage == 1){ #playing
    if(K == "q") {
      stage2()
    }else {
      if(tolower(K) %in% c("up", "down", "left", "right")){
        assign("dir", tolower(K),envir = e)
        stage1()
      }
    }
  }
  return(NULL)
}

