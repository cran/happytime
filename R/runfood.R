#' @title What to Eat Today
#'
#' @description A small program is used to decide what to eat today, and currently only contains delicious Guangdong dishes.
#'
#' @examples runfood()
#'
#' @export runfood
#'
#' @importFrom grDevices getGraphicsEvent rainbow


runfood <- function(){
  par(bty="n",xaxt="n", yaxt="n",mar=c(0,0,0,0),cex=4,font=4)
  plot(c(0,1),c(0,1),type="n",xlab="",ylab="")
  rect(xleft = 0.4, ybottom = 0.05, xright = 0.6, ytop = 0.15)
  text(0.5,0.1,"run")
  text(0.4,0.9,"What to eat today?",
       col=rainbow(1000)[sample(1:1000,1)],cex=0.6)
  getGraphicsEvent("What to eat today",onMouseDown = getre)
}

getre <- function(button,x,y){
  recipe <- c('classic salad', 'orange juice cheese crab meat salad', 'beef vegetable soup', 'fried pork chop', 'bread chicken row', 'chicken rice',
              ' shrimp and cream spaghetti ',' pork stew ',' lobster rice ',' assorted salad ',' tomato beef',
              'German Chai do', 'Kiwi chicken salad', 'yogurt fruit salad', 'North African millet salad', 'fruit tomato seafood', 'sausage salad',
              'broccoli beef', 'fried rice noodle', 'Gong Bao chicken ding', 'spring rolls',' dumplings', 'wonton', 'abalone', 'Guangzhou Wenchang chicken',
              'Ming roast roast suckling pig', 'New Dragon Emperor night banquet', 'Peninsula imperial product official swallow', 'steamed East Star spot', 'hanging stove roast goose', 'raw crab meat stewed Tiger Tiger wing',
              'Yan Nan Fei Cha Tian duck', 'Chaozhou braised flavor', 'white chilled chicken', 'braised pigeon', 'honey barbecued pork', 'crispy roast pork', 'lobster baked with broth',
              'abalone juice', 'buckwheat', 'braised shark fin', 'white scalded elephant clam', 'coconut ice sugar birds nest', 'kylin bass',' pepper salt shrimp ',' scalded shrimp ',
              'dry fried cow river', 'Guangdong morning tea', 'old fire soup', 'pot steamed rice', 'Luo Han Zhai', 'Cantonese style stuffed duck', 'black bean sauce steamed pork ribs',' pineapple grunt meat ',
              'Rose soy sauce chicken', 'radish beef brisket pot', 'Dinghu Shang Su', 'fish flavor eggplant pot', 'sweet potato sauce pork', 'halogen goose liver', 'oyster bake', 'Hibiscus shrimp',
              'Sha Cha beef', 'Hakka tofu', 'meisai braised pork', 'salt baked chicken', 'pork tripe wrapped chicken', 'potable dish', 'pig feet Ginger', 'Lai powder')
  num <- 1:70
  if(button==0&x<=0.6&x>=0.4&y<=0.15&y>=0.05){
    rect(0,0.3,1,0.8,col="white",border=NA);
    B<-sample(num,1);
    text(0.5,0.5,recipe[B],col=rainbow(1000)[sample(1:1000,1)],cex=1);
  }
}


