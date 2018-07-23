.vec <- function(vec) list(n=length(vec), colors=vec)

colors.distinct.list <- list(
# some simple, short color lists
  .vec(c("red")),
  .vec(c("red", "blue")),
  .vec(c("red", "blue", "green")),
  .vec(c("red", "blue", "green", "brown")),
  .vec(c("red", "blue", "green", "brown", "orange")),
  .vec(c("red", "blue", "green", "brown", "orange", "#008833")),
  .vec(c("red", "blue", "green", "brown", "orange", "#008833", "#7788ff")),
  .vec(c("red", "blue", "green", "brown", "orange", "#008833", "#7788ff", "#cf00ff")),
  .vec(c("red", "blue", "green", "brown", "orange", "#008833", "#7788ff", "#cf00ff", "#770066")),
  .vec(c("red", "blue", "green", "brown", "orange", "#008833", "#7788ff", "#cf00ff", "#770066",
         "#cccc00")),
  .vec(c("red", "blue", "green", "brown", "orange", "#008833", "#7788ff", "#cf00ff", "#770066",
         "#cccc00", "darkgray")),

## Kelly's 20 colors of maximum contrast
## from http://stackoverflow.com/questions/470690
#  .vec(c("#FFB300", "#803E75", "#FF6800", "#A6BDD7", "#C10020",
#         "#CEA262", "#817066", "#007D34", "#F6768E", "#00538A",
#         "#FF7A5C", "#53377A", "#FF8E00", "#B32851", "#F4C800",
#         "#7F180D", "#93AA00", "#593315", "#F13A13", "#232C16"))

# 20 distinguishable colors from
# http://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
# which I like better than Kelly's, but with the pale yellow replaced because it
# is virtually invisible in plots
  .vec(c("#E6194B", "#3CB44B", "#FFE119", "#0082C8", "#F58231",
         "#911EB4", "#46F0F0", "#F032E6", "#D2F53C", "#FABEBE",
         "#008080", "#E6BEFF", "#AA6E28", "#cFcA08", "#800000",
         "#AAFFC3", "#808000", "#FFD8B1", "#000080", "#808080")),

# the same colors, extented to 21, with two pseudo-gray levels
.vec(c("#E6194B", "#3CB44B", "#FFE119", "#0082C8", "#F58231",
       "#911EB4", "#46F0F0", "#F032E6", "#D2F53C", "#FABEBE",
       "#008080", "#E6BEFF", "#AA6E28", "#cFcA08", "#800000",
       "#AAFFC3", "#808000", "#FFD8B1", "#000080", "#505000",
       "#90A0A0"))
);

devtools::use_data(colors.distinct.list, internal = TRUE, overwrite = TRUE, compress="xz")
