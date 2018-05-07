.vec <- function(vec) list(n=length(vec), colors=vec)

colors.distinct.list <- list(
# some simple, short color lists
  .vec(c("red")),
  .vec(c("red", "blue")),
  .vec(c("red", "blue", "green")),
  .vec(c("red", "blue", "green", "violet")),
  .vec(c("red", "blue", "orange", "green", "violet")),
  .vec(c("red", "blue", "orange", "green", "violet", "gold")),
# Boynton's list of 9 colors
# from http://stackoverflow.com/questions/470690
  .vec(c("#0000FF", "#FF0000", "#00FF00", "#FFFF00", "#FF00FF",
         "#FF8080", "#808080", "#800000", "#FF8000")),
# Kelly's 20 colors of maximum contrast
# from http://stackoverflow.com/questions/470690
  .vec(c("#FFB300", "#803E75", "#FF6800", "#A6BDD7", "#C10020",
         "#CEA262", "#817066", "#007D34", "#F6768E", "#00538A",
         "#FF7A5C", "#53377A", "#FF8E00", "#B32851", "#F4C800",
         "#7F180D", "#93AA00", "#593315", "#F13A13", "#232C16"))
);

devtools::use_data(colors.distinct.list, internal = TRUE, overwrite = TRUE, compress="xz")
