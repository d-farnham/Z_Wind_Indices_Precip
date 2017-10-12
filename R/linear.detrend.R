linear.detrend = function(y, year){
  mod = lm(y ~ year)
  out = y - predict(mod)
  return(out)
}