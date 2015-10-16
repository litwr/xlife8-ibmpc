BEGIN {
   c = sprintf("%c", 27) ""
   red = c "[31m"
   green = c "[32m"
   yellow = c "[33m"
   blue = c "[34m"
   purple = c "[35m"
   cyan = c "[36m"
   white = c "[37m"
   bold = c "[1m"
   normal = c "[0m"
   r = "\r"
}
{
    b = ""
    e = $0
    while (p = index(e, "\\")) {
      m = substr(e, p + 1, 1)
      b = b substr(e, 1, p - 1)
      if (m == "x") {
         b = b yellow substr(e, p + 2, 1) green
         e = substr(e, p + 3)
      }
      else if (m == "g") {
         b = b green
         e = substr(e, p + 2)
      }
      else if (m == "b") {
         b = b white
         e = substr(e, p + 2)
      }
      else if (m == "p") {
         b = b purple
         e = substr(e, p + 2)
      }
      else if (m == "r") {
         b = b c
         e = substr(e, p + 2)
      }
   }
   printf "%s", b e
   gsub("\\\\.","")
   if (length($0) < 80) print r
}

