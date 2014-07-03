GD.data <- function (trackdata)
{A= trackdata[,1]; V=trackdata[,5];
 {for (i in min(A):max(A))
   while (A<=i) {GD = (sum (V)*(1/25))
                 return (GD(i))}}


 aggregate( V ~ A , data = t1 , sum , na.rm = TRUE )