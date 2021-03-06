set style data dots
set pointsize 0.01
set key at screen 0.52, 0.98 spacing 0.8 maxcols 1 samplen 1 horizontal textcolor rgb variable 
set size 1.0, 1.0
set label " x-xp y-yp x-x z-zp                                                             " at screen 0.13 ,0.993
set label "  1000 particles" at screen 0.45,0.49
set multiplot
set size 0.5, 0.5
set origin 0.,0.5
set xlabel "x (cm)                                  "
set ylabel "xp (mrad)                               "
set xrange [   -1.00:    1.00]
set yrange [    -5.00000:     5.00000]
plot "dynac.plt" using 1:2 title "" with dots lc 0, \
     "dynac.cnt" using 1:2 title "" with lines
set nokey
set nolabel
set size 0.5, 0.5
set origin 0.5,0.5
set xlabel "y (cm)                                  "
set ylabel "yp (mrad)                               "
set xrange [   -1.00:    1.00]
set yrange [    -5.00000:     5.00000]
plot "dynac.plt" using 3:4 with dots lc 0, \
     "dynac.cnt" using 3:4 with lines
set size 0.5, 0.5
set origin 0.,0.
set xlabel "x (cm)                                  "
set ylabel "y (cm)                                  "
set xrange [   -1.00:    1.00]
set yrange [    -1.00000:     1.00000]
plot "dynac.plt" using 1:3 with dots lc 0
set size 0.5, 0.5
set origin 0.5,0.
set xlabel "z (deg)                                 "
set ylabel "zp (MeV)                                "
set xrange [-7000.00: 7000.00]
set yrange [   -10.00000:    10.00000]
plot "dynac.plt" using 5:6 with dots lc 0, \
     "dynac.cnt" using 5:6 with lines
set nomultiplot
pause -1 "hit return to continue"
