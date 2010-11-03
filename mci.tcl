#!/usr/bin/env tclsh

puts "MonteCarlo Integration FTW"

set function {pow(2.71,-x) + 2}
set domain Real
set constraintsXBeg -1
set constraintsXEnd 1
set constraintsYBeg 0
set constraintsYEnd 4.72

set N 100000

set pointsInside 0
set evalFunction [regsub -all x $function {$x}]

for {set i 0} {$i < $N} {incr i} {
    set x [expr rand() * ($constraintsXEnd - $constraintsXBeg) + $constraintsXBeg]
    set y [expr rand() * ($constraintsYEnd - $constraintsYBeg) + $constraintsYBeg]

    set q [expr $evalFunction]

    if {$y <= $q} {
	incr pointsInside
	#puts [format {%.5f:%.5f%.5f inside} $x $y $q]
    } else {
	#puts [format {%.5f:%.5f%.5f} $x $y $q]
    }
}

set area [expr ($constraintsYEnd - $constraintsYBeg) * ($constraintsXEnd - $constraintsXBeg)]
set percentage [expr 1.0*$pointsInside / $N]

puts "Points inside the curve: $pointsInside"
puts "Integration perimeter: ($constraintsXBeg,$constraintsYBeg)x($constraintsXEnd,$constraintsYEnd)"
puts "Integration area: $area"
puts "Function area percentage: [format {%.3f %%} [expr $percentage*100]]"
puts "Integral: [expr $percentage * $area]"

