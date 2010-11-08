#!/usr/bin/env tclsh

proc main {argv} {
    if {![checkCommandLine $argv]} {
	error "A"
    }

    array set clData [parseClData $argv]

    set fnHandle [open $clData(FnFile) r]
    set fnText [read $fnHandle]
    close $fnHandle

    if {![checkFnData $fnText]} {
	error "A"
    }

    array set fnData [parseFnData $fnText]

    if {![postCheckFnData [array get fnData]]} {
	error "A"
    }

    foreach fnName $fnData(FnNames) {
	array set fnData [getFunctionMinMax $fnName [array get clData] [array get fnData]]
	set integralValue [integrateFunction $fnName [array get clData] [array get fnData]]

	puts "Integral for $fnName: $integralValue"
    }
}

proc getFunctionMinMax {fnName arClData arFnData} {
    array set clData $arClData
    array set fnData $arFnData

    set position [list]
    set begs [list]
    set ends [list]
    set done 0

    set min +inf
    set max -inf

    foreach rangeName $fnData(Fns,$fnName,RangesNames) {
	lappend position $fnData(Fns,$fnName,Ranges,$rangeName,Beg)
	lappend begs $fnData(Fns,$fnName,Ranges,$rangeName,Beg)
	lappend ends $fnData(Fns,$fnName,Ranges,$rangeName,End)
    }

    set minMax [_recMinMax position $fnName $arClData $arFnData $begs $ends 0]

    set fnData(Fns,$fnName,Min) [lindex $minMax 0]
    set fnData(Fns,$fnName,Max) [lindex $minMax 1]

    return [array get fnData]
}

proc _recMinMax {positionName fnName arClData arFnData begs ends index} {
    upvar 1 $positionName position

    array set clData $arClData
    array set fnData $arFnData

    set min +inf
    set max -inf
    
    if {$index == ([llength $position] - 1)} {
	set function $fnData(Fns,$fnName,Body)

	array set rangesBeg {}
	array set rangesEnd {}
	
	foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	    set function [regsub -all $paramName $function "\$__$paramName"]
	}

	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	    set function [regsub -all $rangeName $function "\$__$rangeName"]
	}

	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	    set rangesBeg($rangeName) $fnData(Fns,$fnName,Ranges,$rangeName,Beg)
	    set rangesEnd($rangeName) $fnData(Fns,$fnName,Ranges,$rangeName,End)
	}
    }

    while {[lindex $position $index] <= [lindex $ends $index]} {
	if {$index == ([llength $position] - 1)} {
	    for {set i 0} {$i < [llength $position]} {incr i} {
		set __[lindex $fnData(Fns,$fnName,RangesNames) $i] [lindex $position $i]
	    }

	    set functionValue [eval expr $function]
	    set minMax [list $functionValue $functionValue]
	} else {
	    for {set i [expr $index + 1]} {$i < [llength $position]} {incr i} {
		lset position $i [lindex $begs $i]
	    }

	    set minMax [_recMinMax $positionName $fnName $arClData $arFnData $begs $ends [expr $index + 1]]
	}

	if {[lindex $minMax 0] < $min} {
	    set min [lindex $minMax 0]
	}

	if {[lindex $minMax 1] > $max} {
	    set max [lindex $minMax 1]
	}

	lset position $index [expr [lindex $position $index] + $clData(MinMaxStep)]
    }

    return [list $min $max]
}

proc integrateFunction {fnName arClData arFnData} {
    array set clData $arClData
    array set fnData $arFnData

    set function $fnData(Fns,$fnName,Body)
    array set rangesBeg {}
    array set rangesEnd {}

    set min $fnData(Fns,$fnName,Min)
    set max $fnData(Fns,$fnName,Max)
    set mcPointsAbove0 0
    set mcPointsBelow0 0
    set area [expr 1.0 * ($max - $min)]

    foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	set function [regsub -all $paramName $function "\$__$paramName"]
    }

    foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	set function [regsub -all $rangeName $function "\$__$rangeName"]
    }

    foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	set rangesBeg($rangeName) $fnData(Fns,$fnName,Ranges,$rangeName,Beg)
	set rangesEnd($rangeName) $fnData(Fns,$fnName,Ranges,$rangeName,End)

	set area [expr $area * ($rangesEnd($rangeName) - $rangesBeg($rangeName))]
    }

    puts $function

    for {set i 0} {$i < $clData(SampleSetSize)} {incr i} {
    	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
    	    set __$rangeName [randInRange $rangesBeg($rangeName) $rangesEnd($rangeName)]
    	}

    	set monteCarloValue [expr rand() * ($max - $min) + $min]
    	set functionValue [eval expr $function]

	if {$functionValue >= 0 && $monteCarloValue <= $functionValue} {
	    incr mcPointsAbove0
	}

	if {$functionValue < 0 && $monteCarloValue >= $functionValue} {
	    incr mcPointsBelow0
	}
    }

    return [expr $area * ($mcPointsAbove0 - $mcPointsBelow0) / $clData(SampleSetSize)]
}

proc checkCommandLine {argv} {
    return 1
}


proc checkFnData {fnText} {
    return 1
}

proc postCheckFnData {arFnData} {
    return 1
}

proc parseClData {argv} {
    array set clData {}

    set clData(FnFile) fn.mci
    set clData(MinMaxStep) 0.1
    set clData(SampleSetSize) 200000

    return [array get clData]
}

proc parseFnData {fnText} {
    # The "fnData" array holds the application configuration as scanned from
    # the command line and the .fn file. We initialize it to {} and preset
    # the "FnNames" key to the empty list. "FnNames" holds the names of all
    # defined functions.

    array set fnData {}

    set fnData(FnNames) {}

    # Start the parsing. Notice the lack of error handling. Malformed input
    # should have been signaled by "checkFnInputFormat" and whatever we're
    # working on in "fnText" is syntactically valid.

    foreach {_ fnName fnBody} $fnText {
	lappend fnData(FnNames) $fnName
	
	set fnData(Fns,$fnName,ParamsNames) [list]
	set fnData(Fns,$fnName,RangesNames) [list]
	set fnData(Fns,$fnName,Body) {}

	# Extract section information.

	set paramsIndex [lsearch $fnBody Params]
	set paramsBody [lindex $fnBody [expr $paramsIndex + 1]]

	set rangesIndex [lsearch $fnBody Ranges]
	set rangesBody [lindex $fnBody [expr $rangesIndex + 1]]

	set ruleIndex [lsearch $fnBody Rule]
	set ruleBody [lindex $fnBody [expr $ruleIndex + 1]]

	# Extract function parameter information.

	foreach {paramName paramBody} $paramsBody {
	    switch [lindex $paramBody 0] {
		Real {
		    lappend fnData(Fns,$fnName,ParamsNames) $paramName

		    set fnData(Fns,$fnName,Params,$paramName,Type) Real
		    set fnData(Fns,$fnName,Params,$paramName,Beg) [lindex $paramBody 1]
		    set fnData(Fns,$fnName,Params,$paramName,End) [lindex $paramBody 2]
		}

		default {
		    error "A"
		}
	    }
	}

	# Extract function range information.

	foreach {rangeName rangeBody} $rangesBody {
	    switch [lindex $rangeBody 0] {
		Real {
		    lappend fnData(Fns,$fnName,RangesNames) $rangeName

		    set fnData(Fns,$fnName,Ranges,$rangeName,Type) Real
		    set fnData(Fns,$fnName,Ranges,$rangeName,Beg) [lindex $rangeBody 1]
		    set fnData(Fns,$fnName,Ranges,$rangeName,End) [lindex $rangeBody 2]
		}

		default {
		    error "A"
		}
	    }
	}

	# Extract function rule information.

	set fnData(Fns,$fnName,Body) [join [split [string trim $ruleBody]] { }]
    }

    return [array get fnData]
}

proc printFnData {arFnData} {
    array set fnData $arFnData

    foreach {fnName} $fnData(FnNames) {
	puts "$fnName:"

	puts "  Params:"

	foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	    puts "    $paramName:"
	    puts "      Type: $fnData(Fns,$fnName,Params,$paramName,Type)"
	    puts "      Beg: $fnData(Fns,$fnName,Params,$paramName,Beg)"
	    puts "      Beg: $fnData(Fns,$fnName,Params,$paramName,End)"
	}

	puts "  Ranges:"

	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	    puts "    $rangeName:"
	    puts "      Type: $fnData(Fns,$fnName,Ranges,$rangeName,Type)"
	    puts "      Beg: $fnData(Fns,$fnName,Ranges,$rangeName,Beg)"
	    puts "      Beg: $fnData(Fns,$fnName,Ranges,$rangeName,End)"
	}
	
	puts "  Args: $fnData(Fns,$fnName,Args)"
	puts "  Body: $fnData(Fns,$fnName,Body)"
    }
}

proc randInRange {beg end} {
    return [expr rand() * ($end - $beg) + $beg]
}

main $argv
