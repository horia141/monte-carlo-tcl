#!/usr/bin/env tclsh

set CommandLineArguments {f N step help info}

set InfoMessage {
Usage: mci.tcl -f [FILE] -N [SAMPLECOUNT] -step [MINMAXSTEP] [...PARAMETERS]
       mci.tcl -help : A help message.
       mci.tcl -info : This tutorial message.

-f               Path to function definitions file.
-N               Number of iterations used in integration.
-step            Input space granularity for MinMax detection.
-[name] [value]  A parameter from the "Params" section of a function. This
                 sets the parameter [name] to the value [value]. Used when
                 integrating and in MinMax.

Computes the definite integral of multi-dimensional functions using Monte
Carlo Integration. The target function, it's domain and external parameters
are defined in an .mci file specified as a path argument to the -f option.
To control the precision of the computation, use the -N option (with a number
of iterations as an argument: higher values correspond to more precise results,
but cause higher runtimes). An important part in the integration process is
finding out the minimum and maximum of the defined functions. These are hard
to compute by hand, so the application must determine them as well. The number
passed as an argument to the -step option determines the granularity with which
the input space is scanned for extrema points. Generally, values between
0 and 1 are good, with smaller values better from a precision point of view.
The same precision-runtime tradeoff encounterd for the -N option occurs here
as well. Finally, parameters defined in a function's "Params" section are
passed into the application as arguments to appropriately named command line
options. The option format employed is -[parameter name] [parameter value].

A parameter cannot be named "f","N","step","help" or "info". These are
reserved names and, when present as command line options, will change the
behaviour of the whole program. Valid characters are just the alphanumerics
and "_" in usual C identifier rules (a parameter name cannot start with a
digit). Also, the parameter's value must pass all the type and range
constraints in the definition file.
}

set HelpMessage {
Usage: mci.tcl -f [FILE] -N [SAMPLECOUNT] -step [MINMAXSTEP] [...PARAMETERS]
       mci.tcl -help : This help message.
       mci.tcl -info : A tutorial message.

-f               Path to function definitions file.
-N               Number of iterations used in integration.
-step            Input space granularity for MinMax detection.
-[name] [value]  A parameter from the "Params" section of a function. This
                 sets the parameter [name] to the value [value]. Used when
                 integrating and in MinMax. See "mci.tcl -info" for more.
}

proc main {argv} {
    set clErrors [checkCommandLine $argv]

    if {[llength $clErrors] != 0} {
	printErrors $clErrors
	exit 1
    }

    array set clData [parseClData $argv]

    set readResult [catch {
	set fnHandle [open $clData(FnFile) r]
	set fnText [read $fnHandle]
	close $fnHandle
    } readErrors]

    if {$readResult != 0} {
	puts $readErrors
	exit 1
    }

    set fnErrors [checkFnData $fnText]

    if {[llength $fnErrors] != 0} {
	printErrors $fnErrors
	exit 1
    }

    array set fnData [parseFnData $fnText]

    set postCheckErrors [postCheckFnData [array get clData] [array get fnData]]

    if {[llength $postCheckErrors] != 0} {
	printErrors $postCheckErrors
	exit 1
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
	
	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	    set rangesBeg($rangeName) $fnData(Fns,$fnName,Ranges,$rangeName,Beg)
	    set rangesEnd($rangeName) $fnData(Fns,$fnName,Ranges,$rangeName,End)
	}
    }

    while {[lindex $position $index] <= [lindex $ends $index]} {
	if {$index == ([llength $position] - 1)} {
	    foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
		set __$paramName $clData(Params,$paramName)
	    }

	    for {set i 0} {$i < [llength $position]} {incr i} {
		set __[lindex $fnData(Fns,$fnName,RangesNames) $i] [lindex $position $i]
	    }

	    set functionValue [eval expr $fnData(Fns,$fnName,Body)]
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

    array set rangesBeg {}
    array set rangesEnd {}

    set min $fnData(Fns,$fnName,Min)
    set max $fnData(Fns,$fnName,Max)
    set mcPointsAbove0 0
    set mcPointsBelow0 0
    set area [expr 1.0 * ($max - $min)]

    foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	set rangesBeg($rangeName) $fnData(Fns,$fnName,Ranges,$rangeName,Beg)
	set rangesEnd($rangeName) $fnData(Fns,$fnName,Ranges,$rangeName,End)

	set area [expr $area * ($rangesEnd($rangeName) - $rangesBeg($rangeName))]
    }

    for {set i 0} {$i < $clData(SampleSetSize)} {incr i} {
	foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	    set __$paramName $clData(Params,$paramName)
	}

    	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
    	    set __$rangeName [randInRange $rangesBeg($rangeName) $rangesEnd($rangeName)]
    	}

    	set monteCarloValue [expr rand() * ($max - $min) + $min]
    	set functionValue [eval expr $fnData(Fns,$fnName,Body)]

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
    global InfoMessage
    global HelpMessage

    set clErrors [list]

    if {[lsearch $argv {-info}] != -1} {
	lappend clErrors $InfoMessage
    } elseif {[lsearch $argv {-help}] != -1} {
	lappend clErrors $HelpMessage
    } else {
	if {[llength $argv] % 2 != 0} {
	    lappend clErrors "Invalid number of options for command line : [join $argv]"
	}

	foreach {optName optValue} $argv {
	    if {![string equal [string index $optName 0] {-}]} {
		lappend clErrors "Invalid format for option name : $optName"
	    }

	    if {![regexp {[a-zA-Z_][a-zA-Z0-9_]*} $optName]} {
		lappend clErrors "Invalid format for option name : $optName"
	    }
	}
    }

    return $clErrors
}


proc checkFnData {fnText} {
    global CommandLineArguments

    set fnErrors [list]

    if {[llength $fnText] % 3 != 0} {
	lappend fnErrors "Invalid format for input file!"
    }

    foreach {functionKeyword fnName fnBody} $fnText {
	if {![string equal $functionKeyword function]} {
	    lappend fnErrors "Invalid keyword \"$functionKeyword\"! Expecting \"function\" in:\n$functionKeyword [list $fnName] {$fnBody}"
	}

	set paramsIndex [lsearch $fnBody Params]
	set paramsBody [lindex $fnBody [expr $paramsIndex + 1]]

	if {$paramsIndex == -1} {
	    lappend fnErrors "Function \"$fnName\" does not contain a \"Params\" section!"
	} elseif {$paramsIndex % 2 != 0} {
	    lappend fnErrors "Invalid format for function \"$fnName\"! Misplaced \"Params\" section:\n$functionKeyword [list $fnName] {$fnBody}"
	} else {
	    if {[llength $paramsBody] % 2 != 0} {
		lappend fnErrors "Invalid format for \"Params\" section in function \"$fnName\": {\n        [string trim $paramsBody]\n}"
	    }

	    foreach {paramName paramBody} $paramsBody {
		if {[lsearch $CommandLineArguments $paramName] != -1} {
		    lappend fnErrors "Invalid parameter name \"$paramName\" in function \"$fnName\"! Parameters must not be named: $CommandLineArguments"
		}

		switch -- [lindex $paramBody 0] {
		    Real {
			if {[llength $paramBody] != 3} {
			    lappend fnErrors "Invalid format for Real Param \"$paramName\" in function \"$fnName\": $paramBody!"
			} else {
			    if {![string is double [lindex $paramBody 1]]} {
				lappend fnErrors "Invalid format for interval start in Real Param \"$paramName\" in function \"$fnName\": $paramBody!"
			    }

			    if {![string is double [lindex $paramBody 2]]} {
				lappend fnErrors "Invalid format for interval end in Real Param \"$paramName\" in function \"$fnName\": $paramBody!"
			    }

			    if {[lindex $paramBody 1] >= [lindex $paramBody 2]} {
				lappend fnErrors "Real Param \"$paramName\" from function \"$fnName\" has an interval start value greater than the interval end!"
			    }
			}
		    }

		    default {
			lappend fnErrors "Params of type \"[lindex $paramBody 0]\" in function \"$fnName\" are unsupported: $paramBody!"
		    }
		}
	    }
	}

	set rangesIndex [lsearch $fnBody Ranges]
	set rangesBody [lindex $fnBody [expr $rangesIndex + 1]]

	if {$rangesIndex == -1} {
	    lappend fnErrors "Function \"$fnName\" does not contain a \"Ranges\" section!"
	} elseif {$rangesIndex % 2 != 0} {
	    lappend fnErrors "Invalid format for function \"$fnName\"! Misplaced \"Ranges\" section:\n$functionKeyword [list $fnName] {$fnBody}"
	} else {
	    if {[llength $rangesBody] == 0} {
		lappend fnErrors "Function \"$fnName\" has an empty Ranges section!"
	    }

	    if {[llength $rangesBody] % 2 != 0} {
		lappend fnErrors "Invalid format for \"Ranges\" section in function \"$fnName\": {\n        [string trim $rangesBody]\n}"
	    }

	    foreach {rangeName rangeBody} $rangesBody {
		switch -- [lindex $rangeBody 0] {
		    Real {
			if {[llength $rangeBody] != 3} {
			    lappend fnErrors "Invalid format for Real Range \"$rangeName\" in function \"$fnName\": $rangeBody!"
			} else {
			    if {![string is double [lindex $rangeBody 1]]} {
				lappend fnErrors "Invalid format for interval start in Real Range \"$rangeName\" in function \"$fnName\": $rangeBody!"
			    }

			    if {![string is double [lindex $rangeBody 2]]} {
				lappend fnErrors "Invalid format for interval end in Real Range \"$rangeName\" in function \"$fnName\": $rangeBody!"
			    }

			    if {[lindex $rangeBody 1] >= [lindex $rangeBody 2]} {
				lappend fnErrors "Real Range \"$rangeName\" from function \"$fnName\" has an interval start value greater than the interval end!"
			    }
			}
		    }

		    default {
			lappend fnErrors "Ranges of type \"[lindex $rangeBody 0]\" in function \"$fnName\" are unsupported: $rangeBody!"
		    }
		}
	    }
	}

	set ruleIndex [lsearch $fnBody Rule]
	set ruleBody [lindex $fnBody [expr $ruleIndex + 1]]
    }

    return $fnErrors
}

proc postCheckFnData {arClData arFnData} {
    array set clData $arClData
    array set fnData $arFnData

    set postCheckErrors [list]

    set allParams [list]
    set clParams [list]

    foreach {fnName} $fnData(FnNames) {
	set allParams [concat $allParams $fnData(Fns,$fnName,ParamsNames)]
    }

    set allParams [lsort -unique $allParams]
    set clParams [lsort -unique $clData(ParamsNames)]

    # Check no conflicts between ranges and params appear.

    foreach {fnName} $fnData(FnNames) {
	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	    if {[lsearch $fnData(Fns,$fnName,ParamsNames) $rangeName] != -1} {
		lappend postCheckErrors "Function \"$fnName\" uses name \"$rangeName\" both as a range name and as a parameter name!"
	    }
	}
    }

    # Check all required parameters appear in command line.

    foreach {fnName} $fnData(FnNames) {
	foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	    if {[lsearch -sorted $clParams $paramName] == -1} {
		lappend postCheckErrors "Parameter \"$paramName\" required by function \"$fnName\" is not specified!"
	    }
	}
    }

    # Check all command line supplied parameters are required by a function.

    foreach {paramName} $clParams {
	if {[lsearch -sorted $allParams $paramName] == -1} {
	    lappend postCheckErrors "Option \"-$paramName $clData(Params,$paramName)\" specified an unknown parameter!"
	}
    }

    # Check supplied values are of type real and between the specified limits.

    foreach {fnName} $fnData(FnNames) {
	foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	    if {[lsearch -sorted $clParams $paramName] != -1} {
		switch -- $fnData(Fns,$fnName,Params,$paramName,Type) {
		    Real {
			if {![string is double -strict $clData(Params,$paramName)]} {
			    lappend postCheckErrors "Parameter \"$paramName\" for function \"$fnName\" does not look like a real number!"
			} elseif {$clData(Params,$paramName) < $fnData(Fns,$fnName,Params,$paramName,Beg)} {
			    lappend postCheckErrors "Parameter \"$paramName\" for function \"$fnName\" is smaller than the allowed minimum of \"$fnData(Fns,$fnName,Params,$paramName,Beg)\"!"
			} elseif {$clData(Params,$paramName) > $fnData(Fns,$fnName,Params,$paramName,End)} {
			    lappend postCheckErrors "Parameter \"$paramName\" for function \"$fnName\" is greater than the allowed maximum of \"$fnData(Fns,$fnName,Params,$paramName,End)\"!"
			}
		    }

		    default {
			error "Unsupported execution path! Should not have arrived here"
		    }
		}
	    }
	}
    }

    # Check function expressions are valid. Only do this test if all other
    # tests have passed (and, considering that this is the last action in
    # "postCheckFnData", by now, all other checks, be they in "checkCommandLine"
    # or "checkFnData", should have passed).

    if {[llength $postCheckErrors] == 0} {
	foreach {fnName} $fnData(FnNames) {
	    foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
		set __$paramName $clData(Params,$paramName)
	    }

	    foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
		set __$rangeName $fnData(Fns,$fnName,Ranges,$rangeName,Beg)
	    }

	    set evalResult [catch {eval expr $fnData(Fns,$fnName,Body)} errorResult]

	    if {$evalResult != 0} {
		lappend postCheckErrors "Invalid rule for function \"$fnName\"!\nReason: $errorResult"
	    }
	}
    }

    return $postCheckErrors
}

proc parseClData {argv} {
    array set clData {}
    
    set clData(FnFile) fn.mci
    set clData(MinMaxStep) 0.1
    set clData(SampleSetSize) 10000
    set clData(ParamsNames) [list]

    foreach {optName optValue} $argv {
	switch -- [string range $optName 1 end] {
	    f {set clData(FnFile) $optValue}
	    N {set clData(SampleSetSize) $optValue}
	    step {set clData(MinMaxStep) $optValue}
	    default {
		set paramName [string range $optName 1 end]
		lappend clData(ParamsNames) $paramName
		set clData(Params,$paramName) $optValue
	    }
	}
    }

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
	    switch -- [lindex $paramBody 0] {
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
	    switch -- [lindex $rangeBody 0] {
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

	set fnData(Fns,$fnName,Rule) [join [split [string trim $ruleBody]] { }]
	set fnData(Fns,$fnName,Body) $fnData(Fns,$fnName,Rule)

	foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	    set fnData(Fns,$fnName,Body) [regsub -all "\\\$$paramName" $fnData(Fns,$fnName,Body) "\$__$paramName"]
	}

	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	    set fnData(Fns,$fnName,Body) [regsub -all "\\\$$rangeName" $fnData(Fns,$fnName,Body) "\$__$rangeName"]
	}
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

	puts "  Rule: $fnData(Fns,$fnName,Rule)"
	puts "  Body: $fnData(Fns,$fnName,Body)"
    }
}

proc randInRange {beg end} {
    return [expr rand() * ($end - $beg) + $beg]
}

proc printErrors {errorsList} {
    foreach {errorMessage} $errorsList {
	puts $errorMessage
    }
}

main $argv
