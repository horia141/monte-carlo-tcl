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

    return [list]
}

proc integrateFunction {fnName arClData arFnData} {
    array set clData $arClData
    array set fnData $arFnData

    return 0.0
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
    set clData(MinMaxStep) 0.01
    set clData(SampleSetSize) 1000

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
	set fnData(Fns,$fnName,Args) {}
	set fnData(Fns,$fnName,Body) {}

	# Extract section information.

	set paramsIndex [lsearch $fnBody Params]
	set paramsBody [lindex $fnBody [expr $paramsIndex + 1]]

	set rangesIndex [lsearch $fnBody Ranges]
	set rangesBody [lindex $fnBody [expr $rangesIndex + 1]]

	set ruleIndex [lsearch $fnBody Rule]
	set ruleArgs [lindex $fnBody [expr $ruleIndex + 1]]
	set ruleBody [lindex $fnBody [expr $ruleIndex + 2]]

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

	set fnData(Fns,$fnName,Args) $ruleArgs
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

main $argv
