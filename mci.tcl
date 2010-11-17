#!/usr/bin/env tclsh

#
# A list of command line option names. This is used in "checkFnData" to ensure
# no function parameter is defined with the same name as one of mci's own
# option names. Since parameters are passed as command line options we have
# no way to separate, for example, a parameter "N" from the "N" option, which
# tells mci how many samplings of the function space to use when computing
# the integral. When adding or removing options this list should be updated.
#
set CommandLineOptionNames {f N step help info}

#
# The message displayed when a user invokes mci with the "info" option. This
# represents a small tutorial on the use of mci. When changing the visible
# usege of mci (adding or removing options, for example), this should be 
# updated.
#
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
Carlo Integration. The target function, its domain and external parameters
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

#
# The message displayed when a user invokes mci with the "help" option. This
# just presents some tipical usecases for the application and explains the
# meaning of options. When changing the visible usage of mci (adding or
# removing options, for example), this should be updated.
#
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

#
# Application entry point.
#
# Context:
#   This function is called when Tcl finishes evaluating the source file.
#   It basically ties together the components making up the application.
#   It handles parsing the command line, reading and parsing the function
#   definition file and computing the definite integral.
# Arguments:
#   argv: a list representation of the command line.
# Return:
#   Nothing.
# Effect:
#   The summed effects of all the functions in the application. Whatever the
#   application does, "main" does as well. Currently, this includes reading
#   a file and sending messages to standard output.
#
proc main {argv} {
    # Check and parse the command line into "clData".

    set clErrors [checkCommandLine $argv]

    if {[llength $clErrors] != 0} {
	printErrors $clErrors
	exit 1
    }

    array set clData [parseClData $argv]

    # Read the contents of the function definition file into "fnText".

    set readResult [catch {
	set fnHandle [open $clData(FnFile) r]
	set fnText [read $fnHandle]
	close $fnHandle
    } readErrors]

    if {$readResult != 0} {
	puts $readErrors
	exit 1
    }

    # Check and parse "fnText" into "fnData".

    set fnErrors [checkFnData $fnText]

    if {[llength $fnErrors] != 0} {
	printErrors $fnErrors
	exit 1
    }

    array set fnData [parseFnData $fnText]

    # Do a post-check for inconsitencies between "clData" and "fnData".

    set postCheckErrors [postCheckFnData [array get clData] [array get fnData]]

    if {[llength $postCheckErrors] != 0} {
	printErrors $postCheckErrors
	exit 1
    }

    foreach fnName $fnData(FnNames) {
	# Determine the minimum and maximum of each function (a piece of
	# crucial information for the next step) and compute the definite
	# integral of the current function.

	array set fnData [getFunctionMinMax $fnName [array get clData] [array get fnData]]
	set integralValue [integrateFunction $fnName [array get clData] [array get fnData]]

	puts "Integral for $fnName: $integralValue"
    }
}

#
# Get the minimum and maximum value of a function.
#
# Context:
#   This function is used in "main" after processing has begun, to determine
#   the minimum and maximum value of a function. These are important values
#   in the integration process and are usually hard to determine. To find the
#   maximum we use a naive aproach: sample the hypervolume defined by the
#   ranges of a function with a delta-hypervolume of side determined by the
#   "step"  option and produce a multidimensional array of points. Search this
#   array for the smallest and largest values and return in the "fnData" array,
#   by updating the "Min" and "Max" keys for the selected function. Sampling
#   and building the array are conceptual steps. We just walk in each dimension
#   with a user defined step and remember the current best at each step.
# Arguments:
#   fnName: the name of a function. This must be present in the "fnData" array.
#   arClData: the "array get" representation of "clData".
#   arFnData: the "array get" representation of "fnData".
# Return:
#   The "array get" representation of "fnData", updated with the proper "Min"
#   and "Max" keys.
# Effect:
#   None.
#
proc getFunctionMinMax {fnName arClData arFnData} {
    array set clData $arClData
    array set fnData $arFnData

    # The current minimum and maximum, initialized to easy to beat values.

    set min +inf
    set max -inf

    # These are used to cache the start and end value for each range's
    # interval. Makes is easier to work with, down the line.

    set begs [list]
    set ends [list]

    foreach rangeName $fnData(Fns,$fnName,RangesNames) {
	lappend begs $fnData(Fns,$fnName,Ranges,$rangeName,Beg)
	lappend ends $fnData(Fns,$fnName,Ranges,$rangeName,End)
    }

    # This holds the current position in the range hypervolume. Its initial
    # value is the first point in the range hypervolume.

    set position $begs

    # Find the minimum and maximum. This function basically just does a setup
    # for "_recMinMax".

    set minMax [_recMinMax position $fnName $arClData $arFnData $begs $ends 0]

    # Update the "fnData" array.

    set fnData(Fns,$fnName,Min) [lindex $minMax 0]
    set fnData(Fns,$fnName,Max) [lindex $minMax 1]

    return [array get fnData]
}

#
# Do the actual work of determining the minimum and maximum of a function.
#
# Context:
#   This function is used by "getFunctionMinMax" to actually find the minimum
#   and maximum of a certain function. This function will be invoked
#   recursivley and each function will work on the "position" list passed
#   by name from the caller. What this function does is pretty simple. In the
#   one dimensional case (one range defined), we sample over the range interval
#   with a step defined by the "step" option and return the minimum and maximum
#   found. In an higher dimensional case (two or more ranges defined), we
#   sample the range invterval indentified by "index" and for each step find
#   the minimum and maximum by varying all the ranges from "index+1" to the
#   end.
# Arguments:
#   positionName: the name of the position list in the caller's context.
#   fnName: the name of a function. This must be present in the "fnData" array.
#   arClData: the "array get" representation of "clData".
#   arFnData: the "array get" representation of "fnData".
#   begs: the list of interval start values for each range.
#   ends: the list of interval end values for each range.
#   index: the current range to sample.
# Return:
#   A list of two elements: the minimum and the maximum found.
# Effect:
#   Update the variable identified by "positionName" in the caller's context.
#
proc _recMinMax {positionName fnName arClData arFnData begs ends index} {
    upvar 1 $positionName position

    array set clData $arClData
    array set fnData $arFnData

    # The current minimum and maximum, initialized to easy to beat values.

    set min +inf
    set max -inf

    # Cache the compiled form of the rule expression for this function.

    set function $fnData(Fns,$fnName,Body)

    # Here we sample each point from the current range (indentified by "index")
    # walking with a step of "step".

    while {[lindex $position $index] <= [lindex $ends $index]} {
	if {$index == ([llength $position] - 1)} {
	    # When we've reached the final range/dimension, we evaluate the
	    # rule expression.

	    # Setup all parameters.

	    foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
		set __$paramName $clData(Params,$paramName)
	    }

	    # Setup all range values to the values in "position".

	    for {set i 0} {$i < [llength $position]} {incr i} {
		set __[lindex $fnData(Fns,$fnName,RangesNames) $i] [lindex $position $i]
	    }

	    # Do the evaluation and set the minimum and maximum as the values
	    # produced by "eval".

	    set functionValue [eval expr $fnData(Fns,$fnName,Body)]
	    set minMax [list $functionValue $functionValue]
	} else {
	    # If this isn't the final range/dimension, reset all the position
	    # values following this one and call "_recMinMax" for the rest of
	    # the range elements. Running this process from the first to the
	    # last range will yield a complete sampling of the function input
	    # space and an sufficiently good minimum and maximum.

	    for {set i [expr $index + 1]} {$i < [llength $position]} {incr i} {
		lset position $i [lindex $begs $i]
	    }

	    set minMax [_recMinMax $positionName $fnName $arClData $arFnData $begs $ends [expr $index + 1]]
	}

	# Filter the minima and maxima.

	if {[lindex $minMax 0] < $min} {
	    set min [lindex $minMax 0]
	}

	if {[lindex $minMax 1] > $max} {
	    set max [lindex $minMax 1]
	}

	# Next step.

	lset position $index [expr [lindex $position $index] + $clData(MinMaxStep)]
    }

    return [list $min $max]
}

#
# Integrate a function in the defined range.
#
# Context:
#   This function does the actual integration. It is called by "main" after
#   the minimum and maximum have been computed. In a sense, this function does
#   the core of the application's work, while the rest just massages the data
#   into a proper format and extracts some information from the it.
#   Integration is done by the Monte Carlo Method. This method works by
#   placing a user controllable number of points randomly into the input
#   hypervolume and at different output domain positions (therefore, we place
#   them in the n+1-dimensional hypervolume) and seeing how many of them are
#   in the volume determined by the function's surface. This ratio leads us
#   to the final value of the definite integral.
# Arguments:
#   fnName: the name of a function. This must be present in the "fnData" array.
#   arClData: the "array get" representation of "clData".
#   arFnData: the "array get" representation of "fnData".
# Return:
#   The value of the definite integral in the defined range.
# Effect:
#   None.
#
proc integrateFunction {fnName arClData arFnData} {
    array set clData $arClData
    array set fnData $arFnData

    array set rangesBeg {}
    array set rangesEnd {}

    # Cache the minimum and maximum of the function in "min" and "max".

    set min $fnData(Fns,$fnName,Min)
    set max $fnData(Fns,$fnName,Max)
    
    # The number of randomly selected points situated below the curve defined
    # by the rule expression and above 0.

    set mcPointsAbove0 0

    # The number of randomly selected points situated above the curve defined
    # by the rule expression and below 0.

    set mcPointsBelow0 0

    # This is the "area" defined by the function ranges. In the one dimensional
    # case it is truly an area, while in later cases is is a hypervolume (in
    # the quantitative sense of the word). It's computed as the product of
    # the lengths of all the range intervals and the length of the image 
    # interval in R. This value will be used to compute the definite integral.

    set area [expr 1.0 * ($max - $min)]

    foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	set rangesBeg($rangeName) $fnData(Fns,$fnName,Ranges,$rangeName,Beg)
	set rangesEnd($rangeName) $fnData(Fns,$fnName,Ranges,$rangeName,End)

	set area [expr $area * ($rangesEnd($rangeName) - $rangesBeg($rangeName))]
    }

    # Preset all parameters. We need to set them once and use them for every
    # generated sample.

    foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	set __$paramName $clData(Params,$paramName)
    }

    # We have to generate "SampleSetSize" points randomly in the hypervolume
    # determined by the function range interals and the image interval of
    # the function. We'll compare this point with the value of the function
    # at the same position in the range hypervolume.

    for {set i 0} {$i < $clData(SampleSetSize)} {incr i} {
	# Random generation of range parameters.

    	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
    	    set __$rangeName [randInRange $rangesBeg($rangeName) $rangesEnd($rangeName)]
    	}

	# Here we generate the last coordinate of the sample point. We also
	# generate the function value at the sample point.

    	set monteCarloValue [randInRange $min $max]
    	set functionValue [eval expr $fnData(Fns,$fnName,Body)]

	if {$functionValue >= 0 && $monteCarloValue <= $functionValue} {
	    # If the value is above the 0 plane and below the function surface
	    # we count it as an integral defining point.

	    incr mcPointsAbove0
	}

	if {$functionValue < 0 && $monteCarloValue >= $functionValue} {
	    # Also, if the value is below the 0 plane and above the function
	    # surface, we count count it as an integral defining point.

	    incr mcPointsBelow0
	}
    }

    # The ratio between the number of integral defining points and the total
    # number of sample points gives us a measure of how much the hypervolume
    # generated by the function on the range hypervolume plus the image
    # dimension fills the whole hypervolume. Multipling this by the computed
    # "area" gives us the approximate value of the definite integral.

    return [expr $area * ($mcPointsAbove0 - $mcPointsBelow0) / $clData(SampleSetSize)]
}

#
# Parse a list representation of a command line and build a "clData" array.
#
# Context:
#   This function parses a list representation of a command line, as obtained
#   from Tcl and produces an "array get" representation of "clData". No other
#   function writes into "clData" so this is the "reference" point for its
#   format. This function assumes it's been called after "checkCommandLine"
#   and that whatever is in "argv" is valid.
# Arguments:
#   argv: a list representation of the application's command line, as obtained
#     from Tcl.
# Return:
#   An "array get" representation of "clData".
# Effect:
#   None.
#
proc parseClData {argv} {
    # Build the "clData" array with initial values. In case no suitable options
    # are provided in the command line, some sensible defaults are used.

    array set clData {}

    set clData(FnFile) fn.mci
    set clData(MinMaxStep) 0.1
    set clData(SampleSetSize) 10000
    set clData(ParamsNames) [list]

    # Knowing that "argv" is a list of items in the usual option format (an
    # option name as a C-style token starting with "-" and an option value as
    # a non-whitespace containing string) we can do a simple job of parsing.
    # We also know that the "info" and "help" option do not appear. Every
    # option that is not "f", "N" or "step" is treated as defining the value
    # for a function parameter and we build the "ParamsNames"/"Params" keys
    # accordingly.

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

#
# Parse a list representation of the function definition input file contents
# and build a "fnData" array.
#
# Context:
#   This function parses a list representation of the function definition
#   input file contents, as obtained by reading the file specified in the
#   "clData" array. Nobody writes into "fnData" after this function so this
#   is the reference for it. The function assumes it's been called after
#   "checkFnData" and whatever is in "fnText" is valid. Also, it makes the
#   assumption, that "fnText" can be interpreted as a Tcl list, although,
#   strictly speaking, it is just a string read from a file.
# Arguments:
#   fnText: a list representation of the function definition file contents.
# Return:
#   An "array get" representation of "fnData".
# Effect:
#   None.
#
proc parseFnData {fnText} {
    # Build the "fnData" array with initial values. We should be able to
    # complete this later with more information from the function definition
    # file, but some sensible defaults are provided nonetheless. The empty
    # "FnNames" key will trigger no actual processing (all subsequent steps
    # walk the "FnNames" list and do an operation on the associated content.
    # Lacking any value, no operations are performed, so we should be safe
    # from side effects).

    array set fnData {}

    set fnData(FnNames) [list]

    # Start the parsing. Notice the lack of error handling.

    foreach {_ fnName fnBody} $fnText {
	# Add sensible defaults for each function specific key. These will
	# be overwritten later and are initialized here for documentation
	# purposes.

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

	# Extract function parameter information. Each parameter type has it's
	# own way of interpreting it's body. We only have the "Real" type now,
	# and it's pretty straight forward to extract.

	foreach {paramName paramBody} $paramsBody {
	    switch -- [lindex $paramBody 0] {
		Real {
		    # Add another parameter in "Fns,$fnName,ParamNames" and 
		    # set its leaf values.

		    lappend fnData(Fns,$fnName,ParamsNames) $paramName

		    set fnData(Fns,$fnName,Params,$paramName,Type) Real
		    set fnData(Fns,$fnName,Params,$paramName,Beg) [lindex $paramBody 1]
		    set fnData(Fns,$fnName,Params,$paramName,End) [lindex $paramBody 2]
		}

		default {
		    # We shouldn't ever get here. An unrecognized parameter
		    # type should have been flagged by "checkFnData".

		    error "Unsupported execution path! Should not have arrived here"
		}
	    }
	}

	# Extract function range information. Each range type has it's own way
	# of interpreting it's body. We only have the "Real" type now, and
	# it's pretty straight forward to extract.

	foreach {rangeName rangeBody} $rangesBody {
	    switch -- [lindex $rangeBody 0] {
		Real {
		    # Add another range in "Fns,$fnName,RangeNames" and 
		    # set its leaf values.

		    lappend fnData(Fns,$fnName,RangesNames) $rangeName

		    set fnData(Fns,$fnName,Ranges,$rangeName,Type) Real
		    set fnData(Fns,$fnName,Ranges,$rangeName,Beg) [lindex $rangeBody 1]
		    set fnData(Fns,$fnName,Ranges,$rangeName,End) [lindex $rangeBody 2]
		}

		default {
		    # We shouldn't ever get here. An unrecognized range type
		    # should have been flagged by "checkFnData".

		    error "Unsupported execution path! Should not have arrived here"
		}
	    }
	}

	# Extract function rule information. We also "compile" the expression
	# and transform every variable reference to names which won't cause
        # problems when using Tcl's "eval" to evaluate the expression.

	set fnData(Fns,$fnName,Rule) [join [split [string trim $ruleBody]] { }]
	set fnData(Fns,$fnName,Body) $fnData(Fns,$fnName,Rule)

	# Replace parameter names from "$[name]" to "$__[name]"

	foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	    set fnData(Fns,$fnName,Body) [regsub -all "\\\$$paramName" $fnData(Fns,$fnName,Body) "\$__$paramName"]
	}

	# Replace range names from "$[name]" to "$__[name]"

	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	    set fnData(Fns,$fnName,Body) [regsub -all "\\\$$rangeName" $fnData(Fns,$fnName,Body) "\$__$rangeName"]
	}
    }

    return [array get fnData]
}

#
# Check the supplied command line for errors.
#
# Context:
#   This function is called by "main" before "parseCommandLine". Its job is to
#   make ensure the command line is valid so that "parseCommandLine" doesn't
#   have to worry about it (and, as a consequence, can do a clean job of
#   parsing). The function, as all other "check*" functions, returns a list of
#   error messages. If this list is empty, "main" will continue with
#   application execution. Otherwise, it will print the errors with
#   "printErrors" and exit. Mci's command line is very simple: a list of
#   option names and their associated values. No flags or arguments are
#   allowed. Sole exceptions are the "info" and "help" options, which, when
#   encountered in a command line, trigger the printing of the info and help
#   messages, accordingly. To make processing easier, the appearance of one
#   of these two options is handled as an error. The error message provided
#   is "InfoMessage" for the "info" option and "HelpMessage" for the "help"
#   option. This is techincally a HACK, but because we don't have to add
#   extra processing steps for this situation in "main", it ends up saving us
#   some headaches. The rest of the tests check that we have a proper number
#   of items in "argv" (a multiple of two), and that the odd items are proper
#   option names.
# Arguments:
#   argv: the list representation of the command line, as obtained from Tcl.
# Return:
#   A list of error messages.
# Effect:
#   None.
proc checkCommandLine {argv} {
    global InfoMessage
    global HelpMessage

    # This will hold all gathered error messages. We'll try to add as many as
    # we can into this, so a user can do multiple fixes at once.

    set clErrors [list]

    # Scan for the "info" and "help" options. These are exclusive with normal
    # operation so other checks are not done. Notice that if both "info" and
    # "help" are present, "info" has precedence.

    if {[lsearch $argv {-info}] != -1} {
	lappend clErrors $InfoMessage
    } elseif {[lsearch $argv {-help}] != -1} {
	lappend clErrors $HelpMessage
    } else {
	# If neither "info" nor "help" were present, we perform normal checks.

	# This is a crude test to see if we have enough items to form "option
	# name" / "option value" pairs. For example "-hello 123 -v x" will
	# pass, while "-hello -v x" will not.

	if {[llength $argv] % 2 != 0} {
	    lappend clErrors "Invalid number of options for command line : [join $argv]"
	}

	foreach {optName optValue} $argv {
	    # An option name's representation in "argv" must have a certain
	    # form to be valid. It must start with a "-" and must contain only
	    # alpha-numerical characters and "_". Also, the first character
	    # cannot be a digit.

	    if {![regexp {^-[a-zA-Z_][a-zA-Z0-9_]*} $optName]} {
		lappend clErrors "Invalid format for option name : $optName"
	    }
	}
    }

    return $clErrors
}

#
# Check the contents of the function definition file for errors.
#
# Context:
#   This function is called from "main" before "parseFnData". Its job is to
#   make ensure the contents of the function definition file are valid so that
#   "parseFnData" doesn't have to worry about it (and, as a consequence, can do
#   a clean job of parsing). The function, as all other "check*" functions,
#   returns a list of error messages. If this list is empty, "main" will
#   continue with application execution. Otherwise, it will print the errors
#   with "printErrors" and exit. The tests check that the high-level format
#   is correct, that all sections in a function definition are present and
#   that the descriptor for the single allowed range and parameter type (Real)
#   is properly written. Finally, a test is done to see if the rule expression
#   for all functions is a valid Tcl "expr" expression.
# Arguments:
#   fnText: the contents of a function definition file.
# Return:
#   A list of error messages.
# Effect:
#   None.
#
proc checkFnData {fnText} {
    global CommandLineOptionNames

    # This will hold all gathered error messages. We'll try to add as many as
    # we can into this, so a user can do multiple fixes at once.

    set fnErrors [list]

    # This is a crude test to see if we have enough items to form "function 
    # [name] [body]" triples. For example "function Q {...}" will pass, while
    # "function T" will not.

    if {[llength $fnText] % 3 != 0} {
	lappend fnErrors "Invalid format for input file!"
    }

    foreach {functionKeyword fnName fnBody} $fnText {
	# We scan each triple and check if the function it defines is allright.

	# Every function definition must start with the "function" keyword.

	if {![string equal $functionKeyword function]} {
	    lappend fnErrors "Invalid keyword \"$functionKeyword\"! Expecting \"function\" in:\n$functionKeyword [list $fnName] {$fnBody}"
	}

	# Find the "Params" section and extract the associated "body" : a list
	# of parameter descriptors.

	set paramsIndex [lsearch $fnBody Params]
	set paramsBody [lindex $fnBody [expr $paramsIndex + 1]]

	# Several mutually exclusive options follow. The simplest cases are
	# when no section is found or the section is not an even entry in the
	# function descriptor "body". The last case usually means an incomplete
	# section somewhere or some other syntax error.

	if {$paramsIndex == -1} {
	    lappend fnErrors "Function \"$fnName\" does not contain a \"Params\" section!"
	} elseif {$paramsIndex % 2 != 0} {
	    lappend fnErrors "Invalid format for function \"$fnName\"! Misplaced \"Params\" section:\n$functionKeyword [list $fnName] {$fnBody}"
	} else {
	    # Further processing of the section body.

	    # Again, we need to make sure we have proper "[param name] [param
	    # body]" pairs.

	    if {[llength $paramsBody] % 2 != 0} {
		lappend fnErrors "Invalid format for \"Params\" section in function \"$fnName\": {\n        [string trim $paramsBody]\n}"
	    }

	    foreach {paramName paramBody} $paramsBody {
		# For each pair in the "Params" section "body", we do several
		# checks.

		# First, the defined name should not come in conflict with
		# one of mci's own command line option names. This is because
		# parameters are specified as options on the command line and
		# we have no way to resolve ambiguities.

		if {[lsearch $CommandLineOptionNames $paramName] != -1} {
		    lappend fnErrors "Invalid parameter name \"$paramName\" in function \"$fnName\"! Parameters must not be named: $CommandLineOptionNames"
		}

		# Each parameter type has it's own checks.

		switch -- [lindex $paramBody 0] {
		    Real {
			# The only supported type, Real, has, as a descriptor,
			# a triple of the form: "Real [interval beg] [interval
			# end]".

			# This checks that the parameter "body" is exactly 3.

			if {[llength $paramBody] != 3} {
			    lappend fnErrors "Invalid format for Real Param \"$paramName\" in function \"$fnName\": $paramBody!"
			} else {
			    # Here we test if "interval beg" and "interval end"
			    # (the second and third items in the parameter
			    # "body") are floating point numbers.

			    if {![string is double [lindex $paramBody 1]]} {
				lappend fnErrors "Invalid format for interval start in Real Param \"$paramName\" in function \"$fnName\": $paramBody!"
			    }

			    if {![string is double [lindex $paramBody 2]]} {
				lappend fnErrors "Invalid format for interval end in Real Param \"$paramName\" in function \"$fnName\": $paramBody!"
			    }

			    # A final integrity test checks if "interval beg"
			    # is smaller than "interval end".

			    if {[lindex $paramBody 1] >= [lindex $paramBody 2]} {
				lappend fnErrors "Real Param \"$paramName\" from function \"$fnName\" has an interval start value greater than the interval end!"
			    }
			}
		    }

		    default {
			# An unrecognized parameter type will be reported.

			lappend fnErrors "Params of type \"[lindex $paramBody 0]\" in function \"$fnName\" are unsupported: $paramBody!"
		    }
		}
	    }
	}

	# Find the "Ranges" section and extract the associated "body" : a list
	# of ranges descriptors.

	set rangesIndex [lsearch $fnBody Ranges]
	set rangesBody [lindex $fnBody [expr $rangesIndex + 1]]

	# Several mutually exclusive options follow. The simplest cases are
	# when no section is found or the section is not an even entry in the
	# function descriptor "body". The last case usually means an incomplete
	# section somewhere or some other syntax error.

	if {$rangesIndex == -1} {
	    lappend fnErrors "Function \"$fnName\" does not contain a \"Ranges\" section!"
	} elseif {$rangesIndex % 2 != 0} {
	    lappend fnErrors "Invalid format for function \"$fnName\"! Misplaced \"Ranges\" section:\n$functionKeyword [list $fnName] {$fnBody}"
	} else {
	    # Further processing of the section body.

	    # Again, we need to make sure we have proper "[range name] [range
	    # body]" pairs.

	    if {[llength $rangesBody] == 0} {
		lappend fnErrors "Function \"$fnName\" has an empty Ranges section!"
	    }

	    if {[llength $rangesBody] % 2 != 0} {
		lappend fnErrors "Invalid format for \"Ranges\" section in function \"$fnName\": {\n        [string trim $rangesBody]\n}"
	    }

	    foreach {rangeName rangeBody} $rangesBody {
		# For each pair in the "Ranges" section "body", we do several
		# checks.

		# Each range type has it's own checks.

		switch -- [lindex $rangeBody 0] {
		    Real {
			# The only supported type, Real, has, as a descriptor,
			# a triple of the form: "Real [interval beg] [interval
			# end]".

			# This checks that the range "body" is exactly 3.

			if {[llength $rangeBody] != 3} {
			    lappend fnErrors "Invalid format for Real Range \"$rangeName\" in function \"$fnName\": $rangeBody!"
			} else {
			    # Here we test if "interval beg" and "interval end"
			    # (the second and third items in the range "body") 
			    # are floating point numbers.

			    if {![string is double [lindex $rangeBody 1]]} {
				lappend fnErrors "Invalid format for interval start in Real Range \"$rangeName\" in function \"$fnName\": $rangeBody!"
			    }

			    if {![string is double [lindex $rangeBody 2]]} {
				lappend fnErrors "Invalid format for interval end in Real Range \"$rangeName\" in function \"$fnName\": $rangeBody!"
			    }

			    # A final integrity test checks if "interval beg"
			    # is smaller than "interval end".

			    if {[lindex $rangeBody 1] >= [lindex $rangeBody 2]} {
				lappend fnErrors "Real Range \"$rangeName\" from function \"$fnName\" has an interval start value greater than the interval end!"
			    }
			}
		    }

		    default {
			# An unrecognized parameter type will be reported.

			lappend fnErrors "Ranges of type \"[lindex $rangeBody 0]\" in function \"$fnName\" are unsupported: $rangeBody!"
		    }
		}
	    }
	}

	# Find the "Rule" section and extract the associated "body".

	set ruleIndex [lsearch $fnBody Rule]
	set ruleBody [lindex $fnBody [expr $ruleIndex + 1]]

	if {$ruleIndex == -1} {
	    lappend fnErrors "Function \"$fnName\" does not contain a \"Rule\" section!"
	}
    }

    return $fnErrors
}

#
# Do checks after both the command line and the function definition file have
# been read.
#
# Context:
#   This function is called from "main" before doing the actual integration.
#   Its job is to make sure the command line options and the data extracted
#   from the function definition file are compatible. The function, as all
#   other "check*" functions, returns a list of error messages. If this list
#   is empty, "main" will continue with application execution. Otherwise, it
#   will print the errors with "printErrors" and exit. The tests check that no
#   clashes occur between parameter and range names. Also, we check that all
#   parameters for all functions appear in the command line (sometimes, if
#   different functions have the same parameter, the single supplied value is
#   used for both of them, so this test works accordingly) and that no
#   unrequired parameters appear. Finally, we check that the rule expressions
#   for functions are valid by Tcl's standards.
# Arguments:
#   arClData: an "array get" representation of the "clData" array.
#   arFnData: an "array get" representation of the "fnData" array.
# Return:
#   A list of error messages.
# Effect:
#   None.
#
proc postCheckFnData {arClData arFnData} {
    array set clData $arClData
    array set fnData $arFnData

    # This will hold all gathered error messages. We'll try to add as many as
    # we can into this, so a user can do multiple fixes at once.

    set postCheckErrors [list]

    # A list of the parameters required by all functions. For our purposes,
    # we eliminate repeating elements and sort the whole list.

    set allParams [list]

    foreach {fnName} $fnData(FnNames) {
	set allParams [concat $allParams $fnData(Fns,$fnName,ParamsNames)]
    }

    set allParams [lsort -unique $allParams]

    # A list of the parameters encountered in the command line. For our
    # purposes, we sort this list.

    set clParams [lsort $clData(ParamsNames)]

    # Check that no conflicts appear between range names and parameter names.

    foreach {fnName} $fnData(FnNames) {
	foreach {rangeName} $fnData(Fns,$fnName,RangesNames) {
	    if {[lsearch $fnData(Fns,$fnName,ParamsNames) $rangeName] != -1} {
		lappend postCheckErrors "Function \"$fnName\" uses name \"$rangeName\" both as a range name and as a parameter name!"
	    }
	}
    }

    # Check that all parameters, for all functions, appear as command line
    # options.

    foreach {fnName} $fnData(FnNames) {
	foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	    if {[lsearch -sorted $clParams $paramName] == -1} {
		lappend postCheckErrors "Parameter \"$paramName\" required by function \"$fnName\" is not specified!"
	    }
	}
    }

    # Check that all command line supplied parameters are defined in at least
    # a function.

    foreach {paramName} $clParams {
	if {[lsearch -sorted $allParams $paramName] == -1} {
	    lappend postCheckErrors "Option \"-$paramName $clData(Params,$paramName)\" specified an unknown parameter!"
	}
    }

    # Check supplied values are of type real and between the specified limits.
    # We can only do this check after we've read the function definition file.

    foreach {fnName} $fnData(FnNames) {
	foreach {paramName} $fnData(Fns,$fnName,ParamsNames) {
	    # We test that a parameter has proper values, only if it appears
	    # in the command line. If not, the error should have been reported
	    # previously and there's nothing we can do here.

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
			# We shouldn't ever get here. An unrecognized parameter
			# type should have been flagged by "checkFnData".

			error "Unsupported execution path! Should not have arrived here"
		    }
		}
	    }
	}
    }

    # Check if the rule expression for each function is valid. We do this
    # by invoking Tcl's "eval" function with our "parsed" expression. The
    # values used are the ones at the beginning of our range hypervolume.

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

#
# Print a representation of the internal structure of the data in a function
# definition file.
#
# Context:
#   For debug purposes is is often necessary to print how the application
#   managed to parse whatever data was in a function file. This procedure
#   parses a "fnData" array, which holds information about each defined
#   function, and prints a YAML-like representation for it.
# Arguments:
#   arFnData: an "array get" representation of the "fnData" array.
# Return:
#   Nothing.
# Effect:
#   Send a number of messages to standard output. A number of calls are made
#   to "puts", depending on the structure of "fnData" (how many parameters and
#   ranges are defined).
#
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

#
# Produce a random number in a certain range.
#
# Context:
#   This function is used to produce random floating point numbers in a given
#   range. It's less painful to use than Tcl's "rand", which produces a random
#   number in the range 0 to 1.
# Arguments:
#   beg: the smalles value which could be generated.
#   end: the largest value which could be generated.
# Return:
#   A random floating point number, no smaller than "beg" and no larger than
#   "end".
# Effect:
#   None.
#
proc randInRange {beg end} {
    return [expr rand() * ($end - $beg) + $beg]
}

#
# Print a list of error messages to standard output.
#
# Context:
#   This function is used to print error messages in the format returned by
#   the "check*" family of functions. The messages are passed as a list and
#   "printErrors" prints them on "stdout" in order.
# Arguments:
#   errorList: a list of error messages (simple Tcl strings).
# Return:
#   Nothing.
# Effect:
#   Print a number of messages to standard output using "puts". Whatever effect
#   "[llength errorList]" calls to "puts" have, this function has as well.
#
proc printErrors {errorsList} {
    foreach {errorMessage} $errorsList {
	puts $errorMessage
    }
}

#
# This is where we invoke "main". By now, all instructions in this file have
# been evaluated and we can actually start doing some work.
#
main $argv
