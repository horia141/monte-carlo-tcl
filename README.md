# monte-carlo-tcl
A Monte Carlo integration utility, written in Tcl.

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