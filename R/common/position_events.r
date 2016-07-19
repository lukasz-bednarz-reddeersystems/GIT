source("../lib/mapper.r")
#Specialisation of the mapper base class to deal with position events,
#i.e. signals firing, events or dealer/PM annotations

setClass(
	Class     = "SignalObject",
	contains  = "VirtualMapper"
)

setMethod("mapsTo","SignalObject",
	function(object,signal_class){
		return(signal_class%in%names(object@maps_to))
	}
)

