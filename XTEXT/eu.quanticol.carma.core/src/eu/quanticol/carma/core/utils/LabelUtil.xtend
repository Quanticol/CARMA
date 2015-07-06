package eu.quanticol.carma.core.utils

import eu.quanticol.carma.core.carma.Model

class LabelUtil {
	
	def String name(Model model){
		model.eResource.URI.lastSegment.split("\\.").get(0)
	}
	
}