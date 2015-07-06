package eu.quanticol.carma.core.generator.ms.component

import java.util.ArrayList
import eu.quanticol.carma.core.carma.Parameter
import eu.quanticol.carma.core.generator.ms.attributes.Attribute

class Component {
	
	var String name = ""
	var ArrayList<String> arguments = null
	var ArrayList<Parameter> parameters = null
	var ArrayList<Attribute> attributes = null
	var Behaviour behaviour = null
	var State initial = null
	
}