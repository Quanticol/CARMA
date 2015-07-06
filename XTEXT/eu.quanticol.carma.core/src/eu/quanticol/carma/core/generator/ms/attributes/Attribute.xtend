package eu.quanticol.carma.core.generator.ms.attributes

class Attribute {
	
	public var String name = ""
	public var String type = ""
	public var String assignment = ""
	
	def set(String n, String t, String a){
		name = n
		type = t
		assignment = a
	}
	
}