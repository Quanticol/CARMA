package eu.quanticol.carma.core.utils

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Name

class Util {
	
	@Inject extension LabelUtil
	
	def boolean sameName(Name name1, Name name2){
		name1.name.equals(name2.name)
	}
}