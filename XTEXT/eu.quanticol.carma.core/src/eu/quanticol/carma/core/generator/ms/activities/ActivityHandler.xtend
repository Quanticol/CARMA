package eu.quanticol.carma.core.generator.ms.activities

import eu.quanticol.carma.core.carma.Activity
import eu.quanticol.carma.core.utils.Util
import com.google.inject.Inject

class ActivityHandler {
	
	@Inject extension Util
	
	def activitiesToJava( Iterable<Activity> activities) {
		
		'''
		«FOR a:activities.indexed»
		public static final int «a.value.name.actionName» = «a.key»;	
		«ENDFOR»
		'''
		
	}
	
	
}