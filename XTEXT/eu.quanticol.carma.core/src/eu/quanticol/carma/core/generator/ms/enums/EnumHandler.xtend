package eu.quanticol.carma.core.generator.ms.enums

import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.carma.EnumDefinition
import com.google.inject.Inject

class EnumHandler {

	@Inject extension Util

	
	def String enumToJava(EnumDefinition enumDefinition){
		'''
		public enum «enumDefinition.name.enumClass» {
			
			«FOR ecase:enumDefinition.values SEPARATOR ','»
			 «ecase.name.enumCaseName»
			«ENDFOR»;
			
		}
		'''
	}
		
}