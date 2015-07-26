package eu.quanticol.carma.core.generator.ms.enums

import static extension eu.quanticol.carma.core.utils.Util.*
import eu.quanticol.carma.core.carma.EnumDefinition

class EnumHandler {

	def String getRecords(Iterable<EnumDefinition> enums){
		'''
		«FOR enumEdfinition : enums»
		«enumEdfinition.getEnum»
		«ENDFOR»
		'''
	}
	
	def String getEnum(EnumDefinition enumDefinition){
		'''
		public enum «enumDefinition.name.enumClass» {
			
			«FOR ecase:enumDefinition.values SEPARATOR ','»
			 «ecase.name.enumCaseName»
			«ENDFOR»;
			
		}
		'''
	}
		
}