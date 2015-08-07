package eu.quanticol.carma.core.generator.ms.record

import eu.quanticol.carma.core.carma.RecordDefinition

import com.google.inject.Inject
import eu.quanticol.carma.core.utils.Util

class RecordHandler {
	
	@Inject extension Util
	
	def String recordToJava(RecordDefinition recordDefinition){
		'''
		public static class «recordDefinition.name.recordClass» {
			
			«FOR field : recordDefinition.fields»
			public final «field.fieldType.toJavaType» «field.name.fieldName»;
			«ENDFOR»
			
			public «recordDefinition.name.recordClass»( «FOR field:recordDefinition.fields SEPARATOR ','»«field.fieldType.toJavaType» «field.name.fieldName»«ENDFOR») {
				«FOR field :  recordDefinition.fields»
				this.«field.name.fieldName» = «field.name.fieldName»;
				«ENDFOR»
			}
			
		}
		'''
	}
	
}