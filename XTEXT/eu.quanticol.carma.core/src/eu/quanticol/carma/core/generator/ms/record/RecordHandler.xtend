package eu.quanticol.carma.core.generator.ms.record

import eu.quanticol.carma.core.carma.RecordDefinition

import static extension eu.quanticol.carma.core.utils.Util.*

class RecordHandler {
	
	def String getRecords(Iterable<RecordDefinition> records){
		'''
		«FOR recordDefinition : records»
		«recordDefinition.getRecord»
		«ENDFOR»
		'''
	}
	
	def String getRecord(RecordDefinition recordDefinition){
		'''
		public static class «recordDefinition.name.recordClass» {
			
			«FOR field : recordDefinition.fields»
			public final «field.fieldType.toJavaType»;
			«ENDFOR»
			
			public «recordDefinition.name.recordClass»( «FOR field:recordDefinition.fields SEPARATOR ','»«field.fieldType.toJavaType» «field.name.fieldName»«ENDFOR») {
				«FOR field :  recordDefinition.fields»
				this.«field.name.fieldName» = «field.name»;
				«ENDFOR»
			}
			
		}
		'''
	}
	
}