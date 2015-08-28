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
			public «field.fieldType.toJavaType» «field.name.fieldName»;
			«ENDFOR»
			
			public «recordDefinition.name.recordClass»( «FOR field:recordDefinition.fields SEPARATOR ','»«field.fieldType.toJavaType» «field.name.fieldName»«ENDFOR») {
				«FOR field :  recordDefinition.fields»
				this.«field.name.fieldName» = «field.name.fieldName»;
				«ENDFOR»
			}
			
			public String toString() {
				return "[ "«FOR field:recordDefinition.fields SEPARATOR "+\" , \""»+"«field.name»="+«field.name.fieldName»«ENDFOR»+" ]";
			}
			
			public boolean equals( Object o ) {
				if (o instanceof «recordDefinition.name.recordClass») {
					«recordDefinition.name.recordClass» other = («recordDefinition.name.recordClass») o;
					return «FOR field:recordDefinition.fields SEPARATOR "&&"»
						this.«field.name.fieldName».equals( other.«field.name.fieldName» )					
					«ENDFOR»;	
				}	
				return false;
			}
			
		}
		'''
	}
	
}