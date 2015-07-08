package eu.quanticol.carma.core.generator.ms.record

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Records
import eu.quanticol.carma.core.carma.RecordDefinition
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.AttribParameter
import eu.quanticol.carma.core.carma.FeildDeclaration

class RecordHandler {
	
	@Inject extension RecordJavaniser
	
	def String getRecords(Records records){
		'''
		«FOR recordDefinition : records.recordDefinitions»
		«recordDefinition.getRecord»
		«ENDFOR»
		'''
	}
	
	def String getRecord(RecordDefinition recordDefinition){
		var name = recordDefinition.recordSignature.type.name
		var ArrayList<AttribParameter> parameters = new ArrayList<AttribParameter>(recordDefinition.eAllOfType(AttribParameter))
		'''
		public static class «name» {
			
			«FOR feild : recordDefinition.recordDefinitionStatementBlock.feilds»
			«(feild as FeildDeclaration).declare»
			«ENDFOR»
			
			public «name» ( «parameters.getParameters» ) {
				«FOR feild : recordDefinition.recordDefinitionStatementBlock.feilds»
				«(feild as FeildDeclaration).javanise»
				«ENDFOR»
			}
			
		}
		'''
	}
}