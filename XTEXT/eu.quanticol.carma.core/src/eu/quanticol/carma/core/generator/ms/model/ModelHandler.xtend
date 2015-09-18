package eu.quanticol.carma.core.generator.ms.model

import eu.quanticol.carma.core.carma.Model
import com.google.inject.Inject
import eu.quanticol.carma.core.generator.ms.enums.EnumHandler
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.generator.ms.record.RecordHandler
import eu.quanticol.carma.core.generator.ms.activities.ActivityHandler
import eu.quanticol.carma.core.generator.ms.function.FunctionHandler
import eu.quanticol.carma.core.generator.ms.collective.CollectiveHandler
import eu.quanticol.carma.core.generator.ms.system.SystemHandler
import eu.quanticol.carma.core.generator.ms.measure.MeasureHandler
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler
import eu.quanticol.carma.core.typing.TypeSystem

class ModelHandler {
	
	@Inject extension EnumHandler
	@Inject extension RecordHandler
	@Inject extension Util
	@Inject extension ActivityHandler
	@Inject extension FunctionHandler
	@Inject extension CollectiveHandler
	@Inject extension SystemHandler
	@Inject extension MeasureHandler
	@Inject extension ExpressionHandler
	@Inject extension TypeSystem
	
	def modelToJava( Model model , String className , String packageName ) {
		'''
		package «packageName»;
		
		import org.apache.commons.math3.random.RandomGenerator;
		import org.cmg.ml.sam.sim.*;		
		import eu.quanticol.carma.simulator.*;
		import java.util.LinkedList;
		import java.util.HashMap;
		import java.util.TreeSet;
		import org.cmg.ml.sam.sim.sampling.*;
		

		public class «className» implements CarmaModel {
			
			public «className»() {
				«FOR c:model.components»
				«c.methofForComponentBehaviourCreation»;
				«ENDFOR»
				«MeasureHandler::SetUpMeasureMethodName»;
			}
			
			«FOR e:model.enums» 
			«e.enumToJava»
			«ENDFOR»
			
			«FOR r:model.records»
			«r.recordToJava»
			«ENDFOR»
		
			«FOR c:model.constants»
			public static final «c.value.typeOf.toJavaType(true)» «c.name.constantName» = «c.value.expressionToJava»;
			«ENDFOR»
		
			«FOR f:model.functions»
			«f.function»
			«ENDFOR»
			
			«FOR c:model.components»
			«c.componentBehaviour(model.globalProcesses)»
			«ENDFOR»
			
			«model.activities.activitiesToJava»
			
			«model.systems.systemsCollector»
			
			«FOR s:model.systems»
			«s.systemFactory»
			«ENDFOR»
			
			«model.measures.collectMeasures»
			
		}
		'''
	}
	
}