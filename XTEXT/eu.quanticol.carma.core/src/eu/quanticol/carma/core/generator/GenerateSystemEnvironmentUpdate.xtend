package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*

class GenerateSystemEnvironmentUpdate {
	
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	@Inject extension Util
	@Inject extension GeneratorUtils
	
	def String defineEnvironmentUpdatesPredicates(System system){
		
		var updates = system.getContainerOfType(Model).eAllOfType(EnvironmentUpdate)
		var unicasts = new ArrayList<EnvironmentUpdate>()
		var broadcasts = new ArrayList<EnvironmentUpdate>()
		
		for(eu : updates)
			if(eu.stub.isBroadcast)
				broadcasts.add(eu)
			else
				unicasts.add(eu)
		'''
		/*ENVIRONMENT UPDATE PREDICATES*/
		//BROADCAST ENVIRONMENT UPDATE PREDICATES
		«FOR broadcast : broadcasts»
		«defineCastPredicates(broadcast)»
		«ENDFOR»
		
		//UNICAST ENVIRONMENT UPDATE PREDICATES
		«FOR unicast : unicasts»
		«defineCastPredicates(unicast)»
		«ENDFOR»
		'''
		
	}
	
}