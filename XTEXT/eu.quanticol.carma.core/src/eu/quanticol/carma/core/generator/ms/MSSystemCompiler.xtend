package eu.quanticol.carma.core.generator.ms

import java.util.HashMap
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.BlockSystem
import eu.quanticol.carma.core.generator.ms.collective.CollectiveHandler
import com.google.inject.Inject
import eu.quanticol.carma.core.carma.BlockStyle

class MSSystemCompiler {
	
	@Inject extension CollectiveHandler
	
	public static var SYSTEMNAME = ""
	
	def void extractSystem(System system, Model model, HashMap<String,String> output){
		MSSystemCompiler.SYSTEMNAME = (system as BlockSystem).name.name
		println((system as BlockSystem).collective.constructor((system as BlockSystem).environment.stores))
		println((model.components as BlockStyle).components)
		println((model.components as BlockStyle).createProcesses)
	}
	
}