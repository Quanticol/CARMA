package eu.quanticol.carma.core.generator.ms

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.generator.Compiler
import eu.quanticol.carma.core.utils.LabelUtil
import java.util.HashMap
import org.eclipse.emf.ecore.resource.Resource
import eu.quanticol.carma.core.generator.ms.model.ModelHandler

class MSCompiler implements Compiler {

	@Inject extension ModelHandler
	
	public static var PATH = "ms/" 
	public static var PACK = "ms"
	
	@Inject extension LabelUtil
	
	/**
	 * File structures:
	 * 
	 * 	System
	 * 		FunctionHandler
	 * 			functions
	 * 		RecordHandler
	 * 			records
	 * 		Collective
	 * 			Constructor
	 * 				add(getComponent)
	 * 				global_store declarations
	 * 			getComponent Methods
	 * 				arguments
	 * 				assign attribute values
	 * 				assign behaviour
	 * 			behaviours
	 * 				automatons
	 * 		measures(environmental)
	 * 			measures
	 * 		Measures
	 * 			measures
	 * 		predicates
	 * 			broadcast-rate
	 * 			unicast-rate
	 * 			broadcast-prob
	 * 			unicast-prob
	 * 			broadcast-update
	 * 			unicast-update
	 * 		evolution rules
	 * 			broadcast-rate
	 * 			unicast-rate
	 * 			broadcast-prob
	 * 			unicast-prob
	 * 			broadcast-update
	 * 			unicast-update
	 * 		main
	 * 			Declare environment
	 * 			declare deadline
	 * 			SamplingCollection
	 * 			addToCollection
	 * 			setSampling
	 * 			simulate
	 * 
	 * 	Factory
	 * 		contructor
	 * 		getModel()
	 */
		
	override HashMap<String,CharSequence> extractJava(Resource resource){
		
		var Model model = resource.allContents.toIterable.filter(Model).get(0)
		var HashMap<String,CharSequence> toReturn = new HashMap<String,CharSequence>()
		
		var className = model.className
		var packageName = MSCompiler.PACK
		var fileName = MSCompiler.PATH + className + ".java"
		
		toReturn.put( fileName , model.modelToJava(className,packageName) );		
		
		return toReturn
		
	}
	
	def getClassName( Model model ) {
		model.name
	}
	
	def getPackageName( Resource resource ) {
		
	}

	def targetFile( Resource resource ) {
		
	}
	
//		var Model model = resource.allContents.toIterable.filter(Model).get(0)
//		var systems = resource.allContents.toIterable.filter(System)
//		
//		var variableManager = new CarmaVariableManager()
//		variableManager.populateCarmaVariableManager(model)
//		
//		var actionManager = new ActionManager()
//		actionManager.populateActionManager(model)
//		
//		var componentManager = new ComponentManager(actionManager,variableManager)
//		componentManager.populateComponentManager(model)
//		
//		var measureManager = new MeasureManager(actionManager,variableManager,componentManager)
//		measureManager.populateMeasureManager(model)
//		
//		var modelName = model.label
//		var URI = "carma" + "/" + modelName.toLowerCase
//		var packageName = "package carma." + modelName.toLowerCase
//		
//		//Definitions
//		fsa.generateFile(URI + "/" + modelName + "Definition.java",model.compileDefinitions(packageName,
//			variableManager,
//			actionManager,
//			componentManager,
//			measureManager
//		))
//		
//		//Systems
//		for(system : systems){
//			fsa.generateFile(URI + "/" + system.disarm + ".java", system.compileSystem(packageName,
//				variableManager,
//				actionManager,
//				componentManager,
//				measureManager
//			))
//		}
//		
//		//Factory
//		fsa.generateFile(URI + "/" + modelName + "Factory.java",model.compileFactory(packageName))
	
}