package eu.quanticol.carma.core.generator.ms

import eu.quanticol.carma.core.carma.Model
import java.util.HashMap
import org.eclipse.emf.ecore.resource.Resource
import eu.quanticol.carma.core.generator.Compiler
import com.google.inject.Inject

import eu.quanticol.carma.core.generator.ms.attributes.Directory
import eu.quanticol.carma.core.utils.LabelUtil

class MSCompiler implements Compiler {
	
	public static final val DEFINITION = "Definition"
	public static var PATH = "ms/" 
	public static var PACK = "ms."
	public static var ATTRIBUTE_DIRECTORY = null
	
	@Inject extension LabelUtil
	@Inject extension Directory
	
	/**
	 * File structures:
	 * 
	 * 	Definition file
	 * 		Function blocks
	 * 		Attributes and type enumeration
	 * 		Action enumeration
	 *		Component behaviour
	 * 		Measures
	 * 
	 * 	System files
	 * 		Constructor
	 * 			add(getComponent)
	 * 			global_store declarations
	 * 		getComponent Methods
	 * 			arguments
	 * 			assign attribute values
	 * 			assign behaviour
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
		
	override HashMap<String,String> extractJava(Resource resource){
		
		var Model model = resource.allContents.toIterable.filter(Model).get(0)
		var String modelName = model.name
		
		MSCompiler.PATH = MSCompiler.PATH + modelName.toLowerCase + "/"
		MSCompiler.PACK = MSCompiler.PACK + modelName.toLowerCase 
		
		
		MSCompiler.ATTRIBUTE_DIRECTORY = model.populate()
		
		println(MSCompiler.ATTRIBUTE_DIRECTORY)
		
		var toReturn = new HashMap<String,String>()
		
		
		return toReturn
		
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