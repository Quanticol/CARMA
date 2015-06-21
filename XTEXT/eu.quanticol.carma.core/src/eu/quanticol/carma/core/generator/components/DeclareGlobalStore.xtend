package eu.quanticol.carma.core.generator.components

import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager
import eu.quanticol.carma.core.carma.System

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.VariableDeclaration
import java.util.ArrayList
import com.google.inject.Inject
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.generator.GeneratorUtils

class DeclareGlobalStore {
	
	@Inject extension LabelUtil
	@Inject extension GeneratorUtils
	
	def String setGlobalStore(CarmaVariableManager cvm, System system){
		var ArrayList<VariableDeclaration> vds = new ArrayList<VariableDeclaration>(system.eAllOfType(VariableDeclaration))
		var prefix = cvm.definitionsPrefix
		'''
		«FOR vd : vds»
		«FOR name : vd.fullNamesAndHash.keySet»
		global_store.set(«prefix»«cvm.cleanName(name)»,«cvm.getValueAsString(name,vd.fullNamesAndHash.get(name))»);
		«ENDFOR»
		«ENDFOR»
		'''
	}
}