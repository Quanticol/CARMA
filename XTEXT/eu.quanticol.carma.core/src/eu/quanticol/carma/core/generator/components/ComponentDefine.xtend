package eu.quanticol.carma.core.generator.components

import java.util.ArrayList
import com.google.inject.Inject
import eu.quanticol.carma.core.utils.LabelUtil

class ComponentDefine {
	
	@Inject extension LabelUtil
	
	def String defineComponents(ComponentManager componentManager){
		var cvm = componentManager.CVM
		var prefix = cvm.definitionsPrefix;
		'''
		«FOR component_name : componentManager.getComponentGenerators().keySet»
		«var component = componentManager.getComponentGenerators().get(component_name)»
		private CarmaComponent get«component_name.toFirstUpper»(«component.getMyArgs.convertArrayList») {
			CarmaComponent c4rm4 = new CarmaComponent();
			«var vds = component.myVariables»
			«FOR key : vds.keySet»
			«prefix.setComponent(cvm.cleanName(key),cvm.getValueAsString(key,vds.get(key))+"")»
			«ENDFOR»
			«FOR mer : component.getMacros»
			c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
			«prefix»«component_name»Process,
			«prefix»«component_name»Process.getState("state_«mer.name.label»")));
			«ENDFOR»
			return c4rm4;
		}
			«ENDFOR»
		'''
	}
	
	def String convertArrayList(ArrayList<String> list){
		var output = ""
		
		if(list.size > 0){
			output = "int " + list.get(0)
			for(var i = 1; i < list.size; i++){
				output = output + ", " + "int " +list.get(i)
			}
		}
		
		return output;
	}
	
	def String setComponent(String prefix, String name, String value){
		'''c4rm4.set(«prefix»«name»,«value»);'''
	}
	
}