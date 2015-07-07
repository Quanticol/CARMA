package eu.quanticol.carma.core.generator.ms.attributes

import eu.quanticol.carma.core.carma.AttribVariableDeclaration
import eu.quanticol.carma.core.carma.Declaration
import eu.quanticol.carma.core.carma.FeildDeclaration
import eu.quanticol.carma.core.carma.GlobalStoreBlock
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordDefinition
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.StoreBlock
import java.util.HashMap

import static extension org.eclipse.xtext.EcoreUtil2.*

class AttributeDirectory {
	
	def HashMap<String,String> populate(Model model){
		
		var directory = new HashMap<String,String>()
		var declarations = model.eAllOfType(Declaration)
		
		for(declaration : declarations){
			if(declaration.getContainerOfType(StoreBlock) != null || declaration.getContainerOfType(GlobalStoreBlock) != null)
				switch(declaration){
					AttribVariableDeclaration:{
						var name = declaration.name.name
						var type = "Integer.class"
						directory.put(name,type)
					}
					RecordDeclaration: {
						var name = declaration.name.name
						var type = "Integer.class"
						var attribs = (declaration.type as RecordType).name.getContainerOfType(RecordDefinition).eAllOfType(FeildDeclaration)
						for(var i = 0; i < attribs.size; i++){
							directory.put(name+"_"+attribs.get(i).name.name,type)
						}
					}
				}
		}
		
		return directory
		
	}
	
}