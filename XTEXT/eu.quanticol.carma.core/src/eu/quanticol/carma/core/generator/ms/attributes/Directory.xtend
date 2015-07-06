package eu.quanticol.carma.core.generator.ms.attributes

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.AttribVariableDeclaration
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.Component
import eu.quanticol.carma.core.carma.Declaration
import eu.quanticol.carma.core.carma.FeildDeclaration
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordArgument
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordDefinition
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.generator.ms.Javaniser
import java.util.ArrayList
import java.util.HashMap

import static extension org.eclipse.xtext.EcoreUtil2.*

class Directory {
	
	@Inject extension Javaniser
	
	def HashMap<String,Attribute> populate(Model model){
		
		var directory = new HashMap<String,Attribute>()
		var declarations = model.eAllOfType(Declaration)
		
		for(declaration : declarations){
			if(declaration.getContainerOfType(Component) != null || declaration.getContainerOfType(System) != null)
				switch(declaration){
					AttribVariableDeclaration:{
						var name = declaration.name.name
						var type = "Integer.class"
						var assignment = (declaration as AttribVariableDeclaration).assign
						var assign = ""
						switch(assignment){
							VariableReference:	assign = assignment.javanise
							CarmaInteger:		assign = assignment.javanise
						}
						var attrib = new Attribute() 
						attrib.set(name,type,assign)
						directory.put(name,attrib)
					}
					RecordDeclaration: {
						var name = declaration.name.name
						var type = "Integer.class"
						var attribs = (declaration.type as RecordType).name.getContainerOfType(RecordDefinition).eAllOfType(FeildDeclaration)
						var args = declaration.eAllOfType(RecordArgument)
						var argsName = new ArrayList<String>()
						var assign = ""				
						if(args.size == 0){
							if((declaration as RecordDeclaration).assign != null)
								assign = (declaration as RecordDeclaration).assign.javanise
								for(a : attribs)
									argsName.add(assign+"_"+a.name.name)
						} else {
							for(arg : args){
								var value = arg.value
								switch(value){
									VariableReference:	assign = value.javanise
									CarmaInteger:		assign = value.javanise
									Range:				assign = value.javanise
								}
								argsName.add(assign)
							}
						}
						
						if(attribs.size == argsName.size){
							for(var i = 0; i < attribs.size; i++){
								var attrib = new Attribute()
								attrib.set(name+"_"+attribs.get(i).name.name,type,argsName.get(i))
								directory.put(name+"_"+attribs.get(i).name.name,attrib)
							}
						}
					}
				}
		}
		
		return directory
		
	}
	
}