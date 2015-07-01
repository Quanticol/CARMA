package eu.quanticol.carma.core.typing

import eu.quanticol.carma.core.carma.VariableName

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.Declaration
import eu.quanticol.carma.core.carma.Type
import eu.quanticol.carma.core.carma.DoubleType
import eu.quanticol.carma.core.carma.IntgerType
import eu.quanticol.carma.core.carma.AttribType
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.Parameter
import java.util.ArrayList
import eu.quanticol.carma.core.carma.InputParameter

class BaseType {
	
	public var String parent = "null"
	public var String me = "null"
	
	def setMe(String t){
		me = t
	}
	
	override toString() { parent + ":" + me }
	
}

class TypeProvider {
	
	def boolean sameType(BaseType one, BaseType two){
		one.me.equals(two.me)
	}
	
	def BaseType getType(VariableName variableName){
		var declaration = variableName.getContainerOfType(Declaration)
		var parameter = variableName.getContainerOfType(Parameter)
		var ArrayList<Type> types = new ArrayList<Type>()
		
		if(declaration != null){
			types.addAll(declaration.eAllOfType(Type))
		}
		if(parameter != null){
			types.addAll(parameter.eAllOfType(Type))
		}
		
		//it could be an input action parameter, which doesn't have a "type" attribute
		if(types.size == 0){
			parameter = variableName.getContainerOfType(InputParameter)
			if(parameter != null)
				return new BaseType() => [ parent="primitive" me="attrib"]
			else
				return new BaseType()
		}
		if(types.size > 1){
			return new BaseType()
		} 
		if(types.size == 1) {
			types.get(0).type
		}
	}
	
	def BaseType getType(Type type){
		switch(type){
			DoubleType: new BaseType() => [ parent="primitive" me="double"]
			IntgerType: new BaseType() => [ parent="primitive" me="int"]
			AttribType: new BaseType() => [ parent="primitive" me="attrib"]
			RecordType: {
				var String t = (type as RecordType).name.name
				var baseType = new BaseType() => [ parent="record"]
				baseType.me = t
				return baseType
			}
		}
	}	

}