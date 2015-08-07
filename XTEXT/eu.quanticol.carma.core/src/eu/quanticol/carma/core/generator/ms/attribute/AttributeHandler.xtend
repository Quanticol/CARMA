package eu.quanticol.carma.core.generator.ms.attribute

import com.google.inject.Inject
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.carma.AttributeDeclaration
import eu.quanticol.carma.core.utils.ReferenceContext
import eu.quanticol.carma.core.typing.TypeSystem

class AttributeHandler {

	@Inject extension Util
	@Inject extension TypeSystem
	
	def attributeTemporaryVariableDeclaration( AttributeDeclaration a , ReferenceContext c , String storeName ) {
		var aType = a.typeOf.toJavaType
		'''final «aType» «a.name.attributeName(c)» = «storeName».get( "«a.name»" , «aType».class );'''		
	}
	
	
	
	
}