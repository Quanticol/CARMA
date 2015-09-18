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
		'''final «a.typeOf.toJavaType(true)» «a.name.attributeName(c)» = «storeName».get( "«a.name»" , «a.typeOf.toJavaType(false)».class );'''		
	}
	
	
	
	
}