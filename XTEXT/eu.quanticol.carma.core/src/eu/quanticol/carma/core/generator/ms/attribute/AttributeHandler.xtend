package eu.quanticol.carma.core.generator.ms.attribute

import com.google.inject.Inject
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.carma.AttributeDeclaration
import eu.quanticol.carma.core.utils.ReferenceContext
import eu.quanticol.carma.core.typing.TypeSystem
import eu.quanticol.carma.core.typing.CarmaType

class AttributeHandler {

	@Inject extension Util
	@Inject extension TypeSystem
	
	def attributeTemporaryVariableDeclaration( AttributeDeclaration a , ReferenceContext c , String storeName ) {
		'''final «a.typeOf.toJavaType(false)» «a.name.attributeName(c)» = «storeName».get( "«a.name»" , «a.typeOf.toJavaType(false)».class );'''		
	}
	
	def locTemporaryVariableDeclaration( ReferenceContext c , String storeName ) {
		'''final «CarmaType::LOCATION_TYPE.toJavaType(false)» «"loc".attributeName(c)» = «storeName».get( "loc" , Node.class );'''		
	}
	
	
}