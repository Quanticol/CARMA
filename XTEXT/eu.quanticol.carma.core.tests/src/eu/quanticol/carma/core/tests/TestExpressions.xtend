package eu.quanticol.carma.core.tests

import org.junit.runner.RunWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.InjectWith
import eu.quanticol.carma.core.CARMAInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import eu.quanticol.carma.core.carma.Model
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import static extension org.junit.Assert.*
import eu.quanticol.carma.simulator.CarmaModel
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class TestExpressions {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	@Inject extension ReflectExtensions
	
	@Test
	def void test_Expression_01(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C = 1;		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( 1 , o.get("__CONST__C") as Integer )
		]
	}
	
}