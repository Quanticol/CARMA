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
		enum TEST_ENUM = CASE_1, CASE_2, CASE_3;
		
		const C1 = CASE_1;		
		const C2 = CASE_2;		
		const C3 = CASE_3;		
		
		const TEST_1 = C1==CASE_1;
		const TEST_2 = C2==CASE_2;
		const TEST_3 = C3==CASE_3;
		const TEST_4 = C1==CASE_2;
		const TEST_5 = C1==CASE_3;
		const TEST_6 = C2==CASE_1;
		const TEST_7 = C2==CASE_3;
		const TEST_8 = C3==CASE_1;
		const TEST_9 = C3==CASE_2;
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( true , o.get("__CONST__TEST_1") as Boolean );
			assertEquals( true , o.get("__CONST__TEST_2") as Boolean );
			assertEquals( true , o.get("__CONST__TEST_3") as Boolean );
			assertEquals( false , o.get("__CONST__TEST_4") as Boolean );
		]
	}
	
}