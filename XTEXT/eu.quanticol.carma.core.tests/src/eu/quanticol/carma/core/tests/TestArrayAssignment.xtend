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
import java.util.List
import java.util.Set

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class TestArrayAssignment {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	@Inject extension ReflectExtensions

	@Test
	def void test_ArrayAssignement1_Parser(){
		class.classLoader.setJavaCompilerClassPath
		'''
fun int testArrayAssignment1( list<int> test1 ) {
	test1[0] = 99;
	return 0;
}
		'''.parse.assertNoErrors
	}	

	@Test
	def void test_ArrayAssignement1_Compiler(){
		class.classLoader.setJavaCompilerClassPath
		'''
fun int testArrayAssignment1( list<int> test1 ) {
	test1[0] = 99;
	return 0;
}
		'''.compile[ 
					var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )
		]
	}	

	@Test
	def void test_ArrayAssignement2_Parser() {
		class.classLoader.setJavaCompilerClassPath
		'''
fun int testArrayAssignment2( list<list<int>> test1 ) {
	test1[0][0] = 99;
	return 0;
}
		
		'''.parse.assertNoErrors
	}	

	@Test
	def void test_ArrayAssignement2_Compiler() {
		class.classLoader.setJavaCompilerClassPath
		'''
fun int testArrayAssignment2( list<list<int>> test1 ) {
	test1[0][0] = 99;
	return 0;
}
		
		'''.compile[ 
					var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )
		]
	}		

	@Test
	def void test_ArrayAssignement3_Compiler(){
		class.classLoader.setJavaCompilerClassPath
		'''
component TestComponent() {
	store {
		list<list<int>> test = [: [: 0 :] :];
	}

	behaviour {
		A = tick*{ test[0][0] = 10; }.nil;
	}

	init {
		A
	}
}
		
		'''.compile[ 
					var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )
		]
	}	

	@Test
	def void test_ArrayAssignement3_Parser(){
		class.classLoader.setJavaCompilerClassPath
		'''
component TestComponent() {
	store {
		list<list<int>> test = {: {: 0 :} :};
	}

	behaviour {
		A = tick*{ test[0][0] = 10; }.nil;
	}

	init {
		A
	}
}
		
		'''.parse.assertNoErrors
	}	
	
	@Test
	def void test_ArrayAssignement4_Compiler(){
		class.classLoader.setJavaCompilerClassPath
		'''
component TestComponent() {
	store {
		list<list<int>> test = [: [: 0 :] :];
	}

	behaviour {
		A = tick*{ test.remove( 0 ); }.nil;
	}

	init {
		A
	}
}
		
		'''.compile[ 
					var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )
		]
	}	

	@Test
	def void test_ArrayAssignement4_Parser(){
		class.classLoader.setJavaCompilerClassPath
		'''
component TestComponent() {
	store {
		list<list<int>> test = [: [: 0 :] :];
	}

	behaviour {
		A = tick*{ test.remove( [: 0 :] ); }.nil;
	}

	init {
		A
	}
}
		
		'''.parse.assertNoErrors
	}		
	
	
}
