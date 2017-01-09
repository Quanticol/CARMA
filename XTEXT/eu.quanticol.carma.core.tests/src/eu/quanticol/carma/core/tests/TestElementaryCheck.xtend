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
import eu.quanticol.carma.core.carma.FunctionDefinition
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.carma.ConstantDefinition
import eu.quanticol.carma.core.carma.CarmaPackage
import eu.quanticol.carma.core.validation.CARMAValidator

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class TestElementaryCheck {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	@Inject extension Util
	
	
	@Test
	def void test_SimpleRecursive(){
		var m = 
			'''
			fun int simpleFib(int n) {
				if (n < 2) {
					return n;		
				} else {
					return n+simpleFib(n-1)+simpleFib(n-2);
				}
			}
			'''.parse
		assertNotNull( m )
		m.assertNoErrors
		var f = m.elements.filter(typeof(FunctionDefinition)).findFirst[it.name == "simpleFib" ]
		assertTrue(f.isRecursive)
	}

	@Test
	def void test_ComplexRecursive(){
		var m = 
			'''
			fun int simpleFib1(int n) {
				if (n < 2) {
					return n;		
				} else {
					return n+simpleFib2(n-1)+simpleFib2(n-2);
				}
			}
			
			fun int simpleFib2( int n ) {
				if (n < 2) {
					return n;		
				} else {
					return n+simpleFib1(n-1)+simpleFib1(n-2);
				}
			}
			'''.parse
		assertNotNull( m )
		m.assertNoErrors
		var f1 = m.elements.filter(typeof(FunctionDefinition)).findFirst[it.name == "simpleFib1" ]
		var f2 = m.elements.filter(typeof(FunctionDefinition)).findFirst[it.name == "simpleFib1" ]
		assertTrue(f1.isRecursive)
		assertTrue(f2.isRecursive)
	}

	@Test
	def void test_SimpleNotRecursive(){
		var m = 
			'''
			fun int aFunction(int n) {
				return n+1;
			}
			'''.parse
		assertNotNull( m )
		m.assertNoErrors
		var f = m.elements.filter(typeof(FunctionDefinition)).findFirst[it.name == "aFunction" ]
		assertFalse(f.isRecursive)
	}	

	@Test
	def void test_ComplexNotRecursive(){
		var m = 
			'''
			fun int aFunction(int n) {
				return anotherFunction( n );
			}

			fun int anotherFunction(int n) {
				return n+1;
			}
			'''.parse
		assertNotNull( m )
		m.assertNoErrors
		var f = m.elements.filter(typeof(FunctionDefinition)).findFirst[it.name == "aFunction" ]
		assertFalse(f.isRecursive)
	}	


	@Test
	def void test_directIsRandom(){
		var m = 
			'''
			const test = RND;
			'''.parse
		assertNotNull( m )
		m.assertNoErrors
		var c = m.elements.filter(typeof(ConstantDefinition)).findFirst[it.name == "test" ]
		assertTrue(c.value.usesRandomExpressions)
	}	

	@Test
	def void test_nestedRandom(){
		var m = 
			'''
			const test = RND+0.5;
			'''.parse
		assertNotNull( m )
		m.assertNoErrors
		var c = m.elements.filter(typeof(ConstantDefinition)).findFirst[it.name == "test" ]
		assertTrue(c.value.usesRandomExpressions)
	}	
	
	@Test
	def void test_BuildElementaryTable(){
		var m = 
			'''
			fun int aFunction(int n) {
				return anotherFunction( n );
			}

			fun int anotherFunction(int n) {
				return n+1;
			}
			'''.parse
		assertNotNull( m )
		m.assertNoErrors
		val table = m.buildRecursiveTable()
		m.elements.filter(typeof(FunctionDefinition)).forEach[
			assertNotNull(table.get(it))
			assertFalse(table.get(it))			
		]
	}	

	@Test
	def void test_BuildElementaryTable2(){
		var m = 
			'''
			fun int aFunction(int n) {
				return anotherFunction( n );
			}

			fun int anotherFunction(int n) {
				return aFunction( n );
			}
			'''.parse
		assertNotNull( m )
		m.assertNoErrors
		val table = m.buildRecursiveTable()
		m.elements.filter(typeof(FunctionDefinition)).forEach[
			assertNotNull(table.get(it))
			assertTrue(table.get(it))			
		]
	}	

	@Test
	def void test_BuildElementaryTable3(){
		var m = 
			'''
			fun real aFunction(int n) {
				return anotherFunction( n );
			}

			fun real anotherFunction(int n) {
				return RND;
			}
			'''.parse
		assertNotNull( m )
		m.assertNoErrors
		val table = m.buildRecursiveTable()
		m.elements.filter(typeof(FunctionDefinition)).forEach[
			assertNotNull(table.get(it))
			assertFalse(table.get(it))			
		]
	}		

	@Test
	def void test_BuildElementaryTable4(){
		var m = 
			'''
			fun real aFunction(int n) {
				return RND;
			}
			'''.parse
		assertNotNull( m )
		m.assertNoErrors
		val table = m.buildRandomFuncitonTable()
		m.elements.filter(typeof(FunctionDefinition)).forEach[
			assertNotNull(table.get(it))
			assertTrue(table.get(it))			
		]
	}		

	@Test
	def void test_CheckExpressionsInOutputActions_Random_Output(){
		'''
		fun int factorial( int n ) {
			if (n<=1) {
				return n;
			} else {
				return n*factorial( n-1 );
			}
		}
		
		component Test() {
			store {
				attrib a = 3;
				attrib b = my.a+1;
			}
			behaviour {
				A = test*< RND >.A;
			}
			init {
				A		
			}
		}
		
		system SystemTest {
			collective {
				new Test();
			}
			environment {
				store {
					attrib a = 2;
					attrib b = global.a+2;	
				}
				update {
					test* {
						b = global.a+1;
						a = b+1; 
					}
				}
			}
		}			
		'''.parse.assertError(CarmaPackage::eINSTANCE.outputAction,CARMAValidator::ERROR_Dangerous_use_of_expressions,"Random expressions cannot be used in output actions!")
	}	

	@Test
	def void test_CheckExpressionsInOutputActions_Random_Predicate(){
		'''
		fun int factorial( int n ) {
			if (n<=1) {
				return n;
			} else {
				return n*factorial( n-1 );
			}
		}
		
		component Test() {
			store {
				attrib a = 3;
				attrib b = my.a+1;
			}
			behaviour {
				A = test*[ a == int(RND)]< 2 >.A;
			}
			init {
				A		
			}
		}
		
		system SystemTest {
			collective {
				new Test();
			}
			environment {
				store {
					attrib a = 2;
					attrib b = global.a+2;	
				}
				update {
					test* {
						b = global.a+1;
						a = b+1; 
					}
				}
			}
		}			
		'''.parse.assertError(CarmaPackage::eINSTANCE.actionGuard,CARMAValidator::ERROR_Dangerous_use_of_expressions,"Random expressions cannot be used in predicates!")
	}	
	

	@Test
	def void test_CheckExpressionsInOutputActions_Recursive_Predicate(){
		'''
		fun int factorial( int n ) {
			if (n<=1) {
				return n;
			} else {
				return n*factorial( n-1 );
			}
		}
		
		component Test() {
			store {
				attrib a = 3;
				attrib b = my.a+1;
			}
			behaviour {
				A = test*[ a == factorial( 2 ) ]< 2 >.A;
			}
			init {
				A		
			}
		}
		
		system SystemTest {
			collective {
				new Test();
			}
			environment {
				store {
					attrib a = 2;
					attrib b = global.a+2;	
				}
				update {
					test* {
						b = global.a+1;
						a = b+1; 
					}
				}
			}
		}			
		'''.parse.assertError(CarmaPackage::eINSTANCE.actionGuard,CARMAValidator::ERROR_Dangerous_use_of_expressions,"Recursive functions cannot be used in predicates!")
	}	
	
	@Test
	def void test_CheckExpressionsInInputActions_Random_Predicate(){
		'''
		fun int factorial( int n ) {
			if (n<=1) {
				return n;
			} else {
				return n*factorial( n-1 );
			}
		}
		
		component Test() {
			store {
				attrib a = 3;
				attrib b = my.a+1;
			}
			behaviour {
				A = test*< 2 >.A
					+
					test*[ a == int(RND) ]( x ).A;			}
			init {
				A		
			}
		}
		
		system SystemTest {
			collective {
				new Test();
			}
			environment {
				store {
					attrib a = 2;
					attrib b = global.a+2;	
				}
				update {
					test* {
						b = global.a+1;
						a = b+1; 
					}
				}
			}
		}			
		'''.parse.assertError(CarmaPackage::eINSTANCE.actionGuard,CARMAValidator::ERROR_Dangerous_use_of_expressions,"Random expressions cannot be used in predicates!")
	}	
	

	@Test
	def void test_CheckExpressionsInInputActions_Recursive_Predicate(){
		'''
		fun int factorial( int n ) {
			if (n<=1) {
				return n;
			} else {
				return n*factorial( n-1 );
			}
		}
		
		component Test() {
			store {
				attrib a = 3;
				attrib b = my.a+1;
			}
			behaviour {
				A = test*< 2 >.A
					+
					test*[ a == factorial( my.a ) ]( x ).A;
			}
			init {
				A		
			}
		}
		
		system SystemTest {
			collective {
				new Test();
			}
			environment {
				store {
					attrib a = 2;
					attrib b = global.a+2;	
				}
				update {
					test* {
						b = global.a+1;
						a = b+1; 
					}
				}
			}
		}			
		'''.parse.assertError(CarmaPackage::eINSTANCE.actionGuard,CARMAValidator::ERROR_Dangerous_use_of_expressions,"Recursive functions cannot be used in predicates!")
	}	
	
	@Test
	def void test_CheckExpressionsInOutputActions_Recursive_Output(){
		'''
		fun int factorial( int n ) {
			if (n<=1) {
				return n;
			} else {
				return n*factorial( n-1 );
			}
		}
		
		component Test() {
			store {
				attrib a = 3;
				attrib b = my.a+1;
			}
			behaviour {
				A = test*< factorial( 1 ) >.A;
			}
			init {
				A		
			}
		}
		
		system SystemTest {
			collective {
				new Test();
			}
			environment {
				store {
					attrib a = 2;
					attrib b = global.a+2;	
				}
				update {
					test* {
						b = global.a+1;
						a = b+1; 
					}
				}
			}
		}			
		'''.parse.assertError(CarmaPackage::eINSTANCE.outputAction,CARMAValidator::ERROR_Dangerous_use_of_expressions,"Recursive functions cannot be used in output actions!")
	}	

	@Test
	def void test_CheckExpressionsInOutputActions_Recursive_Update(){
		'''
		fun int factorial( int n ) {
			if (n<=1) {
				return n;
			} else {
				return n*factorial( n-1 );
			}
		}
		
		component Test() {
			store {
				attrib a = 3;
				attrib b = my.a+1;
			}
			behaviour {
				A = test*< 0 >{ my.a = factorial( 2 ); }.A;
			}
			init {
				A		
			}
		}
		
		system SystemTest {
			collective {
				new Test();
			}
			environment {
				store {
					attrib a = 2;
					attrib b = global.a+2;	
				}
				update {
					test* {
						b = global.a+1;
						a = b+1; 
					}
				}
			}
		}			
		'''.parse.assertError(CarmaPackage::eINSTANCE.updateAssignment,CARMAValidator::ERROR_Dangerous_use_of_expressions,"Recursive functions cannot be used in attribute updates!")		
	}

	@Test
	def void test_CheckExpressionsInInputActions_Recursive_Update(){
		'''
		fun int factorial( int n ) {
			if (n<=1) {
				return n;
			} else {
				return n*factorial( n-1 );
			}
		}
		
		component Test() {
			store {
				attrib a = 3;
				attrib b = my.a+1;
			}
			behaviour {
				A = test*< 0 >.A
					+
					test*( x ){ a = factorial( 2 ) }.A;
				
			}
			init {
				A		
			}
		}
		
		system SystemTest {
			collective {
				new Test();
			}
			environment {
				store {
					attrib a = 2;
					attrib b = global.a+2;	
				}
				update {
					test* {
						b = global.a+1;
						a = b+1; 
					}
				}
			}
		}			
		'''.parse.assertError(CarmaPackage::eINSTANCE.updateAssignment,CARMAValidator::ERROR_Dangerous_use_of_expressions,"Recursive functions cannot be used in attribute updates!")		
	}
	@Test
	def void test_CheckExpressionsInProcessGuards_Use_of_Random(){
		'''
		fun int factorial( int n ) {
			if (n<=1) {
				return n;
			} else {
				return n*factorial( n-1 );
			}
		}
		
		component Test() {
			store {
				attrib a = 3;
				attrib b = my.a+1;
			}
			behaviour {
				A = [RND > 0.5]test*< 0 >{ my.a = factorial( 2 ); }.A;
			}
			init {
				A		
			}
		}
		
		system SystemTest {
			collective {
				new Test();
			}
			environment {
				store {
					attrib a = 2;
					attrib b = global.a+2;	
				}
				update {
					test* {
						b = global.a+1;
						a = b+1; 
					}
				}
			}
		}			
		'''.parse.assertError(CarmaPackage::eINSTANCE.processExpressionGuard,CARMAValidator::ERROR_Dangerous_use_of_expressions,"Random expressions cannot be used in process guards!")		
	}

	@Test
	def void test_CheckExpressionsInProcessGuards_Use_of_RecursiveFunctions(){
		'''
		fun int factorial( int n ) {
			if (n<=1) {
				return n;
			} else {
				return n*factorial( n-1 );
			}
		}
		
		component Test() {
			store {
				attrib a = 3;
				attrib b = my.a+1;
			}
			behaviour {
				A = [factorial(5) > 2]test*< 0 >{ my.a = factorial( 2 ); }.A;
			}
			init {
				A		
			}
		}
		
		system SystemTest {
			collective {
				new Test();
			}
			environment {
				store {
					attrib a = 2;
					attrib b = global.a+2;	
				}
				update {
					test* {
						b = global.a+1;
						a = b+1; 
					}
				}
			}
		}			
		'''.parse.assertError(CarmaPackage::eINSTANCE.processExpressionGuard,CARMAValidator::ERROR_Dangerous_use_of_expressions,"Recursive functions cannot be used in process guards!")		
	}
}