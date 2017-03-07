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
class TestExpressions {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	@Inject extension ReflectExtensions

	@Test
	def void test_MinMaxValues(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = MAXINT;
		const C2 = MININT;
		const C3 = MAXREAL;
		const C4 = MINREAL;		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			var i1 =  o.get("__CONST__C1") as Integer
			var i2 =  o.get("__CONST__C2") as Integer
			var r1 =  o.get("__CONST__C3") as Double
			var r2 =  o.get("__CONST__C4") as Double
			assertEquals( Integer.MAX_VALUE , i1 )
			assertEquals( Integer.MIN_VALUE , i2 )
			assertEquals( Double.MAX_VALUE as Double , r1 )
			assertEquals( Double.MIN_VALUE as Double , r2 )
		]
	}	

	
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

	@Test
	def void test_Expression_02(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = [: 1 , 2 , 3 :];		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			var l =  o.get("__CONST__C1") as List<?>
			assertNotNull( l );
			assertEquals( 3 , l.size() )
			assertEquals( 1 , l.get(0) )
			assertEquals( 2 , l.get(1) )
			assertEquals( 3 , l.get(2) )
		]
	}	

	@Test
	def void test_Expression_03(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = {: 1 , 2 , 3 :};		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			var l =  o.get("__CONST__C1") as Set<?>
			assertNotNull( l );
			assertEquals( 3 , l.size() )
			assertTrue( l.contains(1) )
			assertTrue( l.contains(2) )
			assertTrue( l.contains(3) )
		]
	}

	@Test
	def void test_Expression_04(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = [: 1 , 2 , 3 :];		
		const C2 = exist( C1 , @>2 );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( true , o.get("__CONST__C2") as Boolean );
		]
	}	

	@Test
	def void test_Expression_05(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = {: 1 , 2 , 3 :};		
		const C2 = exist( C1 , @>2 );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( true , o.get("__CONST__C2") as Boolean );
		]
	}	
	
	@Test
	def void test_Expression_06(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = [: 1 , 2 , 3 :];		
		const C2 = forall( C1 , @>2 );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( false , o.get("__CONST__C2") as Boolean );
		]
	}	

	@Test
	def void test_Expression_07(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = {: 1 , 2 , 3 :};		
		const C2 = forall( C1 , @>2 );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( false , o.get("__CONST__C2") as Boolean );
		]
	}	

	@Test
	def void test_Expression_08(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = {: 1 , 2 , 3 :};		
		const C2 = map( C1 , -@ );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			var l =  o.get("__CONST__C2") as Set<?>
			assertNotNull( l );
			assertEquals( 3 , l.size() )
			assertTrue( l.contains(-1) )
			assertTrue( l.contains(-2) )
			assertTrue( l.contains(-3) )
		]
	}	
	
	@Test
	def void test_Expression_09(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = [: 1 , 2 , 3 :];		
		const C2 = map( C1 , -@ );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			var l =  o.get("__CONST__C2") as List<?>
			assertNotNull( l );
			assertEquals( 3 , l.size() )
			assertEquals( -1 , l.get(0) )
			assertEquals( -2 , l.get(1) )
			assertEquals( -3 , l.get(2) )
		]
	}	

	@Test
	def void test_Expression_10(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = {: 1 , 2 , 3 :};		
		const C2 = filter( C1 , @>2 );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			var l =  o.get("__CONST__C2") as Set<?>
			assertNotNull( l );
			assertEquals( 1 , l.size() )
			assertTrue( l.contains(3) )
		]
	}	
	
	@Test
	def void test_Expression_11(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = [: 1 , 2 , 3 :];		
		const C2 = filter( C1 , @>2 );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			var l =  o.get("__CONST__C2") as List<?>
			assertNotNull( l );
			assertEquals( 1 , l.size() )
			assertEquals( 3 , l.get(0) )
		]
	}	
	
	@Test
	def void test_Expression_12(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = [: 1 , 2 , 3 :];		
		const C2 = head ( C1 );
		const C3 = tail( C1 );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			var c =  o.get("__CONST__C2") as Integer
			assertEquals( 1 , c )
			var l =  o.get("__CONST__C3") as List<?>
			assertNotNull( l );
			assertEquals( 2 , l.size() )
			assertEquals( 2 , l.get(0) )
			assertEquals( 3 , l.get(1) )
		]
	}		

	@Test
	def void test_Expression_13(){
		class.classLoader.setJavaCompilerClassPath
		'''
		const C1 = [: 1 , 2 , 3 :];		
		const C2 = C1[0];
		const C3 = C1[1];		
		const C4 = C1[2];		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( 1 , o.get("__CONST__C2") as Integer )
			assertEquals( 2 , o.get("__CONST__C3") as Integer )
			assertEquals( 3 , o.get("__CONST__C4") as Integer )
		]
	}	
	
	@Test
	def void test_Function_1(){
		class.classLoader.setJavaCompilerClassPath
		'''
		fun int test( list<int> l ) {
			int x = l[0];
			l[0] = x+1;
			return x;
		}
			
		const C1 = [: 1 , 2 , 3 :];
		const C2 = test( C1 );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( 1 , o.get("__CONST__C2") as Integer )
			var l =  o.get("__CONST__C1") as List<?>
			assertNotNull( l );
			assertEquals( 2 , l.get(0) )
		]
	}	

	@Test
	def void test_Function_2(){
		class.classLoader.setJavaCompilerClassPath
		'''
		fun int test( list<list<int>> l ) {
			list<int> x = l[0];
			return x[0];
		}
			
		const C1 = [: [: 1 , 2 , 3 :] :];
		const C2 = test( C1 );		
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( 1 , o.get("__CONST__C2") as Integer )
		]
	}	

	@Test
	def void test_Function_3(){
		class.classLoader.setJavaCompilerClassPath
		'''
		fun int minValue( list<int> l ) {
			int v = MAXINT;
			for i in l {
				v = min( v , i );		
			}
			return v;
		}
			
		const C1 = [: 1 , 2 , 3 , 4 :];
		const C2 = minValue( C1 );
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( 1 , o.get("__CONST__C2") as Integer )
		]
	}	

	@Test
	def void test_Function_4(){
		class.classLoader.setJavaCompilerClassPath
		'''
		fun int maxValue( list<int> l ) {
			int v = MININT;
			for i in l {
				v = max( v , i );		
			}
			return v;
		}
			
		const C1 = [: 1 , 2 , 3 , 4 :];
		const C2 = maxValue( C1 );
		
		'''.compile[ 
			var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )			
			assertEquals( 4 , o.get("__CONST__C2") as Integer )
		]
	}		
}
