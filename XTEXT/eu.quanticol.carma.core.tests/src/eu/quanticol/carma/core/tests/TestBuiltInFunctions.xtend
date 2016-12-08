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
import java.util.Map
import org.cmg.ml.sam.sim.RandomGeneratorRegistry

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class TestBuiltInFunctions {

		var model = new CarmaModel() {
			
			override getFactory(String name) {
				throw new UnsupportedOperationException("TODO: auto-generated method stub")
			}
			
			override getMeasure(String name, Map<String, Object> parameters) {
				throw new UnsupportedOperationException("TODO: auto-generated method stub")
			}
			
			override getMeasureParameters(String name) {
				throw new UnsupportedOperationException("TODO: auto-generated method stub")
			}
			
			override getMeasures() {
				throw new UnsupportedOperationException("TODO: auto-generated method stub")
			}
			
			override getParametersType(String name) {
				throw new UnsupportedOperationException("TODO: auto-generated method stub")
			}
			
			override getSystems() {
				throw new UnsupportedOperationException("TODO: auto-generated method stub")
			}
			
		}
	
	@Test
	def void test_Map_Set(){
		var set = newHashSet( 0 , 1 , 2 , 3 );
		val set2 = model.map(set,[x | -x]);
		assertEquals(4,set2.size());
		set.forEach[ assertTrue(set2.contains(-it) )]
	}

	@Test
	def void test_Map_List(){
		var set = newLinkedList( 0 , 1 , 2 , 3 );
		val set2 = model.map(set,[x | -x]);
		assertEquals(4,set2.size());
		set.forEach[ assertTrue(set2.contains(-it) )]
	}

	@Test
	def void test_Map_Set_Empty(){
		var set = newHashSet( );
		val set2 = model.map(set,[int x | -x]);
		assertEquals(0,set2.size());
		set.forEach[ assertTrue(set2.contains(-it) )]
	}

	@Test
	def void test_Map_List_Empty(){
		var set = newLinkedList( );
		val set2 = model.map(set,[int x | -x]);
		assertEquals(0,set2.size());
	}
	
	@Test
	def void test_Filter_Set(){
		val f = [ int x | x>0 ]
		var set = newHashSet( 0 , 1 , 2 , 3 , -1 , -2 , -3 );
		val set2 = model.filter(set,f);
		assertEquals(3,set2.size());
		set.forEach[ 
			if (f.apply(it)) {
				assertTrue( set2.contains(it) )			
			} else {
				assertFalse( set2.contains(it) )
			}
		]
	}

	@Test
	def void test_Filter_List(){
		val f = [ int x | x>0 ]
		var set = newLinkedList( 0 , 1 , 2 , 3 , -1 , -2 , -3 );
		val set2 = model.filter(set,f);
		assertEquals(3,set2.size());
		set.forEach[ 
			if (f.apply(it)) {
				assertTrue( set2.contains(it) )			
			} else {
				assertFalse( set2.contains(it) )
			}
		]
	}
	
	@Test
	def void test_Filter_Set2(){
		val f = [ int x | x>99 ]
		var set = newHashSet( 0 , 1 , 2 , 3 , -1 , -2 , -3 );
		val set2 = model.filter(set,f);
		assertEquals(0,set2.size());
		set.forEach[ 
			assertFalse( set2.contains(it) )
		]
	}

	@Test
	def void test_Filter_List2(){
		val f = [ int x | x>99 ]
		var set = newLinkedList( 0 , 1 , 2 , 3 , -1 , -2 , -3 );
		val set2 = model.filter(set,f);
		assertEquals(0,set2.size());
		set.forEach[ 
			assertFalse( set2.contains(it) )
		]
	}
	
	@Test
	def void test_Filter_Set3(){
		val f = [ int x | x>99 ]
		var set = newHashSet( );
		val set2 = model.filter(set,f);
		assertEquals(0,set2.size());
	}

	@Test
	def void test_Filter_List3(){
		val f = [ int x | x>99 ]
		var set = newLinkedList( );
		val set2 = model.filter(set,f);
		assertEquals(0,set2.size());
	}
	
	@Test
	def void test_Exists_Set(){
		val f = [ int x | x>0 ]
		var set = newHashSet( 0 , 1 , 2 , 3 , -1 , -2 , -3 );
		val value = model.exist(set,f);
		assertTrue(value)
	}

	@Test
	def void test_Exists_List(){
		val f = [ int x | x>0 ]
		var set = newLinkedList( 0 , 1 , 2 , 3 , -1 , -2 , -3 );
		val value = model.exist(set,f);
		assertTrue(value)
	}	

	@Test
	def void test_Exists_Set2(){
		val f = [ int x | x>99 ]
		var set = newHashSet( 0 , 1 , 2 , 3 , -1 , -2 , -3 );
		val value = model.exist(set,f);
		assertFalse(value)
	}

	@Test
	def void test_Exists_List2(){
		val f = [ int x | x>99 ]
		var set = newLinkedList( 0 , 1 , 2 , 3 , -1 , -2 , -3 );
		val value = model.exist(set,f);
		assertFalse(value)
	}		
	
	@Test
	def void test_ForAll_Set(){
		val f = [ int x | x>0 ]
		var set = newHashSet( 0 , 1 , 2 , 3 , -1 , -2 , -3 );
		val value = model.forall(set,f);
		assertFalse(value)
	}

	@Test
	def void test_ForAll_List(){
		val f = [ int x | x>0 ]
		var set = newLinkedList( 0 , 1 , 2 , 3 , -1 , -2 , -3 );
		val value = model.forall(set,f);
		assertFalse(value)
	}		

	@Test
	def void test_ForAll_Set2(){
		val f = [ int x | x>=0 ]
		var set = newHashSet( 0 , 1 , 2 , 3 );
		val value = model.forall(set,f);
		assertTrue(value)
	}

	@Test
	def void test_ForAll_List2(){
		val f = [ int x | x>=0 ]
		var set = newLinkedList( 0 , 1 , 2 , 3 );
		val value = model.forall(set,f);
		assertTrue(value)
	}		

	@Test
	def void test_ForAll_Set3(){
		val f = [ int x | x>0 ]
		var set = newHashSet( );
		val value = model.forall(set,f);
		assertTrue(value)
	}

	@Test
	def void test_ForAll_List3(){
		val f = [ int x | x>0 ]
		var set = newLinkedList( );
		val value = model.forall(set,f);
		assertTrue(value)
	}	
	
	@Test
	def void test_Select_List(){
		val size = 1000000
		val f = [ int x | x as double ]
		var set = newLinkedList( 1 , 2 , 3 , 4 )
		val frequency = newArrayList(0.0,0.0,0.0,0.0)
		for( var i=0 ; i<size ; i++ ) {
			var selected = RandomGeneratorRegistry.select(set,f) 
			frequency.set(selected-1,frequency.get(selected-1)+1)
		}
		frequency.forEach[v, i|
			assertEquals((i+1.0)/10,frequency.get(i)/size,0.01)						
		]
	}		

	@Test
	def void test_Select_Set(){
		val size = 1000000
		val f = [ int x | x as double ]
		var set = newHashSet( 1 , 2 , 3 , 4 )
		val frequency = newArrayList(0.0,0.0,0.0,0.0)
		for( var i=0 ; i<size ; i++ ) {
			var selected = RandomGeneratorRegistry.select(set,f) 
			frequency.set(selected-1,frequency.get(selected-1)+1)
		}
		frequency.forEach[v, i|
			assertEquals((i+1.0)/10,frequency.get(i)/size,0.01)						
		]
	}		

	
	
}