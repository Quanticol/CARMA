package eu.quanticol.carma.core.tests

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import org.junit.Test
import org.junit.runner.RunWith
import static extension org.junit.Assert.*
import eu.quanticol.carma.simulator.CarmaModel

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_List_Error {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
fun real getElementAt( list<list<real>> l , int i , int j ){
	return l[i][j];
}

space Grid (){
	universe <int x, int y>
	nodes {
		[0,0];
	}		
	connections{}
	areas{}
}

const DISTANCES = [: [:0.0, 1.0:], [:1.0, 1.0:] :];


component Robot(location myLocation){
	store{
		attrib location currentLocation := myLocation;	
	}
	behaviour{
		ReadyToMove = move*[false]<>{}.kill;
	}
	init{ReadyToMove}
}

measure RobotsAt00 = #{Robot[ReadyToMove] | my.currentLocation == [0,0]};

system Test{
	space Grid()
	
	collective{
		new Robot([0,0]);
	}
	environment{
		store{
			attrib list<list<real>> attributes := [: [:0.0, 1.0:], [:1.0, 1.0:] :];
		}
		prob{}
		weight{}
		rate{
// This getElementAt call works. DISTANCE defined as a global constant.
//		move* {return getElementAt(DISTANCES, 0, 0);}
// This getElementAt call throws a compile error. LinkedList cannot be resolved to a variable.
		move* {return getElementAt(global.attributes, 0, 0);}
			default {
				return 1.0;
			}
		}
		update{}
	}
}

	'''
	
	@Test
	def void test_Parser(){
		code.parse.assertNoErrors
	}

	@Test
	def void test_Compiler(){
		class.classLoader.setJavaCompilerClassPath
		code.compile[ 
					var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )
			var m = o as CarmaModel
			assertEquals( 2 , m.systems.length )
			assertEquals( 3 , m.measures.length )					

	]
	}
	
}