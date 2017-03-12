package eu.quanticol.carma.core.tests

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.simulator.CarmaModel
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class TestSpace2 {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension MyCompilationTestHelper
	@Inject extension ReflectExtensions
	
	CharSequence code = 	'''
fun list<int> example() {
   list<int> x := [:0, 1, 2:];
   return x;
}

fun int example2() {
   list<int> x := [:0, 1, 2:];
   return x[2];
}

enum District = Berea, Leribe, Maseru;
space Lesotho () {
       nodes {
           Berea;
           Leribe;
           Maseru ;
       }
       connections {
           Berea <-> Leribe {w=2.0};
           Berea <-> Maseru {w=1.0};
       }
       areas {
           starting {Maseru;}
           goal {Berea;}
       }
}
//measure NumberAt(District d) = #{*|(my.loc.name == d)};
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
		]
	}
	
	
}