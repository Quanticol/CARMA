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
import eu.quanticol.carma.simulator.CarmaSystem
import org.cmg.ml.sam.sim.sampling.StatisticSampling
import org.cmg.ml.sam.sim.sampling.SamplingCollection
import org.cmg.ml.sam.sim.SimulationEnvironment
import org.cmg.ml.sam.sim.sampling.SamplingFunction
import eu.quanticol.carma.simulator.CarmaPredicate
import eu.quanticol.carma.simulator.CarmaStore
import java.util.HashSet
import org.eclipse.xtext.xbase.compiler.GeneratorConfig
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions
import eu.quanticol.carma.simulator.space.SpaceModel
import eu.quanticol.carma.simulator.space.Tuple

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
   universe <District name>
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
           source {Maseru;}
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