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

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_UseSet {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension MyCompilationTestHelper
	
	CharSequence code = 	'''
fun int test( ) {
	set<int> aSet := {: 1 :};
	set<int> bSet := {: 2 :};
	set<int> cSet := aSet || bSet;
	set<int> dSet := aSet && bSet;
	set<int> eSet := cSet - dSet;
	int x := 2;
	int y := 3;
	if (x+1==y) {
		aSet := {: 2 :};
	}
	if (1 in aSet) {
		return 0;
	} else {
		return 1;
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
		]
	}
	
	
}