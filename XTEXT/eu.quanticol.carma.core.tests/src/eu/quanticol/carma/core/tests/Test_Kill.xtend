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

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_Kill {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
component Agent_R(){
	store{}
	behaviour{
		R1 = [3>2]r1*[true]<>.kill;
		}
	init{R1}
	}
		
measure c_R1  = 2+#{Agent_R[R1]|true};
measure c_R   = #{Agent_R[*]|true};

system Sys{
    collective{
    	new Agent_R();
        }
    environment{
        store{}
        prob{default { return 1.0;} }
        rate{default { return 0.5;} }
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
			val m = o as CarmaModel
			val samplings = 1000
			val deadline = 100.0
			val dt = deadline/samplings
			var statistics = m.measures.map[  new StatisticSampling<CarmaSystem>(samplings+1, dt,m.getMeasure(it)) ]
			var sim = new SimulationEnvironment( m.getFactory( "Sys" ) )
			sim.sampling  = new SamplingCollection( statistics )

			sim.simulate(deadline)
		]
	}
	
}