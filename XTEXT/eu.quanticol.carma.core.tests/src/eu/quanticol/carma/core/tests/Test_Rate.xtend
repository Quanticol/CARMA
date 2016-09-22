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
class Test_Rate {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
//simulation parameters used: time 10, replications 1000, samplings 100

//If there is only 1 A_Agent, then regardless of the choice of rates, 
//the action a* is a very fast action, and the result is a sharp drop to zero

//If there are both A_Agent and B_Agent, then there are different outcomes
//depending on the rates chosen
//
//only _[true] a* : 0.001_ 
//   concave curve that meets x-axis between 5 and 6
//only _default : 0.001_
//   sharp drop to 0.49 then almost level
//both _[true] a* : 0.01_ and _default : 0.001_
//   sharp drop to 0.49 then almost level
//only _default : 100_ 
//   sharp drop to zero
//both _[true] a* : 0.001_ and _default : 100_
//   sharp drop to zero

component A_Agent(){
	store {
		attrib test := 0;
	}
	behaviour {A = a*[false]<>{ test := 1; }.nil;
	}
	init{A}
}

component B_Agent(){
	store {}
	behaviour {B = b*[false]<>.nil;
	}
	init{B}
}

measure c_A  = #{A_Agent[A]|true};
measure c_A_0  = #{A_Agent[*]|my.test == 0};
measure c_A_1  = #{A_Agent[*]|my.test == 1};

system Sys{
    collective{
    	new A_Agent();
    	//new B_Agent();
   }
    environment{
        store{}
        prob{ default { return 1.0;} }
        rate{ a* { return 0.01; }
        	 //default : 100;
        	 default { return 10.0; }
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