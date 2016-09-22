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
class Test_Now {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
component Test_Agent(){
	store {}
	behaviour {
		S0 = [now>5.0]hello_there*[true]<>.nil
			 +
			 [now<=5.0]wait*[true]<>.S0;
		S1 = [now>5.0]hello[true]().nil
			 +
			 [now<=5.0]wait*[true]<>.S1;
		S2 = hello[now>5.0]().nil;
		S3 = hello_hello[now>5.0]<>.nil;
	}
	//init{S0}	// gives an error: "An internal error occurred during: "Simulation".
                // Unresolved compilation problem: now cannot be resolved to a variable
	
    init{S1}	// gives an error: "An internal error occurred during: "Simulation".
                // Unresolved compilation problem: now cannot be resolved to a variable
                
    //init{S2}  // works as expected: 1 Test_Agent[S2] before 5, and none after
    
    //init{S3}  // ?
                // 
}


component Comm_Agent(){
	store{}
	behaviour{A = hello[true]<>.nil;}
	init{A}
}

component Comm_Agent1(){
	store{}
	behaviour{B = hello_hello[true]().nil;}
	init{B}
}

measure TA_0  = #{Test_Agent[S0]|true};
measure TA_1  = #{Test_Agent[S1]|true};
measure TA_2  = #{Test_Agent[S2]|true};
measure TA_3  = #{Test_Agent[S3]|true};
measure gCount = global.g;

system Sys{
    collective{
    	new Test_Agent();
    	new Comm_Agent();
        new Comm_Agent1();
    	
    }
    environment{
        store{attrib g := 0;}
        prob{default { return 1.0;} }
        rate{default { return 100.0; } }
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
			val samplings = 20
			val deadline = 10.0
			val dt = deadline/samplings
			var statistics = m.measures.map[  new StatisticSampling<CarmaSystem>(samplings+1, dt,m.getMeasure(it)) ]
			var sim = new SimulationEnvironment( m.getFactory( "Sys" ) )
			sim.sampling  = new SamplingCollection( statistics )

			sim.simulate(deadline)
		]
	}
	
}