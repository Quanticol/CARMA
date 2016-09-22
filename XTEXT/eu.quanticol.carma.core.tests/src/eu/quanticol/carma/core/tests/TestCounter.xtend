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
class Test_Counter {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
component Agent(){
	store{
		attrib localCounter := 0;
		attrib fire := 0.0;
	}
	
	behaviour{
		A = [localCounter < 10] step*{ localCounter := localCounter+1 ; fire := now ; }.A;
	}
	
	init{
		A
	}
}

measure AgentsUnder5 = #{ * | my.localCounter < 5 };
measure AgentsOver5 = #{ * | my.localCounter >= 5 };

measure Global = global.globalCounter;
measure FireTime = global.lastFire;

system Simple{
    collective{
    	for( i;i<10;1) {
    		new Agent();
    	} 
    }

    environment{
    	
    	store {
    		attrib globalCounter := 0;
    		attrib lastFire := 0.0;
    	}
    	
    	prob{
    		default { return 1.0; }
    	}
	    	
	    rate{
	    		default { return 1.0; }
	    }
	    
	    update {
	    	step* { 
	    		globalCounter := global.globalCounter+1;
	    		lastFire := now; 
			}
	    }
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
			val dt = 1
			val deadline = samplings*dt
//			var statistics = m.measures.map[  new StatisticSampling<CarmaSystem>(samplings+1, dt,m.getMeasure(it)) ]
			var statistics = new StatisticSampling<CarmaSystem>(samplings+1,dt,m.getMeasure("FireTime"))
			var sim = new SimulationEnvironment( m.getFactory( "Simple" ) )
			sim.sampling  = new SamplingCollection( statistics )

			sim.simulate(deadline)
			var data = sim.timeSeries
			data.forEach[ it.printTimeSeries(System::out)]			
			
		]
	}
	
}