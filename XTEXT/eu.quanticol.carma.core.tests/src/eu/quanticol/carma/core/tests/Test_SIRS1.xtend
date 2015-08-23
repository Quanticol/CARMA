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
import eu.quanticol.carma.simulator.CarmaModel
import org.cmg.ml.sam.sim.SimulationEnvironment
import org.cmg.ml.sam.sim.sampling.StatisticSampling
import eu.quanticol.carma.simulator.CarmaSystem
import org.cmg.ml.sam.sim.sampling.SamplingCollection
import static extension org.junit.Assert.*

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_SIRS1 {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
fun int Mover(int zone) {
	if (zone == 4 || zone == 1) {
		return U(2,zone,3); 
	} else {
		return U(1,zone,4);
	}
}


component Agent(int a, process Z){

    store{
        attrib zone := a;
    }

    behaviour{
        S = contact*[z == my.zone](z).I +
			move*{zone := Mover(zone)}.S;
			
		I = contact*<zone>.I +
			move*{zone := Mover(zone)}.I +
			recovery*.R;
		
		R = susceptible*.S +
			move*{zone := Mover(zone)}.R;
    }

    init{
        Z
    }
}

//FIXME
measure Susceptibles = #{Agent[S]| true };
measure Infected = #{Agent[I]| true };
measure Recovered = #{Agent[R]| true };


system Simple{

    collective{
    	new Agent(1:4,S);
    	new Agent(1:4,I);
    	new Agent(1:4,R);
    }

    environment{

        store{
        }

        prob{
			default : 1.0;
        }

        rate{
        	[true] move* 		: 1.0;
			[true] contact* 	: 0.03;
			[true] recovery*	: 0.2;
			[true] susceptible* : 0.2;
			default : 1.0;
        }

        update{
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
		code.compile[ 
		var m = getCompiledClass.newInstance as CarmaModel
		assertEquals( 1 , m.systems.length )
		assertEquals( 3 , m.measures.length )
		var deadline = 100
		var sim = new SimulationEnvironment( m.getFactory( "Simple" ) )
		var stat1 = new StatisticSampling<CarmaSystem>(deadline+1, 1.0, m.getMeasure("Susceptibles") );
		var stat2 = new StatisticSampling<CarmaSystem>(deadline+1, 1.0, m.getMeasure("Infected") );
		var stat3 = new StatisticSampling<CarmaSystem>(deadline+1, 1.0, m.getMeasure("Recovered") );
		sim.sampling = new SamplingCollection( stat1 , stat2 , stat3 );
		sim.simulate(200,deadline)
		stat1.printTimeSeries(System.out)
		stat2.printTimeSeries(System.out)
		stat3.printTimeSeries(System.out)
	]
}
	
}