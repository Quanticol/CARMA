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

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_SIRS1 {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	@Test
	def void test_Parser(){
	'''
fun int Mover(int zone) = ((zone == 4 || zone == 1) ? U(2,zone,3) : U(1,zone,4));


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
	'''.parse.assertNoErrors
	}

	@Test
	def void test_Compiler(){
	'''
fun int Mover(int zone) = ((zone == 4 || zone == 1) ? U(2,zone,3) : U(1,zone,4));


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
'''.compile[ 
		var m = getCompiledClass.newInstance as CarmaModel
		var deadline = 100
		var sim = new SimulationEnvironment( m.getFactory( "Simple" ) )
		var stat = new StatisticSampling<CarmaSystem>(deadline+1, 1.0, m.getMeasure("Susceptibles") );
		sim.sampling = stat
		sim.simulate(200,deadline)
		stat.printTimeSeries(System.out)
	]
}
	
}