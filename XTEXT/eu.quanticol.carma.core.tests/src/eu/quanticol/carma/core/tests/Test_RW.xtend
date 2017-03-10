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
import org.cmg.ml.sam.sim.sampling.StatisticSampling
import eu.quanticol.carma.simulator.CarmaSystem
import org.cmg.ml.sam.sim.SimulationEnvironment
import org.cmg.ml.sam.sim.sampling.SamplingCollection

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_RW {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''

fun location next( location l ) {
	return U( l.post );
}


component A(){
	store{ 
		location dest = none;
	}
	behaviour{
		S = [!my.loc.goal]choose*[false]<>{ my.dest := next( my.loc ); }.M;
		M = move*[false]<>{my.loc := my.dest; }.S;
	}
	init{S}
}

space Grid (int width, int height) { 
	universe <int x, int y>
        nodes {
            for i from 0 to width{
                for j from 0 to height{
                    [i, j];
                } 
             }
        }
        connections {
            for i from 0 to width{
                for j from 0 to height{
                       [i, j] <-> [i+1,j] { w=1.0 };
                       [i, j] <-> [i,j+1] { w=1.0 }; 
                }
            } 
        }
        areas {
            corner { [0,0];[ width-1,0];[0,height-1]; [width-1,height-1];}
            border {for r from 0 to width 
            	        { [r,0]; [r,height-1]; }
                    for c from 0 to height 
                        { [0,c];[ width-1,c ] ; 
                        }
}
                        goal { [0,0]; }            
}
}

measure NumberAt( int x , int y ) = #{ * | my.loc == [x,y] };

measure Distance = global.distance;

system basic{
	space Grid (5,5)	
	collective{new A()@[4,4]<1>;}
    environment{
    store{ real distance = 0.0; }
    prob {default {return 1.0;}}
    weight { default {return 1.0;}}
    rate{ default {return 0.2;}}
    update{
    		move* {
    			distance = distance + U( edgeValues( sender.loc , w , sender.dest ) );
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
			val samplings = 100
			val dt = 1
			val deadline = samplings*dt
			var statistics = new StatisticSampling<CarmaSystem>(samplings+1, dt,m.getMeasure("Distance")) 
//			var statistics = new StatisticSampling<CarmaSystem>(samplings+1,dt,m.getMeasure("FireTime"))
			var sim = new SimulationEnvironment( m.getFactory( "basic" ) )
			sim.sampling  = new SamplingCollection( statistics )

			sim.simulate(deadline)
			var data = sim.timeSeries
			data.forEach[ it.printTimeSeries(System::out)]			

	]
	}
	
}