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
class Test_Ambulance {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
fun int Choice(){
    int ch := 0;
    real p := 0.0;
    p := RND;
    if (p <= 0.5){ch:=1;}else{ch:=2;}
    return ch;
}

component Gen(){
	store{}
	behaviour{
		G = create*[false]<>.G;
	}
	init{G}
}

component Agent(int v){
	store{
		attrib value := v;
	}
	behaviour{
		A = do*[false]<>{value:=2;}.B;
		B = dosomethingelse*[false]<>.A;
	}
	init{A}
}

measure countAgent = #{Agent[*]| true };
measure countA  = #{Agent[A]| true };
measure countB  = #{Agent[B]| true };

measure countA1 = #{Agent[A]| my.value == 1 };
measure countA2 = #{Agent[A]| my.value == 2 };
measure countB1 = #{Agent[B]| my.value == 1 };
measure countB2 = #{Agent[B]| my.value == 2 };
measure count1 = #{Agent[*]| my.value == 1 };
measure count2 = #{Agent[*]| my.value == 2 };

measure Count = global.c;

system simple{
    collective{
    	new Gen();
    }
    environment{
        store{
        	attrib c:=0;        	
        }
        prob{
			default { return 1.0; }
        }
        rate{
        	create* { return 0.5; }
         	do* { return 0.01; }
         	dosomethingelse* { return 0.01; }
			default { return 1.0; }
        }
        update{
        	create* { new Agent(Choice()); }
        	do* { 
        		if (sender.value==1) {
        			c:=global.c+1;
        		}
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
			var statistics = m.measures.map[  new StatisticSampling<CarmaSystem>(samplings+1, dt,m.getMeasure(it)) ]
			var sim = new SimulationEnvironment( m.getFactory( "simple" ) )
			sim.sampling  = new SamplingCollection( statistics )

			sim.simulate(deadline)
		]
	}
	
}