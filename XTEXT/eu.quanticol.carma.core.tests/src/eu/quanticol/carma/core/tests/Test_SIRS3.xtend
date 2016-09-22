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

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_SIRS3 {
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
        S = contact*[z == my.zone](z).I;
        I = contact*<zone>.I + recovery*.R;
		R = susceptible*.S;
		M = move*{zone := Mover(zone);}.M;
    }

    init{
        M|Z
    }
}

measure Susceptibles = #{Agent[S]| true };
measure Infectives 	= #{Agent[I]| true };
measure Recovereds 	= #{Agent[R]| true };

system Simple{

    collective{
    	
    	for(i; i < 5 ; i + 1){
    		new Agent(1:4,S);
    	}
    	for(i; i < 5 ; i + 1){
    		new Agent(1:4,I);
    	}
    	for(i; i < 5 ; i + 1){
    		new Agent(1:4,R);
    	}
    }

    environment{

        store{
        }

        prob{
			default { return 1.0; }
        }

        rate{
        	move* 		{ return 1.0; }
			contact* 	{ return 0.03; }
			recovery*	{ return 0.2; }
			susceptible* { return 0.2; }
			default { return 1.0; }
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
		class.classLoader.setJavaCompilerClassPath
		code.compile[ 
					var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )
			var m = o as CarmaModel
			assertEquals( 1 , m.systems.length )
			assertEquals( 3 , m.measures.length )					

	]
	}
	
}