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

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_SIRS3 {
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
        S = contact*[z == my.zone](z).I;
        I = contact*<zone>.I + recovery*.R;
		R = susceptible*.S;
		M = move*{zone := Mover(zone)}.M;
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
        S = contact*[z == my.zone](z).I;
        I = contact*<zone>.I + recovery*.R;
		R = susceptible*.S;
		M = move*{zone := Mover(zone)}.M;
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
'''.compile[ getCompiledClass.newInstance ]
	}
	
}