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
import static extension org.junit.Assert.*
import eu.quanticol.carma.simulator.CarmaModel

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class ModelParserTest {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
		var code = '''
record Position = [ int x, int y ];

fun Position Roving(Position p) {
	if (p.x > 0) {
		return [ x := 0, y := 1 ] ;		
	} else {
		return [ x := 1 , y := 0 ];
	}
}

component Rover(int a, int b,  process Z){

    store{
        attrib data := 0;
        attrib type := 0;
        attrib myPosition := [ x := a, y := b ];

    }

    behaviour{
        Sense     = sense*{data := data + 1;}.Sense;
        Send     = [my.data > 0] send[type == 1]<1>{data := data - 1;}.Send;
    }

    init{
        Sense|Send|Z
    }
}

component Satelite(int a, int b){

    store{
        attrib data := 0;
        attrib packge := 0;
        attrib type := 1;
        attrib myPosition := [ x := a, y := b ];

    }

    behaviour{

        Analyse = [my.data > 0] analyse*{data := data - 1; packge := packge + 1;}.Transmit
        + [my.data == 0] sense*{data := data + 1;}.Transmit;

        Transmit = [my.packge > 0] transmit*{packge := packge - 1;}.Analyse;

        Receive = send(z){data := data + z;}.Receive;
    }

    init{
        Analyse|Receive
    }
}

component Beacon(int a, int b){

    store{
        attrib myPosition := [ x := a, y := b ];
        attrib battery := 5;
    }

    behaviour{
        Signal = [my.battery > 0] signal*{battery := battery - 1;}.Signal + [my.battery <= 0] die*.nil;
    }

    init{
        Signal
    }
}

abstract {
    Rove = rove*{myPosition := Roving(my.myPosition);}.Wait;
    Wait = wait*.Wait;
}

measure Waiting( int i , int j ) = #{ *  | my.myPosition.x == i && my.myPosition.y == j };

system Simple{

    collective{
    	new Satelite( 0:2 , 0:2 );
    	for(i; i < 2; i + 1){
    		for(j; j< 2;j+1) {
    			if (i != j) {
	    			new Rover( i , j , Rove );
	    		}
    		}
    	}
    }

    environment{

        store{
            attrib reports  := 0;
            attrib type     := 2;
            attrib center := [ x := 1, y := 1 ];
        }

        weight {
        	send {
            	if ((receiver.myPosition.x - global.center.x < 0) && (receiver.myPosition.y - global.center.y == 0)) {
            		return 1;
            	}
            	if ((receiver.myPosition.x - global.center.x > 1)) {
            		return 0.75;
            	}
            	if ((receiver.myPosition.x - global.center.x < 0)) {
            		return 0.5;
            	}
            	return 0.25;
        	}
            default { return 0.25; }
        }

        rate{
        	rove* {
        		if ((sender.myPosition.x == 0)) {
        			return 6;
        		}
        		if ((sender.myPosition.x == 1)) {
        			return 4;
        		}
        		if ((sender.myPosition.x == 2)) {
        			return 5;
        		}
        		return 0.25;
        	}
            sense* { return #{ *  | sender.myPosition.x == my.myPosition.x && sender.myPosition.y == my.myPosition.y }/#{* | true}; }
            analyse* { return 0.1; }
            wait* { return 3; }
            signal* { return 0.5; }
            die* { return 0.5; }
            default { return 0.25; }
        }

        update{
            send { reports := global.reports + 1; }
            sense* { 
	            	if (sender.data > 5) { 
	            		new Beacon(sender.myPosition.x,sender.myPosition.y);
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
			var m = o as CarmaModel
			assertEquals( 1 , m.systems.length )
			assertEquals( 1 , m.measures.length )					

	]
	}
	
}