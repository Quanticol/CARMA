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

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class ModelParserTest {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	@Test
	def void test_Parser(){
	'''
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
        Sense     = sense*{data := data + 1}.Sense;
        Send     = [my.data > 0] send[type == 1]<1>{data := data - 1}.Send;
    }

    init{
        Sense|Send|Z
    }
}

component Satelite(real a, real b){

    store{
        attrib data := 0;
        attrib packge := 0;
        attrib type := 1;
        attrib myPosition := [ x := a, y := b ];

    }

    behaviour{

        Analyse = [my.data > 0] analyse*{data := data - 1, packge := packge + 1}.Transmit
        + [my.data == 0] sense*{data := data + 1}.Transmit;

        Transmit = [my.packge > 0] transmit*{packge := packge - 1}.Analyse;

        Receive = send(z){data := data + z}.Receive;
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
        Signal = [my.battery > 0] signal*{battery := battery - 1}.Signal + [my.battery <= 0] die*.nil;
    }

    init{
        Signal
    }
}

abstract {
    Rove = rove*{myPosition := Roving(my.myPosition)}.Wait;
    Wait = wait*.Wait;
}

measure Waiting[i := 0:2, j := 0:2 ] = #{ *  | my.myPosition.x == i && my.myPosition.y == j };

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

        prob{
            [(receiver.myPosition.x - global.center.x < 0) && (receiver.myPosition.y - global.center.y == 0)]	send : 1;
            [(receiver.myPosition.x - global.center.x > 1)]	send : 0.75;
            [(receiver.myPosition.x - global.center.x < 0)]	send : 0.5;
            default : 0.25;
        }

        rate{
        	[(sender.myPosition.x == 0)]	rove* : 6;
        	[(sender.myPosition.x == 1)]	rove* : 4;
        	[(sender.myPosition.x == 2)]	rove* : 5;
            [true]	sense* : #{ *  | sender.myPosition.x == my.myPosition.x && sender.myPosition.y == my.myPosition.y }/#{* | true};
            [true]	analyse* : 0.1;
            [true]	wait* : 3;
            [true]	signal* : 0.5;
            [true]	die* : 0.5;
            default : 0.25;
        }

        update{
            [true] send : reports := global.reports + 1;
            [sender.data > 5] sense* : new Beacon(sender.myPosition.x,sender.myPosition.y);
        }
    }
}

	'''.parse.assertNoErrors
	}

	@Test
	def void test_Compiler(){
	'''
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
        Sense     = sense*{data := data + 1}.Sense;
        Send     = [my.data > 0] send[type == 1]<1>{data := data - 1}.Send;
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

        Analyse = [my.data > 0] analyse*{data := data - 1, packge := packge + 1}.Transmit
        + [my.data == 0] sense*{data := data + 1}.Transmit;

        Transmit = [my.packge > 0] transmit*{packge := packge - 1}.Analyse;

        Receive = send(z){data := data + z}.Receive;
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
        Signal = [my.battery > 0] signal*{battery := battery - 1}.Signal + [my.battery <= 0] die*.nil;
    }

    init{
        Signal
    }
}

abstract {
    Rove = rove*{myPosition := Roving(my.myPosition)}.Wait;
    Wait = wait*.Wait;
}

measure Waiting[i := 0:2, j := 0:2 ] = #{ *  | my.myPosition.x == i && my.myPosition.y == j };

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

        prob{
            [(receiver.myPosition.x - global.center.x < 0) && (receiver.myPosition.y - global.center.y == 0)]	send : 1;
            [(receiver.myPosition.x - global.center.x > 1)]	send : 0.75;
            [(receiver.myPosition.x - global.center.x < 0)]	send : 0.5;
            default : 0.25;
        }

        rate{
        	[(sender.myPosition.x == 0)]	rove* : 6;
        	[(sender.myPosition.x == 1)]	rove* : 4;
        	[(sender.myPosition.x == 2)]	rove* : 5;
            [true]	sense* : #{ *  | sender.myPosition.x == my.myPosition.x && sender.myPosition.y == my.myPosition.y }/#{* | true};
            [true]	analyse* : 0.1;
            [true]	wait* : 3;
            [true]	signal* : 0.5;
            [true]	die* : 0.5;
            default : 0.25;
        }

        update{
            [true] send : reports := global.reports + 1;
            [sender.data > 5] sense* : new Beacon(sender.myPosition.x,sender.myPosition.y);
        }
    }
}


	'''.compile[ 
		getCompiledClass.newInstance 
		
	]
	}
	
}