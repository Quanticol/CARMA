package eu.quanticol.carma.core.typing

import static extension org.eclipse.xtext.EcoreUtil2.*

import eu.quanticol.carma.core.carma.Or
import eu.quanticol.carma.core.carma.And
import eu.quanticol.carma.core.carma.Equality
import eu.quanticol.carma.core.carma.DisEquality
import eu.quanticol.carma.core.carma.Less
import eu.quanticol.carma.core.carma.LessOrEqual
import eu.quanticol.carma.core.carma.GreaterOrEqual
import eu.quanticol.carma.core.carma.Greater
import eu.quanticol.carma.core.carma.Subtraction
import eu.quanticol.carma.core.carma.Addition
import eu.quanticol.carma.core.carma.Multiplication
import eu.quanticol.carma.core.carma.Division
import eu.quanticol.carma.core.carma.Modulo
import eu.quanticol.carma.core.carma.Not
import eu.quanticol.carma.core.carma.UnaryPlus
import eu.quanticol.carma.core.carma.UnaryMinus
import eu.quanticol.carma.core.carma.ValueType
import eu.quanticol.carma.core.carma.ProcessType
import eu.quanticol.carma.core.carma.IntegerType
import eu.quanticol.carma.core.carma.RealType
import eu.quanticol.carma.core.carma.BooleanType
import eu.quanticol.carma.core.carma.CustomType
import eu.quanticol.carma.core.carma.ReferenceableType
import eu.quanticol.carma.core.carma.EnumDefinition
import eu.quanticol.carma.core.carma.RecordDefinition
import eu.quanticol.carma.core.carma.AttributeDeclaration
import eu.quanticol.carma.core.carma.FunctionDefinition
import eu.quanticol.carma.core.carma.EnumCase
import eu.quanticol.carma.core.carma.ConstantDefinition
import eu.quanticol.carma.core.carma.RecordAccess
import eu.quanticol.carma.core.carma.AtomicTrue
import eu.quanticol.carma.core.carma.AtomicFalse
import eu.quanticol.carma.core.carma.AtomicInteger
import eu.quanticol.carma.core.carma.AtomicReal
import eu.quanticol.carma.core.carma.AtomicRecord
import eu.quanticol.carma.core.carma.FieldDefinition
import eu.quanticol.carma.core.carma.AtomicNow
import eu.quanticol.carma.core.carma.SetComp
import eu.quanticol.carma.core.carma.AtomicRnd
import eu.quanticol.carma.core.carma.AtomicPi
import eu.quanticol.carma.core.carma.AtomicExp
import eu.quanticol.carma.core.carma.MyContext
import eu.quanticol.carma.core.carma.ReceiverContext
import eu.quanticol.carma.core.carma.SenderContext
import eu.quanticol.carma.core.carma.GlobalContext
import eu.quanticol.carma.core.carma.AbsFunction
import eu.quanticol.carma.core.carma.AcosFunction
import eu.quanticol.carma.core.carma.AsinFunction
import eu.quanticol.carma.core.carma.AtanFunction
import eu.quanticol.carma.core.carma.Atan2Function
import eu.quanticol.carma.core.carma.CbrtFunction
import eu.quanticol.carma.core.carma.CeilFunction
import eu.quanticol.carma.core.carma.CosFunction
import eu.quanticol.carma.core.carma.ExpFunction
import eu.quanticol.carma.core.carma.FloorFunction
import eu.quanticol.carma.core.carma.LogFunction
import eu.quanticol.carma.core.carma.Log10Function
import eu.quanticol.carma.core.carma.MaxFunction
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.PowFunction
import eu.quanticol.carma.core.carma.SinFunction
import eu.quanticol.carma.core.carma.SqrtFunction
import eu.quanticol.carma.core.carma.TanFunction
import eu.quanticol.carma.core.carma.UniformFunction
import eu.quanticol.carma.core.carma.IfThenElseExpression
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.UntypedVariable
import eu.quanticol.carma.core.carma.InputAction
import java.util.List
import eu.quanticol.carma.core.carma.Activity
import eu.quanticol.carma.core.carma.Variable
import eu.quanticol.carma.core.carma.BasicType
import eu.quanticol.carma.core.carma.Reference
import eu.quanticol.carma.core.carma.ComponentDefinition
import eu.quanticol.carma.core.carma.Environment
import eu.quanticol.carma.core.carma.MaxMeasure
import eu.quanticol.carma.core.carma.MinMeasure
import eu.quanticol.carma.core.carma.AverageMeasure
import org.eclipse.emf.ecore.EObject
import com.google.inject.Inject
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.carma.IterationVariable
import eu.quanticol.carma.core.carma.FieldAssignment
import eu.quanticol.carma.core.carma.TargetAssignmentVariable
import eu.quanticol.carma.core.carma.TargetAssignmentField
import eu.quanticol.carma.core.carma.CastToReal
import eu.quanticol.carma.core.carma.CastToInteger
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.ProcessState
import eu.quanticol.carma.core.carma.MeasureVariableDeclaration

class TypeSystem {

	@Inject extension Util
	
	def  dispatch CarmaType typeOf( EObject e ) {
		CarmaType::ERROR_TYPE	
	}
	
	def  dispatch CarmaType typeOf( Or e ) {
		CarmaType::BOOLEAN_TYPE
	}		
	
	def  dispatch CarmaType typeOf( And e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def  dispatch CarmaType typeOf( Equality e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def  dispatch CarmaType typeOf( DisEquality e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def  dispatch CarmaType typeOf( Less e ) {
		CarmaType::BOOLEAN_TYPE
	}
	
	def  dispatch CarmaType typeOf( LessOrEqual e ) {
		CarmaType::BOOLEAN_TYPE
	}
	
	def  dispatch CarmaType typeOf( GreaterOrEqual e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def  dispatch CarmaType typeOf( Greater e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def  dispatch CarmaType typeOf( Subtraction e ) {
		e.left.typeOf.mostGeneral( e.right.typeOf ) 
	}

	def  dispatch CarmaType typeOf( Addition e ) {
		e.left.typeOf.mostGeneral( e.right.typeOf )
	}

	def  dispatch CarmaType typeOf( Multiplication e ) {
		e.left.typeOf.mostGeneral( e.right.typeOf )
	}

	def  dispatch CarmaType typeOf( Division e ) {
		e.left.typeOf.mostGeneral( e.right.typeOf )
	}

	def  dispatch CarmaType typeOf( Modulo e ) {
		CarmaType::INTEGER_TYPE
	}

	def  dispatch CarmaType typeOf( Not e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def  dispatch CarmaType typeOf( UnaryPlus e ) {
		e.expression.typeOf
	}
	
	def  dispatch CarmaType typeOf( UnaryMinus e ) {
		e.expression.typeOf
	}
	
	def  dispatch CarmaType typeOf( IfThenElseExpression e ) {
		e.thenBranch.typeOf.mostGeneral( e.elseBranch.typeOf )
	}
	
	def  dispatch CarmaType typeOf( Reference e ) {
		e.reference.typeOf
	}
	
	def dispatch CarmaType typeOf( IterationVariable v ) {
		CarmaType::INTEGER_TYPE
	}
	
	def dispatch CarmaType typeOf( TargetAssignmentVariable v ) {
		v.variable.typeOf
	}

	def dispatch CarmaType typeOf( TargetAssignmentField f ) {
		f.field.typeOf
	}
	
	
	def dispatch CarmaType typeOf( FieldAssignment f ) {
		f.field.typeOf
	}
	
	def  dispatch CarmaType typeOf( Variable v ) {
		if (v.type != null) {
			v.type.toCarmaType		
		} else {
			CarmaType::ERROR_TYPE
		}
	}

	def dispatch CarmaType typeOf( CastToReal e ) {
		CarmaType::REAL_TYPE
	}

	def dispatch CarmaType typeOf( CastToInteger e ) {
		CarmaType::INTEGER_TYPE
	}

	def  dispatch CarmaType typeOf( AttributeDeclaration a ) {
		var model = a.getContainerOfType(typeof(Model))
		if (model != null) {
			if (a.getContainerOfType(typeof(ComponentDefinition)) != null) {
				//This is an attribute declared in a component				
				model.getComponentAttributeType( a.name )
			} else {
				if (a.getContainerOfType(typeof(Environment)) != null) {
					model.getGlobalAttributeType( a.name )
				} else {	
					CarmaType::ERROR_TYPE			
				}
			}
		} else {
			CarmaType::ERROR_TYPE			
		}
	}

	def  dispatch CarmaType typeOf( FunctionDefinition f ) {
		if (f.type != null) {
			f.type.toCarmaType
		} else {
			CarmaType::ERROR_TYPE
		}
	}

	def  dispatch CarmaType typeOf( EnumCase ec ) {
		var c = ec.getContainerOfType(typeof(EnumDefinition))
		if (c != null) {
			CarmaType::createEnumType(c)
		} else {
			CarmaType::ERROR_TYPE
		}
	}
	
	def  dispatch CarmaType typeOf( ConstantDefinition c ) {
		if (c.value!=null) {
			c.value.typeOf
		} else {
			CarmaType::ERROR_TYPE
		}
	}
	
	def dispatch CarmaType typeOf( MeasureVariableDeclaration v ) {
		CarmaType::INTEGER_TYPE
	}
	
	def  dispatch CarmaType typeOf( UntypedVariable v ) {
		v.inferTypeOf
	}
	
	
	def  dispatch CarmaType typeOf( RecordAccess e ) {
		e.field.typeOf
	}

	def dispatch CarmaType typeOf( FieldDefinition f ) {
		f.fieldType.toCarmaType 
	}
	
	def  dispatch CarmaType typeOf( AtomicTrue e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def  dispatch CarmaType typeOf( AtomicFalse e ) {
		CarmaType::BOOLEAN_TYPE
	}
	
	def  dispatch CarmaType typeOf( AtomicInteger e ) {
		CarmaType::INTEGER_TYPE
	}
	
	def  dispatch CarmaType typeOf( AtomicReal e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( AtomicRecord e ) {
		e.fields.head ?. field ?. recordType ?: CarmaType::ERROR_TYPE
	}
	
	def  dispatch CarmaType typeOf( AtomicNow e ) {
		CarmaType::REAL_TYPE
	}
	
	def  dispatch CarmaType typeOf( AtomicRnd e ) {
		CarmaType::REAL_TYPE
	}
	
	def  dispatch CarmaType typeOf( AtomicPi e ) {
		CarmaType::REAL_TYPE
	}
	
	def  dispatch CarmaType typeOf( AtomicExp e ) {
		CarmaType::REAL_TYPE
	}
	
	def  dispatch CarmaType typeOf( SetComp e ) {
		CarmaType::INTEGER_TYPE
	}

	def  dispatch CarmaType typeOf( MyContext e ) {
		e.reference.typeOf
	}
		
	def  dispatch CarmaType typeOf( ReceiverContext e ) {
		e.reference.typeOf
	}

	def  dispatch CarmaType typeOf( SenderContext e ) {
		e.reference.typeOf
	}

	def  dispatch CarmaType typeOf( GlobalContext e ) {
		e.reference.typeOf
	}

	def  dispatch CarmaType typeOf( AbsFunction e ) {
		e.arg.typeOf
	}

	def  dispatch CarmaType typeOf( Range e ) {
		CarmaType::INTEGER_TYPE
	}

	def  dispatch CarmaType typeOf( AcosFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( AsinFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( AtanFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( Atan2Function e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( CbrtFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( CeilFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( CosFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( ExpFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( FloorFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( LogFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( Log10Function e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( MaxFunction e ) {
		e.first ?. typeOf.mostGeneral( e.second.typeOf )
	}

	def  dispatch CarmaType typeOf( MaxMeasure e ) {
		e.value.typeOf
	}

	def  dispatch CarmaType typeOf( MinMeasure e ) {
		e.value.typeOf
	}

	def  dispatch CarmaType typeOf( AverageMeasure e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( MinFunction e ) {
		e.first ?. typeOf.mostGeneral( e.second.typeOf )
	}

	def  dispatch CarmaType typeOf( PowFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( SinFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( SqrtFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( TanFunction e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( ProcessState e ) {
		CarmaType::PROCESS_TYPE
	}

	def  dispatch CarmaType typeOf( UniformFunction e ) {
		e.args.head.typeOf
	}

	def  CarmaType toCarmaType( ValueType t ) {
		switch t {
			ProcessType: CarmaType::PROCESS_TYPE
			IntegerType: CarmaType::INTEGER_TYPE
			RealType: CarmaType::REAL_TYPE
			BooleanType: CarmaType::BOOLEAN_TYPE
			CustomType: t.reference.toCarmaType
		}	
	}
	
	def  CarmaType toCarmaType( ReferenceableType ref ) {
		switch ref {
			EnumDefinition: CarmaType::createEnumType(ref)
			RecordDefinition: CarmaType::createRecordType(ref)
		}
	}
	
	def  CarmaType getRecordType( FieldDefinition f ) {
		var v = f ?. getContainerOfType(typeof(RecordDefinition))
		if (v != null) {
			CarmaType::createRecordType(v)
		} else {
			CarmaType::ERROR_TYPE
		}
	}
	
	def  combine( Iterable<CarmaType> l1 , Iterable<CarmaType> l2 ) {
		if (l1 != null) {
			if (l1.length != l2.length) {
				newLinkedList( CarmaType::ERROR_TYPE )
			} else {
				val result = newLinkedList()
				l1.indexed.forEach[ result.add( it.value.mostGeneral( l2.get(it.key)) ) ]
				result
			}	
		} else {
			l2
		}
	}
	
	def  inferTypeOf( UntypedVariable v ) {
		var act = v.getContainerOfType(typeof(InputAction))	
		if (act != null) {
			var actType = act.activity ?. inferActivityType
			if (actType!=null) {
				var vidx = act.parameters.indexOf(v)
				if (vidx < actType.size) {
					actType.get(vidx)
				} else {
					CarmaType::ERROR_TYPE			
				}
			} else {
				CarmaType::ERROR_TYPE
			}
		} else {
			CarmaType::ERROR_TYPE			
		}	
	}
	
	def  extractType( List<BasicType> types , int idx ) {
		if (( types == null )||( idx < 0 )||(types.size<=idx)) {
			CarmaType::ERROR_TYPE
		} else {
			types.get(idx).toCarmaType
		}
	}

	def  getComponentAttributeType( Model m , String name ) {
		var attributes = m.allAttributes.filter[it.name == name]
		attributes.fold(null,[ t,a | if (t==null) { a.value.typeOf } else { t.mostGeneral(a.value.typeOf)} ])		
	}

	def  getGlobalAttributeType( Model m , String name ) {
		var attributes = m.allGlobalAttributes.filter[it.name == name]
		attributes.fold(null,[ t,a | if (t==null) { a.value.typeOf } else { t.mostGeneral(a.value.typeOf)} ])		
	}
	
	def  inferActivityType( Activity a ) {
		var m = a.getContainerOfType(typeof(Model))
		if (m != null) {
			m.getMessages(a).map[
				it.map[it.typeOf]	
			].fold( null , [r,t| r.combine( t ) ] )	?: newLinkedList( CarmaType::ERROR_TYPE )	
		} else {
			newLinkedList( CarmaType::ERROR_TYPE )
		}	
	}
	
		
	def toJavaType( CarmaType t ) {
		switch (t.code) {
			case BOOLEAN: "Boolean"
			case INTEGER: "Integer"
			case REAL: "Double"
			case PROCESS: "CarmaProcessAutomaton.State"
			case RECORD: (t.reference as RecordDefinition).name.recordClass
			case ENUM: (t.reference as EnumDefinition).name.enumClass
			case ERROR: "Object"
			default: null
		}
	}
	
}