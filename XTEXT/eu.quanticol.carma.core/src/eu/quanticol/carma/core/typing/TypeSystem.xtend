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
import eu.quanticol.carma.core.carma.FieldAccess
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
import eu.quanticol.carma.core.carma.SetExpression
import eu.quanticol.carma.core.carma.ListExpression
import eu.quanticol.carma.core.carma.IsIn
import eu.quanticol.carma.core.carma.SetType
import eu.quanticol.carma.core.carma.ListType
import eu.quanticol.carma.core.carma.MyLocation
import eu.quanticol.carma.core.carma.None
import eu.quanticol.carma.core.carma.PreFunction
import eu.quanticol.carma.core.carma.PostFunction
import eu.quanticol.carma.core.carma.MapFunction
import eu.quanticol.carma.core.carma.NewListFunction
import eu.quanticol.carma.core.carma.NewSetFunction
import eu.quanticol.carma.core.carma.SizeFunction
import eu.quanticol.carma.core.carma.LocationVariable
import eu.quanticol.carma.core.carma.NodePattern
import eu.quanticol.carma.core.carma.SpaceDefinition
import eu.quanticol.carma.core.carma.Locations
import eu.quanticol.carma.core.carma.LabelDefinition
import eu.quanticol.carma.core.carma.LocAttribute
import eu.quanticol.carma.core.carma.StoreAttribute
import eu.quanticol.carma.core.carma.AttributeConstDeclaration
import eu.quanticol.carma.core.carma.AttibuteVarDeclaration
import eu.quanticol.carma.core.carma.LoopingVariable
import eu.quanticol.carma.core.carma.LocationType
import eu.quanticol.carma.core.carma.TupleExpression
import org.eclipse.emf.common.util.EList
import eu.quanticol.carma.core.carma.UniverseElement
import eu.quanticol.carma.core.carma.NodeExpressionOrArrayAccess
import eu.quanticol.carma.core.carma.NamedNode
import eu.quanticol.carma.core.carma.AccessToEdgeValue
import eu.quanticol.carma.core.carma.MeasureDefinition
import eu.quanticol.carma.core.carma.PoSetExpression
import eu.quanticol.carma.core.carma.PreSetExpression
import eu.quanticol.carma.core.carma.FilterFunction
import eu.quanticol.carma.core.carma.ExistsFunction
import eu.quanticol.carma.core.carma.ForAllFunction
import com.ibm.icu.text.SelectFormat
import eu.quanticol.carma.core.carma.SelectFunction
import eu.quanticol.carma.core.carma.LambdaParameter
import eu.quanticol.carma.core.carma.LambdaContext
import eu.quanticol.carma.core.carma.NormalSampling
import eu.quanticol.carma.core.carma.TargetAssignmentList
import eu.quanticol.carma.core.carma.MinInt
import eu.quanticol.carma.core.carma.MaxInt
import eu.quanticol.carma.core.carma.MaxReal
import eu.quanticol.carma.core.carma.MinReal
import eu.quanticol.carma.core.carma.InEdgesExpression
import eu.quanticol.carma.core.carma.OutEdgesExpression
import eu.quanticol.carma.core.carma.EdgeSourceExpression
import eu.quanticol.carma.core.carma.EdgeTargetExpression
import eu.quanticol.carma.core.carma.WeightedChoice

class TypeSystem {

	@Inject extension Util
	
	
	def dispatch CarmaType typeOf( TupleExpression e ) {
//		CarmaType::createTupleType(e.values.map[it.typeOf])
		CarmaType::LOCATION_TYPE		
	}
	
	def dispatch CarmaType typeOf( Locations e ) {
		CarmaType::createSetType(CarmaType::LOCATION_TYPE)		
	} 
	
	def dispatch CarmaType typeOf( LabelDefinition e ) {
		CarmaType::createSetType(CarmaType::LOCATION_TYPE)		
	} 
	
	def dispatch CarmaType typeOf( Object e ) {
		CarmaType::ERROR_TYPE
	}
	
	def dispatch CarmaType typeOf( IsIn e ) {
		CarmaType::BOOLEAN_TYPE
	}
	
	def  dispatch CarmaType typeOf( EObject e ) {
		CarmaType::ERROR_TYPE	
	}
	
	def computeBinaryBooleanOperatorType( CarmaType t ) {
		switch t.code {
			case NONE: CarmaType::NONE_TYPE
			case BOOLEAN: CarmaType::BOOLEAN_TYPE
			case SET: t
			default: CarmaType::ERROR_TYPE
		}		
	}
	
	def  dispatch CarmaType typeOf( Or e ) {
		if (e.left==null) {
			CarmaType::NONE_TYPE
		} else {
			e.left.typeOf.computeBinaryBooleanOperatorType
		}
	}		
	
	def  dispatch CarmaType typeOf( And e ) {
		if (e.left==null) {
			CarmaType::NONE_TYPE
		} else {
			e.left.typeOf.computeBinaryBooleanOperatorType
		}
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
		if ((e.left == null)||(e.right==null)) {
			CarmaType::NONE_TYPE
		} else {
			e.left.typeOf.mostGeneral( e.right.typeOf ) 
		}
	}

	def  dispatch CarmaType typeOf( Addition e ) {
		if ((e.left == null)||(e.right==null)) {
			CarmaType::NONE_TYPE
		} else {
			e.left.typeOf.mostGeneral( e.right.typeOf ) 
		}
	}

	def  dispatch CarmaType typeOf( Multiplication e ) {
		if ((e.left == null)||(e.right==null)) {
			CarmaType::NONE_TYPE
		} else {
			var t1 = e.left.typeOf
			var t2 = e.right.typeOf
			if (t1.number&&t2.number) {
				t1.mostGeneral(t2)				
			} else {
				CarmaType::ERROR_TYPE
			}
		}
	}

	def  dispatch CarmaType typeOf( Division e ) {
		if ((e.left == null)||(e.right==null)) {
			CarmaType::NONE_TYPE
		} else {
			var t1 = e.left.typeOf
			var t2 = e.right.typeOf
			if (t1.number&&t2.number) {
				t1.mostGeneral(t2)				
			} else {
				CarmaType::ERROR_TYPE
			}
		}
	}

	def  dispatch CarmaType typeOf( Modulo e ) {
		CarmaType::INTEGER_TYPE
	}

	def  dispatch CarmaType typeOf( Not e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def  dispatch CarmaType typeOf( UnaryPlus e ) {
		if (e.expression == null) {
			CarmaType::NONE_TYPE
		} else {
			var t = e.expression.typeOf
			if (t.number) {
				t
			} else {
				CarmaType::ERROR_TYPE
			}
		}
	}
	
	def  dispatch CarmaType typeOf( UnaryMinus e ) {
		if (e.expression == null) {
			CarmaType::NONE_TYPE
		} else {
			var t = e.expression.typeOf
			if (t.number) {
				t
			} else {
				CarmaType::ERROR_TYPE
			}
		}
	}
	
	def  dispatch CarmaType typeOf( IfThenElseExpression e ) {
		if ((e.elseBranch == null)||(e.thenBranch==null)) {
			CarmaType::NONE_TYPE
		} else {
			e.thenBranch.typeOf.mostGeneral( e.elseBranch.typeOf )
		}
	}
	
	def  dispatch CarmaType typeOf( Reference e ) {
		if (e.reference == null) {
			CarmaType::NONE_TYPE
		} else {
			e.reference.typeOf
		}
	}
	
	def dispatch CarmaType typeOf( IterationVariable v ) {
		CarmaType::INTEGER_TYPE
	}

	def dispatch CarmaType typeOf( LoopingVariable v ) {
		var CarmaType cType = v.value ?. typeOf ?: CarmaType::NONE_TYPE
		if (cType.isSet) {
			cType.asSet.elementsType
		} else {
			if (cType.isList) {
				cType.asList.elementsType
			} else {
				CarmaType::NONE_TYPE
			}
		}
	}
	
	
	def dispatch CarmaType typeOf( TargetAssignmentVariable v ) {
		if (v.variable == null) {
			CarmaType::NONE_TYPE
		} else {
			v.variable.typeOf
		}
	}

	def dispatch CarmaType typeOf( TargetAssignmentField f ) {
		if (f.field == null) {
			CarmaType::NONE_TYPE
		} else {
			f.field.typeOf
		}
	}
	
	def dispatch CarmaType typeOf( TargetAssignmentList f ) {
		if (f.target == null) {
			CarmaType::NONE_TYPE		
		} else {
			var t = f.target.typeOf
			if (t.isList) {
				t.asList.elementsType
			} else {
				CarmaType::ERROR_TYPE
			}
		}
	}
	
	def dispatch CarmaType typeOf( SetExpression e ) {
		if (e.values.isEmpty) {
			CarmaType::createSetType(null)
		} else {
			CarmaType::createSetType(e.values.get(0).typeOf)
		}
	}
		
	def dispatch CarmaType typeOf( ListExpression e ) {
		if (e.values.isEmpty) {
			CarmaType::createListType(null)
		} else {
			CarmaType::createListType(e.values.get(0).typeOf)
		}
	}
		
	def dispatch CarmaType typeOf( FieldAssignment f ) {
		f.field.typeOf
	}
	
	def  dispatch CarmaType typeOf( Variable v ) {
		if (v.type != null) {
			v.type.toCarmaType		
		} else {
			CarmaType::NONE_TYPE
		}
	}

	def  dispatch CarmaType typeOf( LocationVariable v ) {
		var vIndex = v.indexOfLocationVariable
		var space = v.getContainerOfType(typeof(SpaceDefinition))
		if ((vIndex>=0)&&(space!=null)) {
			space.universe ?. locationElementType( vIndex ) ?: CarmaType::NONE_TYPE
		} else {
			CarmaType::NONE_TYPE
		}
	}
	
	def locationElementType( EList<UniverseElement> universe , int index ) {
		if ((index >= 0)&&(index<universe.size)) {
			universe.get(index).type.toCarmaType
		} else {
			CarmaType::NONE_TYPE
		}
	}
	

//	def  dispatch CarmaType typeOf( LambdaExpression f ) {
//		if (f.variables.empty||f.body==null) {
//			CarmaType::NONE_TYPE
//		} else {
//			CarmaType::createFunctionType(
//				CarmaType::createTupleType( f.variables.map[ it.typeOf ] ) ,
//				f.body.typeOf
//			)
//		}
//		
//	}

	def dispatch CarmaType typeOf( CastToReal e ) {
		CarmaType::REAL_TYPE
	}
	
	def dispatch CarmaType typeOf( LambdaParameter e ) {
		var lambda = e.getContainerOfType(typeof(LambdaContext))
		if (lambda == null) {
			CarmaType::NONE_TYPE
		} else {
			var arg1Type = lambda.arg1.typeOf
			if ((arg1Type.set)||(arg1Type.list)) {
				if (arg1Type instanceof CarmaSetType) {
					arg1Type.elementsType
				} else {
					(arg1Type as CarmaListType).elementsType
				}
			} else {
				CarmaType::ERROR_TYPE
			}
		}
	}

	def dispatch CarmaType typeOf( CastToInteger e ) {
		CarmaType::INTEGER_TYPE
	}

	def dispatch CarmaType typeOf( NodeExpressionOrArrayAccess e ) {
		var flag = false
		var CarmaType result = CarmaType::NONE_TYPE
		if (e.source != null) {
			var source = e.source
			if (source instanceof Reference) {
				if (source.reference instanceof NamedNode) {
					result = CarmaType::LOCATION_TYPE
					flag = true
				} 
			} 
			if (!flag) {
				var t = source.typeOf	
				if (t.isList) {
					result = t.asList.elementsType		
				} else {
					result = CarmaType::ERROR_TYPE
				}
			}
		} 
		result
		
	}

	def dispatch CarmaType typeOf( MyLocation e ) {
		CarmaType::LOCATION_TYPE
	}

	def  dispatch CarmaType typeOf( AttributeConstDeclaration a ) {
		a.value.typeOf
	}

	def  dispatch CarmaType typeOf( AttibuteVarDeclaration a ) {
		if (a.type != null) {
			a.type.toCarmaType
		} else {
			a.value.typeOf			
		}
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
			CarmaType::NONE_TYPE			
		}
	}

	def  dispatch CarmaType typeOf( FunctionDefinition f ) {
		if (f.type != null) {
			f.type.toCarmaType
		} else {
			CarmaType::NONE_TYPE
		}
	}

	def dispatch CarmaType typeOf( MeasureDefinition m ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( EnumCase ec ) {
		var c = ec.getContainerOfType(typeof(EnumDefinition))
		if (c != null) {
			CarmaType::createEnumType(c)
		} else {
			CarmaType::NONE_TYPE
		}
	}
	
	def  dispatch CarmaType typeOf( ConstantDefinition c ) {
		if (c.value!=null) {
			c.value.typeOf
		} else {
			CarmaType::NONE_TYPE
		}
	}
	
//	def dispatch CarmaType typeOf( MeasureVariableDeclaration v ) {
//		CarmaType::INTEGER_TYPE
//	}
//	
	def  dispatch CarmaType typeOf( UntypedVariable v ) {
		v.inferTypeOf
	}
	
	
	def  dispatch CarmaType typeOf( FieldAccess e ) {
		if (e.field == null) {
			CarmaType::NONE_TYPE
		} else {
			var f = e.field
			switch f {
				FieldDefinition: f.typeOf
				LabelDefinition: CarmaType::BOOLEAN_TYPE
				UniverseElement: f.type.toCarmaType
			}
		}
	}

	def dispatch CarmaType typeOf( FieldDefinition f ) {
		f.fieldType.toCarmaType 
	}
	
	def  dispatch CarmaType typeOf( AtomicTrue e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def  dispatch CarmaType typeOf( MinInt e ) {
		CarmaType::INTEGER_TYPE
	}

	def  dispatch CarmaType typeOf( MaxInt e ) {
		CarmaType::INTEGER_TYPE
	}

	def  dispatch CarmaType typeOf( MaxReal e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( MinReal e ) {
		CarmaType::REAL_TYPE
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
		e.fields.head ?. field ?. recordType ?: CarmaType::NONE_TYPE
	}
	
	def dispatch CarmaType typeOf( None e ) {
		CarmaType::NONE_TYPE
	}
	
	def  dispatch CarmaType typeOf( AtomicNow e ) {
		CarmaType::REAL_TYPE
	}
	
	def  dispatch CarmaType typeOf( AtomicRnd e ) {
		CarmaType::REAL_TYPE
	}
	
	def  dispatch CarmaType typeOf( NormalSampling e ) {
		CarmaType::REAL_TYPE
	}
	
	def dispatch CarmaType typeOf( WeightedChoice e) {
		e.values.get(0).typeOf
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
		if (e.attribute == null) {
			CarmaType::NONE_TYPE
		} else {
			e.attribute.typeOf
		}
	}
	
	def dispatch CarmaType typeOf( LocAttribute a ) {
		CarmaType::LOCATION_TYPE
	}
	
	def dispatch CarmaType typeOf( StoreAttribute a ) {
		a.reference.typeOf
	}
		
	def  dispatch CarmaType typeOf( ReceiverContext e ) {
		e ?. attribute ?. typeOf ?: CarmaType::NONE_TYPE
	}

	def  dispatch CarmaType typeOf( SenderContext e ) {
		e ?. attribute ?. typeOf ?: CarmaType::NONE_TYPE
	}

	def  dispatch CarmaType typeOf( GlobalContext e ) {
		e ?. reference ?. typeOf ?: CarmaType::NONE_TYPE
	}

	def  dispatch CarmaType typeOf( AbsFunction e ) {
		e ?. arg ?. typeOf ?: CarmaType::NONE_TYPE
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
		if ((e.first == null)||(e.second == null)) {
			CarmaType::NONE_TYPE
		} else {
			e.first.typeOf.mostGeneral( e.second.typeOf )
		}
	}

	def  dispatch CarmaType typeOf( MaxMeasure e ) {
		e ?. value ?. typeOf ?: CarmaType::NONE_TYPE
	}
	
	def  dispatch CarmaType typeOf( MinMeasure e ) {
		e ?. value ?. typeOf ?: CarmaType::NONE_TYPE
	}

	def  dispatch CarmaType typeOf( AverageMeasure e ) {
		CarmaType::REAL_TYPE
	}

	def  dispatch CarmaType typeOf( MinFunction e ) {
		if ((e.first == null)||(e.second == null)) {
			CarmaType::NONE_TYPE
		} else {
			e.first.typeOf.mostGeneral( e.second.typeOf )
		}
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
		if (e.args.size>1) {
			e.args.head.typeOf		
		} else {
			if (e.args.size == 1) {
				var t = e.args.get(0).typeOf
				if (t.isSet) {
					t.asSet.elementsType
				} else {
					if (t.isList) {
						t.asList.elementsType
					} else {
						CarmaType::NONE_TYPE
					}
				}
			} else {
				CarmaType::NONE_TYPE
			}
		}
	}
	
	def dispatch CarmaType typeOf( PreFunction e ) {
		CarmaType::createSetType( CarmaType::LOCATION_TYPE )
	}

	def dispatch CarmaType typeOf( PostFunction e ) {
		CarmaType::createSetType( CarmaType::LOCATION_TYPE )
	}

	def dispatch CarmaType typeOf( PoSetExpression e ) {
		CarmaType::createSetType( CarmaType::LOCATION_TYPE )
	}

	def dispatch CarmaType typeOf( PreSetExpression e ) {
		CarmaType::createSetType( CarmaType::LOCATION_TYPE )
	}

	def dispatch CarmaType typeOf( InEdgesExpression e ) {
		CarmaType::createSetType( CarmaType::EDGE_TYPE )
	}

	def dispatch CarmaType typeOf( OutEdgesExpression e ) {
		CarmaType::createSetType( CarmaType::EDGE_TYPE )
	}

	def dispatch CarmaType typeOf( EdgeSourceExpression e ) {
		CarmaType::LOCATION_TYPE
	}

	def dispatch CarmaType typeOf( EdgeTargetExpression e ) {
		CarmaType::LOCATION_TYPE
	}


	def dispatch CarmaType typeOf( AccessToEdgeValue e ) {
		var labelType = e.label ?. value ?. typeOf	?: CarmaType::NONE_TYPE
		if (labelType.isNone) {
			labelType
		} else {
			CarmaType::createSetType( labelType )
		}
	}
	
	def dispatch CarmaType typeOf( MapFunction e ) {
		if ((e.arg1 == null)||(e.arg2 == null)) {
			CarmaType::NONE_TYPE
		} else {
			var fType = e.arg2.typeOf
			var cType = e.arg1.typeOf
			if ((!fType.none)&&(!fType.error)&&(cType.list||cType.set)) {
				if (cType.list) {
					CarmaType::createListType(fType)
				} else {
					CarmaType::createSetType(fType)
				}
			} else {
				CarmaType::ERROR_TYPE
			}
		}
	}

	def dispatch CarmaType typeOf( FilterFunction e ) {
		if ((e.arg1 == null)||(e.arg2 == null)) {
			CarmaType::NONE_TYPE
		} else {
			var cType = e.arg1.typeOf
			if ((cType.list||cType.set)) {
				cType
			} else {
				CarmaType::ERROR_TYPE
			}
		}
	}	

	
	def dispatch CarmaType typeOf( SelectFunction e ) {
		if ((e.arg1 == null)||(e.arg2 == null)) {
			CarmaType::NONE_TYPE
		} else {
			var cType = e.arg1.typeOf
			if ((cType.list||cType.set)) {
				if (cType instanceof CarmaSetType) {
					cType.elementsType
				} else {
					(cType as CarmaListType).elementsType
				}
			} else {
				CarmaType::ERROR_TYPE
			}
		}
	}	
//
//	def dispatch CarmaType typeOf( ReduceFunction e ) {
//		if ((e.arg1 == null)||(e.arg2 == null)) {
//			CarmaType::NONE_TYPE
//		} else {
//			var fType = e.arg2.typeOf
//			var cType = e.arg1.typeOf
//			if (fType.function&&(cType.list||cType.set)) {
//				(fType as CarmaFunctionType).result
//			} else {
//				CarmaType::ERROR_TYPE
//			}
//		}
//	}	
	
	def dispatch CarmaType typeOf( ExistsFunction e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def dispatch CarmaType typeOf( ForAllFunction e ) {
		CarmaType::BOOLEAN_TYPE
	}

	def dispatch CarmaType typeOf( NewListFunction e ) {
		if (e.arg1 == null) {
			CarmaType::NONE_TYPE
		} else {
			CarmaType::createListType( e.arg1.toCarmaType )
		}
	}

	def dispatch CarmaType typeOf( NewSetFunction e ) {
		if (e.arg2 == null) {
			CarmaType::NONE_TYPE
		} else {
			CarmaType::createSetType( e.arg2.toCarmaType )
		}
	}
	


	def dispatch CarmaType typeOf( SizeFunction e ) {
		CarmaType::INTEGER_TYPE
	}

	def  CarmaType toCarmaType( ValueType t ) {
		switch t {
			ProcessType: CarmaType::PROCESS_TYPE
			IntegerType: CarmaType::INTEGER_TYPE
			RealType: CarmaType::REAL_TYPE
			BooleanType: CarmaType::BOOLEAN_TYPE
			LocationType: CarmaType::LOCATION_TYPE
			SetType: CarmaType::createSetType( t.arg.toCarmaType )
			ListType: CarmaType::createListType( t.arg.toCarmaType )
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
	
		
	def CharSequence toJavaType( CarmaType t , boolean isElementary ) {
		switch (t.code) {
			case BOOLEAN: if (isElementary) {
				"boolean"
			} else {
				"Boolean"
			}
			case INTEGER: if (isElementary) {
				"int"
			} else {
				"Integer"
			}
			case REAL: if (isElementary) {
				"double"
			} else {
				"Double"				
			}
			case LOCATION: "Node"
			case PROCESS: "CarmaProcessAutomaton.State"
			case RECORD: t.asRecord.getReference.name.recordClass
			case ENUM: t.asEnum.getReference.name.enumClass
			case LIST: "LinkedList<"+(t.asList.elementsType.toJavaType(false))+">"
			case SET: "HashSet<"+(t.asSet.elementsType.toJavaType(false))+">"
			case ERROR: "Object"
			default: null
		}
	}
	
	def areEqual( UniverseElement e1 , UniverseElement e2 ) {
		var v = (e1.name==e2.name)
		v = v &&(e1.type.toCarmaType==e2.type.toCarmaType)
		v
	}	
}