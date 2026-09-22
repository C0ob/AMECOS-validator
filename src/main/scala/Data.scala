/** Runtime values used by operation arguments, results, and predicates. */
type DataType = Int | String | Unit | Operation

enum ComparatorTypes:
  case eq, neq, lt, leq, gt, geq, sameObj, sameInput, sameOutput

enum DataTypes:
  case Int, String, Unit, Operation

sealed abstract class Data(val value: DataType):
  def get: DataType = value
  override def toString: String = value.toString

  def getType: DataTypes =
    this match
      case IntData(_) => DataTypes.Int
      case StringData(_) => DataTypes.String
      case UnitData() => DataTypes.Unit
      case OperationData(_) => DataTypes.Operation

  def comp(other: Data, comparator: ComparatorTypes): Boolean = comparator match
    case ComparatorTypes.eq  => false
    case ComparatorTypes.neq => true
    case otherComparator => throw IllegalArgumentException(s"Invalid comparator: $otherComparator")

case class IntData(override val value: Int) extends Data(value):
  override def get: Int = value
  override def comp(other: Data, comparator: ComparatorTypes): Boolean = other match
    case IntData(otherValue) => comparator match
      case ComparatorTypes.eq  => value == otherValue
      case ComparatorTypes.neq => value != otherValue
      case ComparatorTypes.lt  => value < otherValue
      case ComparatorTypes.leq => value <= otherValue
      case ComparatorTypes.gt  => value > otherValue
      case ComparatorTypes.geq => value >= otherValue
      case otherComparator => super.comp(other, otherComparator)
    case _ => super.comp(other, comparator)

case class StringData(override val value: String) extends Data(value):
  override def get: String = value
  override def toString: String = s"\"$value\""
  override def comp(other: Data, comparator: ComparatorTypes): Boolean = other match
    case StringData(otherValue) => comparator match
      case ComparatorTypes.eq  => value == otherValue
      case ComparatorTypes.neq => value != otherValue
      case ComparatorTypes.lt  => value < otherValue
      case ComparatorTypes.leq => value <= otherValue
      case ComparatorTypes.gt  => value > otherValue
      case ComparatorTypes.geq => value >= otherValue
      case otherComparator => super.comp(other, otherComparator)
    case _ => super.comp(other, comparator)

case class UnitData() extends Data(()):
  override def get: Unit = ()
  override def toString: String = "void"
  override def comp(other: Data, comparator: ComparatorTypes): Boolean = other match
    case UnitData() => comparator match
      case ComparatorTypes.eq  => true
      case ComparatorTypes.neq => false
      case otherComparator => super.comp(other, otherComparator)
    case _ => super.comp(other, comparator)

case class OperationData(override val value: Operation) extends Data(value):
  override def get: Operation = value
  override def comp(other: Data, comparator: ComparatorTypes): Boolean = other match
    case OperationData(otherValue) => comparator match
      case ComparatorTypes.eq          => value == otherValue
      case ComparatorTypes.neq         => value != otherValue
      case ComparatorTypes.sameObj     => value.obj == otherValue.obj
      case ComparatorTypes.sameInput  => value.input.sameElements(otherValue.input)
      case ComparatorTypes.sameOutput => value.output == otherValue.output
      case otherComparator => super.comp(other, otherComparator)
    case _ => super.comp(other, comparator)

sealed trait DataSet:
  def data: Set[Data]
  def size: Int = data.size
  def union(other: DataSet): DataSet
  def intersect(other: DataSet): DataSet
  def diff(other: DataSet): DataSet

case class IntDataSet(values: Set[Int]) extends DataSet:
  def data: Set[Data] = values.map(IntData(_))
  def union(other: DataSet): DataSet = other match
    case IntDataSet(v) => IntDataSet(values union v)
    case _ => throw IllegalArgumentException(s"Cannot union $this with $other")
  def intersect(other: DataSet): DataSet = other match
    case IntDataSet(v) => IntDataSet(values intersect v)
    case _ => throw IllegalArgumentException(s"Cannot intersect $this with $other")
  def diff(other: DataSet): DataSet = other match
    case IntDataSet(v) => IntDataSet(values diff v)
    case _ => throw IllegalArgumentException(s"Cannot diff $this with $other")

case class StringDataSet(values: Set[String]) extends DataSet:
  def data: Set[Data] = values.map(StringData(_))
  def union(other: DataSet): DataSet = other match
    case StringDataSet(v) => StringDataSet(values union v)
    case _ => throw IllegalArgumentException(s"Cannot union $this with $other")
  def intersect(other: DataSet): DataSet = other match
    case StringDataSet(v) => StringDataSet(values intersect v)
    case _ => throw IllegalArgumentException(s"Cannot intersect $this with $other")
  def diff(other: DataSet): DataSet = other match
    case StringDataSet(v) => StringDataSet(values diff v)
    case _ => throw IllegalArgumentException(s"Cannot diff $this with $other")

case class OperationDataSet(values: Set[Operation]) extends DataSet:
  def data: Set[Data] = values.map(OperationData(_))
  def union(other: DataSet): DataSet = other match
    case OperationDataSet(v) => OperationDataSet(values union v)
    case _ => throw IllegalArgumentException(s"Cannot union $this with $other")
  def intersect(other: DataSet): DataSet = other match
    case OperationDataSet(v) => OperationDataSet(values intersect v)
    case _ => throw IllegalArgumentException(s"Cannot intersect $this with $other")
  def diff(other: DataSet): DataSet = other match
    case OperationDataSet(v) => OperationDataSet(values diff v)
    case _ => throw IllegalArgumentException(s"Cannot diff $this with $other")

/** Ordered values that can be indexed. */
sealed trait TupleValue:
  def values: Vector[Data]
  def size: Int = values.size

/** The input tuple of an operation execution. */
case class InputTuple(values: Vector[Data]) extends TupleValue
