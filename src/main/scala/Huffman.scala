import scala.io.Source
import java.io._

object Huffman {
  abstract class CodeTree extends Serializable
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {
    case Fork(_,_,_,w) => w
    case Leaf(_,w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_,_,cs,_) => cs
    case Leaf(c,_) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def iterate(map: Map[Char, Int], c: Char) = {
      val count = (map get c).getOrElse(0) + 1
      map + ((c, count))
    }
    (Map[Char, Int]() /: chars)(iterate).iterator.toList
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.sortWith((f1,f2) => f1._2 < f2._2).map((f) => Leaf (f._1, f._2))
  }

  def singleton(trees: List[CodeTree]): Boolean = trees.size == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case left :: right :: cs => (makeCodeTree(left, right) :: cs)
      .sortWith((t1, t2) => weight(t1) < weight(t2))
    case _ => trees
  }

  def until(singleton: List[CodeTree]=>Boolean, combine: List[CodeTree]=>List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (singleton(trees)) trees
    else until(singleton, combine)(combine(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def traverse(remaining: CodeTree, bits: List[Bit]): List[Char] = remaining match {
      case Leaf(c, _) => if (bits.isEmpty) List(c) else c :: traverse(tree, bits)
      case Fork(left, right, _, _) => if (bits.head == 0) traverse(left, bits.tail) else traverse(right, bits.tail)
    }

    traverse(tree, bits)
  }

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeChar(tree: CodeTree)(char: Char): List[Bit] = tree match {
      case Leaf(_, _) => List()
      case Fork(left, right, _, _) if (chars(left).contains(char)) =>  0 :: encodeChar(left)(char)
      case Fork(left, right, _, _)  => 1 :: encodeChar(right)(char)
    }
    text flatMap(encodeChar(tree))
  }

  def encodeFile(input: String, output: String): Unit ={
    val text = Source.fromFile(input).getLines.mkString
    val cd = createCodeTree(text.toList)
    val enc = encode(cd)(text.toList)
    val pw = new PrintWriter(new File(output))
    pw.write(enc.mkString(""))
    pw.close
    val oos = new ObjectOutputStream(new FileOutputStream(output + ".ser"))
    oos.writeObject(cd)
    oos.close
  }

  def decodeFile(input: String, output: String): Unit ={
    val ois = new ObjectInputStream(new FileInputStream(input + ".ser"))
    val cd = ois.readObject.asInstanceOf[CodeTree]
    ois.close
    val enc = Source.fromFile(input).getLines.mkString.map(_.toString).map(_.toInt).toList
    val dec = decode(cd, enc)
    val pw = new PrintWriter(new File(output))
    pw.write(dec.mkString(""))
    pw.close
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 3){
      println("args: mode, input, output")
      println("mode: 0 for encoding, 1 for decoding")
      println("input: if decoding mode, file containing codetree serealised with the same name + .ser as input " +
        "must present in the same dir")
    }else{
      args(0) match {
        case "0" => encodeFile(args(1), args(2))
        case "1" => decodeFile(args(1), args(2))
        case _ => println("mode: 0 for encoding, 1 for decoding")
      }
    }
  }
}
