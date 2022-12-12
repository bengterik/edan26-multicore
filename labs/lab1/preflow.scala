import scala.util._
import java.util.Scanner
import java.io._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await,ExecutionContext,Future,Promise}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.io._


case class Flow(f: Int)
case class Height(h: Int)
case class Debug(debug: Boolean)
case class Control(control:ActorRef)
case class Source(n: Int)
case class Push(e: Edge, f: Int, hOther: Int)
case class Decline(e: Edge, f: Int, h: Int)
case class Accept(f: Int)

case class Done(f: Int)

case object Discharge
case object Print
case object Start
case object Excess
case object Height
case object Relabel
case object Maxflow
case object Sink
case object Hello

class Edge(var u: ActorRef, var v: ActorRef, var c: Int) {
	var	f = 0
	val history = List[String]()

	def add(newF:Int) = {
		f += newF 
		"add " + newF :: history
		//println(f"EDGE ${u.path.name} -> ${v.path.name}" + f": \t Flow changed with $newF, is now $f")
	}

	def printHistory(): Unit = {
		history.foreach(println)
	}
}

class Node(val index: Int) extends Actor {
	var	e = 0;				/* excess preflow. 						*/
	var	h = 0;				/* height. 							*/
	var	control:ActorRef = null		/* controller to report to when e is zero. 			*/
	var	source:Boolean	= false		/* true if we are the source.					*/
	var	sink:Boolean	= false		/* true if we are the sink.					*/
	var	edge: List[Edge] = Nil		/* adjacency list with edge objects shared with other nodes.	*/
	var	debug = true		/* to enable printing.						*/
	var activeEdges: List[Edge] = Nil
	var awaitingReply = 0;

	def min(a:Int, b:Int) : Int = { if (a < b) a else b }

	def id: String = "v" + index;

	def other(a:Edge, u:ActorRef) : ActorRef = { if (u == a.u) a.v else a.u }

	def status: Unit = { if (debug) println(id + " e = " + e + ", h = " + h) }

	def enter(func: String): Unit = { if (debug) { println(id + " enters " + func); status } }
	def exit(func: String): Unit = { if (debug) { println(id + " exits " + func); status } }

	def relabel : Unit = {
		
		enter("relabel")
		h += 1
		exit("relabel")
	}

	def discharge: Unit = {
		var     current:Edge = null

		if (activeEdges != Nil && e > 0 && !source) {

			current = activeEdges.head
			activeEdges = activeEdges.tail 
			var m = 0
			
			if (debug) {
				println(f"$id DISCHARGE:\t edge: " + current.u.path.name + "->" + current.v.path.name + f", from v$index")
				println(f"$id DISCHARGE:\t capacity: " + current.c + f" flow: ${current.f}" + f", excess: $e")
			}
			
			val prevF = current.f

			if (self == current.u) {
				m = min(current.c - current.f, e)
				current.add(m)
			} else {
				m = min(current.c + current.f, e)
				current.add(-m)
			}
			//assert(math.abs(current.f) <= current.c, f"DISCHARGE: $id flow exceeds capacity on edge ${current.u.path.name} -> ${current.v.path.name}, m = $m, e = $e, f = ${current.f}, prevf = $prevF c = ${current.c}")


			if (debug) println(f"$id DISCHARGE:\t v" + index + " with e = " + e + ", m = " + m)

			awaitingReply += 1

			if (m!=0) {
				e -= m
				other(current, self) ! Push(current, m, h)
			} else {
				if (debug) println(f"$id DISCHARGE:\t m = 0")
				self ! Decline(current, 0, -1)
			}

		} else if (!source && e > 0) {
			self ! Relabel
		} else if (e==0) {
			for (e <- edge) {
				other(e, self) ! Discharge
			}
		}
	}

	def receive = {

	case Debug(debug: Boolean)	=> this.debug = debug

	case Print => status

	case Excess => { sender ! Flow(e) /* send our current excess preflow to actor that asked for it. */ }

	case edge:Edge => { this.edge = edge :: this.edge /* put this edge first in the adjacency-list. */ }

	case Control(control:ActorRef)	=> this.control = control

	case Sink	=> { sink = true; if (debug) println(s"$id is sink"); }

	case Source(n:Int)	=> { h = n; source = true; if (debug) println(s"$index is source") }

	case Start => {
		if (debug) println("Started");
		
		for (e <- edge) {
			e.add(e.c)
			this.e -= e.c
			other(e, self) ! Push(e, e.c, Int.MaxValue)
			other(e, self) ! Discharge

		}

		control ! Done(e)
	}

	case Decline(e: Edge, f:Int, hOther: Int) => {
		if (debug) println(f"$id DECLINE:\t " + sender.path.name + f" declines $f from " + id + f" with hOther=$hOther and h=$h ")
		
		awaitingReply -= 1
		val prevF = e.f
		this.e += f
		e.add(if(self == e.u) -f else if (self == e.v) f else Int.MaxValue)

		//assert(math.abs(e.f) <= e.c, f"DECLINE: $id flow exceeds capacity on edge ${e.u.path.name} -> ${e.v.path.name}, e = ${this.e}, e.f = ${e.f}, f = $f, prevf = $prevF c = ${e.c}")

		if (!(source || sink || this.e == 0 ) && awaitingReply == 0) discharge
	}

	case Accept(f:Int) => {
		if (debug) println(f"$id ACCEPT:\t " + sender.path.name + f" accepts $f from $id")

		awaitingReply -= 1

		if (!(source || sink || this.e == 0 ) && awaitingReply == 0) {
			discharge 
		} 
	}

	case Relabel => {
		if (awaitingReply == 0) {
			relabel
			activeEdges = edge
			discharge
		}
	}

	case Discharge => {
		if (awaitingReply == 0 && !(source || sink || this.e == 0)) {
			activeEdges = edge
			discharge
		}
	}

	case Push(e: Edge, f:Int, hSender:Int) => { 
		// Push to all adjacent and if their h >= own h they will send Decline-message with the flow
		
		if (debug) println(f"$id PUSH:\t " + id + f" gets pushed $f from " + sender.path.name)

		if (hSender > h) {
			this.e += f
			//assert(this.e >= 0, f"Excess flow is negative in $id")
			sender ! Accept(f)
			if (sink || source) {
				control ! Done(this.e)
			} 
			else {
				activeEdges = edge
				if (awaitingReply == 0) discharge
			}
		} else {
			sender ! Decline(e, f, h)
		}
	}


	case _		=> {
		if (debug) println("" + index + " received an unknown message" + _) }

		assert(false)
	}

}


class Preflow extends Actor
{
	var	s	= 0;			/* index of source node.					*/
	var	t	= 0;			/* index of sink node.					*/
	var	n	= 0;			/* number of vertices in the graph.				*/
	var	edge:Array[Edge]	= null	/* edges in the graph.						*/
	var	node:Array[ActorRef]	= null	/* vertices in the graph.					*/
	var	ret:ActorRef 		= null	/* Actor to send result to.					*/
	var eSource = 0 				/* excess of source.						*/
	var eSink = 0					/* excess of sink.							*/

	def receive = {

	case node:Array[ActorRef]	=> {
		this.node = node
		n = node.size
		s = 0
		t = n-1

		for (u <- node)
			u ! Control(self)
	}

	case edge:Array[Edge] => this.edge = edge

	case Flow(f:Int) => {
		ret ! f			/* somebody (hopefully the sink) told us its current excess preflow. */
	}

	case Maxflow => {
		ret = sender

		node(s) ! Source(n) // tell s it is source with h = n
		node(t) ! Sink // tell t it is sink
		node(s) ! Start // tell s to start pushing
	}

	case Done(f: Int) => {
		if (sender == node(s)) {
			eSource = f 
		} else if (sender == node(t)) {
			eSink = f
		}
		println(f"DONE:\t\t source excess = ${eSource}, sink excess = ${eSink}")

		if (math.abs(eSource) == eSink) {
			node(t) ! Excess // finished
		}
	}

	}
}

object main extends App {
	implicit val t = Timeout(4 seconds);

	println("App started")

	val	begin = System.currentTimeMillis()
	val system = ActorSystem("Main")
	val control = system.actorOf(Props[Preflow], name = "control")

	var	n = 0;
	var	m = 0;
	var	edge: Array[Edge] = null
	var	node: Array[ActorRef] = null

	val	s = new Scanner(System.in);

	n = s.nextInt
	m = s.nextInt

	/* next ignore c and p from 6railwayplanning */
	s.nextInt
	s.nextInt

	node = new Array[ActorRef](n)

	for (i <- 0 to n-1)
		node(i) = system.actorOf(Props(new Node(i)), name = "v" + i)

	edge = new Array[Edge](m)

	for (i <- 0 to m-1) {

		val u = s.nextInt
		val v = s.nextInt
		val c = s.nextInt

		edge(i) = new Edge(node(u), node(v), c)

		node(u) ! edge(i)
		node(v) ! edge(i)
	}

	control ! node
	control ! edge

	val flow = control ? Maxflow
	val f = Await.result(flow, t.duration)

	println("f = " + f)

	system.stop(control);
	system.terminate()

	val	end = System.currentTimeMillis()

	println("t = " + (end - begin) / 1000.0 + " s")
}
