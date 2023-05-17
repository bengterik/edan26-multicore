import java.util.Scanner;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.LinkedList;
import java.util.concurrent.locks.ReentrantLock;

import java.io.*;

class Graph {
	boolean print = false;
	int	s;
	int	t;
	int	n;
	int	m;
	Node	excess;		// list of nodes with excess preflow
	Node	node[];
	Edge	edge[];

	void print(String s) {
		if (print) {
			System.out.print(s);
		}
	}

	Graph(Node node[], Edge edge[])
	{
		this.node	= node;
		this.n		= node.length;
		this.edge	= edge;
		this.m		= edge.length;
	}

	synchronized void enter_excess(Node u)
	{
		if (u != node[s] && u != node[t]) {
			u.next = excess;
			excess = u;
		}
	}

	synchronized Node leave_excess(){
		Node v = excess;

		if (v != null)
			excess = v.next;

		return v;
	}

	Node other(Edge a, Node u)
	{
		if (a.u == u)	
			return a.v;
		else
			return a.u;
	}

	synchronized void relabel(Node u)
	{
		print("relabeling " + u.i + " => h = " + u.height() + "\n");
		u.relabel();
		enter_excess(u);
	}

	int min(int a, int b) 
	{
		return a <= b ? a : b;
	}

	void push(Node u, Node v, Edge a)
	{
		print("selected " + u.i + "->" + v.i + " for push ");
		print("f = " + a.f + " c = " + a.c + " so ");

		int d;
		if (u == a.u) {
			d = min(u.excess(), a.capacity() - a.flow());
			a.addFlow(d);
		} else {
			d = min(u.excess(), a.capacity() + a.flow());
			a.addFlow(-d);
		}

		print("pushing " + d + "\n");

		if (u.i < v.i) {
			u.addExcess(-d);
			v.addExcess(d);
		} else {
			u.addExcess(-d);
			v.addExcess(d);
		}
		
		if (u.excess() > 0) {	
			print(u.i + " has " + u.excess() + " so entering excess\n");
			enter_excess(u);
		}
	
		if (v.excess() == d) {
			print(v.i + " has " + v.excess() + " so entering excess\n");
			enter_excess(v);
		}

	}

	void work() {
		ListIterator<Edge>	iter;
		int			b;
		Edge			a;
		Node			u;
		Node			v;
		int nodesWorkedOn = 0;


		while ((u = leave_excess()) != null) {
			v = null;
			a = null;
			nodesWorkedOn++;

			iter = u.adj.listIterator();
			inner:
			while (iter.hasNext()) {
				a = iter.next();
				if (u == a.u) {
					v = a.v;
					b = 1;
				} else {
					v = a.u;
					b = -1;
				}

				if (u.i < v.i) {
					u.lock.lock();
					v.lock.lock();
				} else {
					v.lock.lock();
					u.lock.lock();
				}


				if (u.height() > v.height() && b * a.flow() < a.capacity()) {
					break inner;
				} else {
					u.lock.unlock();
					v.lock.unlock();
					v = null;
				}
			}

			if (v != null) {
				push(u, v, a);
				u.lock.unlock();
				v.lock.unlock();
			} else {
				u.lock.lock();
				relabel(u);
				u.lock.unlock();

			}
		}
		print(nodesWorkedOn + " nodes worked on \n");
	}

	int parallellPreflow(int s, int t)
	{
		ListIterator<Edge>	iter;
		Edge			a;
		int i;
				
		this.s = s;
		this.t = t;
		node[s].setHeight(n);

		iter = node[s].adj.listIterator();
		while (iter.hasNext()) {
			a = iter.next();

			node[s].addExcess(a.capacity());

			push(node[s], other(a, node[s]), a);
		}
		
		int nThreads = 4;
		Thread[] thread = new Thread[nThreads];
		
		Graph g = this;

		for(i=0; i < nThreads; i++) {
			Runnable r = new Runnable() {
				public void run() {
					g.work();
				}
			};
			thread[i] = new Thread(r);
			thread[i].start();
		}

		for (i=0; i < nThreads; ++i) {
			try {
				thread[i].join();
			} catch (Exception e) {
				System.out.println("" + e);
			}
		}

		return node[t].excess();
	}
}

class Node {
	private int	h;
	private int	e;
	int	i;
	Node	next;
	LinkedList<Edge>	adj;
	ReentrantLock lock = new ReentrantLock();

	Node(int i)
	{
		this.i = i;
		adj = new LinkedList<Edge>();
	}

	synchronized public void relabel(){
		h++;
	}

	synchronized public void addExcess(int i){
		e += i;
	}

	synchronized public int excess(){
		return e;
	}

	synchronized public int height(){
		return h;
	}
	
	public void setHeight(int i){
		h = i;
	}
}

class Edge {
	Node	u;
	Node	v;
	int	f;
	int	c;

	Edge(Node u, Node v, int c)
	{
		this.u = u;
		this.v = v;
		this.c = c;
	}

	synchronized public void addFlow(int i){
		f += i;
	}

	synchronized public int flow(){
		return f;
	}

	synchronized public int capacity(){
		return c;
	}
}

class Preflow {
	public static void main(String args[])
	{
		double	begin = System.currentTimeMillis();
		Scanner s = new Scanner(System.in);
		int	n;
		int	m;
		int	i;
		int	u;
		int	v;
		int	c;
		int	f;
		Graph	g;

		n = s.nextInt();
		m = s.nextInt();
		s.nextInt();
		s.nextInt();
		Node[] node = new Node[n];
		Edge[] edge = new Edge[m];

		for (i = 0; i < n; i += 1)
			node[i] = new Node(i);

		for (i = 0; i < m; i += 1) {
			u = s.nextInt();
			v = s.nextInt();
			c = s.nextInt(); 
			edge[i] = new Edge(node[u], node[v], c);
			node[u].adj.addLast(edge[i]);
			node[v].adj.addLast(edge[i]);
		}
		
		s.close();

		g = new Graph(node, edge);

		f = g.parallellPreflow(0, n-1);
		
		double	end = System.currentTimeMillis();
		System.out.println("t = " + (end - begin) / 1000.0 + " s");
		System.out.println("f = " + f);
	}
}
