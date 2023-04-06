import java.util.Scanner;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.LinkedList;

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

	void enter_excess(Node u)
	{
		if (u != node[s] && u != node[t]) {
			u.next = excess;
			excess = u;
		}
	}

	Node other(Edge a, Node u)
	{
		if (a.u == u)	
			return a.v;
		else
			return a.u;
	}

	void relabel(Node u)
	{
		print("relabeling " + u.i + " => h = " + u.height() + "\n");
		u.relabel();
		enter_excess(u);
		if (u.height() > n+2) {
			print("h > n+1 so exiting\n");
			System.exit(1);
		}
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
		
		u.addExcess(-d);
		v.addExcess(d);

		if (u.excess() > 0) {	
			print(u.i + " has " + u.excess() + " so entering excess\n");
			enter_excess(u);
		}
	
		if (v.excess() == d) {
			print(v.i + " has " + v.excess() + " so entering excess\n");
			enter_excess(v);
		}

	}

	int preflow(int s, int t)
	{
		ListIterator<Edge>	iter;
		int			b;
		Edge			a;
		Node			u;
		Node			v;
		
		this.s = s;
		this.t = t;
		node[s].setHeight(n);

		iter = node[s].adj.listIterator();
		while (iter.hasNext()) {
			a = iter.next();

			node[s].addExcess(a.capacity());

			push(node[s], other(a, node[s]), a);
		}

		while (excess != null) {
			u = excess;
			v = null;
			a = null;
			excess = u.next;
		
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

				if (u.height() > v.height() && b * a.flow() < a.capacity()) {
					break inner;
				} else {
					v = null;
				}
			}

			if (v != null) {
				push(u, v, a);
			} else {
				relabel(u);
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

	Node(int i)
	{
		this.i = i;
		adj = new LinkedList<Edge>();
	}

	public void relabel(){
		h++;
	}

	public void addExcess(int i){
		e += i;
	}

	public int excess(){
		return e;
	}

	public int height(){
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

	public void addFlow(int i){
		f += i;
	}

	public int flow(){
		return f;
	}

	public int capacity(){
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

		g = new Graph(node, edge);

		int nThreads = 1;
		Thread[] thread = new Thread[nThreads];

		for(i=0; i < nThreads; i++) {
			Runnable r = new Runnable() {
				public void run() {
					g.preflow(0, n-1);
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
		
		f = g.node[n-1].excess();

		double	end = System.currentTimeMillis();
		System.out.println("t = " + (end - begin) / 1000.0 + " s");
		System.out.println("f = " + f);
	}
}
