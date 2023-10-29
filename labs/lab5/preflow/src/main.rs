#[macro_use] extern crate text_io;

use std::sync::{Mutex,Arc};
use std::collections::LinkedList;
use std::cmp;
//use std::thread;
use std::collections::VecDeque;

const DEBUG: bool = false;

struct Node {
	i:	usize,			/* index of itself for debugging.	*/
	e:	i32,			/* excess preflow.			*/
	h:	i32,			/* height.				*/
}

struct Edge {
        u:      usize,  
        v:      usize,
        f:      i32,
        c:      i32,
}

impl Node {
	fn new(ii:usize) -> Node {
		Node { i: ii, e: 0, h: 0 }
	}

}

impl Edge {
	fn new(uu:usize, vv:usize,cc:i32) -> Edge {
			Edge { u: uu, v: vv, f: 0, c: cc }      
	}
}

fn enter_excess(excess: &mut VecDeque<usize>, node: usize, t: usize) {
	if node != 0 && node != t {
		excess.push_back(node);
	}

	if DEBUG {
		println!("enter excess {}", node);
	}
}

fn relabel(excess: &mut VecDeque<usize>, node: &mut Node, t: usize) {
	node.h += 1;
	enter_excess(excess, node.i, t);

	if DEBUG {
		println!("relabeling {} with h = {}", node.i, node.h);
	}
}

fn main() {

	let n: usize = read!();		/* n nodes.						*/
	let m: usize = read!();		/* m edges.						*/
	let _c: usize = read!();	/* underscore avoids warning about an unused variable.	*/
	let _p: usize = read!();	/* c and p are in the input from 6railwayplanning.	*/
	let mut node = vec![];
	let mut edge = vec![];
	let mut adj: Vec<LinkedList<usize>> =Vec::with_capacity(n);
	let mut excess: VecDeque<usize> = VecDeque::new();
	let _debug = DEBUG;

	let s = 0;
	let t = n-1;

	println!("n = {}", n);
	println!("m = {}", m);

	for i in 0..n {
		let u:Node = Node::new(i);
		node.push(Arc::new(Mutex::new(u))); 
		adj.push(LinkedList::new());
	}

	for i in 0..m {
		let u: usize = read!();
		let v: usize = read!();
		let c: i32 = read!();
		let e: Edge = Edge::new(u,v,c);
		adj[u].push_back(i);
		adj[v].push_back(i);
		edge.push(Arc::new(Mutex::new(e))); 
	}


	println!("pushing from sink");
	let iter = adj[s].iter();

	for e in iter {
		let s = &mut node[s].lock().unwrap();
		let edge = &mut &edge[*e].lock().unwrap();
		let other;

		if s.i == edge.u {
			other = edge.v;
		} else {
			other = edge.u;
		}

		let o = &mut node[other].lock().unwrap();
		o.e += edge.c;
		s.h = (t as i32) + 1;
		
		excess.push_back(other);
	}

	println!("working on excess");
	// loop until done
	while !excess.is_empty() {
		let u = excess.pop_front().unwrap();
		let mut o: Option<usize> = None;

		if DEBUG {
			println!("selected {} from excess", u);
		}
		let iter = adj[u].iter();

		for e in iter {
			let edge = &mut edge[*e].lock().unwrap();
			let n = &mut node[u].lock().unwrap();
			let other;
			let b;

			if n.i == edge.u {
				other = edge.v;
				b = 1;
			} else {
				other = edge.u;
				b = -1;
			}
			
			o = Some(other);
			
			let other_node = &mut node[other].lock().unwrap();
			
			if DEBUG {
				println!("edge {}->{} with f = {}, c = {}", edge.u, edge.v, edge.f, edge.c);
			}
			
			if n.h > other_node.h && b * edge.f < edge.c {
				push(edge, n, other_node, &mut excess, t);

				break;
			} else {
				o = None;
			}
		}

		if o.is_none() {
			relabel(&mut excess,&mut node[u].lock().unwrap(), t);
		}
	}

	println!("f = {}", node[t].lock().unwrap().e);


}

fn push(edge: &mut Edge, n: &mut Node, o: &mut Node, excess: &mut VecDeque<usize>, t: usize) {
    let d;
	let b;

    if n.i == edge.u {
		d = cmp::min(n.e, edge.c - edge.f);
		b = 1;
	} else {
		d = cmp::min(n.e, edge.c + edge.f);
		b = -1;
	}
	
	if DEBUG {
		println!("pushing {} from {} to {}", d, n.i, o.i);
	}

    edge.f += d*b;

    n.e -= d;
    o.e += d;

	if n.i != 0 && o.i != 0 {
		assert!(d > 0);
		assert!(n.e >= 0);
		assert!(edge.f.abs() <= edge.c);
	}

    if n.e > 0 {
		enter_excess(excess, n.i, t);
	}

    if o.e == d {
		enter_excess(excess, o.i, t);
	}
}
