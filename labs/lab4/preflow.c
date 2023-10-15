/* This is an implementation of the preflow-push algorithm, by
 * Goldberg and Tarjan, for the 2021 EDAN26 Multicore programming labs.
 *
 * It is intended to be as simple as possible to understand and is
 * not optimized in any way.
 *
 * You should NOT read everything for this course.
 *
 * Focus on what is most similar to the pseudo code, i.e., the functions
 * preflow, push, and relabel.
 *
 * Some things about C are explained which are useful for everyone  
 * for lab 3, and things you most likely want to skip have a warning 
 * saying it is only for the curious or really curious. 
 * That can safely be ignored since it is not part of this course.
 *
 * Compile and run with: make
 *
 * Enable prints by changing from 1 to 0 at PRINT below.
 *
 * Feel free to ask any questions about it on Discord 
 * at #lab0-preflow-push
 *
 * A variable or function declared with static is only visible from
 * within its file so it is a good practice to use in order to avoid
 * conflicts for names which need not be visible from other files.
 *
 */
 
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include "pthread_barrier.h"

#define PRINT		0	/* enable/disable prints. */
#define NBR_THREADS 4

/* the funny do-while next clearly performs one iteration of the loop.
 * if you are really curious about why there is a loop, please check
 * the course book about the C preprocessor where it is explained. it
 * is to avoid bugs and/or syntax errors in case you use the pr in an
 * if-statement without { }.
 *
 */

#if PRINT
#define pr(...)		do { fprintf(stderr, __VA_ARGS__); } while (0)
#else
#define pr(...)		/* no effect at all */
#endif

#define MIN(a,b)	(((a)<=(b))?(a):(b))

/* introduce names for some structs. a struct is like a class, except
 * it cannot be extended and has no member methods, and everything is
 * public.
 *
 * using typedef like this means we can avoid writing 'struct' in 
 * every declaration. no new type is introduded and only a shorter name.
 *
 */

typedef struct graph_t	graph_t;
typedef struct node_t	node_t;
typedef struct edge_t	edge_t;
typedef struct list_t	list_t;
typedef struct work_list_t	work_list_t;

struct list_t {
	edge_t*		edge;
	list_t*		next;
};

struct node_t {
	int		h;	/* height.			*/
	int		e;	/* excess flow.			*/
	list_t*		edge;	/* adjacency list.		*/
	node_t*		next;	/* with excess preflow.		*/
};

struct edge_t {
	node_t*		u;	/* one of the two nodes.	*/
	node_t*		v;	/* the other. 			*/
	int		f;	/* flow > 0 if from u to v.	*/
	int		c;	/* capacity.			*/
};

struct graph_t {
	int		n;	/* nodes.			*/
	int		m;	/* edges.			*/
	node_t*		v;	/* array of n nodes.		*/
	edge_t*		e;	/* array of m edges.		*/
	node_t*		s;	/* source.			*/
	node_t*		t;	/* sink.			*/
	node_t*		excess;	/* nodes with e > 0 except s,t.	*/
	pthread_barrier_t phase_one;
	pthread_barrier_t phase_two;
	int done;
};

typedef struct {
	node_t* u;
	node_t* v;
	edge_t* e;
	int push; // 0 for relabel 1 for push
	int flow;
} op_t;

typedef struct {
	graph_t* g;
	node_t** excess;
	int c;
	int i;
	op_t** ops;
	int opc;
	int opi;
} threadarg_t;

/* a remark about C arrays. the phrase above 'array of n nodes' is using
 * the word 'array' in a general sense for any language. in C an array
 * (i.e., the technical term array in ISO C) is declared as: int x[10],
 * i.e., with [size] but for convenience most people refer to the data
 * in memory as an array here despite the graph_t's v and e members 
 * are not strictly arrays. they are pointers. once we have allocated
 * memory for the data in the ''array'' for the pointer, the syntax of
 * using an array or pointer is the same so we can refer to a node with
 *
 * 			g->v[i]
 *
 * where the -> is identical to Java's . in this expression.
 * 
 * in summary: just use the v and e as arrays.
 * 
 * a difference between C and Java is that in Java you can really not
 * have an array of nodes as we do. instead you need to have an array
 * of node references. in C we can have both arrays and local variables
 * with structs that are not allocated as with Java's new but instead
 * as any basic type such as int.
 * 
 */

static char* progname;

#if PRINT

static int id(graph_t* g, node_t* v)
{
	/* return the node index for v.
	 *
	 * the rest is only for the curious.
	 *
	 * we convert a node pointer to its index by subtracting
	 * v and the array (which is a pointer) with all nodes.
	 *
	 * if p and q are pointers to elements of the same array,
	 * then p - q is the number of elements between p and q.
	 *
	 * we can of course also use q - p which is -(p - q)
	 *
	 * subtracting like this is only valid for pointers to the
	 * same array.
	 *
	 * what happens is a subtract instruction followed by a
	 * divide by the size of the array element.
	 *
	 */

	return v - g->v;
}
#endif

void error(const char* fmt, ...)
{
	/* print error message and exit. 
	 *
	 * it can be used as printf with formatting commands such as:
	 *
	 *	error("height is negative %d", v->h);
	 *
	 * the rest is only for the really curious. the va_list
	 * represents a compiler-specific type to handle an unknown
	 * number of arguments for this error function so that they
	 * can be passed to the vsprintf function that prints the
	 * error message to buf which is then printed to stderr.
	 *
	 * the compiler needs to keep track of which parameters are
	 * passed in integer registers, floating point registers, and
	 * which are instead written to the stack.
	 *
	 * avoid ... in performance critical code since it makes 
	 * life for optimizing compilers much more difficult. but in
	 * in error functions, they obviously are fine (unless we are
	 * sufficiently paranoid and don't want to risk an error 
	 * condition escalate and crash a car or nuclear reactor 		 
	 * instead of doing an even safer shutdown (corrupted memory
	 * can cause even more damage if we trust the stack is in good
	 * shape)).
	 *
	 */

	va_list		ap;
	char		buf[BUFSIZ];

	va_start(ap, fmt);
	vsprintf(buf, fmt, ap);

	if (progname != NULL)
		fprintf(stderr, "%s: ", progname);

	fprintf(stderr, "error: %s\n", buf);
	exit(1);
}

static int next_int()
{
        int     x;
        int     c;

	/* this is like Java's nextInt to get the next integer.
	 *
	 * we read the next integer one digit at a time which is
	 * simpler and faster than using the normal function
	 * fscanf that needs to do more work.
	 *
	 * we get the value of a digit character by subtracting '0'
	 * so the character '4' gives '4' - '0' == 4
	 *
	 * it works like this: say the next input is 124
	 * x is first 0, then 1, then 10 + 2, and then 120 + 4.
	 *
	 */

	x = 0;
        while (isdigit(c = getchar()))
                x = 10 * x + c - '0';

        return x;
}

static void* xmalloc(size_t s)
{
	void*		p;

	/* allocate s bytes from the heap and check that there was
	 * memory for our request.
	 *
	 * memory from malloc contains garbage except at the beginning
	 * of the program execution when it contains zeroes for 
	 * security reasons so that no program should read data written
	 * by a different program and user.
	 *
	 * size_t is an unsigned integer type (printed with %zu and
	 * not %d as for int).
	 *
	 */

	p = malloc(s);

	if (p == NULL)
		error("out of memory: malloc(%zu) failed", s);

	return p;
}

static void* xcalloc(size_t n, size_t s)
{
	void*		p;

	p = xmalloc(n * s);

	/* memset sets everything (in this case) to 0. */
	memset(p, 0, n * s);

	/* for the curious: so memset is equivalent to a simple
	 * loop but a call to memset needs less memory, and also
 	 * most computers have special instructions to zero cache 
	 * blocks which usually are used by memset since it normally
	 * is written in assembler code. note that good compilers 
	 * decide themselves whether to use memset or a for-loop
	 * so it often does not matter. for small amounts of memory
	 * such as a few bytes, good compilers will just use a 
	 * sequence of store instructions and no call or loop at all.
	 *
	 */

	return p;
}

static void add_edge(node_t* u, edge_t* e)
{
	list_t*		p;

	/* allocate memory for a list link and put it first
	 * in the adjacency list of u.
	 *
	 */

	p = xmalloc(sizeof(list_t));
	p->edge = e;
	p->next = u->edge;
	u->edge = p;
}

static void connect(node_t* u, node_t* v, int c, edge_t* e)
{
	/* connect two nodes by putting a shared (same object)
	 * in their adjacency lists.
	 *
	 */

	e->u = u;
	e->v = v;
	e->c = c;

	add_edge(u, e);
	add_edge(v, e);
}

static graph_t* new_graph(FILE* in, int n, int m)
{
	graph_t*	g;
	node_t*		u;
	node_t*		v;
	int		i;
	int		a;
	int		b;
	int		c;
	
	g = xmalloc(sizeof(graph_t));

	g->n = n;
	g->m = m;
	
	g->v = xcalloc(n, sizeof(node_t));
	g->e = xcalloc(m, sizeof(edge_t));

	g->s = &g->v[0];
	g->t = &g->v[n-1];
	g->excess = NULL;
	pthread_barrier_init(&g->phase_one, NULL, NBR_THREADS+1);
	pthread_barrier_init(&g->phase_two, NULL, NBR_THREADS+1); 
	g->done = 0;

	for (i = 0; i < m; i += 1) {
		a = next_int();
		b = next_int();
		c = next_int();
		u = &g->v[a];
		v = &g->v[b];
		connect(u, v, c, g->e+i);
	}

	return g;
}

static void enter_excess(graph_t* g, node_t* v)
{
	/* put v at the front of the list of nodes
	 * that have excess preflow > 0.
	 *
	 * note that for the algorithm, this is just
	 * a set of nodes which has no order but putting it
	 * it first is simplest.
	 *
	 */
	if (v != g->t && v != g->s) {
		v->next = g->excess;
		g->excess = v;
	}
}

static node_t* leave_excess(graph_t* g)
{
	node_t*		v;

	/* take any node from the set of nodes with excess preflow
	 * and for simplicity we always take the first.
	 *
	 */

	v = g->excess;

	if (v != NULL)
		g->excess = v->next;
	
	return v;
}

static void push(graph_t* g, node_t* u, node_t* v, edge_t* e)
{
	int		d;	/* remaining capacity of the edge. */

	pr("push from %d to %d: ", id(g, u), id(g, v));
	pr("f = %d, c = %d, so ", e->f, e->c);
	
	if (u == e->u) {
		d = MIN(u->e, e->c - e->f);
		e->f += d;
	} else {
		d = MIN(u->e, e->c + e->f);
		e->f -= d;
	}

	pr("pushing %d\n", d);

	u->e -= d;
	v->e += d;

	/* the following are always true. */

	if (u != g->s && v != g->s) {
		assert(d >= 0);
		assert(u->e >= 0);
		assert(abs(e->f) <= e->c);
	}

	if (u->e > 0) {
			

		/* still some remaining so let u push more. */

		enter_excess(g, u);
	}

	if (v->e == d) {

		/* since v has d excess now it had zero before and
		 * can now push.
		 *
		 */

		enter_excess(g, v);
	}
	
}

static void relabel(graph_t* g, node_t* u)
{
	u->h += 1;

	pr("relabel %d now h = %d\n", id(g, u), u->h);

	enter_excess(g, u);
}

static void push_op(graph_t* g, node_t* u, node_t* v, edge_t* e, int flow) {
	int		d = flow;

	u->e -= d;
	v->e += d;
	
	if (u == e->u) {
		e->f += d;
	} else {
		e->f -= d;
	}

	pr("push from %d to %d: ", id(g, u), id(g, v));
	pr("f = %d, c = %d, so ", e->f, e->c);

	pr("pushing %d\n", d);
	/* the following are always true. */

	if (u != g->s && v != g->s) {
		assert(d >= 0);
		assert(u->e >= 0);
		assert(abs(e->f) <= e->c);
	}	
}


static node_t* other(node_t* u, edge_t* e)
{
	if (u == e->u)
		return e->v;
	else
		return e->u;
}

static void *work(void *arg) {
	node_t*		u; // selected node
	list_t*		p; // adj list for node u
	int		d;	/* remaining capacity of the edge. */

	node_t*		v = NULL; // currently pushing to
	edge_t*		e; // edge from u to v
	int			b; // current flow dir
	int nodes_worked_on = 0;

    threadarg_t* args = arg;
	graph_t* g = args->g;


	while (g->done != 1) {
		pr("i = %d\n", args->i);
		for (int j = 0; j < args->i; j++) {
			u = args->excess[j];
			pr("selected u = %d with ", id(g, u));
			pr("h = %d and e = %d\n", u->h, u->e);
			nodes_worked_on++;
			p = u->edge;

			while (p != NULL) {
				e = p->edge; 
				p = p->next;

				if (u == e->u) { 
					v = e->v;
					b = 1; 
				} else {
					v = e->u;
					b = -1;
				}

				if (u->h > v->h && b * e->f < e->c) // check height and check flow doesnt exceed capacity
					break;
				else
					v = NULL;
			}

			if (args->opi == args->opc) {
				args->opc *= 2;
				op_t** larger = realloc(args->ops, args->opc * sizeof args->ops[0]);
				if (larger == NULL) {
					error("no memory");
				}
				args->ops = larger;
			}
			
			op_t* op = malloc(sizeof(op_t));
			args->ops[args->opi] = op;
			
			if (v != NULL) {
				// push op
				op->push = 1;
				op->u = u;
				op->v = v;
				op->e = e;

				if (u == e->u) {
					d = MIN(u->e, e->c - e->f);
				} else {
					d = MIN(u->e, e->c + e->f);
				}

				op->flow = d;
				pr("push op created %d->%d with %d\n", id(g,u), id(g,v), d);
			} else {

				// relabel op
				op->push = 0;
				op->u = u;
				pr("relabel op created for %d with h %d\n", id(g,u), u->h);
			}
			args->opi += 1;
		}
		pthread_barrier_wait(&g->phase_one);
		pthread_barrier_wait(&g->phase_two);
	}

	printf("thread %ld terminating with %d nodes worked on\n", pthread_self(), nodes_worked_on);
}

int distribute_work(graph_t *g, threadarg_t* thread_args) {
	node_t*		u;
	int cycle = 0;
	
	while((u = leave_excess(g)) != NULL) {
		threadarg_t* t = &thread_args[cycle];
		int c = t->c;
		int i = t->i;
		pr("cycle = %d\n", cycle);
		if (i == c) {
			t->c *= 2;
			node_t** larger = realloc(t->excess, t->c * sizeof(node_t*));
			if (larger == NULL) {
				error("no memory");
			}

			t->excess = larger;
		}
		t->excess[i] = u;
		t->i++;
		cycle = (cycle + 1) % NBR_THREADS;
	}
}


int parallell_preflow(graph_t *g) {
	node_t*		s;
	node_t*		v;
	edge_t*		e;
	list_t*		p;
	int		b;
	
	s = g->s; // S is source
	s->h = g->n; // H of source is n

	p = s->edge;
	while (p != NULL) {
		e = p->edge;
		p = p->next;
		s->e += e->c;
		b += e->c;
		push(g, s, other(s, e), e); 
	}

	pthread_t thread[NBR_THREADS];
	threadarg_t thread_args[NBR_THREADS];

	for (int i = 0; i < NBR_THREADS; i += 1) { 
		threadarg_t* t = &thread_args[i];
		t->c = 16; // initial capacity
		t->excess = malloc(t->c * sizeof(node_t*));

		if (t->excess == NULL) {
				error("no memory");
		}

		t->i = 0;
		t->g = g;
		t->opc = 16; 
		t->opi = 0;
		t->ops = malloc(t->opc * sizeof t->ops[0]);
	}

	distribute_work(g, thread_args);

	for (int i = 0; i < NBR_THREADS; i += 1) { 
		if (pthread_create(&thread[i], NULL, (void*) work, &thread_args[i]) != 0)
			error("pthread_create failed");
	}

	pthread_barrier_wait(&g->phase_one);

	while(1) {
		for (int j = 0; j < NBR_THREADS; j++) {
			threadarg_t *t = &thread_args[j];
			int opi = t->opi; 
			for (int c = 0; c < opi; c++) {
				op_t* op = t->ops[c];

				if (op->push) {
					push_op(g, op->u, op->v, op->e, op->flow);

					if (op->u->e > 0) {
						enter_excess(g, op->u);
					}

					if (op->v->e == op->flow) {
						enter_excess(g, op->v);
					}
				} else {
					relabel(g, op->u);
				}
			}
			t->opi = 0;
			t->i = 0;
		}
		
		if (g->excess == NULL) {
			break;
		}
		distribute_work(g, thread_args);

		pr("distributing again\n");
		pthread_barrier_wait(&g->phase_two);
		
		pthread_barrier_wait(&g->phase_one);
	}

	g->done = 1;
	pr("done\n");
	pthread_barrier_wait(&g->phase_two);


	for (int i = 0; i < NBR_THREADS; i += 1) { 
		if (pthread_join(thread[i], NULL) != 0)
			error("pthread_join failed");
	}

	return g->t->e;
}

static void free_graph(graph_t* g)
{
	int			i;
	list_t*		p;
	list_t*		q;

	for (i = 0; i < g->n; i += 1) {
		p = g->v[i].edge;
		while (p != NULL) {
			q = p->next;
			free(p);
			p = q;
		}
	}
	pthread_barrier_destroy(&g->phase_one);
	pthread_barrier_destroy(&g->phase_two); 

	free(g->v);
	free(g->e);
	free(g);
}

int main(int argc, char* argv[])
{
	FILE*		in;	/* input file set to stdin	*/
	graph_t*	g;	/* undirected graph. 		*/
	int		f;	/* output from preflow.		*/
	int		n;	/* number of nodes.		*/
	int		m;	/* number of edges.		*/
	

	progname = argv[0];	/* name is a string in argv[0]. */

	in = stdin;		/* same as System.in in Java.	*/

	n = next_int();  /* nr nodes */
	m = next_int();  /* nr vertecies */

	/* skip C and P from the 6railwayplanning lab in EDAF05 */
	next_int();
	next_int();

	g = new_graph(in, n, m);

	fclose(in);

	f = parallell_preflow(g);

	printf("f = %d\n", f);

	free_graph(g);

	return 0;
}