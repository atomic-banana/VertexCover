# VertexCover
VertexCover

Authors : Carboni Sophie, Pecatte Valentin

Graph.hs contains graphs utility code.
VertexCover.hs contains the vertex covers utilities.

We didn't used a clever algorithm by lack of time, but our solution does works.
When we have more than one solution, we print only one of them.
The looped vertices are not taken into account and sorted out from parsing.


To launch our project, you must use this line :

stack run [FILE.hgr] [OUTPUT_FILENAME]

How to test the results (the examples below are corrects results for the given graphs -Beware, the last line doesn't come with a '\n', so the last number appear at the beinning of the next line-) :

```console
icy@icy:~/Desktop/Projets/VertexCover$ stack run data/ex_02.hgr test.vc

Computing...

PrintG
Just (G {getNVertices = 6, getNEdges = 6, getEdges = [(1,6),(5,6),(4,5),(3,4),(2,3),(1,2)]})icy@icy:~/Desktop/Projets/VertexCover$ stack run data/ex_02.hgr testcat test.vc 
s vc 6 3
5
3
icy@icy:~/Desktop/Projets/VertexCover$ stack run data/ex_03.hgr test.vc

Computing...

PrintG
Just (G {getNVertices = 5, getNEdges = 5, getEdges = [(1,5),(4,5),(3,4),(2,3),(1,2)]})icy@icy:~/Desktop/Projets/VertexCover$ cat test.vc 
s vc 5 3
4
2
1icy@icy:~/Desktop/Projets/VertexCover$ stack run data/ex_04.hgr test.vc

Computing...

PrintG
Just (G {getNVertices = 7, getNEdges = 9, getEdges = [(6,7),(3,6),(4,5),(4,6),(3,4),(2,4),(2,3),(2,6),(1,2)]})icy@icy:~/Desktop/Projets/VertexCover$ cat test.vc 
s vc 7 3
6
4
2icy@icy:~/Desktop/Projets/VertexCover$ stack run data/ex_05.hgr test.vc

Computing...

PrintG
Just (G {getNVertices = 9, getNEdges = 8, getEdges = [(6,8),(8,9),(7,8),(6,7),(3,5),(4,5),(3,4),(1,2)]})icy@icy:~/Desktop/Projets/VertexCover$ cat test.vc 
s vc 9 5
6
4
3
8
2icy@icy:~/Desktop/Projets/VertexCover$ stack run data/ex_06.hgr test.vc

Computing...

PrintG
Just (G {getNVertices = 7, getNEdges = 12, getEdges = [(6,7),(5,7),(4,7),(3,7),(2,7),(1,7),(1,6),(5,6),(4,5),(3,4),(2,3),(1,2)]})icy@icy:~/Desktop/Projets/VertexCover$ cat test.vc 
s vc 7 4
5
3
7
1icy@icy:~/Desktop/Projets/VertexCover$ stack run data/ex_07.hgr test.vc

Computing...

PrintG
Just (G {getNVertices = 7, getNEdges = 18, getEdges = [(4,6),(3,5),(2,4),(2,6),(1,3),(1,5),(6,7),(5,7),(4,7),(3,7),(2,7),(1,7),(1,6),(5,6),(4,5),(3,4),(2,3),(1,2)]})icy@icy:~/Desktop/Projets/VertexCover$ cat test.vc 
s vc 7 5
4
7
5
2
1icy@icy:~/Desktop/Projets/VertexCover$ 
```