#include <iostream>
#include <vector>
#include <stdio.h> 


using namespace std; 
const int N = 1000000; 
  
// variables to be used 
// in both functions 
// maybe its better to be global...
vector<int> graph[N]; 
vector<int> cycles[3]; 
// we made this arrays global because they are big
int color[N];
int mark[N];
int par[N];

vector<int> countNumberNodes;
int arr[N];


/* classic mergeSort algorithm */
/* link: https://www.geeksforgeeks.org/merge-sort/ */
void merge(int arr[], int l, int m, int r) { 
    int i, j, k; 
    int n1 = m - l + 1; 
    int n2 =  r - m; 
    int L[n1], R[n2]; 
    for (i = 0; i < n1; i++) 
        L[i] = arr[l + i]; 
    for (j = 0; j < n2; j++) 
        R[j] = arr[m + 1+ j]; 
    i = 0;
    j = 0; 
    k = l; 
    while (i < n1 && j < n2) { 
        if (L[i] <= R[j]) { 
            arr[k] = L[i]; 
            i++; 
        } 
        else { 
            arr[k] = R[j]; 
            j++; 
        } 
        k++; 
    } 
    while (i < n1) { 
        arr[k] = L[i]; 
        i++; 
        k++; 
    } 
    while (j < n2) { 
        arr[k] = R[j]; 
        j++; 
        k++; 
    } 
} 
void mergeSort(int arr[], int l, int r) { 
    if (l < r) { 
        int m = l+(r-l)/2; 
        mergeSort(arr, l, m); 
        mergeSort(arr, m+1, r); 
        merge(arr, l, m, r); 
    } 
} 

  
// Function to mark the vertex with 
// different colors for different cycles 
/* link for dfs algorithm: https://www.geeksforgeeks.org/print-all-the-cycles-in-an-undirected-graph/ */
void dfs_cycle(int u, int p, int& cyclenumber) { 
  
    // already (completely) visited vertex. 
    if (color[u] == 2) { 
        return; 
    } 
    if(cyclenumber > 1) return;
  
    // seen vertex, but was not completely visited -> cycle detected. 
    // backtrack based on parents to find the complete cycle. 
    if (color[u] == 1) { 
  
        cyclenumber++; 
        int cur = p; 
        mark[cur] = cyclenumber; 
  
        // backtrack the vertex which are 
        // in the current cycle thats found 
        while (cur != u) { 
            cur = par[cur]; 
            mark[cur] = cyclenumber; 
        } 
        return; 
    } 
    par[u] = p; 
  
    // partially visited. 
    color[u] = 1; 
  
    // simple dfs on graph 
    for (int v : graph[u]) { 
  
        // if it has not been visited previously 
        if (v == par[u]) { 
            continue; 
        } 
        dfs_cycle(v, u, cyclenumber); 
        if(cyclenumber >1) return;
    } 
  
    // completely visited. 
    color[u] = 2; 
} 
 
void addEdge(int u, int v) { 
    graph[u].push_back(v); 
    graph[v].push_back(u); 
} 

// find the number of nodes of the sub-tree of coronograph
void findNumberNodes(int u, int &countNodes, int x) {
    if (color[u] == 3) { 
        return; 
    }
    else if(color[u] == 2) {
        ++countNodes;
        color[u] = 3;
        for(int v : graph[u]) { 
            if(v == x) continue;
            findNumberNodes(v, countNodes, x); 
        }
    }
}
  
// Function that returns a vector 
// that has the size of the sub-trees of coronograph
vector<int> printingGraph(int edges, int& cyclenumber) { 
  	vector<int> countNumberNodes;
    // push the edges that into the 
    // cycle adjacency list 
    for (int i = 1; i <= edges; i++) { 
        if (mark[i] != 0) {
            cycles[mark[i]].push_back(i); 
        }
    }   
    printf("CORONA %lu\n", cycles[1].size());


    for (int x : cycles[1]) {
        int countNodes = 0;
        for(unsigned int i=0; i<graph[x].size(); i++) {
            if(mark[graph[x][i]] == 1) {
            	continue;
            }
            else {
            	findNumberNodes(graph[x][i], countNodes, x);
            }
        }
        countNumberNodes.push_back(countNodes+1);
    }
    return countNumberNodes;
} 
  
int main(int argc, char *argv[] ) {
    if(argc != 2) {
        printf("Incorrect usage. Try: ./coronograph \"filename.txt\"\n");
        exit(1);
    }
    FILE* ptr = fopen(argv[1],"r"); 
    if (ptr==NULL) 
    { 
        printf("no such file.\n"); 
        return 1; 
    }
    int n;
    if(fscanf(ptr,"%d",&n) != 1) {
        printf("mistake on reading\n");
        return 1;
    } 
    for(int i=0; i<n; i++) {
        int nodes=0, edges=0;
         if(fscanf(ptr,"%d %d\n",&nodes,&edges) != 2)   {
            printf("mistake on reading\n");
            return 1;
        }         

        // setup the global arrays so that they are empty
        for (int k=0; k<3; k++) { 
        	unsigned int a = cycles[k].size();
            for(unsigned l=0; l<a; l++) {
                cycles[k].pop_back();
            }
        }
        for (int k=0; k<nodes+1; k++) { 
        	unsigned int a = graph[k].size();
            for(unsigned l=0; l<a; l++) {
                graph[k].pop_back();
            }
        }
        for(int j=0; j<edges; j++) {
            int node1, node2;
            if(fscanf(ptr,"%d %d\n",&node1,&node2) != 2)   {
                printf("mistake on reading\n");
                return 1;
            }   
            addEdge(node1,node2);
        }
        if(nodes == edges) {
            /* method used in: https://www.geeksforgeeks.org/print-all-the-cycles-in-an-undirected-graph/ */
        
            for(int k=0; k<nodes+1; k++) {
                mark[k] = 0;
                color[k] = 0;
                par[k] = 0;
            }
            int cyclenumber = 0;

            dfs_cycle(1, 0, cyclenumber); // complexity: O(N+M) = O(N)

            // testing connectivity of graph
            // coronograph needs connectivity
            for(int j=1; j<nodes+1; j++) {
                if(color[j] == 0) {
                    // giving cyclenumber a random integer so that it won't go into the next if statement
                    cyclenumber = 42; 
                    break;
                }
            }
            

            

            // handling a coronograph
            if(cyclenumber == 1) {
                countNumberNodes = printingGraph(edges, cyclenumber);
                std::copy(countNumberNodes.begin(), countNumberNodes.end(), arr);
                mergeSort(arr, 0, (countNumberNodes.size() - 1));
                for(unsigned j=0; j<countNumberNodes.size()-1; j++) {
                    printf("%d ", arr[j]);
                }
                printf("%d\n", arr[countNumberNodes.size()-1]);
                unsigned int a = countNumberNodes.size();
                for(unsigned l=0; l<a; l++) {
                    countNumberNodes.pop_back();
                }

            }    
            else {
                printf("NO CORONA\n");
            }
        }
        else  {
            printf("NO CORONA\n");
        }

        
    }
    fclose(ptr);
}