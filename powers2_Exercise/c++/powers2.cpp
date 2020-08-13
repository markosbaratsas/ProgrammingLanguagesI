#include <iostream>
#include <vector>
#include <fstream>
#include <string>

using namespace std;


vector<int> decimal_to_binary(int n, int &k_original) {
	vector<int> bin;
	while (n > 0) {   
        if(n%2 == 1) ++k_original;
        // pushing binary bit into vector 
        bin.push_back(n%2); 
        n = n/2; 
    }
    return bin;
} 


int main(int argc, char *argv[] ) {
	if(argc != 2) {
		cout << "Incorrect usage. Try: ./powers2 \"filename.txt\"" << endl;
		exit(1);
	}
	ifstream myfile;
	myfile.open(argv[1]);
	int n;
	myfile >> n;
	int a[n][2];
	for(int i=0; i<n; i++) {
		myfile >> a[i][0] >> a[i][1];
	}
	myfile.close();
	for(int i=0; i<n; i++) {
		vector<int> b;
		int k=0;
		b = decimal_to_binary(a[i][0], k);
		if(a[i][1]>a[i][0] || a[i][1]<k)
			cout << "[]" << endl;
		else {
			for(int j=0; j<a[i][1]-k; j++) {
				for(unsigned x=1; x<b.size(); x++) {
					if(b[x] > 0 && x<(b.size()-1)) {
						b[x] -= 1;
						b[x-1] += 2;
						break;
					}
					else if(b[x] == 1 && x==(b.size()-1)) {
						b[x-1] += 2;
						b.pop_back();
						break;
					}
					else if(b[x] > 1 && x==(b.size()-1)) {
						b[x-1] += 2;
						b[x] -= 1;
						break;
					}
				}
			}				
			cout << "[";
			for(unsigned j=0; j<b.size()-1; j++)
				cout << b[j] << ",";
			cout << b[b.size()-1] << "]" << endl; 
		}
	}
}
