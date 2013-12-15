#include<iostream>
using namespace std;


int main(){
    int g,m,n;
    cin >> n >> m;
    cout<<n<<" "<<m<<endl;
    for(int i=0;i<m;i++){
        g = rand()%n;
        cout<<g<<" ";
        g = rand()%n;
        cout<<g<<endl;
    }
    
    return 0;
}

