/**
RUN: %scc %s 2>&1 | filecheck %s
*/

int main() {
    // CHECK: error: declaration of block scope identifier with linkage can't have an initializer
    extern int a = 4;
}