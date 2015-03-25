

Class _Dingus {
    Def __call__(self, a, b) {
        print(a);
        print(b);
        Return (a + b);
    }
}
Def main() {
    Var testThing = _Dingus();
    Var result = reduce(testThing, [1, 2, 3, 4]);
    print(result);
    Return null;
}