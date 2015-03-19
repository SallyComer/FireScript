

Def foo(x, y) {
    Return [x, y];
}

Def bar(x, y) {
    Return [y, x];
}


Def main() {
    x = 5;
    y = 0;
    Var z;
    If y {
        z = foo(x, y);
    };
    Else {
        z = bar(x, y);
    };
    Return z;
}