

Class _Foo {
    Def doThing(self, thing) {
        print(thing);
        Return null;
    }
    Def barf(self) {
        print("adsf");
        Return null;
    }
}



Class _Bar {
    Def __init__(self, x, y) {
        self = merge(self, _Foo());
        self.x = x;
        self.y = y;
        Return self;
    }
    Def chirp(self) {
        print("chirp chirp");
        Return null;
    }
}
Def main() {
    Var testThing = _Bar(1, 2);
    testThing.chirp();
    testThing.doThing(2);
    Return null;
}