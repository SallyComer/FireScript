












Class listIter {
    Def __init__(self, list) {
        self.list = list;
        Var length = list.length();
        Var pipe = Ignite;
        self.pipe = pipe;
        self.thing = (Lambda () {
            Var foo = 0;
            While (foo < length) {
                Put pipe, (list !! foo);
                foo =+ 1;
            }
        }) ;
        self.doer = Spark self.thing();
        Return self;
    }
    Def next(self) {
        Return (Take (self.pipe));
    }
}




Def main() {
    Var blah = ["a", "b", "c", "d"] ;
    Var dingus = listIter(blah);
    print(dingus.next());
    print(dingus.next());
    print(dingus.next());
    print(dingus.next());
}