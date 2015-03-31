




Def doThing(flag, pipe) {
    Var chirp = Read flag;
    Var count = 0;
    While (chirp) {
        print("putting");
        Put pipe count;
        print("checking flag");
        chirp = Take flag;
        print("checked flag");
        count = count + 1;
    }
    Return count;
}


Def main() {
    While (0) {
        print("there is a problem!");
    }
    Var flag = Ignite;
    Var pipe = Ignite;
    Put flag 1;
    Var done = Spark doThing(flag, pipe);
    Var out = Take pipe;
    While ( out < 10 ) {
        Put flag 1;
        print("things!");
        out = Take pipe;
    }
    Put flag 0;
    @Take pipe;
    Var foo = Take done;
    print(foo);

    Return null;
}