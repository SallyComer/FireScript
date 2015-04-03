

Def doStuffWithEmber(ember, flag) {
    While (1) {
        Var bleh = Take ember;
        print(bleh);
        Put flag 1;
    }
}



Def main() {
    Var foo = Ignite;
    Var flag = Ignite;
    Var horcrux = Spark doStuffWithEmber(foo, flag);
    Put foo 1;
    Take flag;
    Put foo "I'm not doing two things at once, because of a semaphore";
    Take flag;
    Put foo 2;
    Take flag;
    Kill horcrux;
}