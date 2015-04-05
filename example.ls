






Def doThingWithReference(ref) {
    Var foo = (Take ref) + 1;
    Put ref foo;
}


Def doThingWithNormalValue(val) {
    Return val + 1;
}





Def main() {
    Var thingy = Ignite;
    Put thingy 1;
    print(Read thingy);
    doThingWithReference(thingy);
    print(Read thingy);
}