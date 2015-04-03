




Class _State {
    Def __init__(self, value, baggage) {
        self.value = value;
        self.baggage = baggage;
        Return self;
    }
    Def fmap(self, func) {
        self.value = func(self.value);
        Return self;
    }
    Def bind(self, func) {
        Return func(self.value);
    }
    Def put(self, val) {
        self.baggage = val;
        Return self;
    }
    Def alter(self, func) {
        self.baggage = func(self.baggage);
        Return self;
    }
    Def swap(self) {
        Var scratch = self.value;
        self.value = self.baggage;
        self.baggage = scratch;
        Return self;
    }
    Def state(self) {
        Return self.baggage;
    }
}

Operator f <$> thing {
    Return thing.fmap(f);
}

Operator f << val {
    print("chirp chirp");
    Return val.bind(f);
}



Def state(val) {
    Return _State(val, null);
}

Def main() {
    Var blah = state(1);
    blah=.put("ASDF");
    Var thing = (Lambda (x) {Return x + 1;}) <$> (Lambda (x) {Return x * 7;}) <$> blah;
    Var chirp = (Lambda (x) {Return _State(x, "FDSA");}) << thing;
    print(chirp.value);
    print(chirp.baggage);
}