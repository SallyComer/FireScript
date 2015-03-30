

Class _Tree {
    Def __init__(self, left, right) {
        self.left = left;
        self.right = right;
        self.isTree = 1;
        Return self;
    }
    Def fmap(self, func) {
        If (isATree(self.left)) {
            self.left = self.left.fmap(func);
        }
        Else {
            self.left = func(self.left);
        };
        If (isATree(self.right)) {
            self.right = self.right.fmap(func);
        };
        Else {
            self.right = func(self.right);
        };
        Return self;
    }
    Def toString(self) {
        Var result = "Tree(";
        If (isATree(self.left)) {
            result =+ (self.left.toString());
        };
        Else {
            result =+ (self.left);
        };
        result =+ ", ";
        If (isATree(self.right)) {
            result =+ (self.right.toString());
        };
        Else {
            result =+ (self.right);
        };
        result =+ ")";
        Return result;
    }

    
}

Def isATree(thing) {
    Return nameExists(thing, "isTree");
}

Def succ(x) {
    Return x + 1;
}


Def main() {
    Var test = _Tree(_Tree(1,_Tree(3,5)),_Tree(_Tree(_Tree(12,0),7),_Tree(46,9)));
    print(test.toString());
    test=.fmap(succ);
    print(test.toString());
    Return null;
}