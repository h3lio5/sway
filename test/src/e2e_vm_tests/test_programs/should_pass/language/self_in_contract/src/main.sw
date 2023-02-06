contract;

trait MySuperTrait {
    fn foo();
}

trait MyTrait : MySuperTrait {
    fn bar();
} {
    fn baz() {
        Self::foo()      // Self should mean MyTrait here?
    }
}
