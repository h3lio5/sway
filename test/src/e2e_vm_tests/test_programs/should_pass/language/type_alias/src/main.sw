script;

struct MyType {
    x: AssetId,
}

type MyTypeAlias1 = MyType;

type MyTypeAlias2 = MyTypeAlias1;

fn foo(x: AssetId) { 
}

fn main() -> bool {
    let x = AssetId { value: 0x0000000000000000000000000000000000000000000000000000000000000001 };
    let y: AssetId = x;
    let z = AssetId::from(0x0000000000000000000000000000000000000000000000000000000000000001);
    foo(x); 
    let t = MyTypeAlias1 { 
        x: AssetId { 
            value:  0x0000000000000000000000000000000000000000000000000000000000000001
        } 
    };
    x == z && t.x.value == y.value && z.value == y.value
}
