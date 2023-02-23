contract;

dep interface;
dep data_structures;

use interface::MyContract;
use data_structures::MyStruct;

storage {
    a: StorageMap<u64, MyStruct> = StorageMap {}
}

impl MyContract for Contract {
    #[storage(read)]
    fn test_function() -> Option<MyStruct> {
        storage.a.get(1)
    }
}