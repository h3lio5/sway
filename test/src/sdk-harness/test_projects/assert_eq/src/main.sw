contract;

use std::{b512::B512, bytes::Bytes};

abi AssertEq {
    fn failing_u64() -> bool;
    fn failing_u32() -> bool;
    fn failing_u16() -> bool;
    fn failing_u8() -> bool;
    fn failing_bool() -> bool;
    fn failing_address() -> bool;
    fn failing_contract_id() -> bool;
    fn failing_b256() -> bool;
    fn failing_b512() -> bool;
}

impl AssertEq for Contract {
    fn failing_u64() -> bool {
        assert_eq(1, 2);
        false
    }

    fn failing_u32() -> bool {
        assert_eq(1u32, 2u32);
        false
    }

    fn failing_u16() -> bool {
        assert_eq(1u16, 2u16);
        false
    }

    fn failing_u8() -> bool {
        assert_eq(1u8, 2u8);
        false
    }

    fn failing_bool() -> bool {
        assert_eq(true, false);
        false
    }

    fn failing_address() -> bool {
        let first = Address::from(0x0000000000000000000000000000000000000000000000000000000000000000);
        let second = Address::from(0x0000000000000000000000000000000000000000000000000000000000000001);
        assert_eq(first, second);
        false
    }

    fn failing_contract_id() -> bool {
        let first = ContractId::from(0x0000000000000000000000000000000000000000000000000000000000000000);
        let second = ContractId::from(0x0000000000000000000000000000000000000000000000000000000000000001);
        assert_eq(first, second);
        false
    }

    fn failing_b256() -> bool {
        assert_eq(0x0000000000000000000000000000000000000000000000000000000000000000, 0x0000000000000000000000000000000000000000000000000000000000000001);
        false
    }

    fn failing_b512() -> bool {
        let first = B512::from((0x0000000000000000000000000000000000000000000000000000000000000000, 0x0000000000000000000000000000000000000000000000000000000000000000));
        let second = B512::from((0x0000000000000000000000000000000000000000000000000000000000000000, 0x0000000000000000000000000000000000000000000000000000000000000001));
        assert_eq(first, second);
        false
    }
}
