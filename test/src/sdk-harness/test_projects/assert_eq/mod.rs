use fuels::{prelude::*, tx::ContractId};

// Load abi from json
abigen!(Contract(
    name = "AssertEqContract",
    abi = "test_projects/assert_eq/out/debug/assert_eq-abi.json",
));

async fn get_contract_instance() -> (AssertEqContract, ContractId) {
    // Launch a local network and deploy the contract
    let mut wallets = launch_custom_provider_and_get_wallets(
        WalletsConfig::new(
            Some(1),             /* Single wallet */
            Some(1),             /* Single coin (UTXO) */
            Some(1_000_000_000), /* Amount per coin */
        ),
        None,
        None,
    )
    .await;
    let wallet = wallets.pop().unwrap();

    let id = Contract::deploy(
        "test_projects/assert_eq/out/debug/assert_eq.bin",
        &wallet,
        TxParameters::default(),
        StorageConfiguration::with_storage_path(Some(
            "test_projects/assert_eq/out/debug/assert_eq-storage_slots.json".to_string(),
        )),
    )
    .await
    .unwrap();

    let instance = AssertEqContract::new(id.clone(), wallet);

    (instance, id.into())
}

#[tokio::test]
#[should_panic(expected = "Revert(18446744073709486083)")]
async fn assert_eq_u64_can_fail() {
    let (instance, _id) = get_contract_instance().await;
    let methods = instance.methods();
    methods.failing_u64().call().await.unwrap();
}

#[tokio::test]
#[should_panic(expected = "Revert(18446744073709486083)")]
async fn assert_eq_u32_can_fail() {
    let (instance, _id) = get_contract_instance().await;
    let methods = instance.methods();
    methods.failing_u32().call().await.unwrap();
}

#[tokio::test]
#[should_panic(expected = "Revert(18446744073709486083)")]
async fn assert_eq_u16_can_fail() {
    let (instance, _id) = get_contract_instance().await;
    let methods = instance.methods();
    methods.failing_u16().call().await.unwrap();
}

#[tokio::test]
#[should_panic(expected = "Revert(18446744073709486083)")]
async fn assert_eq_u8_can_fail() {
    let (instance, _id) = get_contract_instance().await;
    let methods = instance.methods();
    methods.failing_u8().call().await.unwrap();
}

#[tokio::test]
#[should_panic(expected = "Revert(18446744073709486083)")]
async fn assert_eq_bool_can_fail() {
    let (instance, _id) = get_contract_instance().await;
    let methods = instance.methods();
    methods.failing_bool().call().await.unwrap();
}

#[tokio::test]
#[should_panic(expected = "Revert(18446744073709486083)")]
async fn assert_eq_address_can_fail() {
    let (instance, _id) = get_contract_instance().await;
    let methods = instance.methods();
    methods.failing_address().call().await.unwrap();
}

#[tokio::test]
#[should_panic(expected = "Revert(18446744073709486083)")]
async fn assert_eq_contract_id_can_fail() {
    let (instance, _id) = get_contract_instance().await;
    let methods = instance.methods();
    methods.failing_contract_id().call().await.unwrap();
}

#[tokio::test]
#[should_panic(expected = "Revert(18446744073709486083)")]
async fn assert_eq_b256_can_fail() {
    let (instance, _id) = get_contract_instance().await;
    let methods = instance.methods();
    methods.failing_b256().call().await.unwrap();
}

#[tokio::test]
#[should_panic(expected = "Revert(18446744073709486083)")]
async fn assert_eq_b512_can_fail() {
    let (instance, _id) = get_contract_instance().await;
    let methods = instance.methods();
    methods.failing_b512().call().await.unwrap();
}
