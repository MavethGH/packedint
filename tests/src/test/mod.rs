use packedint::packed_int;

#[test]
fn basic_test() {
    packed_int! {
        pub struct Test1: 32 {
            pub field1: 16,
            field2: 16
        }
    }

    let test1 = Test1::new(5, 10);
    assert_eq!(Test1::FIELD1_MASK, (1 << 16) - 1);
    assert_eq!(test1.get_field1(), 5);
}
