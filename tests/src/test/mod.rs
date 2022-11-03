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
    assert_eq!(test1.get_field1(), 5);
}

#[test]
fn test_ord() {
    packed_int! {
        pub struct TestOrd: 32 {
            pub field1: 8,
            pub field2: 24
        }
    };
    let mut tests = vec![
        TestOrd::new(1, 50),
        TestOrd::new(2, 40),
        TestOrd::new(0, 1000),
    ];

    tests.sort();

    assert_eq!(
        tests,
        vec![
            TestOrd::new(0, 1000),
            TestOrd::new(1, 50),
            TestOrd::new(2, 40)
        ]
    );
}

#[test]
#[should_panic]
fn test_assertions() {
    packed_int! {
        pub struct TestAssertions: 32 {
            pub field1: 16,
            pub field2: 16,
        }
    };

    let _ = TestAssertions::new(u16::MAX as u32 + 1, 0);
}
