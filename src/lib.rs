fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod test {
    #[test]
    fn can_tun_a_test() {
        assert!(true);
    }
}
