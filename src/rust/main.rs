mod levm;

fn main() {
    println!(">>> rust levm test.");
    levm::initialize();
    levm::interprete(vec![1, 2, 3, 4]);
}
