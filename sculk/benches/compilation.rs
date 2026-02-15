use criterion::{criterion_group, criterion_main, Criterion};

fn noop_benchmark(c: &mut Criterion) {
    c.bench_function("noop", |b| b.iter(|| 1 + 1));
}

criterion_group!(benches, noop_benchmark);
criterion_main!(benches);
