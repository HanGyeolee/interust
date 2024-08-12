# Interust
해당 라이브러리는 Rust 문법을 사용하는 인터프리터 엔진을 제공합니다.
### v0.1.1
사용 가능한 타입이 제한되어 있습니다.
쉽게 원시 타입을 추가할 수 있도록 각 타입의 바이트 코드 자리는 비워두었습니다.
#### 사용 가능한 변수 타입
- i64 : 64bit 부호 있는 정수 
- f64 : 64bit 부호 있는 실수
- bool : 1bit 논리 값(`true`|`false`)
- string : 최대 길이가 usize의 최대값인 문자열
#### 클래스 선언 지원
직접 class 를 선언할 수 있습니다.
public 과 private 그리고 static 을 구분합니다.

다만 Rust 언어와 약간의 차이가 있습니다.
```rust
struct A {
    pub public:i64,
    private:f64,
}
impl A {
    fn static_fn() { .. }
    fn private_fn(&self) { .. }
    pub fn public_fn(&self) { .. }
}
```
해당 코드는 다음과 같이 작성해야 합니다.
```rust 
class A {
    pub public:i64;
    private:f64;
    fn static_fn() { .. }
    fn private_fn(&self) { .. }
    pub fn public_fn(&self) { .. }
}
```
초기화는 Rust 언어와 동일합니다.
```rust
A {
    public: 0,
    private: 0
}
```

## Document
https://hangyeolee.github.io/interust/interust/index.html
