interface Equatable<T> {
}

implements Equatable<String> {
}

// Error: overlapping implementations
implements Equatable<String> {
}
