function* a() {
  ({b() {
    yield;
  }});
}
